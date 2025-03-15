# app_ubcf.R
# Standalone Shiny app for User-Based Collaborative Filtering

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(recommenderlab)
library(plotly)

# Load UBCF model and movie data
ubcf_model <- readRDS("ubcf_model.rds")
content_model <- readRDS("content_based_model.rds")  # Just for accessing the movies data

# Extract movies data
movies <- content_model$movies

# Get the exact movie IDs used in the UBCF model
model_item_ids <- colnames(getModel(ubcf_model)$data)

# Create a sampler of popular movies for users to rate
popular_movies <- movies %>%
  filter(movieId %in% as.numeric(model_item_ids)) %>%  # Ensure the movies exist in the model
  filter(grepl("Action|Comedy|Drama|Romance|Sci-Fi|Thriller", genres)) %>%
  sample_n(20)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "UBCF Movie Recommender"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Rate Movies", tabName = "rate", icon = icon("star")),
      menuItem("Recommendations", tabName = "recommendations", icon = icon("film")),
      menuItem("Model Info", tabName = "info", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Rating Tab
      tabItem(tabName = "rate",
              fluidRow(
                box(
                  width = 12,
                  title = "Rate Movies to Get Recommendations",
                  status = "primary",
                  solidHeader = TRUE,
                  p("Rate at least 5 movies to get personalized recommendations."),
                  p("Higher ratings (4-5) have more influence on your recommendations.")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Your Movie Ratings",
                  status = "info",
                  solidHeader = TRUE,
                  uiOutput("rating_inputs"),
                  actionButton("get_recs", "Get Recommendations", 
                               class = "btn-success btn-lg",
                               style = "margin-top: 20px;")
                )
              )
      ),
      
      # Recommendations Tab
      tabItem(tabName = "recommendations",
              fluidRow(
                valueBoxOutput("rating_count", width = 4),
                valueBoxOutput("top_genre", width = 4),
                valueBoxOutput("rec_count", width = 4)
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Your Personalized Recommendations",
                  status = "success",
                  solidHeader = TRUE,
                  DTOutput("recommendations_table"),
                  uiOutput("no_recs_message")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Genre Distribution in Recommendations",
                  status = "warning",
                  solidHeader = TRUE,
                  plotlyOutput("genre_distribution")
                )
              )
      ),
      
      # Model Info Tab
      tabItem(tabName = "info",
              fluidRow(
                box(
                  width = 12,
                  title = "User-Based Collaborative Filtering Model Information",
                  status = "info",
                  solidHeader = TRUE,
                  h4("Model Details:"),
                  verbatimTextOutput("model_summary"),
                  h4("How UBCF Works:"),
                  p("User-Based Collaborative Filtering finds users with similar tastes to you and recommends movies they enjoyed."),
                  p("The algorithm follows these steps:"),
                  tags$ol(
                    tags$li("Calculate similarity between your ratings and those of other users"),
                    tags$li("Identify users with tastes most similar to yours"),
                    tags$li("Recommend movies those similar users rated highly that you haven't seen")
                  ),
                  h4("Model Performance Metrics:"),
                  tableOutput("metrics_table")
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Generate rating inputs
  output$rating_inputs <- renderUI({
    lapply(1:nrow(popular_movies), function(i) {
      movie <- popular_movies[i, ]
      
      div(
        style = "margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #eee;",
        fluidRow(
          column(
            width = 8,
            div(
              h4(movie$title, style = "margin-top: 0;"),
              p(paste("Genres:", gsub("\\|", ", ", movie$genres)))
            )
          ),
          column(
            width = 4,
            sliderInput(
              inputId = paste0("rating_", movie$movieId),
              label = "Your Rating:",
              min = 0,
              max = 5,
              value = 0,
              step = 0.5,
              ticks = FALSE
            )
          )
        )
      )
    })
  })
  
  # Reactive value to store recommendations
  recommendations <- reactiveVal(NULL)
  
  # Get user ratings
  get_user_ratings <- reactive({
    # Find all rating inputs
    rating_inputs <- names(input)[grepl("^rating_", names(input))]
    
    # Create a data frame of ratings
    ratings_df <- data.frame(
      userId = 9999,  # Dummy user ID
      movieId = as.numeric(gsub("rating_", "", rating_inputs)),
      rating = sapply(rating_inputs, function(x) input[[x]]),
      stringsAsFactors = FALSE
    )
    
    # Filter out unrated movies
    ratings_df <- ratings_df %>% filter(rating > 0)
    
    return(ratings_df)
  })
  
  # Generate recommendations when button is clicked
  observeEvent(input$get_recs, {
    # Get user ratings
    user_ratings_df <- get_user_ratings()
    
    # Check if enough movies are rated
    if(nrow(user_ratings_df) < 3) {
      showNotification("Please rate at least 3 movies to get recommendations", 
                       type = "warning")
      recommendations(NULL)
      return()
    }
    
    # Create new user ratings matrix matching the model dimensions
    ratings_matrix <- matrix(NA, nrow = 1, ncol = length(model_item_ids))
    colnames(ratings_matrix) <- model_item_ids
    rownames(ratings_matrix) <- "9999"  # Dummy user ID
    
    # Fill in user ratings
    for(i in 1:nrow(user_ratings_df)) {
      movie_id_str <- as.character(user_ratings_df$movieId[i])
      if(movie_id_str %in% model_item_ids) {
        ratings_matrix[1, movie_id_str] <- user_ratings_df$rating[i]
      }
    }
    
    # Convert to realRatingMatrix
    user_ratings_matrix <- as(ratings_matrix, "realRatingMatrix")
    
    # Generate recommendations
    tryCatch({
      # Get predictions
      preds <- predict(ubcf_model, 
                       newdata = user_ratings_matrix, 
                       n = 10, 
                       type = "ratings")
      
      # Convert to data frame
      recs_df <- as(preds, "data.frame")
      
      if(nrow(recs_df) > 0) {
        # Format recommendations
        recs <- recs_df %>%
          rename(movieId = item, score = rating) %>%
          mutate(movieId = as.numeric(as.character(movieId))) %>%
          left_join(movies[, c("movieId", "title", "genres")], by = "movieId") %>%
          arrange(desc(score))
        
        # Store recommendations
        recommendations(recs)
        
        # Switch to recommendations tab
        updateTabItems(session, "sidebarItemExpanded", "recommendations")
        
        # Success message
        showNotification("Recommendations generated successfully!", type = "message")
      } else {
        # No recommendations found
        recommendations(NULL)
        showNotification("No recommendations found. Try rating more diverse movies.", 
                         type = "warning")
      }
    }, error = function(e) {
      # Error handling
      recommendations(NULL)
      showNotification(paste("Error generating recommendations:", e$message), 
                       type = "error")
    })
  })
  
  # Display recommendations
  output$recommendations_table <- renderDT({
    recs <- recommendations()
    
    if(!is.null(recs) && nrow(recs) > 0) {
      recs %>%
        mutate(genres = gsub("\\|", ", ", genres),
               score = round(score, 2)) %>%
        select(title, genres, score) %>%
        rename(Title = title, Genres = genres, Score = score)
    }
  })
  
  # Display message when no recommendations
  output$no_recs_message <- renderUI({
    recs <- recommendations()
    
    if(is.null(recs)) {
      div(
        style = "text-align: center; padding: 20px;",
        h4("No recommendations available yet"),
        p("Rate some movies and click 'Get Recommendations' to generate personalized suggestions.")
      )
    }
  })
  
  # Value boxes
  output$rating_count <- renderValueBox({
    user_ratings <- get_user_ratings()
    
    valueBox(
      nrow(user_ratings),
      "Movies Rated",
      icon = icon("check"),
      color = if(nrow(user_ratings) >= 5) "green" else "orange"
    )
  })
  
  output$top_genre <- renderValueBox({
    user_ratings <- get_user_ratings()
    
    if(nrow(user_ratings) > 0) {
      # Get genres of rated movies
      rated_genres <- movies %>%
        filter(movieId %in% user_ratings$movieId) %>%
        pull(genres) %>%
        paste(collapse = "|") %>%
        strsplit("\\|") %>%
        unlist()
      
      # Count genres
      genre_counts <- table(rated_genres)
      top_genre <- names(genre_counts)[which.max(genre_counts)]
      
      valueBox(
        top_genre,
        "Your Favorite Genre",
        icon = icon("film"),
        color = "blue"
      )
    } else {
      valueBox(
        "Unknown",
        "Your Favorite Genre",
        icon = icon("question"),
        color = "blue"
      )
    }
  })
  
  output$rec_count <- renderValueBox({
    recs <- recommendations()
    rec_count <- if(!is.null(recs)) nrow(recs) else 0
    
    valueBox(
      rec_count,
      "Recommendations",
      icon = icon("thumbs-up"),
      color = if(rec_count > 0) "green" else "red"
    )
  })
  
  # Genre distribution plot
  output$genre_distribution <- renderPlotly({
    recs <- recommendations()
    
    if(!is.null(recs) && nrow(recs) > 0) {
      # Extract all genres from recommendations
      all_genres <- recs$genres %>%
        paste(collapse = "|") %>%
        strsplit("\\|") %>%
        unlist()
      
      # Count genres
      genre_counts <- as.data.frame(table(all_genres))
      colnames(genre_counts) <- c("Genre", "Count")
      
      # Sort by count
      genre_counts <- genre_counts %>%
        arrange(desc(Count))
      
      # Plot
      p <- ggplot(genre_counts, aes(x = reorder(Genre, -Count), y = Count, fill = Genre)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = "Genres in Your Recommendations", x = "Genre", y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
      
      ggplotly(p)
    }
  })
  
  # Model summary
  output$model_summary <- renderPrint({
    cat("UBCF Model Parameters:\n")
    cat("- Method:", getModel(ubcf_model)$method, "\n")
    cat("- Number of users in training:", dim(getModel(ubcf_model)$data)[1], "\n")
    cat("- Number of items (movies):", dim(getModel(ubcf_model)$data)[2], "\n")
    cat("- Normalization:", getModel(ubcf_model)$normalize$method, "\n")
    cat("- Sample:", getModel(ubcf_model)$sample, "\n")
  })
  
  # Metrics table
  output$metrics_table <- renderTable({
    data.frame(
      Metric = c("Precision", "Recall", "F1 Score", "RMSE", "MAE"),
      Value = c(0.18, 0.0188, 0.0295, 0.95, 0.75)  # Example values - replace with your actual metrics
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)