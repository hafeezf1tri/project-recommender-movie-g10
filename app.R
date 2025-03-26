# app.R
# Complete Movie Recommender System Dashboard
# For Data Science Project TEB2043

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)
library(recommenderlab)  # Needed for UBCF and IBCF models
library(rsconnect)


# Load the recommendation models
content_model <- readRDS("content_based_model.rds")
ibcf_model <- readRDS("ibcf_model.rds")
ubcf_model <- readRDS("ubcf_model.rds")

# Extract movie data from content model
movies <- content_model$movies

# Get the exact movie IDs used in the recommenderlab models
ubcf_item_ids <- colnames(getModel(ubcf_model)$data)
ibcf_item_ids <- colnames(getModel(ibcf_model)$data)

# Create a sampler of popular movies for users to rate
# Using intersection of movies that exist in all models
valid_movie_ids <- Reduce(intersect, list(
  as.character(movies$movieId),
  ubcf_item_ids,
  ibcf_item_ids
))

# Sample popular/diverse movies across genres for rating
tryCatch({
  popular_movies <- movies %>%
    filter(movieId %in% as.numeric(valid_movie_ids))
  
  # Check if we have movies after filtering
  if(nrow(popular_movies) == 0) {
    # If no movies match the filter, use all movies
    popular_movies <- movies
  }
  
  popular_movies <- popular_movies %>%
    filter(grepl("Action|Adventure|Comedy|Drama|Romance|Sci-Fi|Thriller", genres))
  
  # Check again after genre filtering
  if(nrow(popular_movies) < 10) {
    # If too few movies after genre filtering, loosen the genre filter
    popular_movies <- movies %>%
      filter(movieId %in% as.numeric(valid_movie_ids))
  }
  
  # Now sample with a safety check
  if(nrow(popular_movies) > 0) {
    popular_movies <- popular_movies %>%
      sample_n(min(50, nrow(popular_movies)), replace = FALSE)
  } else {
    # Fallback to using all movies if everything else fails
    popular_movies <- movies %>%
      head(50)
  }
}, error = function(e) {
  # Error handler in case something goes wrong
  message("Error in sampling movies: ", e$message)
  popular_movies <- movies %>%
    head(50)
})

# UI Definition
ui <- dashboardPage(
  dashboardHeader(
    title = "Movie Recommender System"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Content-Based Filtering", tabName = "content", icon = icon("film")),
      menuItem("Item-Based CF", tabName = "ibcf", icon = icon("th")),
      menuItem("User-Based CF", tabName = "ubcf", icon = icon("users")),
      menuItem("Compare Models", tabName = "compare", icon = icon("chart-bar")),
      menuItem("About", tabName = "about", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Introduction Tab
      tabItem(tabName = "intro",
              fluidRow(
                box(
                  width = 12,
                  title = "Movie Recommender System",
                  status = "primary",
                  solidHeader = TRUE,
                  h3("Welcome to the Movie Recommender System Dashboard!"),
                  p("This interactive dashboard demonstrates different approaches to movie recommendations:"),
                  tags$ul(
                    tags$li(strong("Content-Based Filtering:"), "Recommends movies similar to ones you like based on movie features like genres"),
                    tags$li(strong("Item-Based Collaborative Filtering:"), "Recommends movies that are similar to ones you like based on how users have rated them"),
                    tags$li(strong("User-Based Collaborative Filtering:"), "Recommends movies that similar users have enjoyed")
                  ),
                  p("To get started, select one of the recommendation approaches from the sidebar and rate some movies or explore similar movies.")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Dataset Overview",
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("datasetSummary"),
                  p("The MovieLens dataset contains user ratings for movies. This visualization shows the distribution of ratings and movie genres.")
                )
              )
      ),
      
      # Content-Based Filtering Tab
      tabItem(tabName = "content",
              fluidRow(
                box(
                  width = 4,
                  title = "Find Similar Movies",
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("content_movie", "Select a Movie:", 
                              choices = setNames(movies$movieId, movies$title)[order(movies$title)],
                              selected = movies$movieId[which.min(movies$title)]),
                  numericInput("content_n", "Number of Recommendations:", 10, min = 1, max = 20),
                  actionButton("content_recommend", "Find Similar Movies", 
                               class = "btn-primary")
                ),
                box(
                  width = 8,
                  title = "Your Movie Selection",
                  status = "info",
                  solidHeader = TRUE,
                  htmlOutput("content_selected_movie")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Similar Movies (Content-Based)",
                  status = "success",
                  solidHeader = TRUE,
                  DTOutput("content_recommendations")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Similarity Visualization",
                  status = "warning",
                  solidHeader = TRUE,
                  plotlyOutput("content_similarity_plot")
                )
              )
      ),
      
      # Item-Based Collaborative Filtering Tab
      tabItem(tabName = "ibcf",
              fluidRow(
                box(
                  width = 4,
                  title = "Your Movie Ratings",
                  status = "primary",
                  solidHeader = TRUE,
                  p("Rate some movies to get personalized recommendations:"),
                  uiOutput("ibcf_rating_inputs"),
                  actionButton("ibcf_recommend", "Get Recommendations", 
                               class = "btn-primary")
                ),
                box(
                  width = 8,
                  title = "Rating Instructions",
                  status = "info",
                  solidHeader = TRUE,
                  p("Rate movies on a scale of 1-5:"),
                  tags$ul(
                    tags$li("1 star: Disliked it a lot"),
                    tags$li("3 stars: It was okay"),
                    tags$li("5 stars: Loved it!")
                  ),
                  p("The more movies you rate, the better the recommendations will be!")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Your Recommendations (Item-Based)",
                  status = "success",
                  solidHeader = TRUE,
                  DTOutput("ibcf_recommendations")
                )
              )
      ),
      
      # User-Based Collaborative Filtering Tab
      tabItem(tabName = "ubcf",
              fluidRow(
                box(
                  width = 4,
                  title = "Your Movie Ratings",
                  status = "primary",
                  solidHeader = TRUE,
                  p("Rate some movies to get personalized recommendations:"),
                  uiOutput("ubcf_rating_inputs"),
                  actionButton("ubcf_recommend", "Get Recommendations", 
                               class = "btn-primary")
                ),
                box(
                  width = 8,
                  title = "How User-Based CF Works",
                  status = "info",
                  solidHeader = TRUE,
                  p("User-Based Collaborative Filtering finds users similar to you based on rating patterns."),
                  p("It then recommends movies that these similar users enjoyed but you haven't seen yet."),
                  p("This approach is great for discovering new and unexpected recommendations.")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Your Recommendations (User-Based)",
                  status = "success",
                  solidHeader = TRUE,
                  DTOutput("ubcf_recommendations")
                )
              )
      ),
      
      # Compare Models Tab
      tabItem(tabName = "compare",
              fluidRow(
                box(
                  width = 12,
                  title = "Comparison of Recommendation Models",
                  status = "primary",
                  solidHeader = TRUE,
                  p("This section compares recommendations from all three approaches for the same set of user preferences.")
                )
              ),
              fluidRow(
                box(
                  width = 4,
                  title = "Your Movie Ratings",
                  status = "info",
                  solidHeader = TRUE,
                  uiOutput("compare_rating_inputs"),
                  actionButton("compare_recommend", "Compare Recommendations", 
                               class = "btn-primary")
                ),
                box(
                  width = 8,
                  title = "Performance Metrics",
                  status = "warning",
                  solidHeader = TRUE,
                  plotlyOutput("compare_metrics")
                )
              ),
              fluidRow(
                tabBox(
                  width = 12,
                  title = "Recommendations Comparison",
                  id = "compare_tabs",
                  tabPanel("Content-Based", DTOutput("compare_content")),
                  tabPanel("Item-Based CF", DTOutput("compare_ibcf")),
                  tabPanel("User-Based CF", DTOutput("compare_ubcf")),
                  tabPanel("Overlap Analysis", plotlyOutput("compare_overlap"))
                )
              )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  width = 12,
                  title = "About This Project",
                  status = "primary",
                  solidHeader = TRUE,
                  h3("Recommender Systems Dashboard"),
                  p("This project was developed as part of the TEB2043 Data Science course."),
                  h4("Data Source:"),
                  p("This application uses the MovieLens dataset from GroupLens Research."),
                  p("Citation: F. Maxwell Harper and Joseph A. Konstan. 2015. The MovieLens Datasets: History and Context. ACM Transactions on Interactive Intelligent Systems (TiiS) 5, 4: 19:1–19:19."),
                  h4("Recommendation Approaches:"),
                  tags$ul(
                    tags$li(strong("Content-Based Filtering:"), "Uses movie features like genres and tags to find similar movies"),
                    tags$li(strong("Item-Based Collaborative Filtering:"), "Finds similar movies based on user rating patterns"),
                    tags$li(strong("User-Based Collaborative Filtering:"), "Finds similar users and suggests movies they enjoyed")
                  ),
                  h4("Performance Metrics:"),
                  tableOutput("model_metrics"),
                  
                  
                  
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Dataset summary plot
  output$datasetSummary <- renderPlotly({
    # Count genres
    genre_counts <- data.frame(
      genre = unlist(strsplit(movies$genres, "\\|"))
    ) %>%
      count(genre) %>%
      arrange(desc(n)) %>%
      head(10)
    
    p <- ggplot(genre_counts, aes(x = reorder(genre, n), y = n, fill = genre)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top 10 Movie Genres", x = "Genre", y = "Count") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Generate rating inputs for IBCF
  output$ibcf_rating_inputs <- renderUI({
    sample_movies <- popular_movies %>% sample_n(10)
    
    lapply(1:nrow(sample_movies), function(i) {
      movie <- sample_movies[i, ]
      fluidRow(
        column(8, p(strong(movie$title), " (", gsub("\\|", ", ", movie$genres), ")")),
        column(4, 
               sliderInput(
                 inputId = paste0("ibcf_rating_", movie$movieId),
                 label = NULL,
                 min = 0,
                 max = 5,
                 value = 0,
                 step = 0.5,
                 ticks = FALSE
               )
        )
      )
    })
  })
  
  # Generate rating inputs for UBCF
  output$ubcf_rating_inputs <- renderUI({
    sample_movies <- popular_movies %>% sample_n(10)
    
    lapply(1:nrow(sample_movies), function(i) {
      movie <- sample_movies[i, ]
      fluidRow(
        column(8, p(strong(movie$title), " (", gsub("\\|", ", ", movie$genres), ")")),
        column(4, 
               sliderInput(
                 inputId = paste0("ubcf_rating_", movie$movieId),
                 label = NULL,
                 min = 0,
                 max = 5,
                 value = 0,
                 step = 0.5,
                 ticks = FALSE
               )
        )
      )
    })
  })
  
  # Generate rating inputs for comparison
  output$compare_rating_inputs <- renderUI({
    sample_movies <- popular_movies %>% sample_n(10)
    
    lapply(1:nrow(sample_movies), function(i) {
      movie <- sample_movies[i, ]
      fluidRow(
        column(8, p(strong(movie$title), " (", gsub("\\|", ", ", movie$genres), ")")),
        column(4, 
               sliderInput(
                 inputId = paste0("compare_rating_", movie$movieId),
                 label = NULL,
                 min = 0,
                 max = 5,
                 value = 0,
                 step = 0.5,
                 ticks = FALSE
               )
        )
      )
    })
  })
  
  # Model metrics table
  output$model_metrics <- renderTable({
    data.frame(
      Metric = c("RMSE", "MAE", "Precision", "Recall", "F1 Score"),
      `Content-Based` = c(0.967, 0.75, 0.075, 0.0128, 0.0202),
      IBCF = c(0.91, 0.72, 0.11, 0.011, NA),
      UBCF = c(0.89, 0.70, 0.18, 0.0188, 0.0295)
    )
  })
  
  # Content-based recommendations
  observeEvent(input$content_recommend, {
    req(input$content_movie)
    
    # Get the selected movie ID
    movie_id <- as.numeric(input$content_movie)
    
    # Get movie details
    selected_movie <- movies %>% filter(movieId == movie_id)
    
    # Display selected movie info
    output$content_selected_movie <- renderUI({
      tagList(
        h3(selected_movie$title),
        p(strong("Genres:"), gsub("\\|", ", ", selected_movie$genres))
      )
    })
    
    # Get similar movies
    similar_movies <- content_model$get_similar_movies(
      movie_id, 
      content_model$movie_sim_matrix, 
      movies, 
      n = input$content_n
    )
    
    # Display recommendations
    output$content_recommendations <- renderDT({
      similar_movies %>%
        mutate(
          genres = gsub("\\|", ", ", genres),
          similarity = round(similarity * 100, 1)
        ) %>%
        select(title, genres, similarity) %>%
        rename(Title = title, Genres = genres, `Similarity (%)` = similarity)
    }, options = list(pageLength = 5))
    
    # Create similarity visualization
    output$content_similarity_plot <- renderPlotly({
      plot_data <- similar_movies %>%
        mutate(title = factor(title, levels = rev(title))) %>%
        arrange(similarity)
      
      p <- ggplot(plot_data, aes(x = title, y = similarity, fill = similarity)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        theme_minimal() +
        labs(title = "Similarity to Selected Movie", 
             x = "Movie", 
             y = "Similarity Score") +
        theme(legend.position = "none")
      
      ggplotly(p)
    })
  })
  
  
  # Add these debug lines at the beginning of your IBCF recommendation code
  cat("IBCF model dimensions:", dim(getModel(ibcf_model)$data), "\n")
  cat("Number of IBCF item IDs:", length(colnames(getModel(ibcf_model)$data)), "\n")
  ibcf_item_ids <- colnames(getModel(ibcf_model)$data)
  cat("First 5 IBCF item IDs:", head(ibcf_item_ids, 5), "\n")
  
  # IBCF recommendations
  observeEvent(input$ibcf_recommend, {
    # Extract the similarity matrix from the model and get its dimensions
    sim_matrix <- ibcf_model@model$sim
    
    # Extract item IDs from the similarity matrix
    ibcf_item_ids <- colnames(sim_matrix)
    
    # Log dimensions for debugging
    cat("IBCF similarity matrix dimensions:", dim(sim_matrix), "\n")
    cat("Number of IBCF items:", length(ibcf_item_ids), "\n")
    
    # Collect user ratings
    rating_inputs <- names(input)[grepl("^ibcf_rating_", names(input))]
    
    user_ratings_df <- data.frame(
      userId = rep(9999, length(rating_inputs)),
      movieId = as.numeric(gsub("ibcf_rating_", "", rating_inputs)),
      rating = sapply(rating_inputs, function(x) input[[x]]),
      stringsAsFactors = FALSE
    )
    
    user_ratings_df <- user_ratings_df %>% filter(rating > 0)
    
    if(nrow(user_ratings_df) < 3) {
      output$ibcf_recommendations <- renderDT({
        data.frame(Message = "Please rate at least 3 movies to get recommendations.")
      })
      return()
    }
    
    # Create matrix with exact dimensions from similarity matrix
    ratings_matrix <- matrix(NA, nrow = 1, ncol = length(ibcf_item_ids))
    colnames(ratings_matrix) <- ibcf_item_ids
    rownames(ratings_matrix) <- "9999"  # Dummy user ID
    
    # Fill user ratings
    for(i in 1:nrow(user_ratings_df)) {
      movie_id_str <- as.character(user_ratings_df$movieId[i])
      if(movie_id_str %in% ibcf_item_ids) {
        ratings_matrix[1, movie_id_str] <- user_ratings_df$rating[i]
      }
    }
    
    # Convert to realRatingMatrix
    user_ratings_matrix <- as(ratings_matrix, "realRatingMatrix")
    
    # Generate recommendations
    tryCatch({
      # Directly use the predict function
      preds <- predict(ibcf_model, 
                       newdata = user_ratings_matrix, 
                       n = 10, 
                       type = "ratings")
      
      # Convert to data frame
      recs_df <- as(preds, "data.frame")
      
      if(nrow(recs_df) > 0) {
        # Format recommendations
        ibcf_recs <- recs_df %>%
          rename(movieId = item, score = rating) %>%
          mutate(movieId = as.numeric(as.character(movieId))) %>%
          left_join(movies[, c("movieId", "title", "genres")], by = "movieId") %>%
          arrange(desc(score))
        
        # Display recommendations
        output$ibcf_recommendations <- renderDT({
          ibcf_recs %>%
            mutate(genres = gsub("\\|", ", ", genres),
                   score = round(score, 2)) %>%
            select(title, genres, score) %>%
            rename(Title = title, Genres = genres, Score = score)
        }, options = list(pageLength = 5))
      } else {
        output$ibcf_recommendations <- renderDT({
          data.frame(Message = "No recommendations found. Try rating more diverse movies.")
        })
      }
    }, error = function(e) {
      cat("IBCF error:", e$message, "\n")
      output$ibcf_recommendations <- renderDT({
        data.frame(Message = paste("Error:", e$message))
      })
    })
  })
  
  # UBCF recommendations
  observeEvent(input$ubcf_recommend, {
    # Collect user ratings from the inputs
    rating_inputs <- names(input)[grepl("^ubcf_rating_", names(input))]
    
    # Define a dummy user ID
    dummy_user_id <- 9999
    
    user_ratings_df <- data.frame(
      userId = rep(dummy_user_id, length(rating_inputs)),
      movieId = as.numeric(gsub("ubcf_rating_", "", rating_inputs)),
      rating = sapply(rating_inputs, function(x) input[[x]]),
      stringsAsFactors = FALSE
    )
    
    # Filter out unrated movies (rating of 0)
    user_ratings_df <- user_ratings_df %>% filter(rating > 0)
    
    # Check if there are enough ratings
    if(nrow(user_ratings_df) < 3) {
      output$ubcf_recommendations <- renderDT({
        data.frame(Message = "Please rate at least 3 movies to get recommendations.")
      })
      return()
    }
    
    # Create ratings matrix matching the model dimensions
    ratings_matrix <- matrix(NA, nrow = 1, ncol = length(ubcf_item_ids))
    colnames(ratings_matrix) <- ubcf_item_ids
    rownames(ratings_matrix) <- as.character(dummy_user_id)
    
    # Fill in user ratings
    for(i in 1:nrow(user_ratings_df)) {
      movie_id_str <- as.character(user_ratings_df$movieId[i])
      if(movie_id_str %in% ubcf_item_ids) {
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
        ubcf_recs <- recs_df %>%
          rename(movieId = item, score = rating) %>%
          mutate(movieId = as.numeric(as.character(movieId))) %>%
          left_join(movies[, c("movieId", "title", "genres")], by = "movieId") %>%
          arrange(desc(score))
        
        # Display recommendations
        output$ubcf_recommendations <- renderDT({
          ubcf_recs %>%
            mutate(genres = gsub("\\|", ", ", genres),
                   score = round(score, 2)) %>%
            select(title, genres, score) %>%
            rename(Title = title, Genres = genres, Score = score)
        }, options = list(pageLength = 5))
      } else {
        # No recommendations found
        output$ubcf_recommendations <- renderDT({
          data.frame(Message = "No recommendations found. Try rating more diverse movies.")
        })
      }
    }, error = function(e) {
      # Error handling
      output$ubcf_recommendations <- renderDT({
        data.frame(Message = paste("Error:", e$message))
      })
      showNotification(paste("Error generating UBCF recommendations:", e$message), 
                       type = "error")
    })
  })
  
  # Compare recommendations from all three models
  observeEvent(input$compare_recommend, {
    # Collect user ratings
    rating_inputs <- names(input)[grepl("^compare_rating_", names(input))]
    
    # Dummy user ID for new users
    dummy_user_id <- 9999
    
    # Create ratings dataframe
    user_ratings_df <- data.frame(
      userId = rep(dummy_user_id, length(rating_inputs)),
      movieId = as.numeric(gsub("compare_rating_", "", rating_inputs)),
      rating = sapply(rating_inputs, function(x) input[[x]]),
      stringsAsFactors = FALSE
    )
    
    # Filter out unrated movies
    user_ratings_df <- user_ratings_df %>% filter(rating > 0)
    
    # Check if there are enough ratings
    if(nrow(user_ratings_df) < 3) {
      showNotification("Please rate at least 3 movies to get comparable recommendations", type = "warning")
      return()
    }
    
    # 1. Content-based recommendations
    content_recs <- tryCatch({
      # Get content-based recommendations
      content_model$get_user_recommendations(
        dummy_user_id, 
        user_ratings_df, 
        content_model$movie_sim_matrix, 
        movies, 
        n = 10
      )
    }, error = function(e) {
      showNotification(paste("Error generating content-based recommendations:", e$message), type = "error")
      return(NULL)
    })
    
    # 2. IBCF recommendations
    # Extract the similarity matrix from the model and get its dimensions
    sim_matrix <- ibcf_model@model$sim
    
    # Extract item IDs from the similarity matrix
    ibcf_item_ids <- colnames(sim_matrix)
    
    # Create ratings matrix for IBCF
    ibcf_ratings_matrix <- matrix(NA, nrow = 1, ncol = length(ibcf_item_ids))
    colnames(ibcf_ratings_matrix) <- ibcf_item_ids
    rownames(ibcf_ratings_matrix) <- as.character(dummy_user_id)
    
    # Fill in user ratings for IBCF
    for(i in 1:nrow(user_ratings_df)) {
      movie_id_str <- as.character(user_ratings_df$movieId[i])
      if(movie_id_str %in% ibcf_item_ids) {
        ibcf_ratings_matrix[1, movie_id_str] <- user_ratings_df$rating[i]
      }
    }
    
    # Convert to realRatingMatrix for IBCF
    ibcf_user_ratings_matrix <- as(ibcf_ratings_matrix, "realRatingMatrix")
    
    # Get IBCF recommendations
    ibcf_recs <- tryCatch({
      # Get predictions
      preds <- predict(ibcf_model, 
                       newdata = ibcf_user_ratings_matrix, 
                       n = 10, 
                       type = "ratings")
      
      # Convert to data frame
      recs_df <- as(preds, "data.frame")
      
      if(nrow(recs_df) > 0) {
        # Format recommendations
        recs_df %>%
          rename(movieId = item, score = rating) %>%
          mutate(movieId = as.numeric(as.character(movieId))) %>%
          left_join(movies[, c("movieId", "title", "genres")], by = "movieId") %>%
          arrange(desc(score))
      } else {
        NULL
      }
    }, error = function(e) {
      showNotification(paste("Error generating IBCF recommendations:", e$message), type = "error")
      return(NULL)
    })
    
    # 3. UBCF recommendations
    # Create ratings matrix for UBCF
    ubcf_ratings_matrix <- matrix(NA, nrow = 1, ncol = length(ubcf_item_ids))
    colnames(ubcf_ratings_matrix) <- ubcf_item_ids
    rownames(ubcf_ratings_matrix) <- as.character(dummy_user_id)
    
    # Fill in user ratings for UBCF
    for(i in 1:nrow(user_ratings_df)) {
      movie_id_str <- as.character(user_ratings_df$movieId[i])
      if(movie_id_str %in% ubcf_item_ids) {
        ubcf_ratings_matrix[1, movie_id_str] <- user_ratings_df$rating[i]
      }
    }
    
    # Convert to realRatingMatrix for UBCF
    ubcf_user_ratings_matrix <- as(ubcf_ratings_matrix, "realRatingMatrix")
    
    # Get UBCF recommendations
    ubcf_recs <- tryCatch({
      # Get predictions
      preds <- predict(ubcf_model, 
                       newdata = ubcf_user_ratings_matrix, 
                       n = 10, 
                       type = "ratings")
      
      # Convert to data frame
      recs_df <- as(preds, "data.frame")
      
      if(nrow(recs_df) > 0) {
        # Format recommendations
        recs_df %>%
          rename(movieId = item, score = rating) %>%
          mutate(movieId = as.numeric(as.character(movieId))) %>%
          left_join(movies[, c("movieId", "title", "genres")], by = "movieId") %>%
          arrange(desc(score))
      } else {
        NULL
      }
    }, error = function(e) {
      showNotification(paste("Error generating UBCF recommendations:", e$message), type = "error")
      return(NULL)
    })
    
    # Display recommendations from each model
    output$compare_content <- renderDT({
      if(is.null(content_recs) || nrow(content_recs) == 0) {
        return(data.frame(Message = "No content-based recommendations available"))
      }
      
      content_recs %>%
        mutate(genres = gsub("\\|", ", ", genres),
               score = round(score, 2)) %>%
        select(title, genres, score) %>%
        rename(Title = title, Genres = genres, Score = score)
    }, options = list(pageLength = 5))
    
    output$compare_ibcf <- renderDT({
      if(is.null(ibcf_recs) || nrow(ibcf_recs) == 0) {
        return(data.frame(Message = "No IBCF recommendations available"))
      }
      
      ibcf_recs %>%
        mutate(genres = gsub("\\|", ", ", genres),
               score = round(score, 2)) %>%
        select(title, genres, score) %>%
        rename(Title = title, Genres = genres, Score = score)
    }, options = list(pageLength = 5))
    
    output$compare_ubcf <- renderDT({
      if(is.null(ubcf_recs) || nrow(ubcf_recs) == 0) {
        return(data.frame(Message = "No UBCF recommendations available"))
      }
      
      ubcf_recs %>%
        mutate(genres = gsub("\\|", ", ", genres),
               score = round(score, 2)) %>%
        select(title, genres, score) %>%
        rename(Title = title, Genres = genres, Score = score)
    }, options = list(pageLength = 5))
    
    # Create overlap analysis
    output$compare_overlap <- renderPlotly({
      # Extract movie IDs from each recommendation set
      content_ids <- if(!is.null(content_recs)) content_recs$movieId else c()
      ibcf_ids <- if(!is.null(ibcf_recs)) ibcf_recs$movieId else c()
      ubcf_ids <- if(!is.null(ubcf_recs)) ubcf_recs$movieId else c()
      
      # Calculate overlaps
      content_ibcf <- length(intersect(content_ids, ibcf_ids))
      content_ubcf <- length(intersect(content_ids, ubcf_ids))
      ibcf_ubcf <- length(intersect(ibcf_ids, ubcf_ids))
      all_overlap <- length(intersect(intersect(content_ids, ibcf_ids), ubcf_ids))
      
      # Create data frame for plotting
      overlap_data <- data.frame(
        comparison = c("Content-IBCF", "Content-UBCF", "IBCF-UBCF", "All Three"),
        overlap = c(content_ibcf, content_ubcf, ibcf_ubcf, all_overlap)
      )
      
      # Plot
      p <- ggplot(overlap_data, aes(x = comparison, y = overlap, fill = comparison)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = "Recommendation Overlap Between Models", 
             x = "Model Comparison", 
             y = "Number of Common Recommendations") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    })
    
    # Display performance metrics
    output$compare_metrics <- renderPlotly({
      # Prepare data
      metrics <- data.frame(
        Model = c("Content-Based", "IBCF", "UBCF"),
        RMSE = c(0.967, 0.91, 0.89),  # Your actual metrics from model validation
        MAE = c(0.75, 0.72, 0.70),    # Your actual metrics from model validation
        Precision = c(0.075, 0.11, 0.18), # Your actual metrics from model validation
        Recall = c(0.0128, 0.011, 0.0188) # Your actual metrics from model validation
      )
      
      # Reshape for plotting
      metrics_long <- metrics %>%
        pivot_longer(cols = c("RMSE", "MAE", "Precision", "Recall"),
                     names_to = "Metric", values_to = "Value")
      
      # Plot
      p <- ggplot(metrics_long, aes(x = Metric, y = Value, fill = Model)) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_minimal() +
        labs(title = "Performance Metrics Comparison", 
             x = "Metric", 
             y = "Value") +
        scale_fill_brewer(palette = "Set1")
      
      ggplotly(p)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)