# Content-Based Recommendation Model Validation Script
# This script evaluates the content_based_model.rds according to project requirements

# Load necessary libraries
library(tidyverse)  # For data manipulation
library(Matrix)     # For matrix operations
library(knitr)      # For nice table output

# Load prepared data (if needed)
load("prepared_data.rdata")

# Extract relevant data from prepared_data
ratings_filtered <- prepared_data$ratings_filtered
movies_filtered <- prepared_data$movies_filtered

# Create a header-style print function for better output
print_header <- function(text) {
  cat("\n", paste(rep("=", nchar(text) + 4), collapse = ""), "\n")
  cat("  ", text, "  \n")
  cat(paste(rep("=", nchar(text) + 4), collapse = ""), "\n\n")
}

print_subheader <- function(text) {
  cat("\n", text, "\n")
  cat(paste(rep("-", nchar(text)), collapse = ""), "\n")
}

# Check if the model file exists
print_header("CONTENT-BASED MODEL VALIDATION")

model_file <- "content_based_model.rds"
if (!file.exists(model_file)) {
  cat("ERROR: Model file", model_file, "not found!\n")
  cat("Please ensure the file exists in the current directory.\n")
  quit(status = 1)
}

# Load the model
cat("Loading content-based model...\n")
tryCatch({
  content_model <- readRDS(model_file)
  cat("Model loaded successfully.\n")
}, error = function(e) {
  cat("ERROR: Failed to load model:", conditionMessage(e), "\n")
  quit(status = 1)
})

# Validate model structure
print_subheader("Model Structure Validation")

# Check if model is a list with required components
if (!is.list(content_model)) {
  cat("ERROR: Model is not a list structure as expected.\n")
  quit(status = 1)
}

required_components <- c("movie_sim_matrix", "movies", 
                         "get_similar_movies", "get_user_recommendations")
missing_components <- setdiff(required_components, names(content_model))

if (length(missing_components) > 0) {
  cat("ERROR: Model is missing required components:", 
      paste(missing_components, collapse = ", "), "\n")
  quit(status = 1)
} else {
  cat("✓ Model contains all required components.\n")
}

# Check similarity matrix
sim_matrix <- content_model$movie_sim_matrix
if (!is.matrix(sim_matrix)) {
  cat("ERROR: Similarity matrix is not a valid matrix.\n")
  quit(status = 1)
}

# Matrix properties
cat("✓ Similarity matrix dimensions:", nrow(sim_matrix), "x", ncol(sim_matrix), "\n")
cat("✓ Number of movies in matrix:", nrow(sim_matrix), "\n")

# Check sparsity (percentage of near-zero values)
sparsity <- mean(sim_matrix < 0.001)
cat("✓ Similarity matrix sparsity:", round(sparsity * 100, 2), "%\n")

# Validate the movies data frame
movies_df <- content_model$movies
if (!is.data.frame(movies_df)) {
  cat("ERROR: Movies component is not a valid data frame.\n")
  quit(status = 1)
}

cat("✓ Movies data frame contains", nrow(movies_df), "movies.\n")

# Check functions
if (!is.function(content_model$get_similar_movies)) {
  cat("ERROR: get_similar_movies is not a valid function.\n")
  quit(status = 1)
}

if (!is.function(content_model$get_user_recommendations)) {
  cat("ERROR: get_user_recommendations is not a valid function.\n")
  quit(status = 1)
}

cat("✓ Recommendation functions are valid.\n")

# Test function execution with sample data
print_subheader("Function Testing")

# Test get_similar_movies
cat("Testing get_similar_movies function...\n")
test_movie_id <- movies_df$movieId[1]  # Use first movie
test_movie_title <- movies_df$title[1]

tryCatch({
  similar_movies <- content_model$get_similar_movies(
    test_movie_id, sim_matrix, movies_df, n = 5)
  if (is.data.frame(similar_movies) && nrow(similar_movies) > 0) {
    cat("✓ Successfully found similar movies for:", test_movie_title, "\n")
    cat("  Top similar movies:\n")
    for (i in 1:min(3, nrow(similar_movies))) {
      cat("  ", i, ". ", similar_movies$title[i], 
          " (Similarity: ", round(similar_movies$similarity[i], 4), ")\n", sep="")
    }
  } else {
    cat("WARNING: get_similar_movies returned no results.\n")
  }
}, error = function(e) {
  cat("ERROR in get_similar_movies:", conditionMessage(e), "\n")
})

# Test get_user_recommendations
cat("\nTesting get_user_recommendations function...\n")

# Create a sample user with ratings
sample_user_id <- 9999
sample_ratings <- data.frame(
  userId = rep(sample_user_id, 5),
  movieId = sample(movies_df$movieId, 5),
  rating = c(5, 4.5, 4, 3, 2.5),
  stringsAsFactors = FALSE
)

tryCatch({
  user_recs <- content_model$get_user_recommendations(
    sample_user_id, sample_ratings, sim_matrix, movies_df, n = 5)
  if (is.data.frame(user_recs) && nrow(user_recs) > 0) {
    cat("✓ Successfully generated recommendations for sample user.\n")
    cat("  Top recommended movies:\n")
    for (i in 1:min(3, nrow(user_recs))) {
      cat("  ", i, ". ", user_recs$title[i], 
          " (Score: ", round(user_recs$score[i], 4), ")\n", sep="")
    }
  } else {
    cat("WARNING: get_user_recommendations returned no results.\n")
  }
}, error = function(e) {
  cat("ERROR in get_user_recommendations:", conditionMessage(e), "\n")
})

# Evaluate model according to project requirements
print_header("MODEL EVALUATION METRICS")

print_subheader("Project Evaluation Criteria")

# A. Root Mean Squared Error (RMSE) and Mean Absolute Error (MAE)
cat("1. Error metrics calculation (RMSE & MAE)\n")

# Function to calculate RMSE and MAE for content-based recommendations
calculate_error_metrics <- function(test_users = 10, test_proportion = 0.2) {
  # Select users for testing
  if (exists("user_activity", where = prepared_data) && 
      !is.null(prepared_data$user_activity) && 
      nrow(prepared_data$user_activity) > 0) {
    
    # Use active users if available
    test_user_ids <- prepared_data$user_activity %>%
      arrange(desc(rating_count)) %>%
      head(test_users) %>%
      pull(userId)
  } else {
    # Otherwise, select users with most ratings
    rating_counts <- ratings_filtered %>%
      group_by(userId) %>%
      summarize(count = n()) %>%
      filter(count >= 20) %>%
      arrange(desc(count))
    
    test_user_ids <- head(rating_counts$userId, test_users)
  }
  
  # Initialize results
  results <- data.frame()
  
  for (user_id in test_user_ids) {
    # Get all ratings for this user
    user_ratings <- ratings_filtered %>% 
      filter(userId == user_id)
    
    # Only proceed if the user has enough ratings
    if (nrow(user_ratings) < 10) next
    
    # Split into training and test sets
    n_test <- round(nrow(user_ratings) * test_proportion)
    test_indices <- sample(1:nrow(user_ratings), n_test)
    
    train_ratings <- user_ratings[-test_indices, ]
    test_ratings <- user_ratings[test_indices, ]
    
    # Create a temporary ratings dataframe with only training data for this user
    temp_ratings <- ratings_filtered %>%
      filter(userId != user_id) %>%
      bind_rows(train_ratings)
    
    # For each test rating, predict the rating
    predicted_ratings <- c()
    actual_ratings <- c()
    
    for (i in 1:nrow(test_ratings)) {
      movie_id <- test_ratings$movieId[i]
      actual_rating <- test_ratings$rating[i]
      
      # Find similar movies to this test movie
      similar <- content_model$get_similar_movies(
        movie_id, sim_matrix, movies_df, n = 10)
      
      if (nrow(similar) == 0) next
      
      # Get user's ratings for similar movies
      user_sim_ratings <- train_ratings %>%
        filter(movieId %in% similar$movieId)
      
      if (nrow(user_sim_ratings) == 0) {
        # If user hasn't rated any similar movies, use average rating
        pred_rating <- mean(train_ratings$rating)
      } else {
        # Join to get similarities
        rated_similar <- user_sim_ratings %>%
          left_join(similar, by = "movieId")
        
        # Weighted average by similarity
        pred_rating <- sum(rated_similar$rating * rated_similar$similarity) / 
          sum(rated_similar$similarity)
      }
      
      predicted_ratings <- c(predicted_ratings, pred_rating)
      actual_ratings <- c(actual_ratings, actual_rating)
    }
    
    # Calculate RMSE and MAE for this user
    if (length(predicted_ratings) > 0) {
      user_rmse <- sqrt(mean((predicted_ratings - actual_ratings)^2))
      user_mae <- mean(abs(predicted_ratings - actual_ratings))
      
      results <- rbind(results, data.frame(
        userId = user_id,
        rmse = user_rmse,
        mae = user_mae,
        n_predictions = length(predicted_ratings)
      ))
    }
  }
  
  # Calculate average metrics
  if (nrow(results) > 0) {
    avg_rmse <- weighted.mean(results$rmse, results$n_predictions)
    avg_mae <- weighted.mean(results$mae, results$n_predictions)
    return(list(
      user_results = results, 
      average_rmse = avg_rmse,
      average_mae = avg_mae
    ))
  } else {
    return(NULL)
  }
}

# Calculate RMSE and MAE
error_metrics <- calculate_error_metrics(test_users = 20)

if (!is.null(error_metrics)) {
  cat("✓ Error metrics evaluation complete.\n")
  cat("  Average RMSE:", round(error_metrics$average_rmse, 4), "\n")
  cat("  Average MAE:", round(error_metrics$average_mae, 4), "\n")
  cat("  Number of test users:", nrow(error_metrics$user_results), "\n")
} else {
  cat("WARNING: Could not calculate error metrics. Check data availability.\n")
}

# B. Precision & Recall
cat("\n2. Precision and Recall calculation\n")

# Function to calculate precision and recall
calculate_precision_recall <- function(test_users = 10, test_proportion = 0.2, rating_threshold = 4) {
  # Select users for testing (same as RMSE function)
  if (exists("user_activity", where = prepared_data) && 
      !is.null(prepared_data$user_activity) && 
      nrow(prepared_data$user_activity) > 0) {
    
    test_user_ids <- prepared_data$user_activity %>%
      arrange(desc(rating_count)) %>%
      head(test_users) %>%
      pull(userId)
  } else {
    rating_counts <- ratings_filtered %>%
      group_by(userId) %>%
      summarize(count = n()) %>%
      filter(count >= 20) %>%
      arrange(desc(count))
    
    test_user_ids <- head(rating_counts$userId, test_users)
  }
  
  # Initialize results
  results <- data.frame()
  
  for (user_id in test_user_ids) {
    # Get all ratings for this user
    user_ratings <- ratings_filtered %>% 
      filter(userId == user_id)
    
    # Only proceed if the user has enough ratings
    if (nrow(user_ratings) < 10) next
    
    # Check if user has any ratings above threshold
    if (sum(user_ratings$rating >= rating_threshold) < 2) next
    
    # Split into training and test sets
    test_indices <- sample(which(user_ratings$rating >= rating_threshold), 
                           round(sum(user_ratings$rating >= rating_threshold) * test_proportion))
    
    if (length(test_indices) == 0) next
    
    train_ratings <- user_ratings[-test_indices, ]
    test_ratings <- user_ratings[test_indices, ]
    
    # Create a temporary ratings dataframe with only training data for this user
    temp_ratings <- ratings_filtered %>%
      filter(userId != user_id) %>%
      bind_rows(train_ratings)
    
    # Get recommendations for this user
    n_recs <- min(10, nrow(test_ratings) * 3)  # Get more recommendations than test items
    user_recs <- content_model$get_user_recommendations(
      user_id, temp_ratings, sim_matrix, movies_df, n = n_recs)
    
    if (is.null(user_recs) || nrow(user_recs) == 0) next
    
    # Calculate precision and recall
    recommended_ids <- user_recs$movieId
    test_ids <- test_ratings$movieId
    relevant_ids <- test_ratings$movieId[test_ratings$rating >= rating_threshold]
    
    hit_count <- length(intersect(recommended_ids, relevant_ids))
    precision <- hit_count / length(recommended_ids)
    recall <- hit_count / length(relevant_ids)
    f1_score <- if (precision + recall > 0) 2 * precision * recall / (precision + recall) else 0
    
    results <- rbind(results, data.frame(
      userId = user_id,
      precision = precision,
      recall = recall,
      f1_score = f1_score,
      hit_count = hit_count,
      recommended_count = length(recommended_ids),
      relevant_count = length(relevant_ids)
    ))
  }
  
  # Calculate averages
  if (nrow(results) > 0) {
    avg_results <- colMeans(results[, c("precision", "recall", "f1_score")])
    return(list(user_results = results, 
                avg_precision = avg_results["precision"],
                avg_recall = avg_results["recall"],
                avg_f1 = avg_results["f1_score"]))
  } else {
    return(NULL)
  }
}

# Calculate precision and recall
pr_results <- calculate_precision_recall(test_users = 20)

if (!is.null(pr_results)) {
  cat("✓ Precision & Recall evaluation complete.\n")
  cat("  Average Precision:", round(pr_results$avg_precision, 4), "\n")
  cat("  Average Recall:", round(pr_results$avg_recall, 4), "\n")
  cat("  Average F1 Score:", round(pr_results$avg_f1, 4), "\n")
  cat("  Number of test users:", nrow(pr_results$user_results), "\n")
} else {
  cat("WARNING: Could not calculate Precision & Recall. Check data availability.\n")
}

# C. Additional Metrics (based on project description)
print_subheader("Additional Performance Metrics")

# 1. Coverage (percentage of items that can be recommended)
coverage <- nrow(sim_matrix) / nrow(movies_filtered)
cat("✓ Coverage:", round(coverage * 100, 2), "% of the catalog can be recommended\n")

# 2. Diversity (genre diversity in recommendations)
calculate_diversity <- function(n_recommendations = 10) {
  # Select some test users
  if (exists("user_activity", where = prepared_data) && 
      !is.null(prepared_data$user_activity) && 
      nrow(prepared_data$user_activity) > 0) {
    
    test_users <- prepared_data$user_activity %>%
      arrange(desc(rating_count)) %>%
      head(10) %>%
      pull(userId)
  } else {
    test_users <- sample(unique(ratings_filtered$userId), 10)
  }
  
  # Get recommendations for each user
  all_genre_diversity <- c()
  
  for (user_id in test_users) {
    user_recs <- content_model$get_user_recommendations(
      user_id, ratings_filtered, sim_matrix, movies_df, n = n_recommendations)
    
    if (!is.null(user_recs) && nrow(user_recs) > 0 && "genres" %in% colnames(user_recs)) {
      # Calculate genre diversity
      all_genres <- unlist(strsplit(user_recs$genres, "\\|"))
      genre_diversity <- length(unique(all_genres)) / length(all_genres)
      all_genre_diversity <- c(all_genre_diversity, genre_diversity)
    }
  }
  
  if (length(all_genre_diversity) > 0) {
    return(mean(all_genre_diversity))
  } else {
    return(NA)
  }
}

diversity_score <- calculate_diversity()
if (!is.na(diversity_score)) {
  cat("✓ Genre Diversity:", round(diversity_score * 100, 2), 
      "% unique genres in recommendations\n")
} else {
  cat("WARNING: Could not calculate diversity score.\n")
}

# 3. Computation efficiency
cat("\nComputation Efficiency:\n")

# Test recommendation time
start_time <- Sys.time()
test_user_id <- sample(unique(ratings_filtered$userId), 1)
test_recs <- content_model$get_user_recommendations(
  test_user_id, ratings_filtered, sim_matrix, movies_df, n = 10)
end_time <- Sys.time()
rec_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("✓ Time to generate recommendations:", round(rec_time, 4), "seconds\n")

# Model size
model_size_mb <- object.size(content_model) / (1024^2)
cat("✓ Model size in memory:", round(model_size_mb, 2), "MB\n")

# Overall Summary
print_header("VALIDATION SUMMARY")

# Create a summary table of results
summary_metrics <- data.frame(
  Metric = c(
    "RMSE", 
    "MAE",
    "Precision", 
    "Recall", 
    "F1 Score",
    "Coverage", 
    "Genre Diversity",
    "Recommendation Time",
    "Model Size"
  ),
  Value = c(
    ifelse(!is.null(error_metrics), round(error_metrics$average_rmse, 4), NA),
    ifelse(!is.null(error_metrics), round(error_metrics$average_mae, 4), NA),
    ifelse(!is.null(pr_results), round(pr_results$avg_precision, 4), NA),
    ifelse(!is.null(pr_results), round(pr_results$avg_recall, 4), NA),
    ifelse(!is.null(pr_results), round(pr_results$avg_f1, 4), NA),
    paste0(round(coverage * 100, 2), "%"),
    ifelse(!is.na(diversity_score), paste0(round(diversity_score * 100, 2), "%"), NA),
    paste0(round(rec_time, 4), " seconds"),
    paste0(round(model_size_mb, 2), " MB")
  )
)

# Print the summary table
print(knitr::kable(summary_metrics, format = "simple"))

# Assessment based on project requirements
print_subheader("Final Assessment")

# Check if metrics are within acceptable ranges
rmse_acceptable <- !is.null(error_metrics) && error_metrics$average_rmse < 1.0
mae_acceptable <- !is.null(error_metrics) && error_metrics$average_mae < 0.8
precision_acceptable <- !is.null(pr_results) && pr_results$avg_precision > 0.1
recall_acceptable <- !is.null(pr_results) && pr_results$avg_recall > 0.1
coverage_acceptable <- coverage > 0.8
diversity_acceptable <- !is.na(diversity_score) && diversity_score > 0.3
speed_acceptable <- rec_time < 2

overall_acceptable <- rmse_acceptable && mae_acceptable && precision_acceptable && 
  recall_acceptable && coverage_acceptable && 
  diversity_acceptable && speed_acceptable

if (overall_acceptable) {
  cat("\n✓ VALIDATION PASSED: The content-based model meets all required criteria.\n")
} else {
  cat("\n! VALIDATION WARNING: Some metrics may need improvement.\n")
  
  # List issues
  if (!rmse_acceptable) cat("  - RMSE is higher than desired (target: < 1.0)\n")
  if (!mae_acceptable) cat("  - MAE is higher than desired (target: < 0.8)\n")
  if (!precision_acceptable) cat("  - Precision is lower than desired (target: > 0.1)\n")
  if (!recall_acceptable) cat("  - Recall is lower than desired (target: > 0.1)\n")
  if (!coverage_acceptable) cat("  - Coverage is lower than desired (target: > 80%)\n")
  if (!diversity_acceptable) cat("  - Genre diversity is lower than desired (target: > 30%)\n")
  if (!speed_acceptable) cat("  - Recommendation generation is slower than desired (target: < 2s)\n")
}

cat("\nValidation complete.\n")