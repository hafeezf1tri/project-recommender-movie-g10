# Content-Based Filtering Evaluation for MovieLens Dataset
# This script evaluates a content-based recommendation model against user preferences

# Load required libraries
library(tidyverse)
library(Matrix)
library(proxy)

# Set random seed for reproducibility
set.seed(123)

# ===== 1. LOAD MODELS AND DATA =====
cat("Loading content-based model and prepared data...\n")

# Check if the model file exists
if (!file.exists("content_based_model.rds")) {
  stop("Error: content_based_model.rds file not found!")
}

# Load the content-based model
content_model <- readRDS("content_based_model.rds")
cat("Content-based model loaded successfully\n")

# Load prepared data
if (!file.exists("prepared_data.RData")) {
  stop("Error: prepared_data.RData file not found!")
}

load("prepared_data.RData")
cat("Prepared data loaded successfully\n")

# Extract ratings data for evaluation
if ("ratings_filtered" %in% names(prepared_data) && 
    nrow(prepared_data$ratings_filtered) > 0) {
  ratings_data <- prepared_data$ratings_filtered
} else if ("ratings_raw" %in% names(prepared_data)) {
  ratings_data <- prepared_data$ratings_raw
} else {
  stop("Error: No valid ratings data found!")
}

cat("Using ratings dataset with", nrow(ratings_data), "ratings\n")

# ===== 2. DEFINE EVALUATION METRICS =====

# 2.1 Metric 1: Genre Matching Accuracy
# This measures how well the recommended movies match the genres of movies a user has rated highly
evaluate_genre_matching <- function(user_id, n_recs = 10) {
  # Get user's highly rated movies (4+ stars)
  user_ratings <- ratings_data %>% 
    filter(userId == user_id, rating >= 4.0)
  
  # If user has fewer than 3 highly rated movies, return NA
  if (nrow(user_ratings) < 3) {
    return(list(genre_match_score = NA, coverage = NA))
  }
  
  # Get the genres of the user's highly rated movies
  user_liked_movies <- user_ratings %>%
    pull(movieId)
  
  user_liked_genres <- content_model$movies %>%
    filter(movieId %in% user_liked_movies) %>%
    pull(genres) %>%
    paste(collapse = "|") %>%
    strsplit("\\|") %>%
    unlist() %>%
    table()
  
  # Convert to normalized frequency
  user_genre_prefs <- user_liked_genres / sum(user_liked_genres)
  
  # Get recommendations for this user
  recs <- content_model$get_user_recommendations(
    user_id, 
    ratings_data, 
    content_model$movie_sim_matrix, 
    content_model$movies, 
    n = n_recs
  )
  
  # If no recommendations, return NA
  if (nrow(recs) == 0) {
    return(list(genre_match_score = NA, coverage = 0))
  }
  
  # Get genres of recommended movies
  rec_genres <- recs %>%
    pull(genres) %>%
    paste(collapse = "|") %>%
    strsplit("\\|") %>%
    unlist() %>%
    table()
  
  # Convert to normalized frequency
  rec_genre_freq <- rep(0, length(user_genre_prefs))
  names(rec_genre_freq) <- names(user_genre_prefs)
  
  for (genre in names(rec_genres)) {
    if (genre %in% names(rec_genre_freq)) {
      rec_genre_freq[genre] <- rec_genres[genre] / sum(rec_genres)
    }
  }
  
  # Calculate overlap between user preferences and recommendation genres
  # Using cosine similarity between genre frequency vectors
  match_score <- sum(user_genre_prefs * rec_genre_freq) / 
    (sqrt(sum(user_genre_prefs^2)) * sqrt(sum(rec_genre_freq^2)))
  
  # Calculate coverage (percentage of requested recommendations actually returned)
  coverage <- nrow(recs) / n_recs
  
  return(list(genre_match_score = match_score, coverage = coverage))
}

# 2.2 Metric 2: User Satisfaction Prediction
# This estimates how well the recommendations would satisfy users based on rating history
evaluate_satisfaction_prediction <- function(user_id, n_recs = 10, test_ratio = 0.3) {
  # Get all user ratings
  user_ratings <- ratings_data %>% 
    filter(userId == user_id) %>%
    arrange(desc(rating))
  
  # Need enough ratings to split
  if (nrow(user_ratings) < 10) {
    return(list(rmse = NA, hit_rate = NA, coverage = NA))
  }
  
  # Split ratings into training and testing sets
  n_test <- ceiling(nrow(user_ratings) * test_ratio)
  test_movies <- user_ratings$movieId[1:n_test]  # Take top-rated movies for testing
  train_ratings <- user_ratings %>% filter(!movieId %in% test_movies)
  
  # Create temporary ratings data with only training data for this user
  temp_ratings <- ratings_data %>%
    filter(userId != user_id | !movieId %in% test_movies)
  
  # Generate recommendations based on training data
  recs <- content_model$get_user_recommendations(
    user_id, 
    temp_ratings, 
    content_model$movie_sim_matrix, 
    content_model$movies, 
    n = n_recs
  )
  
  # If no recommendations, return NA
  if (nrow(recs) == 0) {
    return(list(rmse = NA, hit_rate = 0, coverage = 0))
  }
  
  # Calculate how many test movies were correctly recommended (hit rate)
  hits <- sum(recs$movieId %in% test_movies)
  hit_rate <- hits / length(test_movies)
  
  # For RMSE, we need to predict ratings for test movies
  # This is a simplification - using recommendation score as a proxy for predicted rating
  test_ratings <- user_ratings %>% 
    filter(movieId %in% test_movies) %>%
    select(movieId, actual_rating = rating)
  
  # Match with recommendations if possible
  matched_recs <- test_ratings %>%
    left_join(recs %>% select(movieId, pred_rating = score), by = "movieId")
  
  # Calculate RMSE for available predictions
  if (sum(!is.na(matched_recs$pred_rating)) > 0) {
    # Normalize prediction scores to same range as ratings
    max_score <- max(recs$score, na.rm = TRUE)
    min_score <- min(recs$score, na.rm = TRUE)
    
    # Scale to 0.5-5 range (typical MovieLens rating range)
    matched_recs$pred_rating_scaled <- 0.5 + 4.5 * 
      (matched_recs$pred_rating - min_score) / (max_score - min_score)
    
    rmse <- sqrt(mean((matched_recs$actual_rating - matched_recs$pred_rating_scaled)^2, 
                      na.rm = TRUE))
  } else {
    rmse <- NA
  }
  
  # Calculate coverage (percentage of requested recommendations actually returned)
  coverage <- nrow(recs) / n_recs
  
  return(list(rmse = rmse, hit_rate = hit_rate, coverage = coverage))
}

# 2.3 Metric 3: Diversity of Recommendations
# Measures how diverse the recommendations are in terms of genres
evaluate_diversity <- function(user_id, n_recs = 10) {
  # Get recommendations
  recs <- content_model$get_user_recommendations(
    user_id, 
    ratings_data, 
    content_model$movie_sim_matrix, 
    content_model$movies, 
    n = n_recs
  )
  
  # If no recommendations, return NA
  if (nrow(recs) == 0) {
    return(list(genre_diversity = NA, coverage = 0))
  }
  
  # Get all genres across recommendations
  all_genres <- recs %>%
    pull(genres) %>%
    paste(collapse = "|") %>%
    strsplit("\\|") %>%
    unlist() %>%
    unique()
  
  # Count genres per movie
  genre_counts <- numeric(nrow(recs))
  
  for (i in 1:nrow(recs)) {
    movie_genres <- strsplit(recs$genres[i], "\\|")[[1]]
    genre_counts[i] <- length(movie_genres)
  }
  
  # Calculate average number of genres per movie
  avg_genres_per_movie <- mean(genre_counts)
  
  # Calculate genre diversity: unique genres / (avg genres per movie * movies)
  # This gives a normalized measure of genre variety
  genre_diversity <- length(all_genres) / (avg_genres_per_movie * nrow(recs))
  
  # Calculate coverage
  coverage <- nrow(recs) / n_recs
  
  return(list(genre_diversity = genre_diversity, coverage = coverage))
}

# ===== 3. RUN EVALUATION =====
cat("\nEvaluating content-based filtering model...\n")

# Get a sample of users for evaluation
n_users_to_evaluate <- 100
all_users <- unique(ratings_data$userId)
users_to_evaluate <- sample(all_users, min(n_users_to_evaluate, length(all_users)))

cat("Evaluating model with", length(users_to_evaluate), "users\n")

# Initialize results storage
genre_match_results <- list()
satisfaction_results <- list()
diversity_results <- list()

# Process each user
for (i in 1:length(users_to_evaluate)) {
  user_id <- users_to_evaluate[i]
  
  # Run evaluations
  genre_match_results[[i]] <- evaluate_genre_matching(user_id)
  satisfaction_results[[i]] <- evaluate_satisfaction_prediction(user_id)
  diversity_results[[i]] <- evaluate_diversity(user_id)
  
  # Show progress
  if (i %% 10 == 0 || i == length(users_to_evaluate)) {
    cat(sprintf("Processed %d/%d users\n", i, length(users_to_evaluate)))
  }
}

# ===== 4. SUMMARIZE RESULTS =====
cat("\nSummarizing evaluation results...\n")

# Calculate average genre matching score
genre_match_scores <- sapply(genre_match_results, function(x) x$genre_match_score)
genre_match_scores <- genre_match_scores[!is.na(genre_match_scores)]
avg_genre_match <- mean(genre_match_scores)
cat("Average Genre Matching Score:", round(avg_genre_match, 4), "\n")

# Calculate average hit rate and RMSE
hit_rates <- sapply(satisfaction_results, function(x) x$hit_rate)
hit_rates <- hit_rates[!is.na(hit_rates)]
avg_hit_rate <- mean(hit_rates)
cat("Average Hit Rate:", round(avg_hit_rate, 4), "\n")

rmse_values <- sapply(satisfaction_results, function(x) x$rmse)
rmse_values <- rmse_values[!is.na(rmse_values)]
avg_rmse <- mean(rmse_values)
cat("Average RMSE:", round(avg_rmse, 4), "\n")

# Calculate average diversity
diversity_scores <- sapply(diversity_results, function(x) x$genre_diversity)
diversity_scores <- diversity_scores[!is.na(diversity_scores)]
avg_diversity <- mean(diversity_scores)
cat("Average Genre Diversity Score:", round(avg_diversity, 4), "\n")

# Calculate average coverage across metrics
coverage_genre <- mean(sapply(genre_match_results, function(x) x$coverage), na.rm = TRUE)
coverage_satisfaction <- mean(sapply(satisfaction_results, function(x) x$coverage), na.rm = TRUE)
coverage_diversity <- mean(sapply(diversity_results, function(x) x$coverage), na.rm = TRUE)
avg_coverage <- mean(c(coverage_genre, coverage_satisfaction, coverage_diversity))
cat("Average Coverage:", round(avg_coverage * 100, 2), "%\n")

# Create a summary metrics table
metrics_table <- data.frame(
  Metric = c("Genre Matching", "Hit Rate", "RMSE", "Genre Diversity", "Coverage"),
  Value = c(
    round(avg_genre_match, 4),
    round(avg_hit_rate, 4),
    round(avg_rmse, 4),
    round(avg_diversity, 4),
    round(avg_coverage, 4)
  )
)

# Print the table
print(metrics_table)

# ===== 5. COMPARE WITH COLLABORATIVE FILTERING =====
cat("\nComparing with collaborative filtering models...\n")

# Try to load the collaborative filtering metrics
cf_metrics_file <- "recommender_precision_recall.csv"
if (file.exists(cf_metrics_file)) {
  cf_metrics <- read.csv(cf_metrics_file)
  cat("Loaded collaborative filtering metrics for comparison\n")
  print(cf_metrics)
  
  # Calculate a comparable "hit rate" for CF models
  # This is a simplified comparison - hit rate is somewhat similar to precision
  cf_metrics$ComparableMetric <- cf_metrics$Precision
  
  # Create a comparison table
  comparison_table <- data.frame(
    Algorithm = c("Content-Based", "UBCF", "IBCF"),
    Precision_or_HitRate = c(
      avg_hit_rate,
      cf_metrics$Precision[cf_metrics$Metric == "Precision" & cf_metrics$X == "UBCF"],
      cf_metrics$Precision[cf_metrics$Metric == "Precision" & cf_metrics$X == "IBCF"]
    ),
    Recall = c(
      NA,  # Content-based doesn't have a direct recall measure
      cf_metrics$Precision[cf_metrics$Metric == "Recall" & cf_metrics$X == "UBCF"],
      cf_metrics$Precision[cf_metrics$Metric == "Recall" & cf_metrics$X == "IBCF"]
    )
  )
  
  # Clean up and print
  comparison_table$Precision_or_HitRate <- round(comparison_table$Precision_or_HitRate, 4)
  comparison_table$Recall <- round(comparison_table$Recall, 4)
  
  cat("\nComparison with collaborative filtering models:\n")
  print(comparison_table)
  
  # Save comparison table
  write.csv(comparison_table, "algorithm_comparison.csv", row.names = FALSE)
  cat("Comparison table saved to 'algorithm_comparison.csv'\n")
  
  # Create visualization data
  viz_data <- data.frame(
    Algorithm = c("Content-Based", "UBCF", "IBCF"),
    Metric = rep("Precision/Hit Rate", 3),
    Value = comparison_table$Precision_or_HitRate
  )
  
  # Add UBCF and IBCF recall
  viz_data <- rbind(
    viz_data,
    data.frame(
      Algorithm = c("UBCF", "IBCF"),
      Metric = rep("Recall", 2),
      Value = comparison_table$Recall[2:3]
    )
  )
  
  # Create visualization
  library(ggplot2)
  
  p <- ggplot(viz_data, aes(x = Metric, y = Value, fill = Algorithm)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = sprintf("%.4f", Value)), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5) +
    labs(title = "Recommendation Algorithm Comparison",
         y = "Score") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
  
  # Save plots
  ggsave("algorithm_comparison.png", p, width = 8, height = 6)
  ggsave("algorithm_comparison.pdf", p, width = 8, height = 6)
  
  cat("Comparison visualization saved to 'algorithm_comparison.png' and 'algorithm_comparison.pdf'\n")
} else {
  cat("Note: Collaborative filtering metrics file not found for comparison\n")
}

# ===== 6. SAVE DETAILED RESULTS =====
cat("\nSaving detailed evaluation results...\n")

# Create detailed results data frame
detailed_results <- data.frame(
  User = users_to_evaluate,
  Genre_Match = sapply(genre_match_results, function(x) ifelse(is.na(x$genre_match_score), NA, round(x$genre_match_score, 4))),
  Hit_Rate = sapply(satisfaction_results, function(x) ifelse(is.na(x$hit_rate), NA, round(x$hit_rate, 4))),
  RMSE = sapply(satisfaction_results, function(x) ifelse(is.na(x$rmse), NA, round(x$rmse, 4))),
  Diversity = sapply(diversity_results, function(x) ifelse(is.na(x$genre_diversity), NA, round(x$genre_diversity, 4))),
  Coverage = sapply(satisfaction_results, function(x) ifelse(is.na(x$coverage), NA, round(x$coverage, 4)))
)

# Save detailed results
write.csv(detailed_results, "content_based_detailed_results.csv", row.names = FALSE)
cat("Detailed results saved to 'content_based_detailed_results.csv'\n")

# Save summary metrics
write.csv(metrics_table, "content_based_metrics.csv", row.names = FALSE)
cat("Summary metrics saved to 'content_based_metrics.csv'\n")

cat("\nContent-based filtering evaluation complete!\n")