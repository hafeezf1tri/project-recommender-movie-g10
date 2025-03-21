# Content-Based Filtering Implementation for MovieLens Dataset
# This script works with the prepared_data.rdata nested structure

# Load necessary libraries
library(tidyverse)  # For data manipulation
library(tm)         # For text mining
library(Matrix)     # For sparse matrix operations
library(proxy)      # For similarity calculations
library(stringr)    # For string manipulation

# Check if packages are installed, install if needed
required_packages <- c("tidyverse", "tm", "Matrix", "proxy", "stringr")
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(new_packages) > 0) {
  cat("Installing required packages:", paste(new_packages, collapse=", "), "\n")
  install.packages(new_packages, repos="https://cloud.r-project.org/")
  # Load newly installed packages
  lapply(new_packages, library, character.only = TRUE)
}

# Load the prepared data
load("prepared_data.rdata")

cat("Loaded prepared_data structure\n")

# Extract data from the prepared_data list
movies_filtered <- prepared_data$movies_filtered
movies_raw <- prepared_data$movies_raw
tags_raw <- prepared_data$tags_raw
ratings_raw <- prepared_data$ratings_raw
ratings_filtered <- prepared_data$ratings_filtered
genre_matrix <- prepared_data$genre_matrix
movie_popularity <- prepared_data$movie_popularity
user_activity <- prepared_data$user_activity

# Print basic information about the data
cat("Movies (filtered):", nrow(movies_filtered), "rows\n")
cat("Movies (raw):", nrow(movies_raw), "rows\n")
cat("Tags:", nrow(tags_raw), "rows\n")
cat("Ratings (filtered):", nrow(ratings_filtered), "rows\n")
cat("Genre matrix dimensions:", dim(genre_matrix), "\n")

# 1. Select which dataset to use
cat("\n1. Preparing data for content-based filtering...\n")

# We'll use the filtered dataset since it's likely already cleaned
movies_to_use <- movies_filtered
cat("Using filtered movie dataset with", nrow(movies_to_use), "movies\n")

# Check what columns we have in the movies dataset
cat("Movie dataset columns:", paste(colnames(movies_to_use), collapse=", "), "\n")

# 2. Process movie titles for TF-IDF
cat("\n2. Processing movie titles...\n")

# Check if movies have a title_clean column
has_clean_titles <- "title_clean" %in% colnames(movies_to_use)

if (has_clean_titles) {
  title_text <- movies_to_use$title_clean
  cat("Using pre-cleaned titles\n")
} else {
  # Clean titles by removing year and special characters
  title_text <- movies_to_use$title %>%
    str_replace_all("\\([^)]*\\)", "") %>%  # Remove content in parentheses (year)
    str_trim() %>%                          # Trim whitespace
    str_to_lower()                          # Convert to lowercase
  cat("Cleaned titles manually\n")
}

# Create a document-term matrix for titles
title_corpus <- VCorpus(VectorSource(title_text))
title_corpus <- title_corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

title_dtm <- DocumentTermMatrix(title_corpus)

# Apply TF-IDF weighting to title terms
title_tfidf <- weightTfIdf(title_dtm)

# Convert to a matrix
title_matrix <- as.matrix(title_tfidf)
rownames(title_matrix) <- movies_to_use$movieId

cat("Title text processed into matrix with dimensions:", dim(title_matrix), "\n")

# 3. Process tags if available
cat("\n3. Processing movie tags...\n")

if (nrow(tags_raw) > 0) {
  # Aggregate tags by movie
  movie_tags <- tags_raw %>%
    group_by(movieId) %>%
    summarize(all_tags = paste(tag, collapse = " "))
  
  cat("Aggregated tags for", nrow(movie_tags), "movies\n")
  
  # Create a corpus from tags
  tags_corpus <- VCorpus(VectorSource(movie_tags$all_tags))
  tags_corpus <- tags_corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stripWhitespace) %>%
    tm_map(stemDocument)
  
  tags_dtm <- DocumentTermMatrix(tags_corpus)
  
  # Apply TF-IDF weighting
  tags_tfidf <- weightTfIdf(tags_dtm)
  
  # Convert to a matrix
  tags_matrix <- as.matrix(tags_tfidf)
  rownames(tags_matrix) <- movie_tags$movieId
  
  cat("Tag features processed into matrix with dimensions:", dim(tags_matrix), "\n")
  
  # Ensure we have tags for all movies in our dataset
  movie_ids <- as.character(movies_to_use$movieId)
  tag_ids <- as.character(movie_tags$movieId)
  
  # Count missing movies
  missing_count <- sum(!movie_ids %in% tag_ids)
  cat(missing_count, "movies don't have tag data\n")
  
  # For movies without tags, we'll need to add dummy rows to tags_matrix
  if (missing_count > 0) {
    cat("Creating dummy tag vectors for movies without tags...\n")
    
    # Create a matrix of zeros for missing movies
    missing_ids <- movie_ids[!movie_ids %in% tag_ids]
    dummy_matrix <- matrix(0, nrow = length(missing_ids), ncol = ncol(tags_matrix))
    rownames(dummy_matrix) <- missing_ids
    colnames(dummy_matrix) <- colnames(tags_matrix)
    
    # Add dummy rows to tags_matrix
    tags_combined <- rbind(tags_matrix, dummy_matrix)
    
    # Reorder to match movies_to_use
    tags_matrix <- tags_combined[movie_ids, ]
    cat("Updated tag matrix dimensions:", dim(tags_matrix), "\n")
  }
} else {
  cat("No tag data available\n")
}

# 4. Combine Features
cat("\n4. Combining features for content-based filtering...\n")

# We'll create a unified feature matrix with appropriate weights
# Default weights for different feature types
genre_weight <- 3.0  # Genres are typically most important
title_weight <- 1.0  # Titles have some keywords but can be noisy
tag_weight <- 2.0    # Tags are user-generated and quite relevant

# Create a common set of movie IDs that exist in all matrices
movie_ids <- as.character(movies_to_use$movieId)

# Check genre matrix dimensions and alignment
if (nrow(genre_matrix) != nrow(movies_to_use)) {
  cat("Warning: Genre matrix dimensions don't match movies_to_use\n")
  cat("Genre matrix rows:", nrow(genre_matrix), "\n")
  cat("Movies to use rows:", nrow(movies_to_use), "\n")
  
  # Make sure rownames are set for genre_matrix
  if (is.null(rownames(genre_matrix))) {
    cat("Setting rownames for genre_matrix\n")
    rownames(genre_matrix) <- movies_to_use$movieId
  }
}

# Ensure all matrices have the same movies in the same order
common_ids <- movie_ids

# 4.1 Feature Combination
# Start with the counts for our final feature matrix
n_movies <- length(common_ids)
n_genre_features <- ncol(genre_matrix)
n_title_features <- ncol(title_matrix)
n_tag_features <- if (exists("tags_matrix")) ncol(tags_matrix) else 0

total_features <- n_genre_features + n_title_features + n_tag_features
cat("Creating combined feature matrix with", n_movies, "movies and", total_features, "features\n")

# Create feature column names
feature_names <- c(
  paste0("genre_", colnames(genre_matrix)),
  paste0("title_", colnames(title_matrix))
)

if (exists("tags_matrix")) {
  feature_names <- c(feature_names, paste0("tag_", colnames(tags_matrix)))
}

# Create the combined feature matrix
combined_features <- matrix(0, nrow = n_movies, ncol = total_features)
rownames(combined_features) <- common_ids
colnames(combined_features) <- feature_names

# Fill in features with appropriate weights
feature_start <- 1
feature_end <- n_genre_features

# Add genre features
if (is.null(rownames(genre_matrix))) {
  # If genre_matrix doesn't have rownames, assume rows are aligned with movies_to_use
  combined_features[, feature_start:feature_end] <- genre_weight * genre_matrix
} else {
  # Use rownames to align data
  combined_features[, feature_start:feature_end] <- genre_weight * genre_matrix[common_ids, ]
}

feature_start <- feature_end + 1
feature_end <- feature_start + n_title_features - 1

# Add title features
combined_features[, feature_start:feature_end] <- title_weight * title_matrix[common_ids, ]

# Add tag features if available
if (exists("tags_matrix")) {
  feature_start <- feature_end + 1
  feature_end <- feature_start + n_tag_features - 1
  combined_features[, feature_start:feature_end] <- tag_weight * tags_matrix[common_ids, ]
}

cat("Combined feature matrix created with dimensions:", dim(combined_features), "\n")

# 5. Calculate Item Similarity
cat("\n5. Calculating movie similarity...\n")

# Add progress tracking
n_movies <- nrow(combined_features)
cat("Calculating similarity matrix for", n_movies, "movies...\n")
cat("This may take some time. Progress updates will appear every 10%.\n")

# We'll use cosine similarity with progress tracking
start_time <- Sys.time()
progress_step <- max(1, floor(n_movies/10))  # Update every 10%

# Initialize matrix
movie_sim_matrix <- matrix(0, nrow = n_movies, ncol = n_movies)
rownames(movie_sim_matrix) <- rownames(combined_features)
colnames(movie_sim_matrix) <- rownames(combined_features)

# Calculate similarity row by row with progress updates
for (i in 1:n_movies) {
  # Calculate cosine similarity between movie i and all other movies
  for (j in 1:n_movies) {
    if (i >= j) {  # Only calculate the lower triangle + diagonal
      sim <- crossprod(combined_features[i,], combined_features[j,]) / 
        (sqrt(crossprod(combined_features[i,])) * sqrt(crossprod(combined_features[j,])))
      movie_sim_matrix[i,j] <- sim
      movie_sim_matrix[j,i] <- sim  # Matrix is symmetric
    }
  }
  
  # Show progress
  if (i %% progress_step == 0 || i == n_movies) {
    pct_done <- round(i/n_movies * 100)
    elapsed <- difftime(Sys.time(), start_time, units = "mins")
    cat(pct_done, "% complete. Elapsed time:", round(elapsed, 2), "minutes\n")
  }
}

end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "mins")
cat("Similarity matrix created with dimensions:", dim(movie_sim_matrix), "\n")
cat("Calculation completed in", round(total_time, 2), "minutes\n")

# We'll use cosine similarity, which is appropriate for sparse TF-IDF vectors
movie_similarity <- proxy::simil(combined_features, method = "cosine")

# Convert to a matrix for easier manipulation
movie_sim_matrix <- as.matrix(movie_similarity)

cat("Similarity matrix created with dimensions:", dim(movie_sim_matrix), "\n")

# Calculate sparsity (percentage of zero values)
sim_sparsity <- sum(movie_sim_matrix < 0.001) / prod(dim(movie_sim_matrix))
cat("Similarity matrix sparsity:", round(sim_sparsity * 100, 2), "%\n")

# 6. Define recommendation functions
cat("\n6. Defining recommendation functions...\n")

# 6.1 Function to get similar movies
get_similar_movies <- function(movie_id, sim_matrix, movie_data, n = 10) {
  # Find movie index
  movie_idx <- which(rownames(sim_matrix) == as.character(movie_id))
  
  if (length(movie_idx) == 0) {
    return(data.frame(movieId = integer(), 
                      title = character(), 
                      genres = character(), 
                      similarity = numeric()))
  }
  
  # Get similarity scores for this movie
  sim_scores <- sim_matrix[movie_idx, ]
  
  # Create a data frame with movie IDs and similarity scores
  similar_movies <- data.frame(
    movieId = as.integer(names(sim_scores)),
    similarity = as.numeric(sim_scores),
    stringsAsFactors = FALSE
  )
  
  # Remove the movie itself
  similar_movies <- similar_movies[similar_movies$movieId != movie_id, ]
  
  # Get top N similar movies
  top_similar <- similar_movies %>%
    arrange(desc(similarity)) %>%
    head(n)
  
  # Join with movie data to get titles and genres
  results <- top_similar %>%
    left_join(movie_data[, c("movieId", "title", "genres")], by = "movieId")
  
  return(results)
}

# 6.2 Function to get user recommendations
get_user_recommendations <- function(user_id, ratings_df, sim_matrix, movie_data, 
                                     n = 10, rating_threshold = 4) {
  # Get the user's ratings
  user_ratings <- ratings_df %>% 
    filter(userId == user_id)
  
  # If user has no ratings, return empty dataframe
  if (nrow(user_ratings) == 0) {
    return(data.frame(movieId = integer(), title = character(), 
                      genres = character(), score = numeric()))
  }
  
  # Find movies the user liked (rated highly)
  liked_movies <- user_ratings %>%
    filter(rating >= rating_threshold) %>%
    pull(movieId)
  
  # If user has no highly rated movies, lower the threshold
  if (length(liked_movies) == 0) {
    rating_threshold <- mean(user_ratings$rating)
    liked_movies <- user_ratings %>%
      filter(rating >= rating_threshold) %>%
      pull(movieId)
  }
  
  # Get all movies the user has already rated
  rated_movies <- user_ratings %>%
    pull(movieId)
  
  # Initialize scores for all movies
  all_movie_ids <- as.integer(rownames(sim_matrix))
  recommendation_scores <- rep(0, length(all_movie_ids))
  names(recommendation_scores) <- all_movie_ids
  
  # Compute scores based on similarity to movies the user liked
  for (movie_id in liked_movies) {
    # Get the movie's index in the similarity matrix
    movie_idx <- which(rownames(sim_matrix) == as.character(movie_id))
    
    if (length(movie_idx) > 0) {
      # Get the rating the user gave to this movie
      user_rating <- user_ratings$rating[user_ratings$movieId == movie_id]
      
      # Weight by how much the user liked the movie
      rating_weight <- (user_rating - min(user_ratings$rating)) / 
        (max(user_ratings$rating) - min(user_ratings$rating))
      if (is.na(rating_weight)) rating_weight <- 1  # Handle case with only one rating
      
      # Get similarity scores
      sim_scores <- sim_matrix[movie_idx, ]
      
      # Add weighted similarity scores to recommendation scores
      recommendation_scores <- recommendation_scores + (rating_weight * sim_scores)
    }
  }
  
  # Convert to a data frame
  recommendations_df <- data.frame(
    movieId = as.integer(names(recommendation_scores)),
    score = recommendation_scores,
    stringsAsFactors = FALSE
  )
  
  # Remove movies the user has already rated
  recommendations_df <- recommendations_df %>%
    filter(!movieId %in% rated_movies)
  
  # Get top N recommendations
  top_recommendations <- recommendations_df %>%
    arrange(desc(score)) %>%
    head(n)
  
  # Join with movie data
  results <- top_recommendations %>%
    left_join(movie_data[, c("movieId", "title", "genres")], by = "movieId")
  
  return(results)
}

# 7. Demonstration Examples
cat("\n7. Demonstrating content-based recommendations...\n")

# 7.1 Example: Get similar movies
# Let's use a popular movie as an example
# Look for a popular movie like Star Wars
popular_movie_idx <- which(grepl("Star Wars|Matrix|Jurassic Park|Shawshank", movies_to_use$title))[1]
if (length(popular_movie_idx) > 0) {
  example_movie_id <- movies_to_use$movieId[popular_movie_idx]
} else {
  # If none found, use the first movie
  example_movie_id <- movies_to_use$movieId[1]
}

# Get movie details
example_movie <- movies_to_use[movies_to_use$movieId == example_movie_id, ]
cat("\nExample movie:", example_movie$title, "\n")
cat("Genres:", example_movie$genres, "\n")

# Check if the movie exists in our similarity matrix
if (as.character(example_movie_id) %in% rownames(movie_sim_matrix)) {
  # Get similar movies
  similar_movies <- get_similar_movies(example_movie_id, movie_sim_matrix, movies_to_use)
  cat("\nSimilar movies:\n")
  print(similar_movies[, c("title", "genres", "similarity")])
} else {
  cat("\nExample movie not found in similarity matrix.\n")
}

# 7.2 Example: Get user recommendations
# Let's use an active user (with many ratings)
if (exists("user_activity") && nrow(user_activity) > 0) {
  active_users <- user_activity %>%
    arrange(desc(rating_count)) %>%
    head(10) %>%
    pull(userId)
  
  example_user_id <- active_users[1]
} else {
  # Pick a user with many ratings from ratings dataset
  rating_counts_per_user <- ratings_filtered %>%
    group_by(userId) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  example_user_id <- rating_counts_per_user$userId[1]
}

cat("\nGenerating recommendations for user", example_user_id, "\n")

# Get movie recommendations for this user
user_recommendations <- get_user_recommendations(example_user_id, ratings_filtered, 
                                                 movie_sim_matrix, movies_to_use)
cat("\nRecommended movies:\n")
print(user_recommendations[, c("title", "genres", "score")])

# 8. Save the model
cat("\n8. Saving content-based filtering model...\n")

# Save the important objects for future use
content_based_model <- list(
  movie_sim_matrix = movie_sim_matrix,
  movies = movies_to_use,
  get_similar_movies = get_similar_movies,
  get_user_recommendations = get_user_recommendations
)

save(content_based_model, file = "content_based_model.rdata")
cat("Model saved to 'content_based_model.rdata'\n")

# 9. Evaluation (simplified)
cat("\n9. Performing simple model evaluation...\n")

# 9.1 Calculate coverage
coverage <- length(rownames(movie_sim_matrix)) / nrow(movies_to_use)
cat("Model coverage (% of movies included):", round(coverage * 100, 2), "%\n")

# 9.2 Check distribution of similarity scores
sim_values <- movie_sim_matrix[lower.tri(movie_sim_matrix)]
cat("Similarity score distribution:\n")
cat("  Min:", min(sim_values), "\n")
cat("  1st Quartile:", quantile(sim_values, 0.25), "\n")
cat("  Median:", median(sim_values), "\n")
cat("  3rd Quartile:", quantile(sim_values, 0.75), "\n")
cat("  Max:", max(sim_values), "\n")

# 9.3 Check diversity of recommendations
check_recommendation_diversity <- function() {
  # Select a few users to check
  if (exists("user_activity") && nrow(user_activity) > 0) {
    test_users <- user_activity %>%
      arrange(desc(rating_count)) %>%
      head(5) %>%
      pull(userId)
  } else {
    test_users <- unique(ratings_filtered$userId)[1:5]
  }
  
  # Get recommendations for each user
  all_recommendations <- lapply(test_users, function(uid) {
    get_user_recommendations(uid, ratings_filtered, movie_sim_matrix, movies_to_use, n = 10)
  })
  
  # Extract unique movie IDs from recommendations
  unique_movies <- unique(unlist(lapply(all_recommendations, function(rec) rec$movieId)))
  
  # Calculate diversity as percentage of unique recommendations
  total_recs <- sum(sapply(all_recommendations, nrow))
  diversity <- length(unique_movies) / total_recs
  
  cat("Recommendation diversity (higher is better):", round(diversity * 100, 2), "%\n")
  cat("Unique movies recommended:", length(unique_movies), "out of", total_recs, "total recommendations\n")
  
  # Check genre diversity
  genre_counts <- table(unlist(strsplit(sapply(unique_movies, function(mid) {
    movies_to_use$genres[movies_to_use$movieId == mid]
  }), "\\|")))
  
  cat("Genre distribution in recommendations:\n")
  genre_df <- data.frame(
    genre = names(genre_counts),
    count = as.numeric(genre_counts)
  ) %>% arrange(desc(count))
  
  print(head(genre_df, 10))
}

# Run diversity check
tryCatch({
  check_recommendation_diversity()
}, error = function(e) {
  cat("Error in diversity check:", conditionMessage(e), "\n")
})

cat("\nContent-based filtering implementation complete!\n")