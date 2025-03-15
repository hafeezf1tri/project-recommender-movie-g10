# Content-Based Filtering Implementation for MovieLens Dataset
# This script creates a content-based recommendation model and saves it as an RDS file
# Includes enhanced progress tracking for time-consuming operations

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

# Create a progress bar function
create_progress_bar <- function(total, title = "Progress") {
  pb <- list(
    total = total,
    current = 0,
    start_time = Sys.time(),
    title = title
  )
  
  # Initialize progress bar
  cat("\n", title, ": 0%", "|", rep("-", 50), "| [0/", total, "] ETA: --:--\n", sep="")
  
  return(pb)
}

# Update progress bar function
update_progress_bar <- function(pb, increment = 1) {
  pb$current <- pb$current + increment
  progress_pct <- pb$current / pb$total
  
  # Calculate elapsed and estimated time
  elapsed <- difftime(Sys.time(), pb$start_time, units = "secs")
  if (progress_pct > 0) {
    eta_seconds <- as.numeric(elapsed) * (1 - progress_pct) / progress_pct
    eta_mins <- floor(eta_seconds / 60)
    eta_secs <- floor(eta_seconds %% 60)
    eta_text <- sprintf("%02d:%02d", eta_mins, eta_secs)
  } else {
    eta_text <- "--:--"
  }
  
  # Create progress bar visual
  bar_length <- 50
  filled_length <- round(bar_length * progress_pct)
  bar <- paste0(rep("=", filled_length), rep("-", bar_length - filled_length))
  
  # Clear previous line and print new progress bar
  cat("\r", pb$title, ": ", round(progress_pct * 100), "% |", bar, "| [", 
      pb$current, "/", pb$total, "] ETA: ", eta_text, "        ", sep="")
  
  # If complete, add a newline
  if (pb$current >= pb$total) {
    total_time <- as.numeric(difftime(Sys.time(), pb$start_time, units = "mins"))
    cat("\nCompleted in", round(total_time, 2), "minutes\n")
  }
  
  return(pb)
}

# Load the prepared data
cat("Loading prepared data...\n")
load("prepared_data.rdata")

# Print the available objects to understand the structure
cat("Available objects in prepared_data:\n")
print(names(prepared_data))

# Extract data from the prepared_data list
movies_filtered <- prepared_data$movies_filtered
movies_raw <- prepared_data$movies_raw
tags_raw <- prepared_data$tags_raw
ratings_filtered <- prepared_data$ratings_filtered
genre_matrix <- prepared_data$genre_matrix

# Decide which dataset to use (filtered or raw)
movies_to_use <- movies_filtered
if (is.null(movies_to_use) || nrow(movies_to_use) == 0) {
  movies_to_use <- movies_raw
}

ratings_to_use <- ratings_filtered
if (is.null(ratings_to_use) || nrow(ratings_to_use) == 0) {
  ratings_to_use <- prepared_data$ratings_raw
}

cat("Using", nrow(movies_to_use), "movies and", nrow(ratings_to_use), "ratings\n")

# STEP 1: Create or use existing genre matrix
if (is.null(genre_matrix) || !is.matrix(genre_matrix)) {
  cat("Creating new genre matrix...\n")
  
  # Extract unique genres
  all_genres <- movies_to_use$genres %>%
    strsplit("\\|") %>%
    unlist() %>%
    unique()
  
  # Remove empty genre if present
  unique_genres <- all_genres[all_genres != "(no genres listed)" & all_genres != ""]
  
  # Create a binary genre matrix (movies x genres)
  genre_matrix <- matrix(0, nrow = nrow(movies_to_use), ncol = length(unique_genres))
  colnames(genre_matrix) <- unique_genres
  rownames(genre_matrix) <- movies_to_use$movieId
  
  # Create progress bar
  pb <- create_progress_bar(nrow(movies_to_use), "Creating genre matrix")
  
  # Fill the matrix
  for (i in 1:nrow(movies_to_use)) {
    movie_genres <- unlist(strsplit(movies_to_use$genres[i], "\\|"))
    for (g in movie_genres) {
      if (g %in% unique_genres) {
        genre_matrix[i, g] <- 1
      }
    }
    
    # Update progress every 100 movies
    if (i %% 100 == 0 || i == nrow(movies_to_use)) {
      pb <- update_progress_bar(pb, 100)
    }
  }
} else {
  cat("Using existing genre matrix with dimensions", dim(genre_matrix), "\n")
  
  # Ensure rownames are set correctly
  if (is.null(rownames(genre_matrix))) {
    cat("Setting rownames for genre_matrix\n")
    rownames(genre_matrix) <- movies_to_use$movieId
  }
}

# STEP 2: Process movie titles
cat("\nProcessing movie titles...\n")

# Check if movies have a title_clean column
has_clean_titles <- "title_clean" %in% colnames(movies_to_use)

if (has_clean_titles) {
  title_text <- movies_to_use$title_clean
  cat("Using pre-cleaned titles\n")
} else {
  # Clean titles by removing year and special characters
  cat("Cleaning movie titles...\n")
  pb <- create_progress_bar(nrow(movies_to_use), "Cleaning titles")
  
  title_text <- vector("character", nrow(movies_to_use))
  
  for (i in 1:nrow(movies_to_use)) {
    title_text[i] <- movies_to_use$title[i] %>%
      str_replace_all("\\([^)]*\\)", "") %>%  # Remove content in parentheses (year)
      str_trim() %>%                          # Trim whitespace
      str_to_lower()                          # Convert to lowercase
    
    # Update progress every 100 movies
    if (i %% 100 == 0 || i == nrow(movies_to_use)) {
      pb <- update_progress_bar(pb, min(100, nrow(movies_to_use) - (i - 100)))
    }
  }
}

# Create a document-term matrix for titles
cat("Creating document-term matrix for titles...\n")
title_corpus <- VCorpus(VectorSource(title_text))

cat("Processing title corpus (this may take a moment)...\n")
start_time <- Sys.time()

title_corpus <- title_corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

cat("Creating document-term matrix...\n")
title_dtm <- DocumentTermMatrix(title_corpus)

# Apply TF-IDF weighting to title terms
cat("Applying TF-IDF weighting...\n")
title_tfidf <- weightTfIdf(title_dtm)

# Convert to a matrix
title_matrix <- as.matrix(title_tfidf)
rownames(title_matrix) <- movies_to_use$movieId

end_time <- Sys.time()
proc_time <- difftime(end_time, start_time, units = "secs")
cat("Title text processed in", round(proc_time, 2), "seconds\n")
cat("Title matrix dimensions:", dim(title_matrix), "\n")

# STEP 3: Process tags if available
if (!is.null(tags_raw) && nrow(tags_raw) > 0) {
  cat("\nProcessing movie tags...\n")
  
  # Aggregate tags by movie
  cat("Aggregating tags by movie...\n")
  movie_tags <- tags_raw %>%
    group_by(movieId) %>%
    summarize(all_tags = paste(tag, collapse = " "))
  
  cat("Aggregated tags for", nrow(movie_tags), "movies\n")
  
  # Create a corpus from tags
  cat("Creating and processing tag corpus...\n")
  start_time <- Sys.time()
  
  tags_corpus <- VCorpus(VectorSource(movie_tags$all_tags))
  tags_corpus <- tags_corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stripWhitespace) %>%
    tm_map(stemDocument)
  
  cat("Creating tag document-term matrix...\n")
  tags_dtm <- DocumentTermMatrix(tags_corpus)
  
  # Apply TF-IDF weighting
  cat("Applying TF-IDF weighting to tags...\n")
  tags_tfidf <- weightTfIdf(tags_dtm)
  
  # Convert to a matrix
  tags_matrix <- as.matrix(tags_tfidf)
  rownames(tags_matrix) <- movie_tags$movieId
  
  end_time <- Sys.time()
  proc_time <- difftime(end_time, start_time, units = "secs")
  cat("Tag processing completed in", round(proc_time, 2), "seconds\n")
  cat("Tag matrix dimensions:", dim(tags_matrix), "\n")
  
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
  cat("\nNo tag data available, proceeding without tags\n")
}

# STEP 4: Combine Features
cat("\nCombining features for content-based filtering...\n")

# We'll create a unified feature matrix with appropriate weights
# Default weights for different feature types
genre_weight <- 3.0  # Genres are typically most important
title_weight <- 1.0  # Titles have some keywords but can be noisy
tag_weight <- 2.0    # Tags are user-generated and quite relevant

# Get a common set of movie IDs
movie_ids <- as.character(movies_to_use$movieId)

# Ensure matrices have proper dimensions
if (nrow(genre_matrix) != length(movie_ids)) {
  cat("Warning: Genre matrix dimensions don't match movies_to_use\n")
  cat("Genre matrix rows:", nrow(genre_matrix), "\n")
  cat("Movies to use rows:", length(movie_ids), "\n")
  
  # Find the intersection of IDs
  common_ids <- intersect(rownames(genre_matrix), movie_ids)
  cat("Using", length(common_ids), "common movie IDs\n")
  movie_ids <- common_ids
} else {
  cat("Genre matrix dimensions match movies_to_use\n")
}

# Ensure title matrix has the same movies
if (!all(movie_ids %in% rownames(title_matrix))) {
  cat("Warning: Not all movies have title data\n")
  common_ids <- intersect(movie_ids, rownames(title_matrix))
  cat("Using", length(common_ids), "common movie IDs\n")
  movie_ids <- common_ids
}

# Calculate feature dimensions
n_movies <- length(movie_ids)
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
cat("Populating combined feature matrix...\n")
combined_features <- matrix(0, nrow = n_movies, ncol = total_features)
rownames(combined_features) <- movie_ids
colnames(combined_features) <- feature_names

# Fill in features with appropriate weights
feature_start <- 1
feature_end <- n_genre_features

# Add genre features
cat("Adding genre features (weight:", genre_weight, ")...\n")
combined_features[, feature_start:feature_end] <- genre_weight * genre_matrix[movie_ids, ]
feature_start <- feature_end + 1
feature_end <- feature_start + n_title_features - 1

# Add title features
cat("Adding title features (weight:", title_weight, ")...\n")
combined_features[, feature_start:feature_end] <- title_weight * title_matrix[movie_ids, ]

# Add tag features if available
if (exists("tags_matrix")) {
  cat("Adding tag features (weight:", tag_weight, ")...\n")
  feature_start <- feature_end + 1
  feature_end <- feature_start + n_tag_features - 1
  
  # Make sure all movie_ids are in tags_matrix
  common_tag_ids <- intersect(movie_ids, rownames(tags_matrix))
  if (length(common_tag_ids) < length(movie_ids)) {
    cat("Warning: Not all movies have tag data. Using available tag data.\n")
    
    # Create progress bar
    pb <- create_progress_bar(length(common_tag_ids), "Adding tag features")
    
    count <- 0
    for (id in common_tag_ids) {
      idx <- which(movie_ids == id)
      tag_idx <- which(rownames(tags_matrix) == id)
      combined_features[idx, feature_start:feature_end] <- tag_weight * tags_matrix[tag_idx, ]
      count <- count + 1
      
      # Update progress every 100 movies
      if (count %% 100 == 0 || count == length(common_tag_ids)) {
        pb <- update_progress_bar(pb, min(100, length(common_tag_ids) - (count - 100)))
      }
    }
  } else {
    combined_features[, feature_start:feature_end] <- tag_weight * tags_matrix[movie_ids, ]
  }
}

cat("Combined feature matrix created with dimensions:", dim(combined_features), "\n")

# STEP 5: Calculate Item Similarity
cat("\nCalculating movie similarity matrix (this may take a while)...\n")

# Add progress tracking
n_movies <- nrow(combined_features)
cat("Calculating similarity matrix for", n_movies, "movies\n")

# Determine the best approach based on matrix size
start_time <- Sys.time()

if (n_movies <= 3000) {  # For smaller datasets
  cat("Using direct similarity calculation approach\n")
  
  # Calculate the entire similarity matrix at once (faster but more memory-intensive)
  movie_similarity <- proxy::simil(combined_features, method = "cosine")
  movie_sim_matrix <- as.matrix(movie_similarity)
  
} else {  # For larger datasets, calculate row by row with detailed progress tracking
  cat("Using row-by-row calculation approach to manage memory\n")
  
  # Initialize the similarity matrix
  movie_sim_matrix <- matrix(0, nrow = n_movies, ncol = n_movies)
  rownames(movie_sim_matrix) <- rownames(combined_features)
  colnames(movie_sim_matrix) <- rownames(combined_features)
  
  # Create progress bar for row-by-row calculation
  pb <- create_progress_bar(n_movies, "Calculating similarity matrix")
  
  # Calculate similarity row by row with progress updates
  for (i in 1:n_movies) {
    # Option 1: Calculate by dot product (usually faster)
    movie_i <- combined_features[i, , drop = FALSE]
    
    # Get the norm of movie_i
    norm_i <- sqrt(sum(movie_i^2))
    
    for (j in 1:n_movies) {
      # Only calculate for j >= i (symmetric matrix)
      if (j >= i) {
        movie_j <- combined_features[j, , drop = FALSE]
        norm_j <- sqrt(sum(movie_j^2))
        
        # Cosine similarity = dot product / (norm_i * norm_j)
        dot_product <- sum(movie_i * movie_j)
        sim_value <- if (norm_i * norm_j > 0) dot_product / (norm_i * norm_j) else 0
        
        # Store in both places (symmetric matrix)
        movie_sim_matrix[i, j] <- sim_value
        movie_sim_matrix[j, i] <- sim_value
      }
    }
    
    # Update progress
    if (i %% 10 == 0 || i == n_movies) {
      pb <- update_progress_bar(pb, min(10, n_movies - (i - 10)))
    }
  }
}

end_time <- Sys.time()
calc_time <- difftime(end_time, start_time, units = "mins")
cat("\nSimilarity matrix calculation completed in", round(calc_time, 2), "minutes\n")

# Verify the similarity matrix
cat("Similarity matrix dimensions:", dim(movie_sim_matrix), "\n")

# Check sparsity (percentage of near-zero values)
sparsity <- mean(movie_sim_matrix < 0.001)
cat("Similarity matrix sparsity:", round(sparsity * 100, 2), "%\n")

# STEP 6: Define recommendation functions
cat("\nDefining recommendation functions...\n")

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

# STEP 7: Test the recommendation functions
cat("\nTesting recommendation functions...\n")

# 7.1 Test get_similar_movies function
cat("Testing get_similar_movies function...\n")
test_movie_id <- as.numeric(movie_ids[1])  # Use first movie
test_movie_title <- movies_to_use$title[movies_to_use$movieId == test_movie_id]

similar_movies <- get_similar_movies(test_movie_id, movie_sim_matrix, movies_to_use, n = 5)
cat("Similar movies to '", test_movie_title, "':\n", sep="")
print(similar_movies[, c("title", "genres", "similarity")])

# 7.2 Test get_user_recommendations function
cat("\nTesting get_user_recommendations function...\n")
if (nrow(ratings_to_use) > 0) {
  test_user_id <- ratings_to_use$userId[1]  # Use first user
  user_recs <- get_user_recommendations(test_user_id, ratings_to_use, 
                                        movie_sim_matrix, movies_to_use, n = 5)
  cat("Recommendations for user", test_user_id, ":\n")
  print(user_recs[, c("title", "genres", "score")])
} else {
  cat("No ratings data available to test user recommendations\n")
}

# STEP 8: Package the model for RDS export
cat("\nPackaging the model for export...\n")

# Create a list with all the necessary components
content_based_model <- list(
  movie_sim_matrix = movie_sim_matrix,
  movies = movies_to_use,
  get_similar_movies = get_similar_movies,
  get_user_recommendations = get_user_recommendations
)

# Save the model as RDS
save_start_time <- Sys.time()
cat("Saving model to RDS file...\n")
saveRDS(content_based_model, file = "content_based_model.rds")
save_end_time <- Sys.time()
save_time <- difftime(save_end_time, save_start_time, units = "secs")

cat("Model saved to 'content_based_model.rds' in", round(save_time, 2), "seconds\n")

# Get file size
model_file_size <- file.size("content_based_model.rds") / (1024^2)  # Size in MB
cat("File size:", round(model_file_size, 2), "MB\n")
