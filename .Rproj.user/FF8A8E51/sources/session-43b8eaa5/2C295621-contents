# Unified MovieLens Data Preparation
# This script prepares the MovieLens dataset for multiple recommendation algorithms

# Load required libraries
library(recommenderlab)
library(tidyverse)
library(reshape2)
library(Matrix)

# Set random seed for reproducibility
set.seed(123)

# ===== 1. LOAD RAW DATA =====
cat("Loading MovieLens CSV files...\n")

# Set the data directory path
data_dir <- "data/"

# Check if the data directory exists
if (!dir.exists(data_dir)) {
  stop(paste("Error: Directory", data_dir, "does not exist!"))
}

# Load the datasets
movies <- read.csv(paste0(data_dir, "movies.csv"), stringsAsFactors = FALSE)
ratings <- read.csv(paste0(data_dir, "ratings.csv"), stringsAsFactors = FALSE)
tags <- read.csv(paste0(data_dir, "tags.csv"), stringsAsFactors = FALSE)
links <- read.csv(paste0(data_dir, "links.csv"), stringsAsFactors = FALSE)

cat("Successfully loaded", nrow(movies), "movies,", nrow(ratings), "ratings,", 
    nrow(tags), "tags, and", nrow(links), "links\n")

# ===== 2. CLEAN AND PREPROCESS DATA =====
cat("\nCleaning and preprocessing data...\n")

# 2.1 Add year and clean title information to movies
movies <- movies %>%
  mutate(
    year = as.numeric(str_extract(title, "\\(\\d{4}\\)")),
    year = as.numeric(gsub("[\\(\\)]", "", year)),
    title_clean = gsub(" \\(\\d{4}\\)", "", title)
  )

# 2.2 Convert timestamps to dates
ratings <- ratings %>%
  mutate(date = as.POSIXct(timestamp, origin = "1970-01-01"))

if (nrow(tags) > 0) {
  tags <- tags %>%
    mutate(date = as.POSIXct(timestamp, origin = "1970-01-01"))
}

# 2.3 Remove any duplicate ratings (keep most recent)
duplicate_count <- ratings %>%
  group_by(userId, movieId) %>%
  filter(n() > 1) %>%
  nrow()

if (duplicate_count > 0) {
  cat("Found", duplicate_count, "duplicate ratings. Removing duplicates...\n")
  ratings <- ratings %>%
    group_by(userId, movieId) %>%
    arrange(desc(timestamp)) %>%
    slice(1) %>%
    ungroup()
}

# ===== 3. ANALYZE USER AND MOVIE ACTIVITY =====
cat("\nAnalyzing user and movie activity...\n")

# Count ratings per user and per movie
user_activity <- ratings %>%
  group_by(userId) %>%
  summarise(
    rating_count = n(),
    avg_rating = mean(rating),
    rating_std = sd(rating),
    first_rating = min(date),
    last_rating = max(date)
  )

movie_popularity <- ratings %>%
  group_by(movieId) %>%
  summarise(
    rating_count = n(),
    avg_rating = mean(rating),
    rating_std = sd(rating)
  )

# Display activity summaries
cat("User activity summary:\n")
print(summary(user_activity$rating_count))

cat("\nMovie popularity summary:\n")
print(summary(movie_popularity$rating_count))

# ===== 4. DATA FILTERING FOR QUALITY =====
cat("\nFiltering data to improve quality...\n")

# Set thresholds for filtering (balanced for different algorithms)
min_user_ratings <- 15  # Each user must have rated at least 15 movies
min_movie_ratings <- 10  # Each movie must have at least 10 ratings

# Apply filters
active_users <- user_activity %>%
  filter(rating_count >= min_user_ratings) %>%
  pull(userId)

popular_movies <- movie_popularity %>%
  filter(rating_count >= min_movie_ratings) %>%
  pull(movieId)

# Filter ratings
ratings_filtered <- ratings %>%
  filter(userId %in% active_users & movieId %in% popular_movies)

# Filter movies to include only popular ones
movies_filtered <- movies %>%
  filter(movieId %in% popular_movies)

# Update movie popularity statistics
movie_popularity_filtered <- movie_popularity %>%
  filter(movieId %in% popular_movies)

# Update user activity statistics
user_activity_filtered <- user_activity %>%
  filter(userId %in% active_users)

# Report on filtered dataset
cat("After filtering:\n")
cat("- Users:", length(active_users), "out of", length(unique(ratings$userId)), 
    sprintf("(%.1f%%)", length(active_users)/length(unique(ratings$userId))*100), "\n")
cat("- Movies:", length(popular_movies), "out of", nrow(movies), 
    sprintf("(%.1f%%)", length(popular_movies)/nrow(movies)*100), "\n")
cat("- Ratings:", nrow(ratings_filtered), "out of", nrow(ratings), 
    sprintf("(%.1f%%)", nrow(ratings_filtered)/nrow(ratings)*100), "\n")

# ===== 5. CREATE RATINGS MATRIX =====
cat("\nCreating ratings matrix...\n")

# Create the ratings matrix (userId x movieId)
ratings_matrix <- dcast(ratings_filtered, userId ~ movieId, value.var = "rating")

# Set rownames to userId for easier indexing
rownames(ratings_matrix) <- ratings_matrix$userId
ratings_matrix$userId <- NULL  # Remove userId column

# Convert to matrix
ratings_matrix <- as.matrix(ratings_matrix)

# Convert to recommenderlab realRatingMatrix (better for recommendations)
ratings_sparse <- as(ratings_matrix, "realRatingMatrix")

# Report on matrix dimensions and sparsity
dims <- dim(ratings_sparse)
sparsity <- 1 - sum(ratings_sparse@data != 0) / (dims[1] * dims[2])
cat("Ratings matrix dimensions:", dims[1], "users x", dims[2], "movies\n")
cat("Sparsity level:", sprintf("%.2f%%", sparsity * 100), "\n")

# ===== 6. NORMALIZE RATINGS =====
cat("\nCreating normalized versions of the rating matrix...\n")

# Create different normalizations for flexibility with different algorithms
ratings_center <- normalize(ratings_sparse, method = "center")
ratings_z <- normalize(ratings_sparse, method = "Z-score")

cat("Created both center-normalized and Z-score normalized matrices\n")

# ===== 7. EXTRACT GENRE INFORMATION =====
cat("\nExtracting genre information...\n")

# Get all unique genres
genres_list <- strsplit(movies_filtered$genres, "\\|")
unique_genres <- unique(unlist(genres_list))
unique_genres <- unique_genres[unique_genres != "(no genres listed)"]

cat("Found", length(unique_genres), "unique genres\n")

# Create movie-genre matrix (for content-based filtering)
genre_matrix <- matrix(0, nrow = nrow(movies_filtered), ncol = length(unique_genres))
colnames(genre_matrix) <- unique_genres
rownames(genre_matrix) <- movies_filtered$movieId

for (i in 1:nrow(movies_filtered)) {
  movie_genres <- unlist(strsplit(movies_filtered$genres[i], "\\|"))
  for (genre in movie_genres) {
    if (genre %in% unique_genres) {
      col_idx <- which(unique_genres == genre)
      genre_matrix[i, col_idx] <- 1
    }
  }
}

# Convert to a data frame and add movieId for easier joins
genre_df <- as.data.frame(genre_matrix)
genre_df$movieId <- as.numeric(rownames(genre_df))

# ===== 8. CREATE AND SAVE FINAL PREPARED DATASET =====
cat("\nCreating and saving final prepared dataset...\n")

# Bring all prepared data together
prepared_data <- list(
  # Original data
  ratings_raw = ratings,
  movies_raw = movies,
  tags_raw = tags,
  links_raw = links,
  
  # Filtered data
  ratings_filtered = ratings_filtered,
  movies_filtered = movies_filtered,
  
  # Matrix representations
  ratings_matrix = ratings_matrix,         # Regular matrix
  ratings_sparse = ratings_sparse,         # Raw sparse matrix (no normalization)
  ratings_center = ratings_center,         # Center-normalized (for IBCF)
  ratings_z = ratings_z,                   # Z-score normalized (for UBCF)
  
  # Additional information
  genre_matrix = genre_matrix,             # For content-based approaches
  movie_popularity = movie_popularity_filtered,  # Movie statistics
  user_activity = user_activity_filtered,  # User statistics
  
  # Metadata about the preparation process
  preparation_info = list(
    original_users = length(unique(ratings$userId)),
    original_movies = nrow(movies),
    original_ratings = nrow(ratings),
    filtered_users = length(active_users),
    filtered_movies = length(popular_movies),
    filtered_ratings = nrow(ratings_filtered),
    min_user_ratings = min_user_ratings,
    min_movie_ratings = min_movie_ratings,
    matrix_dims = dims,
    sparsity = sparsity,
    preparation_date = Sys.Date(),
    unique_genres = unique_genres
  )
)

# Save the prepared data
save(prepared_data, file = "prepared_data.RData")

# Also save the filtered data as CSVs if needed for other purposes
write.csv(ratings_filtered, "data/ratings_filtered.csv", row.names = FALSE)
write.csv(movies_filtered, "data/movies_filtered.csv", row.names = FALSE)

cat("\nData preparation complete!\n")
cat("Prepared data saved to 'prepared_data.RData'\n")
cat("Filtered CSVs saved to 'data/ratings_filtered.csv' and 'data/movies_filtered.csv'\n")
cat("\nReady for recommendation system modeling!\n")