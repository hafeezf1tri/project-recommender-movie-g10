# Load necessary libraries
library(tidyverse)
library(Matrix)      # For sparse matrices
library(recommenderlab)  # For recommendation algorithms

# Load the processed data from exploration phase
# Assuming you've saved the processed data or run the exploration script

# Handle missing values
movies_clean <- movies %>%
  filter(!is.na(year)) %>%
  filter(genres != "(no genres listed)")

# Filter out users and movies with too few ratings
min_ratings_user <- 5   # Minimum ratings per user
min_ratings_movie <- 5  # Minimum ratings per movie

# Find users and movies meeting minimum criteria
active_users <- ratings %>%
  count(userId) %>%
  filter(n >= min_ratings_user) %>%
  pull(userId)

popular_movies <- ratings %>%
  count(movieId) %>%
  filter(n >= min_ratings_movie) %>%
  pull(movieId)

# Filter ratings
ratings_filtered <- ratings %>%
  filter(userId %in% active_users, 
         movieId %in% popular_movies)

# Create user-item matrix for collaborative filtering
ratings_matrix <- ratings_filtered %>%
  select(userId, movieId, rating) %>%
  spread(movieId, rating)

# Convert to matrix format
matrix_for_recommender <- as.matrix(ratings_matrix[,-1])
rownames(matrix_for_recommender) <- ratings_matrix$userId

# Convert to recommenderlab format
r_matrix <- as(matrix_for_recommender, "realRatingMatrix")

# Create movie features for content-based filtering
# Extract genres into one-hot encoding
genres_list <- movies %>%
  mutate(genres = strsplit(as.character(genres), "\\|")) %>%
  unnest(genres) %>%
  distinct(genres) %>%
  pull(genres)

# Create genre matrix
genre_matrix <- matrix(0, 
                       nrow = nrow(movies_clean), 
                       ncol = length(genres_list),
                       dimnames = list(movies_clean$movieId, genres_list))

# Fill genre matrix
for (i in 1:nrow(movies_clean)) {
  movie_genres <- unlist(strsplit(as.character(movies_clean$genres[i]), "\\|"))
  genre_idx <- which(genres_list %in% movie_genres)
  genre_matrix[i, genre_idx] <- 1
}

# Create train-test split
set.seed(123)
evaluation_scheme <- evaluationScheme(r_matrix, 
                                      method = "split",
                                      train = 0.8,
                                      given = 5,  # Number of items to use for prediction
                                      goodRating = 3.5)  # Threshold for "good" rating

# Save prepared data for modeling
save(movies_clean, ratings_filtered, r_matrix, genre_matrix, evaluation_scheme,
     file = "data/prepared_data.RData")