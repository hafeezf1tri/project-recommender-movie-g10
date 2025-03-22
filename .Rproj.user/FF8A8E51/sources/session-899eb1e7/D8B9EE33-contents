# Simple User-Based Collaborative Filtering Implementation
# This uses a more direct approach to avoid dimension issues

# Load required libraries
library(recommenderlab)
library(tidyverse)
library(Matrix)

# Set random seed for reproducibility
set.seed(123)

# ===== 1. LOAD PREPARED DATA =====
cat("Loading prepared data...\n")
if (!file.exists("prepared_data.RData")) {
  stop("Error: prepared_data.RData file not found!")
}

load("prepared_data.RData")

# Check if we have the right data format
if (!exists("prepared_data") || !is.list(prepared_data)) {
  stop("Error: prepared_data does not have the expected structure")
}

# Use normalized ratings if available, otherwise use regular ratings
if ("ratings_z" %in% names(prepared_data) && 
    class(prepared_data$ratings_z)[1] == "realRatingMatrix") {
  ratings_data <- prepared_data$ratings_z
  cat("Using Z-score normalized ratings\n")
} else if ("ratings_sparse" %in% names(prepared_data) && 
           class(prepared_data$ratings_sparse)[1] == "realRatingMatrix") {
  ratings_data <- prepared_data$ratings_sparse
  cat("Using regular ratings (not normalized)\n")
} else {
  stop("Error: No valid rating matrix found in prepared data")
}

cat("Rating matrix dimensions:", dim(ratings_data), "\n")

# ===== 2. FILTER USERS WITH SUFFICIENT RATINGS =====
cat("\nFiltering users with sufficient ratings...\n")

# Get rating counts per user
ratings_per_user <- rowCounts(ratings_data)
min_ratings_needed <- 20  # Increasing this to ensure more stable results

# Identify users with enough ratings
valid_users <- which(ratings_per_user >= min_ratings_needed)
cat("Users with enough ratings:", length(valid_users), "out of", length(ratings_per_user), 
    sprintf("(%.1f%%)", length(valid_users)/length(ratings_per_user)*100), "\n")

# Filter the rating matrix to include only users with enough ratings
ratings_filtered <- ratings_data[valid_users, ]
cat("Filtered rating matrix dimensions:", dim(ratings_filtered), "\n")

# ===== 3. SIMPLE TRAIN-TEST SPLIT =====
cat("\nPerforming simple train-test split...\n")

# Use a simpler approach with a single train-test split instead of cross-validation
eval_sets <- evaluationScheme(
  data = ratings_filtered,
  method = "split",  # Simple train-test split
  train = 0.8,       # 80% training
  given = 5,         # Use at least 5 items per user for prediction
  goodRating = 3.5   # Threshold for "good" items
)

cat("Split created. Training set proportion:", eval_sets@train, "\n")

# ===== 4. TEST TWO SIMPLE MODELS =====
cat("\nTesting two UBCF models with different parameters...\n")

# Define algorithms to test (just two simple versions to verify functionality)
algorithms <- list(
  "UBCF-Cosine-25" = list(
    name = "UBCF",
    param = list(
      method = "cosine",
      nn = 25  # 25 nearest neighbors
    )
  ),
  "UBCF-Pearson-50" = list(
    name = "UBCF",
    param = list(
      method = "pearson",
      nn = 50  # 50 nearest neighbors
    )
  )
)

# Train and evaluate models
results <- evaluate(
  x = eval_sets,
  method = algorithms,
  type = "ratings"  # Predict ratings
)

# Print results
cat("\nRMSE and MAE for the models:\n")
print(avg(results))

# ===== 5. TRAIN FINAL MODEL =====
cat("\nTraining final UBCF model...\n")

# Use the better-performing algorithm from above (check results)
# Here we'll just use "UBCF-Cosine-25" for simplicity
ubcf_model <- Recommender(
  data = ratings_filtered,
  method = "UBCF",
  parameter = list(
    method = "cosine",
    nn = 25
  )
)

cat("Final model trained successfully\n")

# ===== 6. GENERATE SAMPLE RECOMMENDATIONS =====
cat("\nGenerating sample recommendations for 3 users...\n")

# Get a small sample of users
set.seed(456)
sample_users <- sample(valid_users, 3)

# Number of recommendations to generate
n_recommendations <- 10

for (user in sample_users) {
  cat(sprintf("\nTop %d recommendations for user %d:\n", n_recommendations, user))
  
  # Generate recommendations
  recommendations <- predict(
    ubcf_model, 
    ratings_data[user], 
    n = n_recommendations,
    type = "topNList"
  )
  
  # Get the recommended movie IDs
  rec_movie_ids <- as(recommendations, "list")[[1]]
  
  # Print the recommendations if we have movie data
  if ("movies_filtered" %in% names(prepared_data)) {
    recommended_movies <- prepared_data$movies_filtered %>%
      filter(movieId %in% rec_movie_ids) %>%
      select(movieId, title, genres)
    
    if (nrow(recommended_movies) > 0) {
      for (i in 1:nrow(recommended_movies)) {
        cat(sprintf("  %s (%s)\n", 
                    recommended_movies$title[i], 
                    recommended_movies$genres[i]))
      }
    } else {
      cat("  No movies found matching the recommendations.\n")
    }
  } else {
    cat("  Recommended movie IDs:", paste(rec_movie_ids, collapse=", "), "\n")
  }
}

# ===== 7. SAVE THE MODEL =====
cat("\nSaving the UBCF model...\n")
saveRDS(ubcf_model, "ubcf_model.rds")
cat("Model saved successfully to 'ubcf_model.rds'\n")

cat("\nUBCF model training complete!\n")