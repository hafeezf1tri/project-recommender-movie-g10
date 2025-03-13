# ============================================================================
# MovieLens Recommender System - Item-Based Collaborative Filtering
# 
# This script implements an Item-Based Collaborative Filtering (IBCF) model
# using prepared data from prepared_data.RData
# ============================================================================

# Load required libraries
library(tidyverse)      # For data manipulation
library(recommenderlab) # For recommender algorithms
library(Matrix)         # For sparse matrices

# Create directory for models if it doesn't exist
dir.create("models", showWarnings = FALSE)

# ============================================================================
# 1. Load data from prepared_data.RData
# ============================================================================
cat("Loading data from prepared_data.RData...\n")

# Check if the prepared data file exists
if (file.exists("data/prepared_data.RData")) {
  # Load the data
  load("data/prepared_data.RData")
  cat("Successfully loaded prepared data\n")
  
  # Check if we have the required objects
  required_objects <- c("movies_clean", "ratings_filtered", "r_matrix", "evaluation_scheme")
  missing_objects <- required_objects[!required_objects %in% ls()]
  
  if (length(missing_objects) > 0) {
    cat("Warning: The following required objects are missing from prepared_data.RData:", 
        paste(missing_objects, collapse = ", "), "\n")
    cat("Will try to proceed with available objects\n")
  }
} else {
  stop("Error: data/prepared_data.RData not found. Please prepare the data first.")
}

# Assign variables to standard names if they have different names in the loaded data
# This ensures compatibility with the rest of the script
if (!exists("movies_filtered") && exists("movies_clean")) {
  movies_filtered <- movies_clean
}

if (!exists("eval_scheme") && exists("evaluation_scheme")) {
  eval_scheme <- evaluation_scheme
}

# ============================================================================
# 2. Build IBCF Model
# ============================================================================
ibcf_model_path <- "models/ibcf_model.rds"

# Check if model already exists
if (file.exists(ibcf_model_path)) {
  cat("Loading existing IBCF model...\n")
  ibcf_model <- readRDS(ibcf_model_path)
} else {
  cat("Building IBCF model...\n")
  
  # Train an IBCF model on the training data
  ibcf_model <- Recommender(
    getData(eval_scheme, "train"),
    method = "IBCF",
    parameter = list(
      k = 30,            # Number of similar items to consider
      method = "cosine"  # Similarity measure
    )
  )
  
  # Save the model
  saveRDS(ibcf_model, ibcf_model_path)
  cat("IBCF model built and saved to", ibcf_model_path, "\n")
}

# ============================================================================
# 3. Generate Sample Recommendations
# ============================================================================
cat("Generating sample recommendations...\n")

# Function to get recommendations for a user
get_recommendations <- function(model, user_id, n = 10) {
  # Get user data from the test set
  user_data <- getData(eval_scheme, "known")[user_id, ]
  
  # Generate predictions
  predictions <- predict(model, user_data, n = n)
  
  # Convert to list
  rec_list <- as(predictions, "list")
  
  # Get movie IDs
  movie_ids <- rec_list[[1]]
  
  # Get movie details
  rec_movies <- movies_filtered %>%
    filter(movieId %in% movie_ids) %>%
    select(movieId, title, genres) %>%
    arrange(match(movieId, movie_ids))
  
  return(rec_movies)
}

# Get a sample user ID from the test set
sample_users <- sample(rownames(getData(eval_scheme, "known")), 3)

# Generate recommendations for sample users
for (user in sample_users) {
  cat("\nIBCF Recommendations for User", user, ":\n")
  recs <- get_recommendations(ibcf_model, user, n = 10)
  print(recs)
}

# ============================================================================
# 4. Evaluate the Model
# ============================================================================
cat("\nEvaluating IBCF model...\n")

# Evaluate model using standard evaluation metrics
ibcf_eval <- evaluate(
  eval_scheme,
  method = "IBCF",
  parameter = list(
    k = 30,
    method = "cosine"
  ),
  n = c(1, 5, 10, 15, 20)  # Different recommendation list sizes
)

# Get evaluation results
ibcf_results <- getConfusionMatrix(ibcf_eval)[[1]]
print("Evaluation results by number of recommendations:")
print(ibcf_results)

# Calculate RMSE
ibcf_predictions <- predict(ibcf_model, getData(eval_scheme, "known"))
ibcf_accuracy <- calcPredictionAccuracy(ibcf_predictions, 
                                        getData(eval_scheme, "unknown"), 
                                        goodRating = 3.5,
                                        given = 5)  # Number of items used for prediction
cat("\nPrediction accuracy metrics:\n")
print(ibcf_accuracy)

# ============================================================================
# 5. Compare IBCF with UBCF (if UBCF model exists)
# ============================================================================
cat("\nChecking for UBCF model to compare...\n")

# Check if UBCF model exists
if (file.exists("models/ubcf_model.rds")) {
  cat("Loading UBCF model for comparison...\n")
  # Load UBCF model
  ubcf_model <- readRDS("models/ubcf_model.rds")
  
  # Evaluate UBCF
  ubcf_eval <- evaluate(
    eval_scheme,
    method = "UBCF",
    parameter = list(
      nn = 30,
      method = "cosine"
    ),
    n = c(1, 5, 10, 15, 20)
  )
  
  # Compare results
  cat("\nComparison of IBCF vs UBCF (n=10):\n")
  ibcf_results_n10 <- getConfusionMatrix(ibcf_eval)[[1]][3,]  # Results for n=10
  ubcf_results_n10 <- getConfusionMatrix(ubcf_eval)[[1]][3,]  # Results for n=10
  
  comparison <- rbind(
    IBCF = ibcf_results_n10,
    UBCF = ubcf_results_n10
  )
  print(comparison)
  
  # Calculate UBCF RMSE for comparison
  ubcf_predictions <- predict(ubcf_model, getData(eval_scheme, "known"))
  ubcf_accuracy <- calcPredictionAccuracy(ubcf_predictions, 
                                          getData(eval_scheme, "unknown"), 
                                          goodRating = 3.5,
                                          given = 5)
  
  # Compare RMSE
  cat("\nRMSE Comparison:\n")
  rmse_comparison <- data.frame(
    Algorithm = c("IBCF", "UBCF"),
    RMSE = c(ibcf_accuracy["RMSE"], ubcf_accuracy["RMSE"])
  )
  print(rmse_comparison)
  
  # Generate recommendations for the same user with both algorithms
  if (length(sample_users) > 0) {
    sample_user <- sample_users[1]
    cat("\nComparison of recommendations for User", sample_user, ":\n")
    
    cat("\nIBCF Recommendations:\n")
    ibcf_recs <- get_recommendations(ibcf_model, sample_user, n = 5)
    print(ibcf_recs)
    
    cat("\nUBCF Recommendations:\n")
    ubcf_recs <- get_recommendations(ubcf_model, sample_user, n = 5)
    print(ubcf_recs)
    
    # Check overlap between recommendations
    ibcf_movies <- ibcf_recs$movieId
    ubcf_movies <- ubcf_recs$movieId
    common_movies <- intersect(ibcf_movies, ubcf_movies)
    
    cat("\nNumber of common recommendations:", length(common_movies), "out of 5\n")
    if (length(common_movies) > 0) {
      cat("Common recommendations:\n")
      common_movies_info <- movies_filtered %>%
        filter(movieId %in% common_movies) %>%
        select(movieId, title)
      print(common_movies_info)
    }
  }
} else {
  cat("UBCF model not found. Skipping comparison.\n")
}

# ============================================================================
# 6. Examine Item Similarities (IBCF specific analysis)
# ============================================================================
cat("\nExamining item similarities in IBCF model...\n")

# Extract similarity matrix from IBCF model
sim_matrix <- getModel(ibcf_model)$sim

# Select a popular movie to examine
popular_movie <- "Star Wars"  # Try to find a Star Wars movie
star_wars_id <- movies_filtered %>%
  filter(grepl(popular_movie, title)) %>%
  slice(1) %>%
  pull(movieId)

if (length(star_wars_id) == 0) {
  # If no Star Wars movie, pick first movie that appears in similarity matrix
  movie_id <- as.character(sample(as.integer(rownames(sim_matrix)), 1))
  movie_title <- movies_filtered %>%
    filter(movieId == movie_id) %>%
    pull(title)
} else {
  movie_id <- as.character(star_wars_id)
  movie_title <- movies_filtered %>%
    filter(movieId == movie_id) %>%
    pull(title)
}

# Find similar movies
if (movie_id %in% rownames(sim_matrix)) {
  movie_similarities <- sim_matrix[movie_id, ]
  top_similar <- names(sort(movie_similarities, decreasing = TRUE)[1:11])
  
  # Get details for similar movies
  similar_movies_details <- movies_filtered %>%
    filter(movieId %in% top_similar) %>%
    mutate(similarity = as.numeric(movie_similarities[as.character(movieId)])) %>%
    arrange(desc(similarity))
  
  cat("\nMovies similar to", movie_title, ":\n")
  print(similar_movies_details %>% select(title, genres, similarity))
}

# ============================================================================
# Done
# ============================================================================
cat("\n===============================================\n")
cat("IBCF Implementation Complete!\n")
cat("===============================================\n")