# Create a user-based collaborative filtering model
ubcf_model <- Recommender(r_matrix, method = "UBCF", 
                          parameter = list(
                            nn = 30,  # Number of nearest neighbors
                            method = "cosine",  # Similarity measure (options: cosine, pearson, jaccard)
                            normalize = "center"  # Center the ratings by subtracting user mean
                          ))

# Save the model for later use
saveRDS(ubcf_model, "models/ubcf_model.rds")

# Pick a random user to test
sample_user_id <- sample(rownames(r_matrix), 1)

# Generate 10 recommendations for this user
n_recommendations <- 10
ubcf_predictions <- predict(ubcf_model, r_matrix[sample_user_id,], n = n_recommendations)
ubcf_recommendations <- as(ubcf_predictions, "list")

# Get the recommended movie IDs
ubcf_rec_items <- ubcf_recommendations[[1]]

# Join with movie data to show titles
ubcf_rec_movies <- movies_clean %>%
  filter(movieId %in% ubcf_rec_items) %>%
  select(movieId, title, genres)

# Display the recommendations
print(paste("Recommendations for user:", sample_user_id))
print(ubcf_rec_movies)

get_ubcf_recommendations <- function(user_id, model, movies_data, n = 10) {
  # Check if user exists in the data
  if(!(user_id %in% rownames(r_matrix))) {
    return("User not found in dataset")
  }
  
  # Generate recommendations
  pred <- predict(model, r_matrix[user_id,], n = n)
  rec_list <- as(pred, "list")
  rec_items <- rec_list[[1]]
  
  # Get movie details
  recommendations <- movies_data %>%
    filter(movieId %in% rec_items) %>%
    arrange(match(movieId, rec_items)) %>%
    select(movieId, title, genres)
  
  return(recommendations)
}

# Test the function
test_recs <- get_ubcf_recommendations("1", ubcf_model, movies_clean)
print(test_recs)

# Evaluate on your evaluation scheme
ubcf_eval <- evaluate(evaluation_scheme, 
                      method = "UBCF", 
                      parameter = list(nn = 30, method = "cosine"),
                      n = c(1, 5, 10, 15, 20))

# Plot ROC curve
plot(ubcf_eval, annotate = TRUE, main = "UBCF Performance")

# Calculate RMSE
ubcf_predictions <- predict(ubcf_model, getData(evaluation_scheme, "known"))
ubcf_rmse <- calcPredictionAccuracy(ubcf_predictions, getData(evaluation_scheme, "unknown"))
print(paste("UBCF RMSE:", ubcf_rmse["RMSE"]))