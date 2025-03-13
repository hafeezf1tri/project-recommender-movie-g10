library(recommenderlab)
library(tidyverse)

# Load your prepared data
# This should contain your ratings matrix in recommenderlab format
load("data/prepared_data.RData")

# Create a user-based collaborative filtering model
ubcf_model <- Recommender(r_matrix, method = "UBCF", 
                          parameter = list(
                            nn = 30,  # Number of nearest neighbors
                            method = "cosine",  # Similarity measure (options: cosine, pearson, jaccard)
                            normalize = "center"  # Center the ratings by subtracting user mean
                          ))

# Save the model for later use
saveRDS(ubcf_model, "models/ubcf_model.rds")

