# Item-Based Collaborative Filtering (IBCF) Implementation
# This script trains and evaluates an IBCF model on the MovieLens dataset

# Load required libraries
library(recommenderlab)
library(tidyverse)
library(reshape2)
library(ggplot2)

# Set random seed for reproducibility
set.seed(123)

# ===== 1. LOAD PREPARED DATA =====
cat("Loading prepared data...\n")
if (!file.exists("prepared_data.RData")) {
  stop("Error: prepared_data.RData file not found!")
}

load("prepared_data.RData")

# Decide which ratings matrix to use (center-normalized is often better for IBCF)
if ("ratings_center" %in% names(prepared_data) && 
    class(prepared_data$ratings_center)[1] == "realRatingMatrix") {
  ratings_data <- prepared_data$ratings_center
  cat("Using center-normalized ratings for IBCF\n")
} else if ("ratings_sparse" %in% names(prepared_data) && 
           class(prepared_data$ratings_sparse)[1] == "realRatingMatrix") {
  ratings_data <- prepared_data$ratings_sparse
  cat("Using regular ratings matrix for IBCF\n")
} else {
  stop("Error: No valid rating matrix found in prepared data")
}

cat("Rating matrix dimensions:", dim(ratings_data), "\n")

# ===== 2. TRAIN IBCF MODEL WITH DIFFERENT PARAMETERS =====
cat("\nTraining IBCF models with different parameters...\n")

# Define parameter combinations to test
k_values <- c(10, 20, 30, 50)  # Number of similar items
method_values <- c("cosine", "pearson")  # Similarity measures

# Store results
ibcf_results <- data.frame()

# Record start time for tracking performance
start_time <- Sys.time()

# Iterate through parameter combinations
for (k in k_values) {
  for (method in method_values) {
    cat(sprintf("Training IBCF with k=%d, method=%s...\n", k, method))
    
    # Train the model with current parameters
    tryCatch({
      # Train the model
      ibcf_model <- Recommender(
        ratings_data,
        method = "IBCF",
        parameter = list(
          k = k,  # Number of similar items
          method = method  # Similarity measure
        )
      )
      
      # Extract model details
      model_details <- getModel(ibcf_model)
      
      # Calculate model size (memory footprint)
      model_size <- object.size(ibcf_model)
      
      # Calculate similarity matrix sparsity
      sim_matrix <- model_details$sim
      sim_sparsity <- 1 - (sum(sim_matrix != 0) / (ncol(sim_matrix)^2))
      
      # Add to results
      ibcf_results <- rbind(ibcf_results, data.frame(
        k = k,
        method = method,
        training_time = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
        model_size_mb = as.numeric(model_size) / (1024^2),
        sim_matrix_sparsity = sim_sparsity
      ))
      
      cat(sprintf("  Complete. Model size: %.2f MB, Similarity matrix sparsity: %.2f%%\n", 
                  as.numeric(model_size) / (1024^2), 
                  sim_sparsity * 100))
      
      # Reset start time for next model
      start_time <- Sys.time()
    }, error = function(e) {
      cat("  Error training model:", e$message, "\n")
    })
  }
}

# If we have results, print a summary
if (nrow(ibcf_results) > 0) {
  cat("\nSummary of IBCF models trained:\n")
  print(ibcf_results)
}

# ===== 3. SELECT BEST MODEL BASED ON PRACTICAL CONSIDERATIONS =====
# Since we're not doing accuracy testing, choose based on reasonable balance
# between model size and similarity matrix sparsity

if (nrow(ibcf_results) > 0) {
  # Normalize metrics to 0-1 scale
  ibcf_results$norm_size <- (ibcf_results$model_size_mb - min(ibcf_results$model_size_mb)) / 
    (max(ibcf_results$model_size_mb) - min(ibcf_results$model_size_mb))
  
  # We want higher sparsity (to save memory) so invert the normalization
  ibcf_results$norm_sparsity <- 1 - (ibcf_results$sim_matrix_sparsity - min(ibcf_results$sim_matrix_sparsity)) / 
    (max(ibcf_results$sim_matrix_sparsity) - min(ibcf_results$sim_matrix_sparsity))
  
  # Calculate a score (lower is better)
  ibcf_results$score <- 0.7 * ibcf_results$norm_size + 0.3 * ibcf_results$norm_sparsity
  
  # Select model with best score
  best_model <- ibcf_results[which.min(ibcf_results$score), ]
  
  cat("\nSelected IBCF model parameters:\n")
  cat("k =", best_model$k, ", method =", as.character(best_model$method), "\n")
} else {
  # Default parameters if no models were successfully trained
  cat("\nUsing default IBCF parameters: k=30, method=cosine\n")
  best_k <- 30
  best_method <- "cosine"
}

# ===== 4. TRAIN FINAL MODEL =====
cat("\nTraining final IBCF model...\n")

# Get parameters for final model
if (exists("best_model")) {
  best_k <- best_model$k
  best_method <- as.character(best_model$method)
} else {
  best_k <- 30
  best_method <- "cosine"
}

# Train final model
ibcf_model <- Recommender(
  ratings_data,
  method = "IBCF",
  parameter = list(
    k = best_k,
    method = best_method
  )
)

cat("Final IBCF model trained successfully.\n")

# Extract model details for analysis
model_details <- getModel(ibcf_model)
cat("Model details:\n")
cat("- Number of items:", length(model_details$itemMeans), "\n")
cat("- Similarity matrix dimensions:", dim(model_details$sim), "\n")

# ===== 5. ANALYZE ITEM SIMILARITIES =====
cat("\nAnalyzing item similarities...\n")

# Function to get similar items
get_similar_items <- function(item_id, n = 5) {
  sim_vector <- model_details$sim[item_id, ]
  similar_items <- sort(sim_vector, decreasing = TRUE)[2:(n+1)]  # Skip the first (self)
  return(similar_items)
}

# Get some popular movies to analyze
if ("movie_popularity" %in% names(prepared_data)) {
  popular_movies <- prepared_data$movie_popularity %>%
    arrange(desc(rating_count)) %>%
    head(5) %>%
    pull(movieId)
} else if ("movies_filtered" %in% names(prepared_data)) {
  # Just pick 5 random movies if we don't have popularity data
  popular_movies <- sample(prepared_data$movies_filtered$movieId, 5)
} else {
  # Or pick 5 random column indices if we don't have movie metadata
  popular_movies <- sample(colnames(ratings_data), 5)
}

# Analyze similar items for popular movies
for (movie_id in popular_movies) {
  # Skip if movie is not in the similarity matrix
  if (!(movie_id %in% rownames(model_details$sim))) {
    cat("Movie ID", movie_id, "not found in similarity matrix. Skipping.\n")
    next
  }
  
  cat(sprintf("\nSimilar movies to movie ID %s:\n", movie_id))
  
  # Get similar movies
  similar_items <- get_similar_items(as.character(movie_id), 5)
  
  # Print similar movies with details if available
  if ("movies_filtered" %in% names(prepared_data)) {
    # Get details of the source movie
    source_movie <- prepared_data$movies_filtered %>%
      filter(movieId == movie_id) %>%
      select(movieId, title, genres)
    
    if (nrow(source_movie) > 0) {
      cat("Source movie:", source_movie$title, "(", source_movie$genres, ")\n")
    }
    
    # Get details of similar movies
    for (i in 1:length(similar_items)) {
      similar_id <- names(similar_items)[i]
      similarity <- similar_items[i]
      
      # Get movie details
      movie_details <- prepared_data$movies_filtered %>%
        filter(movieId == as.numeric(similar_id)) %>%
        select(movieId, title, genres)
      
      if (nrow(movie_details) > 0) {
        cat(sprintf("  %.4f: %s (%s)\n", 
                    similarity, 
                    movie_details$title, 
                    movie_details$genres))
      } else {
        cat(sprintf("  %.4f: Movie ID %s (details not available)\n", 
                    similarity, similar_id))
      }
    }
  } else {
    # Just print the IDs and similarities if movie details aren't available
    for (i in 1:length(similar_items)) {
      cat(sprintf("  %.4f: Movie ID %s\n", 
                  similar_items[i], 
                  names(similar_items)[i]))
    }
  }
}

# ===== 6. GENERATE SAMPLE RECOMMENDATIONS =====
cat("\nGenerating sample recommendations...\n")

# Get a sample of users
ratings_per_user <- rowCounts(ratings_data)
valid_users <- which(ratings_per_user >= 15)

if(length(valid_users) < 5) {
  cat("Warning: Very few users with sufficient ratings.\n")
  sample_users <- valid_users
} else {
  sample_users <- sample(valid_users, 5)
}

for (user in sample_users) {
  cat(sprintf("\nTop 10 recommendations for user %d:\n", user))
  
  # Generate recommendations
  tryCatch({
    recommendations <- predict(
      ibcf_model, 
      ratings_data[user], 
      n = 10,
      type = "topNList"
    )
    
    # Get recommended movie IDs
    rec_movie_ids <- as(recommendations, "list")[[1]]
    
    if(length(rec_movie_ids) > 0) {
      # Get movie details if available
      if ("movies_filtered" %in% names(prepared_data)) {
        recommended_movies <- prepared_data$movies_filtered %>%
          filter(movieId %in% rec_movie_ids) %>%
          select(movieId, title, genres)
        
        for (i in 1:nrow(recommended_movies)) {
          cat(sprintf("  %s (%s)\n", 
                      recommended_movies$title[i], 
                      recommended_movies$genres[i]))
        }
      } else {
        cat("  Recommended movie IDs:", paste(rec_movie_ids, collapse=", "), "\n")
      }
    } else {
      cat("  No recommendations generated.\n")
    }
  }, error = function(e) {
    cat("  Error generating recommendations:", e$message, "\n")
  })
}

# ===== 7. RECOMMENDATION DIVERSITY ANALYSIS =====
cat("\nAnalyzing recommendation diversity...\n")

# Get recommendations for 50 random users (or all valid users if less than 50)
diversity_users <- sample(valid_users, min(50, length(valid_users)))
all_rec_movies <- list()

for(user in diversity_users) {
  tryCatch({
    recommendations <- predict(
      ibcf_model, 
      ratings_data[user], 
      n = 10,
      type = "topNList"
    )
    
    # Get recommended movie IDs
    rec_movie_ids <- as(recommendations, "list")[[1]]
    all_rec_movies[[as.character(user)]] <- rec_movie_ids
  }, error = function(e) {
    # Just skip this user if there's an error
  })
}

# Calculate diversity metrics
if(length(all_rec_movies) > 0) {
  # How many unique movies were recommended in total
  all_unique_movies <- unique(unlist(all_rec_movies))
  cat("Number of unique movies recommended:", length(all_unique_movies), "\n")
  cat("Catalog coverage:", sprintf("%.2f%%", length(all_unique_movies) / ncol(ratings_data) * 100), "\n")
  
  # Measure diversity - what percentage of users get the same recommendations
  movie_rec_count <- table(unlist(all_rec_movies))
  most_common <- sort(movie_rec_count, decreasing=TRUE)[1:min(10, length(movie_rec_count))]
  cat("Most frequently recommended movies appeared in recommendations for:")
  for(i in 1:length(most_common)) {
    pct <- most_common[i] / length(all_rec_movies) * 100
    cat(sprintf("\n  %s: %.1f%% of users", names(most_common)[i], pct))
    
    # Print movie title if available
    if("movies_filtered" %in% names(prepared_data)) {
      movie_id <- as.integer(names(most_common)[i])
      movie_info <- prepared_data$movies_filtered %>% 
        filter(movieId == movie_id) %>%
        select(title)
      if(nrow(movie_info) > 0) {
        cat(sprintf(" (%s)", movie_info$title[1]))
      }
    }
  }
  cat("\n")
  
  # Calculate diversity score (0-10)
  diversity_score <- 10 * (1 - (mean(movie_rec_count) / length(diversity_users)))
  cat("Diversity score (0-10):", sprintf("%.1f", diversity_score), 
      "(higher is better)\n")
}

# ===== 8. MODEL QUALITY SUMMARY =====
cat("\n===== IBCF MODEL QUALITY SUMMARY =====\n")

# Show diversity score if it exists
if(exists("diversity_score") && !is.nan(diversity_score) && !is.na(diversity_score)) {
  cat("Recommendation diversity score:", sprintf("%.1f/10", diversity_score), "\n")
  
  # Interpret the diversity score
  if(diversity_score >= 8) {
    cat("EXCELLENT diversity - The model provides highly personalized recommendations\n")
  } else if(diversity_score >= 6) {
    cat("GOOD diversity - The model provides reasonably varied recommendations\n")
  } else if(diversity_score >= 4) {
    cat("FAIR diversity - The model shows some variation in recommendations\n")
  } else {
    cat("POOR diversity - The model tends to recommend the same items to many users\n")
  }
}

# Check recommendation success rate
if(exists("all_rec_movies") && exists("diversity_users")) {
  success_rate <- length(all_rec_movies) / length(diversity_users) * 100
  cat("Recommendation success rate:", sprintf("%.1f%%", success_rate), 
      "(percentage of users who received recommendations)\n")
  
  # Interpret the success rate
  if(success_rate >= 95) {
    cat("EXCELLENT success rate - The model can generate recommendations for almost all users\n")
  } else if(success_rate >= 80) {
    cat("GOOD success rate - The model works for most users\n")
  } else if(success_rate >= 60) {
    cat("FAIR success rate - The model works for the majority of users\n")
  } else {
    cat("POOR success rate - The model fails for many users\n")
  }
}

# Catalog coverage
if(exists("all_unique_movies")) {
  coverage <- length(all_unique_movies) / ncol(ratings_data) * 100
  cat("Catalog coverage:", sprintf("%.2f%%", coverage), 
      "(percentage of catalog that appears in recommendations)\n")
  
  # Interpret the coverage
  if(coverage >= 30) {
    cat("GOOD catalog coverage - The model utilizes a substantial portion of the catalog\n")
  } else if(coverage >= 10) {
    cat("FAIR catalog coverage - The model utilizes a reasonable portion of the catalog\n")
  } else {
    cat("LIMITED catalog coverage - The model only recommends a small fraction of the catalog\n")
  }
}

# Overall assessment based on diversity and success rate
if(exists("diversity_score") && !is.nan(diversity_score) && !is.na(diversity_score) && 
   exists("success_rate") && !is.nan(success_rate) && !is.na(success_rate)) {
  
  # Combined score weighted 70% diversity, 30% success rate
  overall_score <- (diversity_score * 0.7) + (min(success_rate/10, 10) * 0.3)
  
  cat("\nOverall model quality score:", sprintf("%.1f/10", overall_score), "\n")
  
  if(overall_score >= 7.5) {
    cat("EXCELLENT - Model is performing very well and ready for deployment\n")
  } else if(overall_score >= 6.0) {
    cat("GOOD - Model is performing adequately and suitable for most purposes\n")
  } else if(overall_score >= 4.5) {
    cat("FAIR - Model is functional but could benefit from improvements\n")
  } else {
    cat("POOR - Model needs significant improvements before deployment\n")
  }
}

# ===== 9. SAVE THE MODEL =====
cat("\nSaving the final IBCF model...\n")
saveRDS(ibcf_model, "ibcf_model.rds")
cat("Model saved to 'ibcf_model.rds'\n")

cat("\nIBCF model training and evaluation complete!\n")