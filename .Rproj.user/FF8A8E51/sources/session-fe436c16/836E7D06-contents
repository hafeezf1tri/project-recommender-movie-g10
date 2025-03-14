# IBCF Model Validation Script
# This script evaluates a pre-trained IBCF model without accuracy testing

# Load required libraries
library(recommenderlab)
library(tidyverse)
library(reshape2)
library(ggplot2)

# Set random seed for reproducibility
set.seed(123)

# ===== 1. LOAD PRE-TRAINED MODEL =====
cat("Loading pre-trained IBCF model...\n")
if (!file.exists("ibcf_model.rds")) {
  stop("Error: ibcf_model.rds file not found!")
}

ibcf_model <- readRDS("ibcf_model.rds")

# Check if the model is valid
if (class(ibcf_model)[1] != "Recommender") {
  stop("Error: The loaded object is not a valid Recommender model!")
}

cat("IBCF model loaded successfully.\n")
cat("Model method:", ibcf_model@model$method, "\n")

# Extract model details
model_details <- getModel(ibcf_model)
cat("k value (neighbors):", model_details$k, "\n")
cat("Similarity method:", model_details$method, "\n")
cat("Similarity matrix dimensions:", dim(model_details$sim), "\n")

# Calculate similarity matrix sparsity
sim_sparsity <- 1 - (sum(model_details$sim != 0) / (ncol(model_details$sim)^2))
cat("Similarity matrix sparsity:", sprintf("%.2f%%", sim_sparsity * 100), "\n")

# ===== 2. LOAD PREPARED DATA =====
cat("\nLoading prepared data...\n")
if (!file.exists("prepared_data.RData")) {
  stop("Error: prepared_data.RData file not found!")
}

load("prepared_data.RData")

# Determine which rating matrix to use
if ("ratings_center" %in% names(prepared_data) && 
    class(prepared_data$ratings_center)[1] == "realRatingMatrix") {
  ratings_data <- prepared_data$ratings_center
  cat("Using center-normalized ratings for validation\n")
} else if ("ratings_sparse" %in% names(prepared_data) && 
           class(prepared_data$ratings_sparse)[1] == "realRatingMatrix") {
  ratings_data <- prepared_data$ratings_sparse
  cat("Using regular ratings matrix for validation\n")
} else {
  stop("Error: No valid rating matrix found in prepared data")
}

cat("Rating matrix dimensions:", dim(ratings_data), "\n")

# ===== 3. ANALYZE ITEM SIMILARITIES =====
cat("\nAnalyzing item similarities...\n")

# Function to get similar items
get_similar_items <- function(item_id, n = 5) {
  # Check if item exists in similarity matrix
  if (!(item_id %in% rownames(model_details$sim))) {
    cat("Item", item_id, "not found in similarity matrix.\n")
    return(NULL)
  }
  
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
  # Just pick 5 movies randomly if we don't have popularity data
  all_movie_ids <- prepared_data$movies_filtered$movieId
  popular_movies <- sample(all_movie_ids, 5)
} else {
  # Or use column names if we don't have movie data
  popular_movies <- sample(colnames(model_details$sim), 5)
}

# Analysis of similarity patterns
similarity_stats <- c()

# Analyze similar items for popular movies
for (movie_id in popular_movies) {
  # Convert to character if it's not already
  movie_id_char <- as.character(movie_id)
  
  # Skip if movie is not in the similarity matrix
  if (!(movie_id_char %in% rownames(model_details$sim))) {
    cat("Movie ID", movie_id, "not found in similarity matrix. Skipping.\n")
    next
  }
  
  cat(sprintf("\nSimilar movies to movie ID %s:\n", movie_id))
  
  # Get similar movies
  similar_items <- get_similar_items(movie_id_char, 5)
  
  if (is.null(similar_items) || length(similar_items) == 0) {
    cat("  No similar items found.\n")
    next
  }
  
  # Store similarity values for statistics
  similarity_stats <- c(similarity_stats, similar_items)
  
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
      
      # Convert to numeric ID for filtering
      similar_id_num <- as.numeric(similar_id)
      
      # Get movie details
      movie_details <- prepared_data$movies_filtered %>%
        filter(movieId == similar_id_num) %>%
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

# ===== 4. GENERATE SAMPLE RECOMMENDATIONS =====
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

# ===== 5. RECOMMENDATION DIVERSITY ANALYSIS =====
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

# ===== 6. GENRE ANALYSIS =====
cat("\nAnalyzing genre distribution in recommendations...\n")

# Check if we have genre information
if ("movies_filtered" %in% names(prepared_data) && "genres" %in% names(prepared_data$movies_filtered)) {
  # Extract all genres from recommended movies
  if (length(all_rec_movies) > 0 && length(all_unique_movies) > 0) {
    recommended_genres <- prepared_data$movies_filtered %>%
      filter(movieId %in% all_unique_movies) %>%
      pull(genres)
    
    # Split genres and count occurrences
    genre_list <- unlist(strsplit(recommended_genres, "\\|"))
    genre_counts <- table(genre_list)
    
    # Display top genres in recommendations
    cat("Top genres in recommendations:\n")
    top_genres <- sort(genre_counts, decreasing = TRUE)[1:min(10, length(genre_counts))]
    for (i in 1:length(top_genres)) {
      cat(sprintf("  %s: %d movies (%.1f%%)\n", 
                  names(top_genres)[i],
                  top_genres[i],
                  top_genres[i] / length(all_unique_movies) * 100))
    }
  }
}

# ===== 7. MODEL QUALITY SUMMARY =====
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
  
  if(!is.nan(overall_score) && !is.na(overall_score)) {
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
  } else {
    cat("\nUnable to calculate overall score due to invalid values\n")
  }
}

# ===== 8. COMPARISON WITH UBCF (IF AVAILABLE) =====
cat("\nComparing with UBCF model (if available)...\n")

# Load UBCF model if available
if (file.exists("ubcf_model.rds")) {
  tryCatch({
    ubcf_model <- readRDS("ubcf_model.rds")
    cat("UBCF model loaded for comparison.\n")
    
    # Get recommendations from both models for the same users
    comparison_users <- sample(valid_users, min(10, length(valid_users)))
    model_overlap <- 0
    total_recs <- 0
    
    for(user in comparison_users) {
      # Get UBCF recommendations
      ubcf_recs <- predict(
        ubcf_model,
        ratings_data[user],
        n = 10,
        type = "topNList"
      )
      ubcf_movie_ids <- as(ubcf_recs, "list")[[1]]
      
      # Get IBCF recommendations
      ibcf_recs <- predict(
        ibcf_model,
        ratings_data[user],
        n = 10,
        type = "topNList"
      )
      ibcf_movie_ids <- as(ibcf_recs, "list")[[1]]
      
      # Calculate overlap
      common_recs <- intersect(ubcf_movie_ids, ibcf_movie_ids)
      overlap_pct <- length(common_recs) / length(union(ubcf_movie_ids, ibcf_movie_ids)) * 100
      
      model_overlap <- model_overlap + length(common_recs)
      total_recs <- total_recs + length(union(ubcf_movie_ids, ibcf_movie_ids))
    }
    
    # Calculate overall overlap
    if(total_recs > 0) {
      overall_overlap <- model_overlap / total_recs * 100
      cat("Average recommendation overlap between UBCF and IBCF:", 
          sprintf("%.1f%%", overall_overlap), "\n")
      
      if(overall_overlap < 30) {
        cat("LOW overlap - The models provide substantially different recommendations\n")
      } else if(overall_overlap < 60) {
        cat("MODERATE overlap - The models provide somewhat different recommendations\n")
      } else {
        cat("HIGH overlap - The models provide similar recommendations\n")
      }
    }
  }, error = function(e) {
    cat("Error comparing with UBCF model:", e$message, "\n")
  })
} else {
  cat("UBCF model not found - comparison skipped.\n")
}

cat("\nIBCF model validation complete!\n")