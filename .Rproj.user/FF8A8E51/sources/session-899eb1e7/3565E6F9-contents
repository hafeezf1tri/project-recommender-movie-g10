# Focused UBCF Model Validation Script
# This script tests a pre-trained UBCF model without problematic accuracy testing

# Load required libraries
library(recommenderlab)
library(tidyverse)
library(reshape2)
library(ggplot2)

# Set random seed for reproducibility
set.seed(123)

# ===== 1. LOAD PRE-TRAINED MODEL =====
cat("Loading pre-trained UBCF model...\n")
if (!file.exists("ubcf_model.rds")) {
  stop("Error: ubcf_model.rds file not found!")
}

ubcf_model <- readRDS("ubcf_model.rds")

# Check if the model is valid
if (class(ubcf_model)[1] != "Recommender") {
  stop("Error: The loaded object is not a valid Recommender model!")
}

cat("UBCF model loaded successfully.\n")
cat("Model method:", ubcf_model@model$method, "\n")

# ===== 2. LOAD PREPARED DATA =====
cat("\nLoading prepared data...\n")
if (!file.exists("prepared_data.RData")) {
  stop("Error: prepared_data.RData file not found!")
}

load("prepared_data.RData")

# Determine which rating matrix to use
if ("ratings_z" %in% names(prepared_data) && class(prepared_data$ratings_z)[1] == "realRatingMatrix") {
  ratings_data <- prepared_data$ratings_z
  cat("Using Z-score normalized ratings for validation\n")
} else if ("ratings_sparse" %in% names(prepared_data) && 
           class(prepared_data$ratings_sparse)[1] == "realRatingMatrix") {
  ratings_data <- prepared_data$ratings_sparse
  cat("Using regular ratings matrix for validation\n")
} else {
  stop("Error: No valid rating matrix found in prepared data")
}

cat("Rating matrix dimensions:", dim(ratings_data), "\n")

# ===== 3. BASIC RECOMMENDATION TEST =====
cat("\nPerforming basic recommendation test...\n")

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
  cat(sprintf("\nGenerating recommendations for user %d:\n", user))
  
  # Generate recommendations
  tryCatch({
    recommendations <- predict(
      ubcf_model, 
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

# ===== 4. RECOMMENDATION DIVERSITY ANALYSIS =====
cat("\nAnalyzing recommendation diversity...\n")

# Get recommendations for 50 random users (or all valid users if less than 50)
diversity_users <- sample(valid_users, min(50, length(valid_users)))
all_rec_movies <- list()

for(user in diversity_users) {
  tryCatch({
    recommendations <- predict(
      ubcf_model, 
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

# ===== 5. GENRE ANALYSIS =====
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

# ===== 6. MODEL QUALITY SUMMARY =====
cat("\n===== UBCF MODEL QUALITY SUMMARY =====\n")

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

# ===== 7. RECOMMENDATIONS FOR IMPROVEMENT =====
cat("\nRecommendations for improvement:\n")

# Check diversity score
if(exists("diversity_score") && !is.nan(diversity_score) && !is.na(diversity_score) && diversity_score < 6.0) {
  cat("- Recommendation diversity is low; consider adjusting neighborhood size\n")
}

# Check success rate
if(exists("success_rate") && !is.nan(success_rate) && !is.na(success_rate) && success_rate < 90) {
  cat("- Model fails to generate recommendations for some users; consider lowering minimum rating thresholds\n")
}

# Check catalog coverage
if(exists("coverage") && !is.nan(coverage) && !is.na(coverage) && coverage < 15) {
  cat("- Limited catalog coverage; consider techniques to promote long-tail recommendations\n")
}

# Default recommendation if no specific issues
if((!exists("diversity_score") || is.nan(diversity_score) || is.na(diversity_score)) && 
   (!exists("success_rate") || is.nan(success_rate) || is.na(success_rate))) {
  cat("- Insufficient metrics were obtained; consider alternative evaluation methods\n")
} else if(exists("diversity_score") && exists("success_rate") && 
          !is.nan(diversity_score) && !is.na(diversity_score) && 
          !is.nan(success_rate) && !is.na(success_rate) && 
          diversity_score >= 6.0 && success_rate >= 90) {
  cat("- Model is performing well on key metrics; consider implementing IBCF for comparison\n")
}

cat("\nUBCF model validation complete!\n")