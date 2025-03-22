# Recommendation System Data Validation
# This script validates prepared data for different recommendation approaches

# Load required libraries
library(recommenderlab)
library(tidyverse)
library(Matrix)
library(ggplot2)
library(gridExtra)
library(stringr)

# ===== 1. LOAD PREPARED DATA =====
cat("Loading prepared data...\n")
if (!file.exists("prepared_data.RData")) {
  stop("Error: prepared_data.RData file not found!")
}

load("prepared_data.RData")

cat("Prepared data loaded successfully.\n")

# ===== 2. BASIC DATA PRESENCE VALIDATION =====
cat("\n===== BASIC DATA VALIDATION =====\n")

expected_objects <- c(
  "ratings_sparse", "ratings_center", "ratings_z",
  "movies_filtered", "ratings_filtered", "genre_matrix"
)

missing_objects <- expected_objects[!expected_objects %in% names(prepared_data)]

if (length(missing_objects) > 0) {
  cat("WARNING: The following expected objects are missing:\n")
  for (obj in missing_objects) {
    cat("- ", obj, "\n")
  }
} else {
  cat("All expected objects are present.\n")
}

# ===== 3. DATA FORMAT VALIDATION =====
cat("\n===== DATA FORMAT VALIDATION =====\n")

# Check if the rating matrices are in the correct format
if ("ratings_sparse" %in% names(prepared_data)) {
  if (class(prepared_data$ratings_sparse)[1] == "realRatingMatrix") {
    cat("✓ ratings_sparse is in correct format (realRatingMatrix)\n")
  } else {
    cat("✗ ratings_sparse is NOT in correct format! Expected realRatingMatrix, found", 
        class(prepared_data$ratings_sparse)[1], "\n")
  }
}

if ("ratings_center" %in% names(prepared_data)) {
  if (class(prepared_data$ratings_center)[1] == "realRatingMatrix") {
    cat("✓ ratings_center is in correct format (realRatingMatrix)\n")
  } else {
    cat("✗ ratings_center is NOT in correct format! Expected realRatingMatrix, found", 
        class(prepared_data$ratings_center)[1], "\n")
  }
}

if ("ratings_z" %in% names(prepared_data)) {
  if (class(prepared_data$ratings_z)[1] == "realRatingMatrix") {
    cat("✓ ratings_z is in correct format (realRatingMatrix)\n")
  } else {
    cat("✗ ratings_z is NOT in correct format! Expected realRatingMatrix, found", 
        class(prepared_data$ratings_z)[1], "\n")
  }
}

# Check if we have the necessary columns in the movies dataframe
if ("movies_filtered" %in% names(prepared_data)) {
  required_columns <- c("movieId", "title", "genres")
  missing_columns <- required_columns[!required_columns %in% names(prepared_data$movies_filtered)]
  
  if (length(missing_columns) > 0) {
    cat("✗ movies_filtered is missing required columns:", paste(missing_columns, collapse=", "), "\n")
  } else {
    cat("✓ movies_filtered has all required columns\n")
  }
}

# Check genre matrix format
if ("genre_matrix" %in% names(prepared_data)) {
  if (is.matrix(prepared_data$genre_matrix)) {
    cat("✓ genre_matrix is in correct format (matrix)\n")
  } else {
    cat("✗ genre_matrix is NOT in correct format! Expected matrix, found", 
        class(prepared_data$genre_matrix)[1], "\n")
  }
}

# ===== 4. DATA QUALITY VALIDATION FOR COLLABORATIVE FILTERING =====
cat("\n===== COLLABORATIVE FILTERING VALIDATION =====\n")

# Validate rating matrices
validate_rating_matrix <- function(matrix_name, matrix_object) {
  cat("Validating", matrix_name, "...\n")
  
  # Check dimensions
  dims <- dim(matrix_object)
  cat("- Dimensions:", dims[1], "users x", dims[2], "items\n")
  
  # Check sparsity
  sparsity <- 1 - sum(matrix_object@data != 0) / (dims[1] * dims[2])
  cat("- Sparsity level:", sprintf("%.2f%%", sparsity * 100), "\n")
  
  # Get ratings
  ratings_vector <- as.vector(matrix_object@data[matrix_object@data != 0])
  
  # Check rating range
  cat("- Rating range:", min(ratings_vector), "to", max(ratings_vector), "\n")
  
  # Calculate average ratings per user and item
  ratings_per_user <- rowCounts(matrix_object)
  ratings_per_item <- colCounts(matrix_object)
  
  cat("- Average ratings per user:", mean(ratings_per_user), "\n")
  cat("- Average ratings per item:", mean(ratings_per_item), "\n")
  
  # Check minimum thresholds for UBCF and IBCF
  sufficient_for_ubcf <- dims[1] >= 10 && mean(ratings_per_user) >= 10
  sufficient_for_ibcf <- dims[2] >= 10 && mean(ratings_per_item) >= 5 
  
  cat("- Sufficient for UBCF:", if(sufficient_for_ubcf) "YES" else "NO", "\n")
  cat("- Sufficient for IBCF:", if(sufficient_for_ibcf) "YES" else "NO", "\n")
  
  # Check if sparsity is too high
  if (sparsity > 0.995) {
    cat("⚠️ WARNING: Data is extremely sparse (", sprintf("%.2f%%", sparsity * 100), 
        "). This may affect recommendation quality.\n")
    cat("   Consider additional filtering or different algorithms.\n")
  } else if (sparsity > 0.98) {
    cat("⚠️ WARNING: Data is very sparse (", sprintf("%.2f%%", sparsity * 100), 
        "). This is typical but may affect some algorithms.\n")
  } else {
    cat("✓ Sparsity level is acceptable for collaborative filtering.\n")
  }
  
  # Verify normalization (for center and z-score)
  if (matrix_name %in% c("ratings_center", "ratings_z")) {
    if (matrix_name == "ratings_center") {
      # Center normalization should have mean near zero
      mean_rating <- mean(ratings_vector)
      if (abs(mean_rating) < 0.1) {
        cat("✓ Properly center-normalized (mean near zero)\n")
      } else {
        cat("⚠️ WARNING: Center normalization may not be applied correctly. Mean =", mean_rating, "\n")
      }
    } else if (matrix_name == "ratings_z") {
      # Z-score normalization should have mean near zero and std near 1
      mean_rating <- mean(ratings_vector)
      std_rating <- sd(ratings_vector)
      if (abs(mean_rating) < 0.1 && abs(std_rating - 1) < 0.1) {
        cat("✓ Properly Z-score normalized (mean near zero, std near 1)\n")
      } else {
        cat("⚠️ WARNING: Z-score normalization may not be applied correctly.", 
            "Mean =", mean_rating, "Std =", std_rating, "\n")
      }
    }
  }
  
  cat("\n")
}

# Validate all rating matrices
if ("ratings_sparse" %in% names(prepared_data)) {
  validate_rating_matrix("ratings_sparse", prepared_data$ratings_sparse)
}
if ("ratings_center" %in% names(prepared_data)) {
  validate_rating_matrix("ratings_center", prepared_data$ratings_center)
}
if ("ratings_z" %in% names(prepared_data)) {
  validate_rating_matrix("ratings_z", prepared_data$ratings_z)
}

# ===== 5. DATA QUALITY VALIDATION FOR CONTENT-BASED FILTERING =====
cat("\n===== CONTENT-BASED FILTERING VALIDATION =====\n")

# Check for genre information
if ("genre_matrix" %in% names(prepared_data) && is.matrix(prepared_data$genre_matrix)) {
  genre_matrix <- prepared_data$genre_matrix
  
  # Basic info
  num_movies <- nrow(genre_matrix)
  num_genres <- ncol(genre_matrix)
  
  cat("Genre matrix dimensions:", num_movies, "movies x", num_genres, "genres\n")
  
  # Check if movies have genres assigned
  movies_with_genres <- sum(rowSums(genre_matrix) > 0)
  cat("Movies with at least one genre:", movies_with_genres, 
      sprintf("(%.1f%%)", movies_with_genres/num_movies*100), "\n")
  
  # Check genre distribution
  genre_counts <- colSums(genre_matrix)
  genre_df <- data.frame(
    genre = colnames(genre_matrix),
    count = genre_counts
  ) %>% arrange(desc(count))
  
  cat("Top 10 most common genres:\n")
  print(head(genre_df, 10))
  
  # Sufficient for content-based?
  if (num_genres < 5) {
    cat("⚠️ WARNING: Very few genres (", num_genres, ") for content-based filtering.\n")
  } else {
    cat("✓ Number of genres is sufficient for content-based filtering.\n")
  }
  
  if (movies_with_genres / num_movies < 0.8) {
    cat("⚠️ WARNING: Many movies (", num_movies - movies_with_genres, 
        ") have no genre information.\n")
  } else {
    cat("✓ Most movies have genre information.\n")
  }
} else {
  cat("✗ No genre_matrix found! This is required for content-based filtering.\n")
}

# Check for additional text features (title, etc.)
if ("movies_filtered" %in% names(prepared_data)) {
  cat("\nChecking movie text features...\n")
  
  # Check if we have title information
  if ("title" %in% names(prepared_data$movies_filtered)) {
    # Check title length - short titles aren't useful for TF-IDF
    title_lengths <- nchar(prepared_data$movies_filtered$title)
    avg_title_length <- mean(title_lengths)
    
    cat("Average title length:", sprintf("%.1f", avg_title_length), "characters\n")
    
    if (avg_title_length < 10) {
      cat("⚠️ WARNING: Movie titles are very short on average. May not be useful for TF-IDF.\n")
    } else {
      cat("✓ Movie titles have sufficient length for text analysis.\n")
    }
  } else {
    cat("✗ No title information found in movies_filtered!\n")
  }
  
  # Check for additional text fields (tags, descriptions, etc.)
  if ("tags_raw" %in% names(prepared_data)) {
    num_tags <- nrow(prepared_data$tags_raw)
    unique_tags <- length(unique(prepared_data$tags_raw$tag))
    
    cat("Number of tag entries:", num_tags, "\n")
    cat("Number of unique tags:", unique_tags, "\n")
    
    if (num_tags > 1000 && unique_tags > 100) {
      cat("✓ Tag information is sufficient for text-based content filtering.\n")
    } else {
      cat("⚠️ Limited tag information for text-based filtering.\n")
    }
  } else {
    cat("ℹ️ No tag information found. This limits text-based content filtering.\n")
  }
}

# ===== 6. RATING DISTRIBUTION ANALYSIS =====
cat("\n===== RATING DISTRIBUTION ANALYSIS =====\n")

if ("ratings_filtered" %in% names(prepared_data)) {
  rating_counts <- table(prepared_data$ratings_filtered$rating)
  cat("Rating distribution:\n")
  print(rating_counts)
  
  # Calculate rating distribution metrics
  ratings <- prepared_data$ratings_filtered$rating
  cat("\nRating statistics:\n")
  cat("- Mean rating:", mean(ratings), "\n")
  cat("- Median rating:", median(ratings), "\n")
  cat("- Standard deviation:", sd(ratings), "\n")
  
  # Check for rating bias
  if (abs(mean(ratings) - median(ratings)) > 0.5) {
    cat("⚠️ WARNING: Rating distribution is skewed. This may affect recommendation quality.\n")
  } else {
    cat("✓ Rating distribution is relatively balanced.\n")
  }
  
  # Check rating variability
  if (sd(ratings) < 0.5) {
    cat("⚠️ WARNING: Low variability in ratings. This may make it harder to differentiate preferences.\n")
  } else {
    cat("✓ Rating variability is sufficient.\n")
  }
  
  # Create rating distribution plot
  pdf("rating_distribution_analysis.pdf")
  ggplot(data.frame(rating = ratings), aes(x = rating)) +
    geom_histogram(binwidth = 0.5, fill = "steelblue", color = "darkblue") +
    labs(title = "Rating Distribution", x = "Rating", y = "Count") +
    theme_minimal()
  dev.off()
  
  cat("Rating distribution plot saved to 'rating_distribution_analysis.pdf'\n")
}

# ===== 7. OVERALL ASSESSMENT =====
cat("\n===== OVERALL ASSESSMENT =====\n")

# Collect assessments for different algorithms
if ("ratings_sparse" %in% names(prepared_data) && 
    class(prepared_data$ratings_sparse)[1] == "realRatingMatrix") {
  
  matrix_object <- prepared_data$ratings_sparse
  dims <- dim(matrix_object)
  ratings_per_user <- rowCounts(matrix_object)
  ratings_per_item <- colCounts(matrix_object)
  sparsity <- 1 - sum(matrix_object@data != 0) / (dims[1] * dims[2])
  
  # UBCF assessment
  ubcf_suitable <- dims[1] >= 10 && mean(ratings_per_user) >= 10 && sparsity < 0.99
  
  # IBCF assessment
  ibcf_suitable <- dims[2] >= 10 && mean(ratings_per_item) >= 5 && sparsity < 0.995
  
  # Content-based assessment
  content_based_suitable <- "genre_matrix" %in% names(prepared_data) && 
    is.matrix(prepared_data$genre_matrix) &&
    ncol(prepared_data$genre_matrix) >= 5
  
  # Hybrid assessment (needs both CF and content data)
  hybrid_suitable <- (ubcf_suitable || ibcf_suitable) && content_based_suitable
  
  # Display overall assessment
  cat("UBCF (User-Based Collaborative Filtering): ", 
      if(ubcf_suitable) "SUITABLE" else "NOT RECOMMENDED", "\n")
  
  cat("IBCF (Item-Based Collaborative Filtering): ", 
      if(ibcf_suitable) "SUITABLE" else "NOT RECOMMENDED", "\n")
  
  cat("Content-Based Filtering: ", 
      if(content_based_suitable) "SUITABLE" else "NOT RECOMMENDED", "\n")
  
  cat("Hybrid Approaches: ", 
      if(hybrid_suitable) "SUITABLE" else "NOT RECOMMENDED", "\n")
  
  # Provide specific recommendations
  cat("\nRecommendations based on data assessment:\n")
  
  if (ubcf_suitable && ibcf_suitable) {
    if (dims[1] > dims[2] && sparsity < 0.98) {
      cat("- UBCF might perform better (more users than items and reasonable sparsity)\n")
    } else if (dims[2] > dims[1]) {
      cat("- IBCF might perform better (more items than users)\n")
    } else {
      cat("- Both UBCF and IBCF should be tested\n")
    }
  } else if (ubcf_suitable) {
    cat("- UBCF is recommended based on your data characteristics\n")
  } else if (ibcf_suitable) {
    cat("- IBCF is recommended based on your data characteristics\n")
  }
  
  if (content_based_suitable && !ubcf_suitable && !ibcf_suitable) {
    cat("- Content-based filtering might be your best option\n")
  } else if (content_based_suitable) {
    cat("- Consider including content-based features in a hybrid approach\n")
  }
  
  if (sparsity > 0.98) {
    cat("- Consider matrix factorization approaches (SVD) to handle high sparsity\n")
  }
} else {
  cat("Cannot provide overall assessment due to missing or incorrectly formatted data.\n")
}

cat("\nValidation complete!\n")