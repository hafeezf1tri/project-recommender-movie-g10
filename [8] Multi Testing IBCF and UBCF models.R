# Improved Precision-Recall Evaluation for Recommender Systems
# This script calculates precision and recall metrics with enhanced error handling

# Load required libraries
library(recommenderlab)
library(tidyverse)
library(Matrix)  # For handling sparse matrices efficiently
library(grid)

# Set random seed for reproducibility
set.seed(456)

# ===== 1. LOAD MODELS AND DATA =====
cat("Loading models and prepared data...\n")

# Check for model files with better error messages
if (!file.exists("ubcf_model.rds")) {
  stop("Error: ubcf_model.rds file not found! Please ensure models are correctly saved.")
}

if (!file.exists("ibcf_model.rds")) {
  stop("Error: ibcf_model.rds file not found! Please ensure models are correctly saved.")
}

# Load models
ubcf_model <- readRDS("ubcf_model.rds")
ibcf_model <- readRDS("ibcf_model.rds")

# Print model summaries to help with debugging
cat("UBCF Model Summary:\n")
print(ubcf_model)

cat("\nIBCF Model Summary:\n")
print(ibcf_model)

# Load data
if (!file.exists("prepared_data.RData")) {
  stop("Error: prepared_data.RData file not found! Please ensure data is correctly prepared.")
}

load("prepared_data.RData")

# Check data structure and provide more detailed diagnostics
if (exists("prepared_data")) {
  cat("\nAvailable objects in prepared data:\n")
  print(names(prepared_data))
  
  # Try to find a valid rating matrix
  if ("ratings_sparse" %in% names(prepared_data) && 
      inherits(prepared_data$ratings_sparse, "realRatingMatrix")) {
    ratings_data <- prepared_data$ratings_sparse
    cat("Using realRatingMatrix from ratings_sparse\n")
    
    # Print some info about the matrix
    cat("Rating matrix dimensions:", dim(ratings_data), "\n")
    
    # Use getRatingMatrix to safely get the number of non-zero entries
    ratings_matrix <- getRatingMatrix(ratings_data)
    if (!is.null(ratings_matrix)) {
      n_ratings <- sum(ratings_matrix@x != 0)
      total_cells <- prod(dim(ratings_data))
      cat("Density:", sprintf("%.4f%%", 100 * n_ratings / total_cells), "\n")
    } else {
      cat("Unable to calculate density (could not access rating matrix)\n")
    }
  } else if (is.list(prepared_data) && 
             "ratings_matrix" %in% names(prepared_data) && 
             inherits(prepared_data$ratings_matrix, "realRatingMatrix")) {
    ratings_data <- prepared_data$ratings_matrix
    cat("Using realRatingMatrix from ratings_matrix\n")
    
    # Print some info about the matrix
    cat("Rating matrix dimensions:", dim(ratings_data), "\n")
    cat("Density:", sprintf("%.4f%%", 100 * nnzero(ratings_data) / prod(dim(ratings_data))), "\n")
  } else {
    # Try to find any matrix that could be converted
    for (name in names(prepared_data)) {
      if (inherits(prepared_data[[name]], c("matrix", "data.frame", "dgCMatrix"))) {
        cat("Found potential ratings data in", name, "\n")
        # Try to convert to realRatingMatrix
        tryCatch({
          ratings_data <- as(prepared_data[[name]], "realRatingMatrix")
          cat("Successfully converted", name, "to realRatingMatrix\n")
          break
        }, error = function(e) {
          cat("Could not convert", name, "to realRatingMatrix:", e$message, "\n")
        })
      }
    }
  }
} else {
  # If prepared_data doesn't exist, look for other variables
  all_vars <- ls()
  cat("prepared_data not found. Searching for rating matrices in environment...\n")
  
  for (var_name in all_vars) {
    var_obj <- get(var_name)
    if (inherits(var_obj, "realRatingMatrix")) {
      ratings_data <- var_obj
      cat("Found realRatingMatrix in variable:", var_name, "\n")
      break
    }
  }
}

# Ensure we have a valid ratings matrix
if (!exists("ratings_data")) {
  stop("Error: No valid rating matrix found for evaluation. Please check your data preparation.")
}

# ===== 2. IMPROVED PRECISION-RECALL EVALUATION =====
cat("\nPerforming improved precision-recall evaluation...\n")

# Set this to TRUE for detailed debug output, FALSE for cleaner output
DEBUG_MODE <- TRUE

improved_precision_recall <- function(model, data, model_name, n_recs = 10, n_users = 100, min_ratings = 30, min_high_ratings = 5, debug = DEBUG_MODE) {
  cat(sprintf("\n===== Evaluating %s model =====\n", model_name))
  
  # Get users with sufficient ratings
  ratings_per_user <- rowCounts(data)
  valid_users <- which(ratings_per_user >= min_ratings)
  
  if (length(valid_users) == 0) {
    cat(sprintf("Not enough users with %d+ ratings for evaluation\n", min_ratings))
    return(NULL)
  }
  
  cat("Found", length(valid_users), "users with", min_ratings, "+ ratings\n")
  
  # Sample users for testing
  test_users <- sample(valid_users, min(n_users, length(valid_users)))
  cat("Selected", length(test_users), "users for evaluation\n")
  
  # Initialize results
  results <- list()
  errors <- list()
  
  # Process each test user
  for (i in 1:length(test_users)) {
    user_id <- test_users[i]
    
    # Get items this user has already rated - with safer conversion
    user_data <- data[user_id,]
    
    # Try different methods to get the user profile
    user_profile <- NULL
    
    # Try method 1: Direct conversion if possible
    tryCatch({
      user_profile <- as(user_data, "matrix")[1,]
    }, error = function(e) {
      cat("Warning: Could not directly convert user data to matrix\n")
    })
    
    # Try method 2: If method 1 fails
    if (is.null(user_profile)) {
      tryCatch({
        # Get indices and ratings separately
        user_items <- colnames(user_data)[as(user_data@data, "matrix")[1,] > 0]
        user_ratings <- as(user_data@data, "matrix")[1, user_items]
        
        # Create a named vector
        user_profile <- rep(NA, ncol(data))
        names(user_profile) <- colnames(data)
        user_profile[user_items] <- user_ratings
      }, error = function(e) {
        cat("Warning: Alternative method for extracting user ratings failed:", e$message, "\n")
      })
    }
    
    # If we still don't have a profile, skip this user
    if (is.null(user_profile)) {
      cat("Error: Could not extract profile for user", user_id, "- skipping\n")
      next
    }
    
    # Get items this user has rated
    user_rated_items <- which(!is.na(user_profile))
    
    # Get only highly rated items (4+ stars) as "relevant" items
    highly_rated <- which(user_profile >= 4.0 & !is.na(user_profile))
    
    # Skip users with too few highly rated items
    if (length(highly_rated) < min_high_ratings) {
      next
    }
    
    # Create a test profile with half of the highly rated items hidden
    n_to_hide <- floor(length(highly_rated) / 2)
    items_to_hide <- sample(highly_rated, n_to_hide)
    
    test_profile <- user_profile
    test_profile[items_to_hide] <- NA
    
    # Create the test matrix more safely
    test_matrix <- NULL
    
    tryCatch({
      # Method 1: Try creating a new matrix with proper row and column names
      mat <- matrix(test_profile, nrow=1)
      rownames(mat) <- rownames(data)[user_id]
      colnames(mat) <- colnames(data)
      test_matrix <- as(mat, "realRatingMatrix")
    }, error = function(e) {
      cat("Warning: First method to create test matrix failed:", e$message, "\n")
      
      # Method 2: Modify the original user data
      tryCatch({
        # Create a copy of the user data
        test_matrix <- data[user_id,]
        
        # Remove the ratings for items we want to hide
        for (item_id in items_to_hide) {
          item_name <- names(item_id)
          if (is.null(item_name)) item_name <- colnames(data)[item_id]
          test_matrix@data[1, item_name] <- 0
        }
      }, error = function(e) {
        cat("Warning: Second method to create test matrix also failed:", e$message, "\n")
      })
    })
    
    # If we couldn't create a test matrix, skip this user
    if (is.null(test_matrix)) {
      cat("Error: Could not create test matrix for user", user_id, "- skipping\n")
      next
    }
    
    # Generate recommendations with enhanced error handling
    tryCatch({
      # Try to generate recommendations with additional debugging
      if (debug) cat("Generating recommendations for user", user_id, "...\n")
      
      rec_list <- NULL
      tryCatch({
        rec_list <- predict(model, test_matrix, n = n_recs, type = "topNList")
        if (debug) cat("Successfully generated recommendation list\n")
      }, error = function(e) {
        cat("Error generating recommendations:", e$message, "\n")
        errors[[length(errors) + 1]] <- paste("User", user_id, "- Prediction error:", e$message)
      })
      
      # If prediction failed, skip this user
      if (is.null(rec_list)) {
        next
      }
      
      # Try to convert to list with error handling
      rec_items <- NULL
      tryCatch({
        rec_list_as_list <- as(rec_list, "list")
        
        # Check if recommendations were generated
        if (length(rec_list_as_list) == 0) {
          cat("Warning: No recommendations generated for user", user_id, "\n")
          errors[[length(errors) + 1]] <- paste("User", user_id, "- No recommendations")
          next
        }
        
        rec_items <- rec_list_as_list[[1]]
        if (debug) cat("Number of recommendations:", length(rec_items), "\n")
        
        # Check if any items were recommended
        if (length(rec_items) == 0) {
          cat("Warning: Empty recommendation list for user", user_id, "\n")
          errors[[length(errors) + 1]] <- paste("User", user_id, "- Empty recommendations")
          next
        }
      }, error = function(e) {
        cat("Error converting recommendations to list:", e$message, "\n")
        errors[[length(errors) + 1]] <- paste("User", user_id, "- List conversion error:", e$message)
        next
      })
      
      # If we couldn't get recommendation items, skip this user
      if (is.null(rec_items)) {
        next
      }
      
      # Convert recommended items to column indices safely
      if (debug) cat("Converting recommended items to indices...\n")
      rec_indices <- match(rec_items, colnames(data))
      
      # Debug recommendation matching
      if (debug) cat("Matching stats - Total:", length(rec_indices), 
                     "Matched:", sum(!is.na(rec_indices)),
                     "Unmatched:", sum(is.na(rec_indices)), "\n")
      
      # Remove NA values that might occur if item matching fails
      rec_indices <- rec_indices[!is.na(rec_indices)]
      
      if (length(rec_indices) == 0) {
        cat("Warning: All recommendations invalid for user", user_id, "\n")
        errors[[length(errors) + 1]] <- paste("User", user_id, "- Invalid recommendations")
        next
      }
      
      # Calculate how many hidden highly rated items were recommended
      hits <- sum(rec_indices %in% items_to_hide)
      
      # Calculate precision and recall with protection against division by zero
      precision <- if (length(rec_indices) > 0) hits / length(rec_indices) else 0
      recall <- if (length(items_to_hide) > 0) hits / length(items_to_hide) else 0
      
      # Calculate F1 score
      f1 <- if (precision + recall > 0) 2 * (precision * recall) / (precision + recall) else 0
      
      # Store results
      results[[i]] <- list(
        user_id = user_id,
        precision = precision,
        recall = recall,
        f1 = f1,
        n_recommended = length(rec_indices),
        n_hidden = length(items_to_hide),
        n_hits = hits
      )
      
      # Show progress
      if (i %% 10 == 0 || i == length(test_users)) {
        cat(sprintf("Processed %d/%d users\n", i, length(test_users)))
      }
      
    }, error = function(e) {
      cat("Error processing user", user_id, ":", e$message, "\n")
      errors[[length(errors) + 1]] <- paste("User", user_id, "-", e$message)
    })
  }
  
  # Filter out NULL results
  results <- results[!sapply(results, is.null)]
  
  # Summarize errors if any
  if (length(errors) > 0) {
    cat("\nEncountered", length(errors), "errors during evaluation\n")
    # Log first 5 errors for diagnostic purposes
    for (i in 1:min(5, length(errors))) {
      cat(" -", errors[[i]], "\n")
    }
    if (length(errors) > 5) {
      cat(" - ... and", length(errors) - 5, "more errors\n")
    }
  }
  
  # Calculate average metrics
  if (length(results) > 0) {
    # Extract metrics
    precision_values <- sapply(results, function(x) x$precision)
    recall_values <- sapply(results, function(x) x$recall)
    f1_values <- sapply(results, function(x) x$f1)
    
    # Calculate averages
    avg_precision <- mean(precision_values, na.rm = TRUE)
    avg_recall <- mean(recall_values, na.rm = TRUE)
    avg_f1 <- mean(f1_values, na.rm = TRUE)
    
    # Display summary
    cat("\nSummary statistics for", model_name, ":\n")
    cat("Number of users successfully evaluated:", length(results), "\n")
    cat("Average precision:", round(avg_precision, 4), "\n")
    cat("Average recall:", round(avg_recall, 4), "\n")
    cat("Average F1 score:", round(avg_f1, 4), "\n")
    
    return(list(
      model = model_name,
      precision = avg_precision,
      recall = avg_recall,
      f1 = avg_f1,
      n_users = length(results),
      n_errors = length(errors),
      details = results
    ))
  } else {
    cat("No valid results obtained for", model_name, "\n")
    return(list(
      model = model_name,
      precision = NA,
      recall = NA,
      f1 = NA,
      n_users = 0,
      n_errors = length(errors),
      error_message = "No valid evaluation results"
    ))
  }
}

# Evaluate models with more diagnostic information
ubcf_metrics <- improved_precision_recall(ubcf_model, ratings_data, "UBCF")
ibcf_metrics <- improved_precision_recall(ibcf_model, ratings_data, "IBCF")

# ===== 3. SUMMARIZE RESULTS =====
cat("\n===== PRECISION-RECALL RESULTS SUMMARY =====\n")

# Create a summary table
metrics_table <- data.frame(
  Metric = c("Precision", "Recall", "F1 Score", "Users Evaluated", "Errors"),
  UBCF = c(
    ifelse(!is.null(ubcf_metrics) && !is.na(ubcf_metrics$precision), 
           round(ubcf_metrics$precision, 4), "NA"),
    ifelse(!is.null(ubcf_metrics) && !is.na(ubcf_metrics$recall), 
           round(ubcf_metrics$recall, 4), "NA"),
    ifelse(!is.null(ubcf_metrics) && !is.na(ubcf_metrics$f1), 
           round(ubcf_metrics$f1, 4), "NA"),
    ifelse(!is.null(ubcf_metrics), ubcf_metrics$n_users, 0),
    ifelse(!is.null(ubcf_metrics), ubcf_metrics$n_errors, "NA")
  ),
  IBCF = c(
    ifelse(!is.null(ibcf_metrics) && !is.na(ibcf_metrics$precision), 
           round(ibcf_metrics$precision, 4), "NA"),
    ifelse(!is.null(ibcf_metrics) && !is.na(ibcf_metrics$recall), 
           round(ibcf_metrics$recall, 4), "NA"),
    ifelse(!is.null(ibcf_metrics) && !is.na(ibcf_metrics$f1), 
           round(ibcf_metrics$f1, 4), "NA"),
    ifelse(!is.null(ibcf_metrics), ibcf_metrics$n_users, 0),
    ifelse(!is.null(ibcf_metrics), ibcf_metrics$n_errors, "NA")
  )
)

# Print the table
print(metrics_table)

# Save metrics table
write.csv(metrics_table, "recommender_precision_recall.csv", row.names = FALSE)
cat("\nPrecision-recall metrics saved to 'recommender_precision_recall.csv'\n")

# Create visualization
if (!is.null(ubcf_metrics) || !is.null(ibcf_metrics)) {
  # Prepare data for visualization
  plot_data <- data.frame(
    Algorithm = character(),
    Metric = character(),
    Value = numeric()
  )
  
  # Add UBCF data if available
  if (!is.null(ubcf_metrics)) {
    ubcf_data <- data.frame(
      Algorithm = rep("UBCF", 3),
      Metric = c("Precision", "Recall", "F1 Score"),
      Value = c(
        ifelse(!is.na(ubcf_metrics$precision), ubcf_metrics$precision, 0),
        ifelse(!is.na(ubcf_metrics$recall), ubcf_metrics$recall, 0),
        ifelse(!is.na(ubcf_metrics$f1), ubcf_metrics$f1, 0)
      )
    )
    plot_data <- rbind(plot_data, ubcf_data)
  }
  
  # Add IBCF data if available
  if (!is.null(ibcf_metrics)) {
    ibcf_data <- data.frame(
      Algorithm = rep("IBCF", 3),
      Metric = c("Precision", "Recall", "F1 Score"),
      Value = c(
        ifelse(!is.na(ibcf_metrics$precision), ibcf_metrics$precision, 0),
        ifelse(!is.na(ibcf_metrics$recall), ibcf_metrics$recall, 0),
        ifelse(!is.na(ibcf_metrics$f1), ibcf_metrics$f1, 0)
      )
    )
    plot_data <- rbind(plot_data, ibcf_data)
  }
  
  # Create bar plot if data is available
  if (nrow(plot_data) > 0) {
    # Save data for external visualization
    write.csv(plot_data, "precision_recall_data.csv", row.names = FALSE)
    
    # Create plot using ggplot2
    p <- ggplot(plot_data, aes(x = Metric, y = Value, fill = Algorithm)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = sprintf("%.4f", Value)), 
                position = position_dodge(width = 0.9), 
                vjust = -0.5) +
      labs(title = "Precision, Recall, and F1 Score Comparison",
           y = "Value") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
    
    # Save plot
    ggsave("precision_recall_comparison.png", p, width = 8, height = 6)
    
    # Also save as PDF
    ggsave("precision_recall_comparison.pdf", p, width = 8, height = 6)
    
    cat("Visualization saved to 'precision_recall_comparison.png' and 'precision_recall_comparison.pdf'\n")
    
    # Create table visualization as backup
    pdf("precision_recall_table.pdf", width = 8, height = 6)
    grid.newpage()
    grid.table(metrics_table)
    dev.off()
    cat("Table visualization saved to 'precision_recall_table.pdf'\n")
  }
}

cat("\nImproved precision-recall evaluation complete!\n")