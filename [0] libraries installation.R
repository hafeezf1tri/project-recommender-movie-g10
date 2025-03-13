# ============================================================================
# MovieLens Recommender System Project
# Library Installation Script
# 
# This script installs all required libraries for the project.
# Run this script before running any other scripts.
# ============================================================================

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    message("Installing the following packages: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages)
  } else {
    message("All required packages are already installed.")
  }
}

# Data manipulation and visualization
data_packages <- c(
  "tidyverse",   # For data manipulation and visualization
  "lubridate",   # For handling dates and times
  "scales",      # For better scale formatting
  "RColorBrewer" # For color palettes
)

# Data exploration
exploration_packages <- c(
  "skimr",        # For summary statistics
  "DataExplorer", # For automated data exploration
  "corrplot",     # For correlation plots
  "wordcloud"     # For visualizing tags
)

# Text processing
text_packages <- c(
  "tm",         # For text mining
  "tokenizers", # For text tokenization
  "stopwords",  # For removing stopwords
  "textcat"     # For text categorization
)

# Matrix operations
matrix_packages <- c(
  "Matrix" # For sparse matrices
)

# Recommender system
recommender_packages <- c(
  "recommenderlab", # Main package for recommendation algorithms
  "proxy",          # For similarity calculations
  "arules"          # For association rule mining
)

# Shiny and web interface
shiny_packages <- c(
  "shiny",          # Web application framework
  "shinydashboard", # Dashboard interface
  "shinythemes",    # Themes for Shiny
  "DT",             # Data tables
  "plotly"          # Interactive plots
)

# Model evaluation
evaluation_packages <- c(
  "caret"    # For machine learning
)

# Combine all packages
all_packages <- c(
  data_packages,
  exploration_packages,
  text_packages,
  matrix_packages,
  recommender_packages,
  shiny_packages,
  evaluation_packages
)

# Install missing packages
message("Checking for required packages...")
install_if_missing(all_packages)

# Load and check versions
message("\nLoading packages and checking versions...")
for (pkg in all_packages) {
  if (require(pkg, character.only = TRUE)) {
    message(pkg, " version ", packageVersion(pkg))
  } else {
    warning("Failed to load package: ", pkg)
  }
}

message("\n===============================================")
message("Package installation and verification complete.")
message("===============================================")