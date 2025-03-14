# MovieLens Recommender System

## Overview
This project implements a comprehensive movie recommendation system using the MovieLens Small dataset. The system uses multiple recommendation approaches to suggest relevant movies to users based on their preferences and behavior patterns.

## Dataset
The project utilizes the **MovieLens Small dataset** which contains:
- 100,000 ratings (1-5 stars)
- 3,600 tag applications
- 9,000 movies
- 600 users
- Last updated: September 2018

Source: [MovieLens](https://grouplens.org/datasets/movielens/)

## Project Scope
The project scope includes:

- **Data preprocessing**: Handling missing values, normalizing data, and preparing the dataset for modeling
  
- **Implementation of multiple recommendation approaches**:
  - Collaborative Filtering (User-based and Item-based)
  - Content-Based Filtering (Using TF-IDF, word embeddings, etc.)
  - Hybrid Model (Optional)

- **Model evaluation** using metrics such as:
  - Root Mean Squared Error (RMSE)
  - Mean Absolute Error (MAE)
  - Precision and Recall

- **Visualization dashboard** for user recommendations

- **Deployment** of the recommender system through a dashboard using Shiny

## Implementation Details

### Data Preprocessing
- Filtering users with sufficient ratings
- Handling missing values
- Normalizing ratings (z-score for UBCF, center for IBCF)
- Creating sparse rating matrices

### Models
1. **User-Based Collaborative Filtering (UBCF)**
   - Finds users with similar tastes
   - Recommends items liked by similar users
   - Parameters tuned: number of neighbors, similarity method

2. **Item-Based Collaborative Filtering (IBCF)**
   - Builds item-item similarity matrix
   - Recommends items similar to ones the user rated highly
   - Parameters tuned: k value, similarity method

3. **Content-Based Filtering**
   - Uses movie genres and metadata
   - Creates user profiles based on content preferences

## Results Summary

| Model | RMSE | MAE  | Precision | Recall | F1 Score |
|-------|------|------|-----------|--------|----------|
| UBCF  | 0.85 | 0.49 | 0.101     | 0.0188 | 0.0295   |
| IBCF  | 0.43 | 0.26 | NA        | 0.011  | NA       |

**Key Findings:**
- IBCF shows better rating prediction accuracy (lower RMSE)
- UBCF provides better recommendation quality (higher precision and recall)
- Models show complementary strengths that could be leveraged in a hybrid approach

## File Structure

```
├── data/
│   ├── movies.csv
│   ├── ratings.csv
│   ├── tags.csv
│   └── links.csv
├── [0] libraries installation.R
├── [1] data understanding.R
├── [3] prepping to data.R
├── [3.1] prepared data validation testing.R
├── [4] UBCF model creation and training.R
├── [5] UBCF TESTING.R
├── [6] IBCF model creation and training.R
├── [7] IBCF TESTING.R
├── [8] Multi Testing IBCF and UBCF models.R
├── models/
│   ├── ibcf_model.rds
│   └── ubcf_model.rds
├── prepared_data.RData
├── processed_data.RData
├── movie-recommender-g10.Rproj
├── visualizations/
│   ├── genre_distribution.pdf
│   ├── precision_recall_comparison.pdf
│   ├── rating_distribution.pdf
│   ├── rating_distribution_analysis.pdf
│   ├── recommender_error_metrics.pdf
│   ├── ubcf_rmse_distribution.pdf
│   ├── ubcf_validation_prec_rec.pdf
│   ├── ubcf_validation_roc.pdf
│   └── user_activity.pdf
├── results/
│   ├── precision_recall_data.csv
│   ├── recommender_metrics_summary.csv
│   └── recommender_precision_recall.csv
└── README.md
```

## Requirements
- R 4.0.0 or higher
- Required R packages:
  - recommenderlab
  - tidyverse
  - reshape2
  - ggplot2
  - shiny

## Running the Shiny App
```r
# Navigate to the app directory
setwd("path/to/app")

# Run the application
shiny::runApp()
```

## Future Work
- Implement hybrid recommendation models
- Explore additional evaluation metrics
- Improve catalog coverage
- Enhance visualization in the Shiny dashboard

## References
- MovieLens dataset: https://grouplens.org/datasets/movielens/
- Recommenderlab package: https://cran.r-project.org/web/packages/recommenderlab/
- Shiny for Web Apps: https://shiny.rstudio.com/
