# Load the RData file if you've already saved it
load("data/prepared_data.RData")

# Create a directory for CSV files if it doesn't exist
dir.create("data/csv_exports", showWarnings = FALSE)

# 1. Export cleaned movies data
write.csv(movies_clean, "data/csv_exports/movies_clean.csv", row.names = FALSE)

# 2. Export filtered ratings data
write.csv(ratings_filtered, "data/csv_exports/ratings_filtered.csv", row.names = FALSE)

# 3. Export genre matrix
# Convert to data frame and add movie ID column
genre_matrix_df <- as.data.frame(genre_matrix)
genre_matrix_df$movieId <- rownames(genre_matrix)
write.csv(genre_matrix_df, "data/csv_exports/genre_matrix.csv", row.names = FALSE)

# 4. Export user-item matrix 
# This can be challenging due to its sparse nature
# Convert from recommenderlab format to regular matrix
ratings_matrix_full <- as(r_matrix, "matrix")

# Convert to data frame
ratings_df <- as.data.frame(ratings_matrix_full)

# Add user IDs as a column
ratings_df$userId <- rownames(ratings_matrix_full)

# Export (note: this might be a very large file)
write.csv(ratings_df, "data/csv_exports/ratings_matrix.csv", row.names = FALSE)

# Alternatively, export a long-format version which is more efficient
# Convert from wide to long format
ratings_long <- as(r_matrix, "data.frame")
write.csv(ratings_long, "data/csv_exports/ratings_long.csv", row.names = FALSE)

# 5. Export train-test split information
train_set <- getData(evaluation_scheme, "train")
train_df <- as(train_set, "data.frame")
write.csv(train_df, "data/csv_exports/train_set.csv", row.names = FALSE)

test_set <- getData(evaluation_scheme, "known")
test_df <- as(test_set, "data.frame")
write.csv(test_df, "data/csv_exports/test_set.csv", row.names = FALSE)