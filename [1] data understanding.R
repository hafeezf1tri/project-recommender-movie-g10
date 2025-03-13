# Load necessary libraries
library(tidyverse)
library(skimr)  # For summary statistics

# Load the datasets
movies <- read_csv("data/movies.csv")
ratings <- read_csv("data/ratings.csv")
tags <- read_csv("data/tags.csv")
links <- read_csv("data/links.csv")

# Basic dimensions
cat("Movies dataset:", dim(movies), "\n")
cat("Ratings dataset:", dim(ratings), "\n")
cat("Tags dataset:", dim(tags), "\n")
cat("Links dataset:", dim(links), "\n")

# View structure of each dataset
str(movies)
str(ratings)
str(tags)
str(links)

# Check for missing values
colSums(is.na(movies))
colSums(is.na(ratings))
colSums(is.na(tags))
colSums(is.na(links))

# Basic summaries
summary(movies)
summary(ratings)
summary(tags)
summary(links)

# More detailed summaries with skimr
skim(movies)
skim(ratings)
skim(tags)
skim(links)

# Process movies dataset - extract year
movies <- movies %>%
  mutate(year = str_extract(title, "\\(\\d{4}\\)$"),
         year = as.numeric(str_extract(year, "\\d{4}")),
         title_clean = str_remove(title, " \\(\\d{4}\\)$"))

# Movie distribution by decade
movies %>%
  mutate(decade = floor(year/10)*10) %>%
  count(decade) %>%
  arrange(decade)

# Genre distribution
movies %>%
  mutate(genres = strsplit(as.character(genres), "\\|")) %>%
  unnest(genres) %>%
  count(genres) %>%
  arrange(desc(n))

# Rating distribution
ratings %>%
  ggplot(aes(x = rating)) +
  geom_histogram(binwidth = 0.5)

# Most active users
ratings %>%
  count(userId) %>%
  arrange(desc(n)) %>%
  head(10)

# Most rated movies
top_movies <- ratings %>%
  count(movieId) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  left_join(movies, by = "movieId")
top_movies

# Top rated movies (with at least 100 ratings)
ratings %>%
  group_by(movieId) %>%
  summarise(avg_rating = mean(rating),
            n_ratings = n()) %>%
  filter(n_ratings >= 100) %>%
  arrange(desc(avg_rating)) %>%
  head(20) %>%
  left_join(movies, by = "movieId")

# Most common tags
tags %>%
  count(tag) %>%
  arrange(desc(n)) %>%
  head(20)