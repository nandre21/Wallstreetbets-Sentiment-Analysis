install.packages(c("dplyr", "tidytext", "ggplot2", "tidyr"))

# Load necessary libraries
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)  # For pivot_wider

# Load the Reddit data

# Preprocess the comments
reddit_NVDA_clean <- reddit_NVDA %>%
  unnest_tokens(word, comment) %>%
  mutate(word = tolower(word)) %>%
  anti_join(stop_words)

# Perform sentiment analysis
sentiment_reddit <- reddit_NVDA_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(date_utc, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
  mutate(sentiment_score = positive - negative)

# Remove missing values
sentiment_reddit_clean <- sentiment_reddit %>%
  filter(!is.na(sentiment_score))

# Plot sentiment score over time
ggplot(sentiment_reddit_clean, aes(x = as.Date(date_utc), y = sentiment_score)) +
  geom_line() +
  labs(title = "Sentiment Analysis of Reddit Comments for NVDA",
       x = "Date",
       y = "Sentiment Score") +
  theme_minimal()

sentiment_reddit_table <- sentiment_reddit %>%
  select(date_utc, positive, negative, sentiment_score)


# Define thresholds for sentiment classification
threshold_positive <- 10
threshold_negative <- -10

# Classify sentiment scores
sentiment_reddit_classified <- sentiment_reddit %>%
  mutate(
    sentiment_category = case_when(
      sentiment_score > threshold_positive ~ "Strongly Positive",
      sentiment_score < threshold_negative ~ "Strongly Negative",
      TRUE ~ "Neutral"
    )
  )

# Display the classified sentiment table
print(sentiment_reddit_classified)







# Load the News data

# Preprocess the news
data_amd_clean <- data_amd %>%
  filter(!is.na(News)) %>%
  unnest_tokens(word, News) %>%
  mutate(word = tolower(word)) %>%
  anti_join(stop_words)

# Perform sentiment analysis
sentiment_amd <- data_amd_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(Date, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
  mutate(sentiment_score = positive - negative)

# Remove missing values
sentiment_amd_clean <- sentiment_amd %>%
  filter(!is.na(sentiment_score))

# Plot sentiment score over time
ggplot(sentiment_amd_clean, aes(x = as.Date(Date), y = sentiment_score)) +
  geom_line() +
  labs(title = "Sentiment Analysis of News Articles AMD",
       x = "Date",
       y = "Sentiment Score") +
  theme_minimal()

sentiment_amd_table <- sentiment_amd %>%
  select(Date, positive, negative, sentiment_score)

# Define thresholds for sentiment classification
threshold_positive <- 10
threshold_negative <- -10

# Classify sentiment scores
sentiment_amd_classified <- sentiment_amd %>%
  mutate(
    sentiment_category = case_when(
      sentiment_score > threshold_positive ~ "Strongly Positive",
      sentiment_score < threshold_negative ~ "Strongly Negative",
      TRUE ~ "Neutral"
    )
  )

# Display the classified sentiment table
print(sentiment_amd_classified)
