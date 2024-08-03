#Attempt 3 Reddit

library(RedditExtractoR)

#gets the url
reddit_urls <- find_thread_urls(subreddit = "wallstreetbets", sort_by = "new", period = "week")

#gets the comments
reddit_comments <-get_thread_content(reddit_urls$url)

print(head(reddit_comments$comments))


library(syuzhet)


# Load necessary libraries
library(RedditExtractoR)
library(syuzhet)

# Function to perform sentiment analysis
perform_sentiment_analysis <- function(comments) {
  # Extract the text of the comments
  comment_texts <- comments$comment
  
  # Get sentiment scores using syuzhet
  sentiment_scores <- get_nrc_sentiment(comment_texts)
  
  # Combine the comments with their sentiment scores
  comments_with_sentiment <- cbind(comments, sentiment_scores)
  
  return(comments_with_sentiment)
}

# Perform sentiment analysis
reddit_comments_with_sentiment <- perform_sentiment_analysis(reddit_comments$comments)

# Print the first few rows of the comments with sentiment scores
print(head(reddit_comments_with_sentiment))



library(ggplot2)
library(dplyr)

# Function to plot sentiment analysis results over time
plot_sentiment_over_time <- function(sentiment_data) {
  # Convert timestamp to date
  sentiment_data$date <- as.Date(as.POSIXct(sentiment_data$timestamp, origin="1970-01-01", tz="UTC"))
  
  # Summarize sentiment scores by date
  sentiment_summary <- sentiment_data %>%
    group_by(date) %>%
    summarize(positive = mean(positive - negative), .groups = 'drop')
  
  # Create the plot
  sentiment_plot <- ggplot(sentiment_summary, aes(x = date, y = positive)) +
    geom_line(color = "steelblue") +
    geom_point(color = "steelblue") +
    theme_minimal() +
    labs(title = "Sentiment Analysis of Reddit Comments Over Time",
         x = "Date",
         y = "Average Sentiment Score (Positive - Negative)")
  
  return(sentiment_plot)
}

sentiment_plot <- plot_sentiment_over_time(reddit_comments_with_sentiment)
print(sentiment_plot)

