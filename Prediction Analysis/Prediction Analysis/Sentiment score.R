# Install and load required packages
if (!requireNamespace("RedditExtractoR", quietly = TRUE)) {
  install.packages("RedditExtractoR")
}
if (!requireNamespace("syuzhet", quietly = TRUE)) {
  install.packages("syuzhet")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}

library(RedditExtractoR)
library(syuzhet)
library(dplyr)
library(tidyr)

# Function to fetch comments from a subreddit
fetch_reddit_comments <- function(subreddit, period = "day", sort_by = "new", max_pages = 5) {
  tryCatch({
    # Fetch URLs of recent threads
    reddit_urls <- find_thread_urls(subreddit = subreddit, sort_by = sort_by, period = period)
    
    # Limit the number of URLs to process
    reddit_urls <- reddit_urls[1:min(max_pages, nrow(reddit_urls)), ]
    
    # Fetch comments from these threads
    reddit_comments <- get_thread_content(reddit_urls$url)
    
    return(reddit_comments$comments)
  }, error = function(e) {
    message("Error fetching or processing Reddit data: ", e$message)
    return(NULL)
  })
}

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

# Function to calculate cumulative sentiment score
calculate_cumulative_sentiment <- function(sentiment_data) {
  # Calculate total positive and negative scores
  total_positive <- sum(sentiment_data$positive, na.rm = TRUE)
  total_negative <- sum(sentiment_data$negative, na.rm = TRUE)
  
  # Calculate cumulative sentiment score
  cumulative_sentiment <- total_positive - total_negative
  
  # Determine overall sentiment
  overall_sentiment <- ifelse(cumulative_sentiment > 0, "Positive", "Negative")
  
  return(overall_sentiment)
}

# Fetch comments from r/wallstreetbets
reddit_comments <- fetch_reddit_comments(subreddit = "wallstreetbets", period = "day", sort_by = "new", max_pages = 5)

if (!is.null(reddit_comments)) {
  # Perform sentiment analysis on the comments
  reddit_comments_with_sentiment <- perform_sentiment_analysis(reddit_comments)
  
  # Calculate cumulative sentiment score
  overall_sentiment <- calculate_cumulative_sentiment(reddit_comments_with_sentiment)
  
  # Print overall sentiment
  print(paste("Daily Sentiment for r/wallstreetbets:", overall_sentiment))
} else {
  message("No comments data available for sentiment analysis.")
}
