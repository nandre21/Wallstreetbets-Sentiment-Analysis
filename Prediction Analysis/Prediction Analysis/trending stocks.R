# Install and load required packages
if (!requireNamespace("RedditExtractoR", quietly = TRUE)) {
  install.packages("RedditExtractoR")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("quantmod", quietly = TRUE)) {
  install.packages("quantmod")
}

library(RedditExtractoR)
library(dplyr)
library(stringr)
library(ggplot2)
library(quantmod)

# Load stock tickers from quantmod
tickers <- stockSymbols()$Symbol

# Function to get frequent terms from comments
get_frequent_terms <- function(comments, top_n = 10) {
  # Define a regular expression to match potential stock tickers (uppercase, 1-4 characters)
  ticker_pattern <- "\\b[A-Z]{1,4}\\b"
  
  # Extract all potential tickers from comments
  all_tickers <- str_extract_all(comments, ticker_pattern)
  all_tickers <- unlist(all_tickers)
  
  # Create a data frame and count occurrences
  ticker_counts <- data.frame(ticker = all_tickers, stringsAsFactors = FALSE) %>%
    group_by(ticker) %>%
    summarize(count = n(), .groups = 'drop') %>%
    arrange(desc(count))
  
  # Filter out common non-stock words
  common_words <- c(
    "THE", "AND", "FOR", "WITH", "THIS", "THAT", "HAVE", "ARE", "FROM",
    "WILL", "BE", "HAS", "CAN", "IT", "OF", "TO", "A", "IN", "IS", "ON",
    "AT", "BY", "AS", "OR", "AN", "THAT", "NOT", "WE", "YOU", "HE", "SHE",
    "THEY", "HIS", "HER", "MY", "OUR", "YOUR", "ITS", "BUT", "IF", "THEN",
    "UP", "DOWN", "OUT", "OVER", "UNDER", "NO", "YES", "MAY", "MIGHT", "SHOULD",
    "I", "WSB", "AI", "OP", "DD", "A", "ALL", "ANY", "WHO", "WHAT", "WHEN",
    "WHERE", "WHY", "HOW", "DOES", "DID", "DOING", "DONE", "JUST", "LIKE",
    "WAS", "AM", "BEEN", "IS", "ARE", "BY", "FROM", "FOR", "HAD", "HAS",
    "HAVE", "IT", "ITS", "INTO", "OF", "OFF", "ON", "OUT", "THAT", "THE",
    "THEIR", "THEM", "THEY", "THIS", "TO", "WITH", "YOU", "YOUR", "IT'S", "DON'T"
  )
  ticker_counts <- ticker_counts %>%
    filter(!ticker %in% common_words)
  
  # Return top trending stocks
  top_trending <- ticker_counts %>%
    filter(ticker %in% tickers) %>%
    head(top_n)
  
  return(top_trending)
}

# Helper function to perform API request with retries
safe_api_request <- function(url, max_retries = 5) {
  attempt <- 1
  while (attempt <= max_retries) {
    tryCatch({
      result <- get_thread_content(url)
      return(result)
    }, error = function(e) {
      if (attempt == max_retries) {
        stop("Max retries reached. Error: ", e$message)
      }
      Sys.sleep(2^attempt) # Exponential backoff
      attempt <<- attempt + 1
    })
  }
}

# Function to fetch and analyze trending stocks dynamically
get_trending_stocks_dynamic <- function(subreddit, period = "day", sort_by = "new", top_n = 10, max_threads = 10) {
  tryCatch({
    # Fetch URLs of recent threads
    reddit_urls <- find_thread_urls(subreddit = subreddit, sort_by = sort_by, period = period)
    
    # Initialize empty list for comments
    all_comments <- list()
    
    # Fetch comments from these threads with a delay between requests
    for (url in reddit_urls$url[1:max_threads]) {
      reddit_comments <- safe_api_request(url)
      all_comments <- c(all_comments, reddit_comments$comments$comment)
      Sys.sleep(1) # Delay to respect rate limits
    }
    
    # Convert to a character vector
    comments <- unlist(all_comments)
    
    # Get top trending stocks
    top_trending <- get_frequent_terms(comments, top_n)
    
    return(top_trending)
  }, error = function(e) {
    message("Error fetching or processing Reddit data: ", e$message)
    return(data.frame(ticker = character(0), count = integer(0)))
  })
}

# Function to plot trending stocks
plot_trending_stocks <- function(trending_stocks) {
  ggplot(trending_stocks, aes(x = reorder(ticker, -count), y = count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    labs(title = "Top Trending Stocks on r/wallstreetbets",
         x = "Stock Ticker",
         y = "Mention Count") +
    coord_flip()
}

# Fetch and plot top 10 trending stocks on r/wallstreetbets
trending_stocks <- get_trending_stocks_dynamic(subreddit = "wallstreetbets", period = "day", sort_by = "new", top_n = 10)
print(trending_stocks)

# If there's data to plot, create the plot
if (nrow(trending_stocks) > 0) {
  plot <- plot_trending_stocks(trending_stocks)
  print(plot)
} else {
  message("No trending stocks data available.")
}
