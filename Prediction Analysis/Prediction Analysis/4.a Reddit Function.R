# Load necessary libraries
library(RedditExtractoR)
library(dplyr)
library(lubridate)
library(httr)

# Function to get Reddit thread URLs for a specific ticker and a specific date range
find_reddit_threads <- function(ticker, start_date, end_date, period = "week") {
  all_urls <- data.frame(url = character(), stringsAsFactors = FALSE)
  
  fetch_urls <- function(page_number) {
    urls <- tryCatch({
      find_thread_urls(
        keywords = ticker,
        sort_by = "top",
        period = period
      )
    }, error = function(e) {
      warning(paste("Error fetching URLs from page", page_number, ":", e$message))
      return(data.frame(url = character(), stringsAsFactors = FALSE))
    })
    
    return(urls)
  }
  
  page_number <- 1
  repeat {
    urls <- fetch_urls(page_number)
    
    if (nrow(urls) == 0) break
    
    print(paste("Fetched", nrow(urls), "URLs from page", page_number))
    
    all_urls <- rbind(all_urls, urls)
    
    page_number <- page_number + 1
    if (page_number > 5) break
  }
  
  if (nrow(all_urls) == 0) {
    warning("No URLs found for the given ticker.")
    return(NULL)
  }
  
  return(all_urls$url)
}

# Function to fetch thread content with retry logic and handle rate limits
get_reddit_thread_content <- function(urls, max_retries = 5, base_delay = 60) {
  if (length(urls) == 0) {
    warning("No URLs to fetch content from.")
    return(NULL)
  }
  
  results <- list()
  attempt <- 1
  while (attempt <= max_retries) {
    content <- tryCatch({
      get_thread_content(urls)
    }, error = function(e) {
      warning(paste("Error fetching thread content (attempt", attempt, "):", e$message))
      return(NULL)
    })
    
    if (!is.null(content)) {
      results <- content
      break
    }
    
    # Exponential backoff
    delay <- base_delay * 2^(attempt - 1)
    Sys.sleep(delay)
    attempt <- attempt + 1
  }
  
  if (is.null(results)) {
    warning("Failed to fetch thread content after several attempts.")
  }
  
  return(results)
}

# Function to get weekly data
fetch_weekly_data <- function(ticker, start_date, end_date) {
  current_start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  all_threads <- list()
  
  while (current_start_date <= end_date) {
    current_end_date <- min(current_start_date + weeks(1) - days(1), end_date)
    
    print(paste("Fetching data from", current_start_date, "to", current_end_date))
    
    # Find Reddit thread URLs for the current week
    thread_urls <- find_reddit_threads(ticker, current_start_date, current_end_date)
    
    if (!is.null(thread_urls)) {
      # Get content for the found threads with retry logic
      reddit_content <- get_reddit_thread_content(thread_urls)
      
      if (!is.null(reddit_content)) {
        all_threads[[as.character(current_start_date)]] <- list(
          threads = reddit_content$threads,
          comments = reddit_content$comments
        )
      }
    }
    
    current_start_date <- current_start_date + weeks(1)
  }
  
  return(all_threads)
}

# Example usage
ticker <- "NVDA"
start_date <- "2023-01-01"
end_date <- "2023-06-30"

# Fetch weekly data
weekly_data <- fetch_weekly_data(ticker, start_date, end_date)

# Print example data
if (length(weekly_data) > 0) {
  print(head(weekly_data[[1]]$threads))
  print(head(weekly_data[[1]]$comments))
} else {
  warning("No data found.")
}
