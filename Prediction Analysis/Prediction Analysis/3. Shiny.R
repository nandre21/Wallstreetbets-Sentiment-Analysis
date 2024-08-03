# Load necessary libraries
library(shiny)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(quantmod)
library(rvest)
library(stringr)
library(zoo)
library(RedditExtractoR)

# Function to fetch news data from FinViz
getFinNews <- function(ticker) {
  Sys.sleep(5)
  url <- paste0("https://finviz.com/quote.ashx?t=", ticker)
  data <- read_html(url)
  data <- data %>% html_nodes(xpath = "//*[@id='news-table']") %>% html_table()
  tmp <- do.call(rbind, data)
  dtime <- as.data.frame(tmp[, 1])
  dtime <- t(as.data.frame(str_split(dtime[, 1], pattern = " ")))
  dates <- as.character(dtime[, 1])
  tmz <- as.character(dtime[, 2])
  dates[str_detect(dates, pattern = ":")] <- NA
  dates <- na.locf(dates)
  timeStamp <- as.data.frame(as.POSIXct(paste(dates, tmz), format = "%b-%d-%y %I:%M%p"))
  tmp[, 1] <- timeStamp
  tmp[, 3] <- ticker
  colnames(tmp) <- c("Date", "News", "Ticker")
  tmp
}

# Function to fetch Reddit data
getRedditData <- function(ticker) {
  links <- find_thread_urls(keywords = ticker, subreddit = "wallstreetbets", sort_by = "new", period = "week")
  if (nrow(links) == 0) {
    return(list(threads = data.frame(), comments = data.frame()))
  }
  
  sample_size <- min(20, nrow(links))
  sampled_links <- links[sample(nrow(links), size = sample_size), ]
  content_list <- lapply(sampled_links$url, get_thread_content)
  
  extract_data <- function(content) {
    if (is.null(content$threads) || is.null(content$comments)) {
      return(NULL)
    }
    threads_df <- data.frame(
      url = content$threads$url,
      author = content$threads$author,
      date_utc = as.Date(content$threads$date, format = "%Y-%m-%d"),
      timestamp = content$threads$timestamp,
      title = content$threads$title,
      text = content$threads$text,
      subreddit = content$threads$subreddit,
      score = content$threads$score,
      upvotes = content$threads$upvotes,
      downvotes = content$threads$downvotes,
      up_ratio = content$threads$up_ratio,
      total_awards_received = content$threads$total_awards_received,
      golds = content$threads$golds,
      cross_posts = content$threads$cross_posts,
      comments_count = content$threads$comments,
      stringsAsFactors = FALSE
    )
    comments_df <- data.frame(
      url = content$comments$url,
      author = content$comments$author,
      date_utc = as.Date(content$comments$date, format = "%Y-%m-%d"),
      timestamp = content$comments$timestamp,
      score = content$comments$score,
      upvotes = content$comments$upvotes,
      downvotes = content$comments$downvotes,
      golds = content$comments$golds,
      comment = content$comments$comment,
      stringsAsFactors = FALSE
    )
    list(threads_df = threads_df, comments_df = comments_df)
  }
  
  extracted_data_list <- lapply(content_list, extract_data)
  all_threads <- do.call(rbind, lapply(extracted_data_list, function(x) x$threads_df))
  all_comments <- do.call(rbind, lapply(extracted_data_list, function(x) x$comments_df))
  list(threads = all_threads, comments = all_comments)
}

# Define UI
ui <- fluidPage(
  titlePanel("Stock Sentiment Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("ticker", "Enter Stock Ticker", value = "AAPL"),
      dateRangeInput("dates", "Select Date Range", start = Sys.Date() - 30, end = Sys.Date()),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      textOutput("correlationText"),
      plotOutput("sentimentPlot"),
      plotOutput("stockPlot"),
      tableOutput("sentimentTable")
    )
  )
)

# Define server
server <- function(input, output) {
  
  observeEvent(input$submit, {
    ticker <- toupper(input$ticker)
    start_date <- input$dates[1]
    end_date <- input$dates[2]
    
    # Fetch stock data
    stock_data <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    stock_prices <- data.frame(Date = index(stock_data), coredata(stock_data))
    stock_prices$Date <- as.Date(stock_prices$Date)
    
    # Fetch news data
    news_data <- getFinNews(ticker)
    
    # Fetch Reddit data
    reddit_data <- getRedditData(ticker)
    
    if (nrow(reddit_data$comments) == 0) {
      output$correlationText <- renderText("No Reddit data available for the selected ticker and date range.")
      output$sentimentPlot <- renderPlot(NULL)
      output$stockPlot <- renderPlot(NULL)
      output$sentimentTable <- renderTable(data.frame())
      return()
    }
    
    # Preprocess Reddit comments
    reddit_clean <- reddit_data$comments %>%
      unnest_tokens(word, comment) %>%
      mutate(word = tolower(word)) %>%
      anti_join(stop_words)
    
    # Perform sentiment analysis on Reddit data
    sentiment_reddit <- reddit_clean %>%
      inner_join(get_sentiments("bing")) %>%
      count(date_utc, sentiment) %>%
      pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
      mutate(sentiment_score = positive - negative)
    
    # Preprocess news data
    news_data_clean <- news_data %>%
      filter(!is.na(News)) %>%
      unnest_tokens(word, News) %>%
      mutate(word = tolower(word)) %>%
      anti_join(stop_words)
    
    # Perform sentiment analysis on news data
    sentiment_news <- news_data_clean %>%
      inner_join(get_sentiments("bing")) %>%
      count(Date, sentiment) %>%
      pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
      mutate(sentiment_score = positive - negative)
    
    # Normalize data
    sentiment_reddit <- sentiment_reddit %>%
      mutate(sentiment_score_normalized = (sentiment_score - min(sentiment_score)) / (max(sentiment_score) - min(sentiment_score)))
    
    stock_prices <- stock_prices %>%
      mutate(stock_price_normalized = (stock_prices[, paste0(ticker, ".Adjusted")] - min(stock_prices[, paste0(ticker, ".Adjusted")])) / (max(stock_prices[, paste0(ticker, ".Adjusted")]) - min(stock_prices[, paste0(ticker, ".Adjusted")])))
    
    # Merge data
    merged_data <- sentiment_reddit %>%
      select(Date = date_utc, sentiment_score_normalized) %>%
      left_join(stock_prices %>% select(Date, stock_price_normalized), by = "Date")
    
    # Calculate correlation
    correlation <- cor(merged_data$sentiment_score_normalized, merged_data$stock_price_normalized, use = "complete.obs")
    
    # Update output
    output$correlationText <- renderText({
      paste("Correlation between sentiment score and stock price:", round(correlation, 2))
    })
    
    output$sentimentPlot <- renderPlot({
      ggplot(merged_data, aes(x = Date)) +
        geom_line(aes(y = sentiment_score_normalized, color = "Sentiment Score")) +
        labs(title = paste("Normalized Sentiment Score over Time for", ticker), x = "Date", y = "Normalized Score", color = "Legend") +
        theme_minimal()
    })
    
    output$stockPlot <- renderPlot({
      ggplot(merged_data, aes(x = Date)) +
        geom_line(aes(y = stock_price_normalized, color = "Stock Price")) +
        labs(title = paste("Normalized Stock Price over Time for", ticker), x = "Date", y = "Normalized Price", color = "Legend") +
        theme_minimal()
    })
    
    output$sentimentTable <- renderTable({
      merged_data %>%
        select(Date, sentiment_score_normalized, stock_price_normalized)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
