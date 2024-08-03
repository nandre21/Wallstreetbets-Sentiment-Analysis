# Install necessary packages
required_packages <- c("shiny", "shinydashboard", "plotly", "DT", "dplyr", "tidytext", "quantmod", "rvest", "stringr", "zoo", "RedditExtractoR", "shinythemes", "tidyr")

# Install packages if they are not already installed
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(tidytext)
library(quantmod)
library(rvest)
library(stringr)
library(zoo)
library(RedditExtractoR)
library(shinythemes)
library(tidyr)

# Function to get financial news from FinViz
getFinNews <- function(ticker) {
  Sys.sleep(5)
  url <- paste0("https://finviz.com/quote.ashx?t=", ticker)
  
  data <- tryCatch({
    read_html(url) %>%
      html_nodes(xpath = "//*[@id='news-table']") %>%
      html_table()
  }, error = function(e) {
    print(paste("Error fetching FinViz data:", e$message))
    return(data.frame())
  })
  
  if (length(data) == 0) {
    print("No news data found.")
    return(data.frame())
  }
  
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
  
  # Debugging: Check the structure of the fetched news data
  print("FinNews Data:")
  print(head(tmp))
  
  tmp
}

# Function to get Reddit data
getRedditData <- function(ticker, start_date, end_date) {
  links <- tryCatch({
    find_thread_urls(keywords = ticker, subreddit = "wallstreetbets", sort_by = "new", period = "week")
  }, error = function(e) {
    print(paste("Error finding Reddit threads:", e$message))
    return(data.frame())
  })
  
  if (nrow(links) == 0) {
    print("No Reddit threads found.")
    return(data.frame())
  }
  
  # Check number of links and adjust sample size
  sample_size <- min(20, nrow(links))
  
  set.seed(123)
  sampled_links <- links[sample(nrow(links), size = sample_size), ]
  
  content_list <- lapply(sampled_links$url, function(url) {
    tryCatch({
      get_thread_content(url)
    }, error = function(e) {
      print(paste("Error fetching Reddit thread content:", e$message))
      return(NULL)
    })
  })
  
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
  
  all_comments <- do.call(rbind, lapply(extracted_data_list, function(x) x$comments_df))
  
  # Debugging: Check the structure and content of the Reddit comments data
  if (nrow(all_comments) == 0) {
    print("No Reddit comments data available.")
  } else {
    print("Reddit Comments Data:")
    print(str(all_comments)) # Print the structure of the data
    print(head(all_comments)) # Print the first few rows of the data
  }
  
  all_comments <- all_comments %>%
    filter(date_utc >= as.Date(start_date) & date_utc <= as.Date(end_date))
  
  # Debugging: Check if Reddit comments data is available after filtering
  if (nrow(all_comments) == 0) {
    print("No Reddit comments data after filtering.")
  }
  
  all_comments
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Stock Sentiment Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Settings", tabName = "settings", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            title = "Controls",
            status = "primary",
            solidHeader = TRUE,
            textInput("ticker", "Enter Stock Ticker", value = "AAPL"),
            dateRangeInput("dates", "Select Date Range", start = Sys.Date() - 365, end = Sys.Date()),
            actionButton("submit", "Submit")
          )
        ),
        fluidRow(
          valueBoxOutput("correlationBox")
        ),
        fluidRow(
          box(title = "Reddit Sentiment", status = "primary", solidHeader = TRUE, plotlyOutput("redditSentimentPlot"), width = 12),
          box(title = "Financial News Sentiment", status = "primary", solidHeader = TRUE, plotlyOutput("finnewsSentimentPlot"), width = 12),
          box(title = "Stock Price", status = "primary", solidHeader = TRUE, plotlyOutput("stockPricePlot"), width = 12)
        ),
        fluidRow(
          box(title = "Data Table", status = "primary", solidHeader = TRUE, dataTableOutput("sentimentTable"), width = 12)
        )
      ),
      tabItem(
        tabName = "settings",
        h2("Settings")
        # Add settings options here
      )
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
    stock_data <- tryCatch({
      getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    }, error = function(e) {
      showNotification("Error fetching stock data. Please check the ticker symbol.", type = "error")
      return(NULL)
    })
    
    if (is.null(stock_data)) {
      return()
    }
    
    stock_prices <- data.frame(Date = index(stock_data), coredata(stock_data))
    stock_prices$Date <- as.Date(stock_prices$Date)
    
    # Debugging: Check stock data
    print("Stock Prices Data:")
    print(head(stock_prices))
    
    # Fetch news data
    news_data <- getFinNews(ticker)
    news_data <- news_data %>%
      mutate(Date = as.Date(Date)) %>%
      filter(Date >= as.Date(start_date) & Date <= as.Date(end_date))
    
    # Debugging: Check news data
    print("News Data:")
    print(head(news_data))
    
    # Fetch Reddit data
    reddit_data <- getRedditData(ticker, start_date, end_date)
    
    if (nrow(reddit_data) == 0) {
      output$correlationBox <- renderValueBox({
        valueBox("No Data", "No Reddit data available", icon = icon("exclamation-triangle"), color = "yellow")
      })
      output$redditSentimentPlot <- renderPlotly(NULL)
      output$finnewsSentimentPlot <- renderPlotly(NULL)
      output$stockPricePlot <- renderPlotly(NULL)
      output$sentimentTable <- renderDataTable(data.frame())
      return()
    }
    
    # Preprocess Reddit comments
    reddit_clean <- reddit_data %>%
      unnest_tokens(word, comment) %>%
      mutate(word = tolower(word)) %>%
      anti_join(stop_words)
    
    # Debugging: Check cleaned Reddit data
    print("Cleaned Reddit Data:")
    print(head(reddit_clean))
    
    sentiment_reddit <- reddit_clean %>%
      inner_join(get_sentiments("bing")) %>%
      count(date_utc, sentiment) %>%
      pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
      mutate(sentiment_score = positive - negative)
    
    # Debugging: Check sentiment analysis results
    print("Sentiment Reddit Data:")
    print(head(sentiment_reddit))
    
    # Fetch and preprocess news data
    news_data_clean <- news_data %>%
      unnest_tokens(word, News) %>%
      mutate(word = tolower(word)) %>%
      anti_join(stop_words)
    
    sentiment_news <- news_data_clean %>%
      inner_join(get_sentiments("bing")) %>%
      count(Date, sentiment) %>%
      pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
      mutate(sentiment_score = positive - negative)
    
    # Normalize and calculate correlation
    stock_prices$stock_price_normalized <- scale(stock_prices[, paste0(ticker, ".Adjusted")])
    sentiment_reddit$sentiment_score_normalized <- scale(sentiment_reddit$sentiment_score)
    
    combined_data <- merge(stock_prices, sentiment_reddit, by.x = "Date", by.y = "date_utc", all = TRUE)
    
    # Debugging: Check combined data for correlation
    print("Combined Data:")
    print(head(combined_data))
    
    correlation <- if (nrow(combined_data) > 0) {
      cor(combined_data$sentiment_score_normalized, combined_data$stock_price_normalized, use = "complete.obs")
    } else {
      NA
    }
    
    output$correlationBox <- renderValueBox({
      if (is.na(correlation)) {
        valueBox("No Data", "Not enough data for correlation", icon = icon("exclamation-triangle"), color = "yellow")
      } else {
        valueBox(round(correlation, 2), "Correlation", icon = icon("chart-line"), color = "blue")
      }
    })
    
    output$redditSentimentPlot <- renderPlotly({
      if (nrow(sentiment_reddit) > 0) {
        plot_ly(sentiment_reddit, x = ~date_utc, y = ~sentiment_score, type = 'scatter', mode = 'lines', line = list(color = "blue")) %>%
          layout(title = "Reddit Sentiment Over Time", xaxis = list(title = "Date"), yaxis = list(title = "Sentiment Score"))
      } else {
        plot_ly() %>%
          layout(title = "No Reddit Sentiment Data", xaxis = list(title = "Date"), yaxis = list(title = "Sentiment Score"))
      }
    })
    
    output$finnewsSentimentPlot <- renderPlotly({
      if (nrow(sentiment_news) > 0) {
        plot_ly(sentiment_news, x = ~Date, y = ~sentiment_score, type = 'scatter', mode = 'lines', line = list(color = "green")) %>%
          layout(title = "Financial News Sentiment Over Time", xaxis = list(title = "Date"), yaxis = list(title = "Sentiment Score"))
      } else {
        plot_ly() %>%
          layout(title = "No Financial News Sentiment Data", xaxis = list(title = "Date"), yaxis = list(title = "Sentiment Score"))
      }
    })
    
    output$stockPricePlot <- renderPlotly({
      plot_ly(stock_prices, x = ~Date, y = ~get(paste0(ticker, ".Adjusted")), type = 'scatter', mode = 'lines', line = list(color = "red")) %>%
        layout(title = "Stock Price Over Time", xaxis = list(title = "Date"), yaxis = list(title = "Adjusted Close Price"))
    })
    
    output$sentimentTable <- renderDataTable({
      sentiment_reddit %>%
        select(Date = date_utc, sentiment_score) %>%
        mutate(Source = "Reddit") %>%
        bind_rows(
          sentiment_news %>%
            select(Date, sentiment_score) %>%
            mutate(Source = "News")
        ) %>%
        arrange(Date)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
