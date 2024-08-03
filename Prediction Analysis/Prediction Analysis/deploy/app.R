# Load necessary libraries
library(shiny)
library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RedditExtractoR)
library(syuzhet)
library(stringr)

# Define UI
ui <- fluidPage(
  theme = bslib::bs_theme(
    bg = "#121212",
    fg = "#FFFFFF",
    primary = "#FF00FF",
    base_font = bslib::font_google("Roboto Mono")
  ),
  
  titlePanel("Wallstreetbets Stock Pulse Tracker"),
  
  tabsetPanel(
    tabPanel("Dashboard",
             fluidRow(
               column(12,
                      h2("Real-time Stock Prices & Wallstreetbets Stock-Specific Sentiment Analysis", style = "color: #FF00FF; text-align: center;"),
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          textInput("ticker", "Enter Stock Ticker", value = "AAPL"),
                          selectInput("timeframe", "Select Timeframe", choices = c("Daily" = "day", "Weekly" = "week")),
                          actionButton("submit", "Get Stock Prices & Analyze Sentiment"),
                          helpText("Note: Due to API limits, data is restricted to daily and weekly intervals to prevent blocking.")
                        ),
                        mainPanel(
                          width = 9,
                          textOutput("priceText"),
                          plotOutput("stockPlot"),
                          tableOutput("priceTable"),
                          hr(style = "border-top: 1px solid #FFFFFF;"),
                          textOutput("sentimentText"),
                          plotOutput("sentimentPlot"),
                          div(
                            id = "sentimentResultsBox",
                            style = "background-color: #1E1E1E; border: 1px solid #FF00FF; padding: 10px; max-height: 400px; overflow-y: scroll; margin-top: 10px;",
                            tableOutput("sentimentResults")
                          )
                        )
                      )
               )
             ),
             hr(style = "border-top: 2px solid #FF00FF;"),
             fluidRow(
               column(6,
                      h2("Overall Wallstreetbets Sentiment Analysis", style = "color: #FF00FF; text-align: center;"),
                      h3("Top Trending Tickers", style = "color: #FF00FF;"),
                      tableOutput("trendingStocksTable"),
                      plotOutput("trendingStocksPlot")
               ),
               column(6,
                      h2("Overall Wallstreetbets Sentiment Score", style = "color: #FF00FF; text-align: center;"),
                      tableOutput("overallSentimentTable")
               )
             )
    ),
    
    tabPanel("Guide",
             h3("User Guide"),
             p("Hi! Welcome to the dashboard! This dashboard provides real-time stock price tracking and sentiment analysis based on Reddit comments from the hilarious but sometimes also formidable r/wallstreetbets. The application consists of the first tab containing all this detailed information. This is version 1.4 and as our resources and skills develop we will also be periodically updating this. The following is a short overview of this project."),
             
             h4("Functionality"),
             h5("1. Stock Prices and Sentiment Analysis"),
             p("The first tab is dedicated to displaying real-time stock prices and sentiment analysis. Users can enter a stock ticker symbol and select a time frame (daily or weekly) to fetch and display stock price data. The application uses the `quantmod` package to retrieve stock data from Yahoo Finance. The stock prices are plotted over time, and a table is provided showing detailed price information."),
             p("In addition, the application performs sentiment analysis on Reddit comments related to the specified stock ticker. It uses the `RedditExtractoR` package to scrape recent Reddit threads from the `wallstreetbets` subreddit and the `syuzhet` package to analyze sentiment. Sentiment scores are plotted over time, showing how sentiment trends correlate with stock price movements. The sentiment analysis plot and a scrollable data box displaying the comment details are included for in-depth analysis."),
             
             h5("2. Overall Sentiment Analysis and Trending Stocks"),
             p("Below the interactive sections, the dashboard displays static analyses: trending stocks on Reddit and overall sentiment analysis. The application identifies the top trending tickers by extracting frequent terms from Reddit comments and filters out common non-stock words. A bar graph visualizes these trending stocks, and a table provides detailed counts."),
             p("The overall sentiment analysis summarizes the positive and negative sentiment scores based on Reddit comments from the past day. This includes a cumulative sentiment score indicating whether the overall sentiment is positive or negative. Both the trending stocks and sentiment scores are updated whenever the app is launched, providing users with a snapshot of current trends and sentiment."),
             
             h4("Data Sources"),
             p("- Stock Data: Retrieved from Yahoo Finance using the `quantmod` package."),
             p("- Reddit Data: Extracted from the `wallstreetbets` subreddit using the `RedditExtractoR` package. Sentiment analysis is performed using the `syuzhet` package."),
             
             h4("Limitations"),
             p("1. API Limitations: The application relies on free APIs with usage limits. To avoid exceeding these limits, the data fetching frequency is constrained to daily or weekly intervals. (If you would like to sponsor a paid API so we can do much more, let us know!)"),
             p("2. Data Accuracy: The accuracy of sentiment analysis depends on the quality and relevance of the Reddit comments. Comments may not always be directly related to the stock or may contain noise that affects sentiment scoring. (For example you may notice some comments are just random bots and spams. We are still figuring out how to filter this out from our sentiment scoring)."),
             p("3. Ticker Symbols: At times the application has issues with identifying and analysing certain stock tickers, we recommend sticking to the big ones or the trending ones."),
             p("4. Sentiment Analysis Variability: Sentiment analysis tools may not perfectly capture the nuances of user sentiment, and results may vary depending on the context of comments.")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store the selected ticker
  rv <- reactiveValues(ticker = NULL)
  
  # Real-time stock price fetching and stock-specific sentiment analysis
  observeEvent(input$submit, {
    rv$ticker <- toupper(input$ticker)
    timeframe <- input$timeframe
    start_date <- Sys.Date() - ifelse(timeframe == "day", 1, 7)
    end_date <- Sys.Date()
    
    # Fetch stock data
    stock_data <- tryCatch({
      getSymbols(rv$ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(stock_data)) {
      output$priceText <- renderText("Error fetching stock data. Please check the ticker and try again.")
      output$stockPlot <- renderPlot(NULL)
      output$priceTable <- renderTable(data.frame())
      output$sentimentText <- renderText("")
      output$sentimentPlot <- renderPlot(NULL)
      output$sentimentResults <- renderTable(data.frame())
      return()
    }
    
    # Stock prices
    stock_prices <- data.frame(Date = index(stock_data), coredata(stock_data))
    stock_prices$Date <- as.Date(stock_prices$Date)
    latest_price <- stock_prices[nrow(stock_prices), paste0(rv$ticker, ".Adjusted")]
    
    # Update stock prices outputs
    output$priceText <- renderText({
      paste("The latest price for", rv$ticker, "is", round(latest_price, 2))
    })
    
    output$stockPlot <- renderPlot({
      ggplot(stock_prices, aes(x = Date, y = stock_prices[, paste0(rv$ticker, ".Adjusted")])) +
        geom_line(color = "#FF00FF") +
        labs(title = paste("Stock Prices for", rv$ticker), x = "Date", y = "Adjusted Price") +
        theme_minimal() +
        theme(text = element_text(color = "#FFFFFF"), plot.title = element_text(color = "#FF00FF"))
    })
    
    output$priceTable <- renderTable({
      stock_prices %>%
        mutate(Date = as.Date(Date))
    })
    
    # Sentiment analysis
    fetch_reddit_comments <- function(ticker, period = "day", sort_by = "new", max_pages = 5) {
      tryCatch({
        reddit_urls <- find_thread_urls(keywords = ticker, subreddit = "wallstreetbets", sort_by = sort_by, period = period)
        reddit_urls <- reddit_urls[1:min(max_pages, nrow(reddit_urls)), ]
        reddit_comments <- get_thread_content(reddit_urls$url)
        return(reddit_comments$comments)
      }, error = function(e) {
        message("Error fetching or processing Reddit data: ", e$message)
        return(NULL)
      })
    }
    
    perform_sentiment_analysis <- function(comments) {
      comment_texts <- comments$comment
      sentiment_scores <- get_nrc_sentiment(comment_texts)
      comments_with_sentiment <- cbind(comments, sentiment_scores)
      return(comments_with_sentiment)
    }
    
    plot_sentiment_over_time <- function(sentiment_data) {
      sentiment_data$date <- as.Date(as.POSIXct(sentiment_data$timestamp, origin="1970-01-01", tz="UTC"))
      sentiment_summary <- sentiment_data %>%
        group_by(date) %>%
        summarize(positive = mean(positive - negative), .groups = 'drop')
      sentiment_plot <- ggplot(sentiment_summary, aes(x = date, y = positive)) +
        geom_line(color = "#FF00FF") +
        geom_point(color = "#FF00FF") +
        labs(title = paste("Sentiment Analysis of Reddit Comments Over Time for", rv$ticker),
             x = "Date",
             y = "Average Sentiment Score (Positive - Negative)") +
        theme_minimal() +
        theme(text = element_text(color = "#FFFFFF"), plot.title = element_text(color = "#FF00FF"))
      return(sentiment_plot)
    }
    
    reddit_comments <- fetch_reddit_comments(ticker = rv$ticker, period = timeframe, sort_by = "new", max_pages = 5)
    
    if (!is.null(reddit_comments)) {
      # Perform sentiment analysis
      reddit_comments_with_sentiment <- perform_sentiment_analysis(reddit_comments)
      
      # Update sentiment analysis outputs
      output$sentimentText <- renderText({
        paste("Sentiment analysis for", rv$ticker, "based on Reddit comments:")
      })
      
      output$sentimentPlot <- renderPlot({
        plot_sentiment_over_time(reddit_comments_with_sentiment)
      })
      
      output$sentimentResults <- renderTable({
        reddit_comments_with_sentiment
      })
    } else {
      output$sentimentText <- renderText("No comments data available for sentiment analysis.")
      output$sentimentPlot <- renderPlot(NULL)
      output$sentimentResults <- renderTable(data.frame())
    }
  })
  
  # Overall sentiment analysis and trending stocks
  get_frequent_terms <- function(comments, top_n = 10) {
    ticker_pattern <- "\\b[A-Z]{3,5}\\b"  # Restrict to 3 to 5 letter stock tickers
    all_tickers <- str_extract_all(comments, ticker_pattern)
    all_tickers <- unlist(all_tickers)
    ticker_counts <- data.frame(ticker = all_tickers, stringsAsFactors = FALSE) %>%
      group_by(ticker) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    common_words <- c(
      "AND", "FOR", "THE", "OF", "TO", "IN", "WITH", "ON", "AS", "AT", "BY", "AN",
      "IS", "OR", "IT", "FROM", "THIS", "THAT", "BUT", "NOT", "YOU", "ALL",
      "YOUR", "WHAT", "CAN", "OUT", "GET", "NOW", "THEIR", "MORE", "UP",
      "THAN", "THEY", "SOME", "ONE", "LIKE", "WHEN", "ITS", "ONLY", "INTO",
      "OVER", "VERY", "BEEN", "HAS", "HAD", "HER", "HIM", "ARE", "WAS", "HAS"
    )
    ticker_counts <- ticker_counts[!(ticker_counts$ticker %in% common_words), ]
    
    return(ticker_counts[1:min(top_n, nrow(ticker_counts)), ])
  }
  
  fetch_trending_stocks <- function(subreddit, period, sort_by, top_n) {
    tryCatch({
      reddit_urls <- find_thread_urls(subreddit = subreddit, sort_by = sort_by, period = period)
      reddit_urls <- reddit_urls[1:min(5, nrow(reddit_urls)), ]
      all_comments <- get_thread_content(reddit_urls$url)$comments$comment
      all_comments <- unlist(all_comments)
      top_trending <- get_frequent_terms(all_comments, top_n)
      return(top_trending)
    }, error = function(e) {
      message("Error fetching or processing Reddit data: ", e$message)
      return(data.frame(ticker = character(0), count = integer(0)))
    })
  }
  
  observe({
    trending_stocks <- fetch_trending_stocks(subreddit = "wallstreetbets", period = "day", sort_by = "new", top_n = 10)
    
    output$trendingStocksPlot <- renderPlot({
      if (nrow(trending_stocks) > 0) {
        ggplot(trending_stocks, aes(x = reorder(ticker, -count), y = count, fill = count)) +
          geom_bar(stat = "identity", fill = "#FF00FF") +
          coord_flip() +
          labs(title = "Top Trending Tickers on Reddit", x = "Ticker", y = "Mention Count") +
          theme_minimal() +
          theme(text = element_text(color = "#FFFFFF"), plot.title = element_text(color = "#FF00FF"), legend.position = "none")
      } else {
        ggplot() + 
          theme_void() +
          annotate("text", x = 0, y = 0, label = "No trending stocks data available.", size = 6, color = "#FF00FF")
      }
    })
    
    output$trendingStocksTable <- renderTable({
      trending_stocks
    })
  })
  
  calculate_sentiment_breakdown <- function(sentiment_data) {
    total_positive <- sum(sentiment_data$positive, na.rm = TRUE)
    total_negative <- sum(sentiment_data$negative, na.rm = TRUE)
    cumulative_sentiment <- total_positive - total_negative
    overall_sentiment <- ifelse(cumulative_sentiment > 0, "Positive", "Negative")
    return(list(
      total_positive = total_positive,
      total_negative = total_negative,
      cumulative_score = cumulative_sentiment,
      overall_sentiment = overall_sentiment
    ))
  }
  
  observe({
    reddit_comments <- fetch_reddit_comments(subreddit = "wallstreetbets", period = "day", sort_by = "new", max_pages = 5)
    
    if (!is.null(reddit_comments)) {
      reddit_comments_with_sentiment <- perform_sentiment_analysis(reddit_comments)
      sentiment_breakdown <- calculate_sentiment_breakdown(reddit_comments_with_sentiment)
      
      output$overallSentimentTable <- renderTable({
        sentiment_breakdown_df <- data.frame(
          Metric = c("Total Positive Sentiment", "Total Negative Sentiment", "Sentiment Score", "Overall Sentiment"),
          Value = c(
            sentiment_breakdown$total_positive,
            sentiment_breakdown$total_negative,
            sentiment_breakdown$cumulative_score,
            sentiment_breakdown$overall_sentiment
          ),
          stringsAsFactors = FALSE
        )
        sentiment_breakdown_df
      })
    } else {
      output$overallSentimentTable <- renderTable(data.frame())
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
