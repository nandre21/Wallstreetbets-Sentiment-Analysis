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
  
  titlePanel("Reddit and Stock Market Dashboard"),
  
  tabsetPanel(
    tabPanel("Dashboard",
             fluidRow(
               column(12,
                      h2("Real-time Stock Prices & Stock-Specific Sentiment Analysis", style = "color: #FF00FF;"),
                      sidebarLayout(
                        sidebarPanel(
                          textInput("ticker", "Enter Stock Ticker", value = "AAPL"),
                          selectInput("timeframe", "Select Timeframe", choices = c("Daily" = "day", "Weekly" = "week")),
                          actionButton("submit", "Get Stock Prices & Analyze Sentiment"),
                          helpText("Note: Due to API limits, data is restricted to daily and weekly intervals to prevent blocking.")
                        ),
                        mainPanel(
                          textOutput("priceText"),
                          plotOutput("stockPlot"),
                          tableOutput("priceTable"),
                          hr(style = "border-top: 1px solid #FFFFFF;"),
                          textOutput("sentimentText"),
                          plotOutput("sentimentPlot"),
                          tableOutput("sentimentResults")
                        )
                      ),
                      hr(style = "border-top: 2px solid #FF00FF;"),
                      h2("Overall Sentiment Analysis & Trending Stocks", style = "color: #FF00FF;"),
                      fluidRow(
                        column(6,
                               h3("Top Trending Tickers", style = "color: #FF00FF;"),
                               tableOutput("trendingStocksTable"),
                               plotOutput("trendingStocksPlot")
                        ),
                        column(6,
                               h3("Overall Sentiment Score", style = "color: #FF00FF;"),
                               tableOutput("overallSentimentTable")
                        )
                      )
               )
             )
    ),
    
    tabPanel("Guide",
             mainPanel(
               h3("Guide and Explanations", style = "color: #FF00FF;"),
               p("This section will contain detailed instructions on how to use the dashboard and interpret the results.", style = "color: #FFFFFF;")
             )
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
        mutate(Date = as.Date(Date, origin = "1970-01-01"))
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
      "OVER", "VERY", "BEEN", "MUCH", "SO", "IF", "NO", "WHO", "THEN", "TOO"
    )
    ticker_counts <- ticker_counts %>%
      filter(!ticker %in% common_words) %>%
      head(top_n)
    return(ticker_counts)
  }
  
  get_trending_stocks_dynamic <- function(subreddit, period = "day", sort_by = "new", top_n = 10) {
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
  
  reddit_comments <- fetch_reddit_comments(subreddit = "wallstreetbets", period = "day", sort_by = "new", max_pages = 5)
  
  if (!is.null(reddit_comments)) {
    perform_sentiment_analysis <- function(comments) {
      comment_texts <- comments$comment
      sentiment_scores <- get_nrc_sentiment(comment_texts)
      comments_with_sentiment <- cbind(comments, sentiment_scores)
      return(comments_with_sentiment)
    }
    
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
    
    trending_stocks <- get_trending_stocks_dynamic(subreddit = "wallstreetbets", period = "day", sort_by = "new", top_n = 10)
    
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
  } else {
    output$overallSentimentTable <- renderTable(data.frame())
    output$trendingStocksPlot <- renderPlot(NULL)
    output$trendingStocksTable <- renderTable(data.frame())
  }
}

# Run the application
shinyApp(ui = ui, server = server)
