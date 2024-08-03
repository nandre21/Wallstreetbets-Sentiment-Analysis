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
  titlePanel("Reddit and Stock Market Dashboard"),
  
  # Tabs for different functionalities
  tabsetPanel(
    tabPanel("Real-time Stock Prices",
             sidebarLayout(
               sidebarPanel(
                 textInput("ticker", "Enter Stock Ticker", value = "AAPL"),
                 dateRangeInput("dates", "Select Date Range", start = Sys.Date() - 30, end = Sys.Date()),
                 actionButton("submit", "Get Stock Prices")
               ),
               mainPanel(
                 textOutput("priceText"),
                 plotOutput("stockPlot"),
                 tableOutput("priceTable")
               )
             )
    ),
    
    tabPanel("Sentiment Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("sentimentPeriod", "Select Period", choices = c("day", "week", "month"), selected = "day"),
                 actionButton("analyzeSentiment", "Analyze Sentiment")
               ),
               mainPanel(
                 plotOutput("sentimentPlot"),
                 tableOutput("sentimentResults"),
                 plotOutput("trendingStocksPlot"),
                 tableOutput("trendingStocksTable"),
                 textOutput("sentimentText")
               )
             )
    ),
    
    tabPanel("Guide",
             mainPanel(
               h3("Guide and Explanations"),
               p("This dashboard provides insights into Reddit sentiment and trending stocks."),
               p("1. **Real-time Stock Prices Tab**: Fetch and view real-time stock prices over a specified date range. Enter a stock ticker and date range, then click 'Get Stock Prices' to display the data."),
               p("2. **Sentiment Analysis Tab**: Analyze Reddit comments for sentiment scores and trending stocks. Select a period (day, week, or month) and click 'Analyze Sentiment' to generate the plots and results."),
               p("3. **Guide Tab**: This tab provides an overview of the dashboard functionalities and guidance on how to use them.")
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Real-time stock price fetching and plotting
  observeEvent(input$submit, {
    ticker <- toupper(input$ticker)
    start_date <- input$dates[1]
    end_date <- input$dates[2]
    
    stock_data <- tryCatch({
      getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(stock_data)) {
      output$priceText <- renderText("Error fetching stock data. Please check the ticker and try again.")
      output$stockPlot <- renderPlot(NULL)
      output$priceTable <- renderTable(data.frame())
      return()
    }
    
    stock_prices <- data.frame(Date = index(stock_data), coredata(stock_data))
    stock_prices$Date <- as.Date(stock_prices$Date)
    
    latest_price <- stock_prices[nrow(stock_prices), paste0(ticker, ".Adjusted")]
    
    output$priceText <- renderText({
      paste("The latest price for", ticker, "is", round(latest_price, 2))
    })
    
    output$stockPlot <- renderPlot({
      ggplot(stock_prices, aes(x = Date, y = stock_prices[, paste0(ticker, ".Adjusted")])) +
        geom_line(color = "blue") +
        labs(title = paste("Stock Prices for", ticker), x = "Date", y = "Adjusted Price") +
        theme_minimal()
    })
    
    output$priceTable <- renderTable({
      stock_prices
    })
  })
  
  # Sentiment Analysis and Trending Stocks
  observeEvent(input$analyzeSentiment, {
    sentiment_period <- input$sentimentPeriod
    
    # Fetch Reddit comments
    fetch_reddit_comments <- function(subreddit, period = "day", sort_by = "new", max_pages = 5) {
      tryCatch({
        reddit_urls <- find_thread_urls(subreddit = subreddit, sort_by = sort_by, period = period)
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
    
    # Fetch comments
    reddit_comments <- fetch_reddit_comments(subreddit = "wallstreetbets", period = sentiment_period, sort_by = "new", max_pages = 5)
    
    if (!is.null(reddit_comments)) {
      # Perform sentiment analysis
      reddit_comments_with_sentiment <- perform_sentiment_analysis(reddit_comments)
      sentiment_breakdown <- calculate_sentiment_breakdown(reddit_comments_with_sentiment)
      
      # Display sentiment results
      output$sentimentText <- renderText({
        paste("Overall Sentiment: ", sentiment_breakdown$overall_sentiment, "\n",
              "Cumulative Sentiment Score: ", sentiment_breakdown$cumulative_score)
      })
      
      output$sentimentResults <- renderTable({
        data.frame(
          Metric = c("Total Positive Sentiment", "Total Negative Sentiment", "Sentiment Score", "Overall Sentiment"),
          Value = c(
            sentiment_breakdown$total_positive,
            sentiment_breakdown$total_negative,
            sentiment_breakdown$cumulative_score,
            sentiment_breakdown$overall_sentiment
          ),
          stringsAsFactors = FALSE
        )
      })
      
      # Plot sentiment over time
      plot_sentiment_over_time <- function(sentiment_data) {
        sentiment_data$date <- as.Date(as.POSIXct(sentiment_data$timestamp, origin="1970-01-01", tz="UTC"))
        sentiment_summary <- sentiment_data %>%
          group_by(date) %>%
          summarize(positive = mean(positive - negative), .groups = 'drop')
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
      output$sentimentPlot <- renderPlot(sentiment_plot)
      
      # Fetch and plot trending stocks
      get_frequent_terms <- function(comments, top_n = 10) {
        ticker_pattern <- "\\b[A-Z]{1,4}\\b"
        all_tickers <- str_extract_all(comments, ticker_pattern)
        all_tickers <- unlist(all_tickers)
        ticker_counts <- data.frame(ticker = all_tickers, stringsAsFactors = FALSE) %>%
          group_by(ticker) %>%
          summarize(count = n(), .groups = 'drop') %>%
          arrange(desc(count))
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
        top_trending <- ticker_counts %>%
          filter(ticker %in% stockSymbols()$Symbol) %>%
          head(top_n)
        return(top_trending)
      }
      
      get_trending_stocks_dynamic <- function(subreddit, period = "day", sort_by = "new", top_n = 10, max_threads = 10) {
        tryCatch({
          reddit_urls <- find_thread_urls(subreddit = subreddit, sort_by = sort_by, period = period)
          all_comments <- list()
          for (url in reddit_urls$url[1:max_threads]) {
            reddit_comments <- safe_api_request(url)
            all_comments <- c(all_comments, reddit_comments$comments$comment)
            Sys.sleep(1)
          }
          comments <- unlist(all_comments)
          top_trending <- get_frequent_terms(comments, top_n)
          return(top_trending)
        }, error = function(e) {
          message("Error fetching or processing Reddit data: ", e$message)
          return(data.frame(ticker = character(0), count = integer(0)))
        })
      }
      
      plot_trending_stocks <- function(trending_stocks) {
        ggplot(trending_stocks, aes(x = reorder(ticker, -count), y = count)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          theme_minimal() +
          labs(title = "Top Trending Stocks on r/wallstreetbets",
               x = "Stock Ticker",
               y = "Mention Count") +
          coord_flip()
      }
      
      trending_stocks <- get_trending_stocks_dynamic(subreddit = "wallstreetbets", period = sentiment_period, sort_by = "new", top_n = 10)
      
      output$trendingStocksPlot <- renderPlot({
        if (nrow(trending_stocks) > 0) {
          plot_trending_stocks(trending_stocks)
        } else {
          ggplot() + 
            theme_void() +
            annotate("text", x = 0, y = 0, label = "No trending stocks data available.", size = 6)
        }
      })
      
      output$trendingStocksTable <- renderTable({
        trending_stocks
      })
      
    } else {
      output$sentimentText <- renderText("No comments data available for sentiment analysis.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
