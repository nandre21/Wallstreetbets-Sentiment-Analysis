library(shiny)
library(RedditExtractoR)
library(syuzhet)
library(ggplot2)
library(dplyr)
library(shinycssloaders)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Reddit Sentiment Analysis for Stock Tickers"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("ticker", "Enter Stock Ticker:", value = "NVDA"),
      actionButton("analyze", "Analyze"),
      hr(),
      h4("Instructions"),
      p("Enter a stock ticker symbol (e.g., NVDA) and click 'Analyze'. The application will fetch Reddit comments related to the stock from the 'wallstreetbets' subreddit, perform sentiment analysis, and display the sentiment trend over time.")
    ),
    
    mainPanel(
      h4("Fetched Comments"),
      withSpinner(dataTableOutput("comments_table")),
      hr(),
      h4("Sentiment Trend Over Time"),
      withSpinner(plotOutput("sentiment_plot")),
      hr(),
      h4("Messages"),
      verbatimTextOutput("error_message")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
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
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue") +
      theme_minimal() +
      labs(title = "Sentiment Analysis of Reddit Comments Over Time",
           x = "Date",
           y = "Average Sentiment Score (Positive - Negative)")
    return(sentiment_plot)
  }
  
  observeEvent(input$analyze, {
    req(input$ticker)
    
    tryCatch({
      reddit_urls <- find_thread_urls(keywords = input$ticker, subreddit = "wallstreetbets", sort_by = "new", period = "week")
      reddit_comments <- get_thread_content(reddit_urls$url)
      reddit_comments_with_sentiment <- perform_sentiment_analysis(reddit_comments$comments)
      
      output$comments_table <- renderDataTable({
        head(reddit_comments$comments)
      })
      
      output$sentiment_plot <- renderPlot({
        plot_sentiment_over_time(reddit_comments_with_sentiment)
      })
      
      output$error_message <- renderText({
        ""
      })
      
    }, error = function(e) {
      output$error_message <- renderText({
        paste("Error: The API is having a hard time pulling the data for the stock ticker", input$ticker, ". Please try another one.")
      })
      
      output$comments_table <- renderDataTable({
        data.frame()
      })
      
      output$sentiment_plot <- renderPlot({
        NULL
      })
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
