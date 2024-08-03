# Load necessary libraries
library(shiny)
library(quantmod)

# Define UI
ui <- fluidPage(
  titlePanel("Real-time Stock Price Fetcher"),
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
    
    # Update output
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
}

# Run the application
shinyApp(ui = ui, server = server)
