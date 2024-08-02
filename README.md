# Stock Sentiment Analysis Dashboard

https://2o1out-neil-luetz.shinyapps.io/deploy/

## Overview

This Shiny app provides a comprehensive analysis of stock sentiments based on Reddit discussions and news articles. The dashboard features real-time stock prices, sentiment analysis, and trending stocks on r/wallstreetbets. Additionally, a daily sentiment score and the top trending stocks are updated once daily. You can explore the live application [here](https://2o1out-neil-luetz.shinyapps.io/deploy/).

## Features

### Real-time Stock Prices and Sentiment Analysis

1. **Stock Prices and Sentiment Analysis**
   - Input a stock ticker to fetch its real-time stock prices using the `quantmod` package from Yahoo Finance.
   - Perform sentiment analysis on Reddit comments for a specified stock ticker using the `RedditExtractoR` and `syuzhet` packages.
   - Visualize stock prices and sentiment trends over time.

2. **Overall Sentiment Analysis and Trending Stocks**
   - Displays the top trending stocks on r/wallstreetbets, updated once daily.
   - A gauge chart showing the overall sentiment score for r/wallstreetbets, updated once daily.

### Documentation and Guidance

- An additional tab provides detailed documentation and guidance on how to use the app.

## How It Was Made

### Data Sources

- **Stock Data**: Retrieved from Yahoo Finance using the `quantmod` package.
- **Reddit Data**: Extracted from the `wallstreetbets` subreddit using the `RedditExtractoR` package. Sentiment analysis is performed using the `syuzhet` package.

### Functionality

1. **Stock Prices and Sentiment Analysis**
   - Users can enter a stock ticker symbol and select a time frame (daily or weekly) to fetch and display stock price data.
   - The app performs sentiment analysis on Reddit comments related to the specified stock ticker and plots sentiment scores over time, correlating with stock price movements.

2. **Overall Sentiment Analysis and Trending Stocks**
   - The app identifies the top trending tickers by extracting frequent terms from Reddit comments.
   - A bar graph visualizes these trending stocks, and a table provides detailed counts.
   - The overall sentiment analysis summarizes positive and negative sentiment scores from Reddit comments over the past day.

### Limitations

1. **API Limitations**
   - The application relies on free APIs with usage limits. To avoid exceeding these limits, data fetching is constrained to daily or weekly intervals.

2. **Data Accuracy**
   - The accuracy of sentiment analysis depends on the quality and relevance of Reddit comments. Comments may not always be directly related to the stock or may contain noise that affects sentiment scoring.

3. **Ticker Symbols**
   - At times, the application may have issues identifying and analyzing certain stock tickers. It's recommended to stick to the major or trending ones.

4. **Sentiment Analysis Variability**
   - Sentiment analysis tools may not perfectly capture the nuances of user sentiment, and results may vary depending on the context of comments.

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/stock-sentiment-dashboard.git

2. Open the R project file in RStudio.

3. Install the required packages:
  install.packages(c("shiny", "dplyr", "tidytext", "ggplot2", "tidyr", "quantmod", "rvest", "stringr", "zoo", "RedditExtractoR", "syuzhet", "plotly", "jsonlite"))


## Usage

1. Run the Shiny App:
shiny::runApp()
2. Use the input fields to enter the stock ticker and date range.
3. View the results in the various sections of the dashboard.

## Structure
- app.R: The main Shiny app file that combines all the functionalities.
- README.md: This file providing an overview and instructions.

  
## Acknowledgements
This app uses various R packages to fetch and process data, including:

- quantmod for stock data
- rvest for web scraping news data
- RedditExtractoR for Reddit data
- syuzhet for sentiment analysis
- plotly for interactive visualizations
  
## Contributing
Contributions are welcome! Please create a pull request or open an issue to discuss any changes.

## License
This project is licensed under the MIT License.

## Contact

neilluetz@gmail.com
