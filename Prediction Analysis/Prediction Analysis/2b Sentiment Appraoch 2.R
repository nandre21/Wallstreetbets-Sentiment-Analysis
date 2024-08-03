# Load necessary libraries
library(quantmod)
library(dplyr)

# Define the ticker symbol and date range
ticker <- "NVDA"  # or any other ticker symbol
start_date <- "2024-01-01"
end_date <- Sys.Date()  # or specify a date

# Get historical stock price data
getSymbols(ticker, src = "yahoo", from = start_date, to = end_date)

# Convert to dataframe
stock_prices <- data.frame(Date = index(NVDA), coredata(NVDA))

print(colnames(stock_prices))


# Ensure date formats are consistent
stock_prices$Date <- as.Date(stock_prices$Date)
sentiment_reddit$Date <- as.Date(sentiment_reddit$Date)  # Adjust column name as needed


# Ensure sentiment_reddit has a 'Date' column and is in the correct format
sentiment_reddit$Date <- as.Date(sentiment_reddit$date_utc)  # Ensure the correct column name

# Merge sentiment scores with stock prices using the correct column name for adjusted prices
merged_data <- sentiment_reddit %>%
  select(Date, sentiment_score) %>%
  left_join(stock_prices %>% select(Date, NVDA.Adjusted), by = "Date")

# Calculate correlation
correlation <- cor(merged_data$sentiment_score, merged_data$NVDA.Adjusted, use = "complete.obs")
print(paste("Correlation between sentiment score and stock price:", correlation))

# Plot the data
library(ggplot2)

ggplot(merged_data, aes(x = Date)) +
  geom_line(aes(y = sentiment_score * 10, color = "Sentiment Score (scaled)")) +  # Scale for better visualization
  geom_line(aes(y = NVDA.Adjusted, color = "Stock Price")) +
  labs(title = "Sentiment Score vs. Stock Price",
       x = "Date",
       y = "Value",
       color = "Legend") +
  theme_minimal()



#Normalization

# Define Min-Max Normalization function
min_max_normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Apply normalization to sentiment scores and stock prices
merged_data <- merged_data %>%
  mutate(
    sentiment_score_normalized = min_max_normalize(sentiment_score),
    NVDA_Adj_normalized = min_max_normalize(NVDA.Adjusted)
  )

# Plot the normalized data
ggplot(merged_data, aes(x = Date)) +
  geom_line(aes(y = sentiment_score_normalized, color = "Sentiment Score")) +
  geom_line(aes(y = NVDA_Adj_normalized, color = "Stock Price")) +
  labs(title = "Normalized Sentiment Score vs. Stock Price",
       x = "Date",
       y = "Normalized Value",
       color = "Legend") +
  theme_minimal()


