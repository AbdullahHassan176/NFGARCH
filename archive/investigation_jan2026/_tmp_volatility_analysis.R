#!/usr/bin/env Rscript
# Analyze return volatility characteristics

library(xts)

cat("=== RETURN VOLATILITY ANALYSIS ===\n\n")

# Load data
raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1)
raw_price_data$Date <- as.Date(rownames(raw_price_data))
rownames(raw_price_data) <- NULL

# Assets
equity_tickers <- c("NVDA", "MSFT", "AMZN")
fx_names <- c("EURUSD", "GBPUSD", "USDZAR")
all_assets <- c(equity_tickers, fx_names)

# Calculate returns and volatility stats
results <- data.frame(
  Asset = character(),
  Asset_Class = character(),
  Mean_Return = numeric(),
  SD_Return = numeric(),
  Min_Return = numeric(),
  Max_Return = numeric(),
  Abs_Mean = numeric(),
  stringsAsFactors = FALSE
)

for (asset in all_assets) {
  if (asset %in% names(raw_price_data)) {
    prices <- raw_price_data[[asset]]
    returns <- diff(log(prices))[-1]
    
    asset_class <- ifelse(asset %in% equity_tickers, "Equity", "FX")
    
    results <- rbind(results, data.frame(
      Asset = asset,
      Asset_Class = asset_class,
      Mean_Return = mean(returns, na.rm = TRUE),
      SD_Return = sd(returns, na.rm = TRUE),
      Min_Return = min(returns, na.rm = TRUE),
      Max_Return = max(returns, na.rm = TRUE),
      Abs_Mean = mean(abs(returns), na.rm = TRUE)
    ))
  }
}

cat("=== VOLATILITY STATISTICS ===\n")
print(results, row.names = FALSE)

cat("\n=== COMPARISON ===\n")
cat("\nEquity Volatility (SD):\n")
print(results[results$Asset_Class == "Equity", c("Asset", "SD_Return")])

cat("\nFX Volatility (SD):\n")
print(results[results$Asset_Class == "FX", c("Asset", "SD_Return")])

cat("\n=== RATIO ANALYSIS ===\n")
eq_mean_vol <- mean(results[results$Asset_Class == "Equity", "SD_Return"])
fx_mean_vol <- mean(results[results$Asset_Class == "FX", "SD_Return"])
cat("Equity mean volatility:", eq_mean_vol, "\n")
cat("FX mean volatility:", fx_mean_vol, "\n")
cat("Equity/FX ratio:", eq_mean_vol / fx_mean_vol, "x\n")

cat("\n=== EXTREME RETURNS ===\n")
cat("\nLargest absolute returns:\n")
print(results[order(abs(results$Max_Return), decreasing = TRUE), c("Asset", "Max_Return", "Min_Return")])
