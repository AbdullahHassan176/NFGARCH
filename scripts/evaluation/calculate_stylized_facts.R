#!/usr/bin/env Rscript
# Calculate Stylized Facts for Financial Time Series
# Metrics: Volatility clustering, Leverage effects, Autocorrelation decay, Heavy tails, Gain/loss asymmetry

library(xts)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

# Optional packages
if (!requireNamespace("forecast", quietly = TRUE)) {
  cat("[WARNING] forecast package not found. Some features may be limited.\n")
}
if (!requireNamespace("tseries", quietly = TRUE)) {
  cat("[WARNING] tseries package not found. Some features may be limited.\n")
}

cat("=== CALCULATING STYLIZED FACTS ===\n\n")

# =============================================================================
# Helper Functions
# =============================================================================

# Calculate volatility clustering index (ACF of squared returns)
calculate_volatility_clustering <- function(returns, max_lag = 20) {
  tryCatch({
    squared_returns <- returns^2
    acf_values <- acf(squared_returns, lag.max = max_lag, plot = FALSE, na.action = na.pass)
    
    # Return persistence (sum of ACF values up to lag 10)
    persistence <- sum(acf_values$acf[1:min(10, length(acf_values$acf))], na.rm = TRUE)
    
    # Return first significant lag
    first_sig_lag <- which(abs(acf_values$acf) > 0.1)[1]
    if (is.na(first_sig_lag)) first_sig_lag <- 0
    
    return(list(
      persistence = persistence,
      first_sig_lag = first_sig_lag,
      acf_lag1 = acf_values$acf[2]  # Lag 1 autocorrelation
    ))
  }, error = function(e) {
    return(list(persistence = NA, first_sig_lag = NA, acf_lag1 = NA))
  })
}

# Calculate leverage effect (asymmetric response)
calculate_leverage_effect <- function(returns, max_lag = 5) {
  tryCatch({
    # Create indicator for negative returns
    negative_returns <- returns < 0
    squared_returns <- returns^2
    
    # Calculate correlation between negative indicator and future volatility
    leverage_corrs <- sapply(1:max_lag, function(lag) {
      if (lag >= length(negative_returns)) return(NA)
      cor(negative_returns[1:(length(negative_returns) - lag)], 
          squared_returns[(lag + 1):length(squared_returns)], 
          use = "complete.obs")
    })
    
    # Average leverage effect
    leverage_effect <- mean(leverage_corrs, na.rm = TRUE)
    
    return(list(
      leverage_effect = leverage_effect,
      max_leverage_lag = which.max(abs(leverage_corrs))
    ))
  }, error = function(e) {
    return(list(leverage_effect = NA, max_leverage_lag = NA))
  })
}

# Calculate autocorrelation decay rate
calculate_autocorrelation_decay <- function(returns, max_lag = 20) {
  tryCatch({
    # ACF of raw returns (should be near zero)
    acf_returns <- acf(returns, lag.max = max_lag, plot = FALSE, na.action = na.pass)
    
    # ACF of squared returns (should decay slowly)
    squared_returns <- returns^2
    acf_squared <- acf(squared_returns, lag.max = max_lag, plot = FALSE, na.action = na.pass)
    
    # Decay rate (exponential fit to ACF)
    if (length(acf_squared$acf) > 5) {
      lags <- 1:min(10, length(acf_squared$acf) - 1)
      acf_vals <- acf_squared$acf[2:(length(lags) + 1)]
      
      # Exponential decay: ACF(lag) = A * exp(-lambda * lag)
      # Log-linear regression
      valid_idx <- !is.na(acf_vals) & acf_vals > 0
      if (sum(valid_idx) > 2) {
        log_acf <- log(acf_vals[valid_idx])
        decay_fit <- lm(log_acf ~ lags[valid_idx])
        decay_rate <- -coef(decay_fit)[2]  # Negative of slope
      } else {
        decay_rate <- NA
      }
    } else {
      decay_rate <- NA
    }
    
    return(list(
      returns_acf_lag1 = acf_returns$acf[2],
      squared_acf_lag1 = acf_squared$acf[2],
      decay_rate = decay_rate
    ))
  }, error = function(e) {
    return(list(returns_acf_lag1 = NA, squared_acf_lag1 = NA, decay_rate = NA))
  })
}

# Calculate gain/loss asymmetry
calculate_gain_loss_asymmetry <- function(returns) {
  tryCatch({
    gains <- returns[returns > 0]
    losses <- abs(returns[returns < 0])
    
    if (length(gains) > 0 && length(losses) > 0) {
      mean_gain <- mean(gains, na.rm = TRUE)
      mean_loss <- mean(losses, na.rm = TRUE)
      
      # Asymmetry ratio
      asymmetry_ratio <- mean_loss / mean_gain
      
      # Skewness of returns
      skew <- calculate_skewness(returns)
      
      return(list(
        mean_gain = mean_gain,
        mean_loss = mean_loss,
        asymmetry_ratio = asymmetry_ratio,
        skewness = skew
      ))
    } else {
      return(list(mean_gain = NA, mean_loss = NA, asymmetry_ratio = NA, skewness = NA))
    }
  }, error = function(e) {
    return(list(mean_gain = NA, mean_loss = NA, asymmetry_ratio = NA, skewness = NA))
  })
}

# Helper function for skewness
calculate_skewness <- function(data) {
  tryCatch({
    if (require(moments)) {
      return(moments::skewness(data, na.rm = TRUE))
    } else {
      mean_val <- mean(data, na.rm = TRUE)
      sd_val <- sd(data, na.rm = TRUE)
      if (sd_val == 0) return(NA)
      return(mean(((data - mean_val) / sd_val)^3, na.rm = TRUE))
    }
  }, error = function(e) {
    return(NA)
  })
}

# =============================================================================
# Load Data
# =============================================================================

cat("Loading data...\n")

# Load raw price data
raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1)
raw_price_data$Date <- as.Date(rownames(raw_price_data))
rownames(raw_price_data) <- NULL
raw_price_data <- raw_price_data %>% dplyr::select(Date, everything())

# Extract assets
equity_tickers <- c("NVDA", "MSFT", "AMZN")
fx_names <- c("EURUSD", "GBPUSD", "USDZAR")

# Calculate returns
all_returns <- list()
all_assets <- c()

for (ticker in c(equity_tickers, fx_names)) {
  if (ticker %in% names(raw_price_data)) {
    prices <- raw_price_data[[ticker]]
    dates <- raw_price_data$Date
    
    # Calculate log returns
    returns <- diff(log(prices))
    returns <- returns[!is.na(returns)]
    
    if (length(returns) > 100) {
      all_returns[[ticker]] <- returns
      all_assets <- c(all_assets, ticker)
    }
  }
}

cat("[OK] Loaded", length(all_returns), "assets\n")

# =============================================================================
# Calculate Stylized Facts
# =============================================================================

cat("\n=== CALCULATING STYLIZED FACTS ===\n")

stylized_facts_results <- list()

for (asset_name in names(all_returns)) {
  cat("\nProcessing:", asset_name, "\n")
  
  returns <- all_returns[[asset_name]]
  
  # Determine asset class
  asset_class <- if (asset_name %in% equity_tickers) "Equity" else "FX"
  
  # Calculate metrics
  volatility_clustering <- calculate_volatility_clustering(returns)
  leverage_effect <- calculate_leverage_effect(returns)
  autocorr_decay <- calculate_autocorrelation_decay(returns)
  gain_loss <- calculate_gain_loss_asymmetry(returns)
  
  # Create result row
  result <- data.frame(
    Asset = asset_name,
    Asset_Class = asset_class,
    Volatility_clustering_persistence = volatility_clustering$persistence,
    Volatility_clustering_first_sig_lag = volatility_clustering$first_sig_lag,
    Volatility_clustering_acf_lag1 = volatility_clustering$acf_lag1,
    Leverage_effect = leverage_effect$leverage_effect,
    Leverage_max_lag = leverage_effect$max_leverage_lag,
    Returns_ACF_lag1 = autocorr_decay$returns_acf_lag1,
    Squared_returns_ACF_lag1 = autocorr_decay$squared_acf_lag1,
    Autocorrelation_decay_rate = autocorr_decay$decay_rate,
    Mean_gain = gain_loss$mean_gain,
    Mean_loss = gain_loss$mean_loss,
    Gain_loss_asymmetry_ratio = gain_loss$asymmetry_ratio,
    Skewness = gain_loss$skewness
  )
  
  stylized_facts_results[[asset_name]] <- result
}

# Combine results
stylized_facts_df <- bind_rows(stylized_facts_results)

cat("\n[OK] Stylized facts calculated\n")

# =============================================================================
# Summary Statistics by Asset Class
# =============================================================================

cat("\n=== SUMMARY STATISTICS BY ASSET CLASS ===\n")

summary_by_class <- stylized_facts_df %>%
  group_by(Asset_Class) %>%
  summarise(
    mean_volatility_clustering = mean(Volatility_clustering_persistence, na.rm = TRUE),
    mean_leverage_effect = mean(Leverage_effect, na.rm = TRUE),
    mean_gain_loss_asymmetry = mean(Gain_loss_asymmetry_ratio, na.rm = TRUE),
    mean_skewness = mean(Skewness, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_by_class)

# =============================================================================
# Save Results
# =============================================================================

cat("\n=== SAVING RESULTS ===\n")

output_file <- "results/consolidated/Stylized_Facts.xlsx"
wb <- createWorkbook()

addWorksheet(wb, "Stylized_Facts")
writeData(wb, "Stylized_Facts", stylized_facts_df)

addWorksheet(wb, "Summary_By_Asset_Class")
writeData(wb, "Summary_By_Asset_Class", summary_by_class)

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("[OK] Results saved to:", output_file, "\n")
cat("\n=== STYLIZED FACTS CALCULATION COMPLETE ===\n")

