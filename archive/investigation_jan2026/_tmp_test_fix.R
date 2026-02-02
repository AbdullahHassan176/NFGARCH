#!/usr/bin/env Rscript
# Quick test of asset-specific bounds fix

library(xts)
source("scripts/engines/engine_selector.R")
source("scripts/utils/safety_functions.R")
source("scripts/utils/return_forecast_evaluation.R")

cat("=== TESTING ASSET-SPECIFIC BOUNDS FIX ===\n\n")

# Load data
raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1)
raw_price_data$Date <- as.Date(rownames(raw_price_data))
price_data_matrix <- raw_price_data[, !(names(raw_price_data) %in% "Date")]
date_index <- raw_price_data$Date

# Test the three worst offenders:
# 1. eGARCH AMZN (was 265,545)
# 2. sGARCH_sstd NVDA (was 20.3)
# 3. sGARCH_norm MSFT (was 8.1)

test_cases <- list(
  list(asset = "AMZN", model = "eGARCH", dist = "sstd", old_mse = 265545),
  list(asset = "NVDA", model = "sGARCH", dist = "sstd", old_mse = 20.3),
  list(asset = "MSFT", model = "sGARCH", dist = "norm", old_mse = 8.1)
)

results <- list()

for (tc in test_cases) {
  cat("\n=== Testing:", tc$model, tc$dist, "on", tc$asset, "===\n")
  cat("Old MSE:", tc$old_mse, "\n")
  
  # Get returns
  prices_xts <- xts(price_data_matrix[[tc$asset]], order.by = date_index)
  returns <- diff(log(prices_xts))[-1]
  ret_vec <- as.numeric(returns)
  
  # Split 65/35
  n_obs <- length(ret_vec)
  split_idx <- floor(n_obs * 0.65)
  train_returns <- ret_vec[1:split_idx]
  test_returns <- ret_vec[(split_idx + 1):n_obs]
  
  # Fit model
  fit <- engine_fit(
    model = tc$model,
    returns = train_returns,
    dist = tc$dist,
    submodel = NULL,
    engine = "manual"
  )
  
  if (engine_converged(fit)) {
    # Check fitted volatility
    cat("Fitted avg sigma:", mean(fit$sigma), "\n")
    cat("Fitted max sigma:", max(fit$sigma), "\n")
    
    # Get residuals
    standard_residuals <- engine_residuals(fit, standardize = TRUE)
    
    # Run 1000-path evaluation
    cat("Running 1000-path forecast evaluation...\n")
    eval_result <- evaluate_return_forecasts(
      fit = fit,
      nf_residuals = standard_residuals,
      actual_returns = test_returns,
      horizon = length(test_returns),
      model_type = tc$model,
      submodel = NULL,
      engine = "manual",
      n_paths = 1000L
    )
    
    if (!is.null(eval_result) && !is.na(eval_result$mse)) {
      cat("New MSE:", eval_result$mse, "\n")
      cat("Improvement:", (tc$old_mse - eval_result$mse) / tc$old_mse * 100, "%\n")
      cat("Status:", ifelse(eval_result$mse < 10, "✓ FIXED", "✗ STILL HIGH"), "\n")
      
      results[[length(results) + 1]] <- data.frame(
        Asset = tc$asset,
        Model = tc$model,
        Dist = tc$dist,
        Old_MSE = tc$old_mse,
        New_MSE = eval_result$mse,
        Improvement_Pct = (tc$old_mse - eval_result$mse) / tc$old_mse * 100
      )
    } else {
      cat("ERROR: Evaluation failed\n")
    }
  } else {
    cat("ERROR: Model did not converge\n")
  }
}

cat("\n\n=== SUMMARY ===\n")
if (length(results) > 0) {
  summary_df <- do.call(rbind, results)
  print(summary_df, row.names = FALSE)
  
  cat("\nAll tests passed:", all(summary_df$New_MSE < 10), "\n")
} else {
  cat("No successful tests\n")
}
