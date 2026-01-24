#!/usr/bin/env Rscript
# Test Return Forecast Evaluation
# Quick test on single asset/model to verify implementation

# Load required libraries
library(xts)
library(dplyr)

# Load utilities
source("scripts/core/config.R")
source("scripts/engines/engine_selector.R")
source("scripts/utils/return_forecast_evaluation.R")
source("scripts/utils/standardize_residuals.R")

set.seed(REPRODUCIBILITY_SEED)

cat("=== TESTING RETURN FORECAST EVALUATION ===\n\n")

# Load data
cat("Loading data...\n")
raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1, stringsAsFactors = FALSE)
raw_price_data$Date <- as.Date(rownames(raw_price_data))

# Test on single asset: EURUSD
asset_name <- "EURUSD"
cat("Testing on asset:", asset_name, "\n")

# Calculate returns
prices <- raw_price_data[[asset_name]]
returns <- diff(log(prices))
returns <- returns[!is.na(returns)]

cat("  Total observations:", length(returns), "\n")

# Create train/test split (65/35)
n_obs <- length(returns)
train_size <- floor(n_obs * 0.65)
test_size <- n_obs - train_size

train_returns <- as.numeric(returns[1:train_size])
test_returns <- as.numeric(returns[(train_size + 1):n_obs])

cat("  Train size:", train_size, "\n")
cat("  Test size:", test_size, "\n")

# Test model: sGARCH
model_type <- "sGARCH"
cat("\nTesting model:", model_type, "\n")

# Fit GARCH model
cat("Fitting GARCH model...\n")
fit <- tryCatch({
  engine_fit(
    model = model_type,
    returns = train_returns,
    dist = "norm",
    submodel = NULL,
    engine = "manual"
  )
}, error = function(e) {
  cat("ERROR: Fit failed:", e$message, "\n")
  NULL
})

if (is.null(fit) || !engine_converged(fit)) {
  cat("ERROR: Model did not converge\n")
  quit(status = 1)
}

cat("  [OK] Model converged\n")

# Load NF residuals (if available)
nf_residual_file <- paste0("outputs/manual/nf_models/", model_type, "_", asset_name, "_synthetic_residuals.csv")
nf_residuals <- NULL

if (file.exists(nf_residual_file)) {
  cat("Loading NF residuals...\n")
  nf_data <- read.csv(nf_residual_file, header = FALSE)
  nf_residuals <- as.numeric(nf_data[[1]])
  
  # Skip header if present
  if (is.character(nf_residuals[1])) {
    nf_residuals <- nf_residuals[-1]
  }
  nf_residuals <- as.numeric(nf_residuals[!is.na(nf_residuals)])
  
  # Standardize
  nf_residuals <- (nf_residuals - mean(nf_residuals)) / sd(nf_residuals)
  
  cat("  [OK] Loaded", length(nf_residuals), "NF residuals\n")
} else {
  cat("  [WARNING] NF residuals not found, using standard residuals\n")
  nf_residuals <- engine_residuals(fit, standardize = TRUE)
}

# Test return forecast evaluation
cat("\nTesting return forecast evaluation...\n")
cat("  Generating 100 paths (reduced for testing)...\n")

test_horizon <- min(20, length(test_returns))  # Use first 20 observations for quick test
test_returns_subset <- test_returns[1:test_horizon]

eval_result <- tryCatch({
  evaluate_return_forecasts(
    fit = fit,
    nf_residuals = nf_residuals,
    actual_returns = test_returns_subset,
    horizon = test_horizon,
    model_type = model_type,
    submodel = NULL,
    engine = "manual",
    n_paths = 100  # Reduced for testing
  )
}, error = function(e) {
  cat("ERROR: Evaluation failed:", e$message, "\n")
  traceback()
  NULL
})

if (is.null(eval_result)) {
  cat("ERROR: Evaluation returned NULL\n")
  quit(status = 1)
}

# Display results
cat("\n=== TEST RESULTS ===\n")
cat("MSE:", eval_result$mse, "\n")
cat("MAE:", eval_result$mae, "\n")
cat("Predictive Log-Likelihood:", eval_result$loglik, "\n")
cat("Number of valid paths:", eval_result$n_valid_paths, "\n")
cat("Point forecast length:", length(eval_result$point_forecast), "\n")
cat("Sigma forecast length:", length(eval_result$sigma_forecast), "\n")

# Validation checks
cat("\n=== VALIDATION CHECKS ===\n")
checks_passed <- TRUE

if (is.na(eval_result$mse) || is.na(eval_result$mae)) {
  cat("  [FAIL] MSE or MAE is NA\n")
  checks_passed <- FALSE
} else {
  cat("  [PASS] MSE and MAE are valid\n")
}

if (eval_result$n_valid_paths < 50) {
  cat("  [WARNING] Only", eval_result$n_valid_paths, "valid paths (expected ~100)\n")
} else {
  cat("  [PASS] Sufficient valid paths:", eval_result$n_valid_paths, "\n")
}

if (length(eval_result$point_forecast) != test_horizon) {
  cat("  [FAIL] Point forecast length mismatch\n")
  checks_passed <- FALSE
} else {
  cat("  [PASS] Point forecast length correct\n")
}

if (any(is.na(eval_result$point_forecast))) {
  cat("  [WARNING] Some point forecasts are NA\n")
} else {
  cat("  [PASS] All point forecasts are valid\n")
}

if (checks_passed) {
  cat("\n=== TEST PASSED ===\n")
  cat("Return forecast evaluation is working correctly.\n")
  cat("You can proceed with full pipeline rerun.\n")
} else {
  cat("\n=== TEST FAILED ===\n")
  cat("Please fix issues before running full pipeline.\n")
  quit(status = 1)
}
