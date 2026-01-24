#!/usr/bin/env Rscript
# Test return forecast evaluation with real data to identify issues

# Load required libraries
library(xts)
library(dplyr)

# Load utilities
source("scripts/core/config.R")
source("scripts/engines/engine_selector.R")
source("scripts/utils/return_forecast_evaluation.R")
source("scripts/utils/standardize_residuals.R")

set.seed(REPRODUCIBILITY_SEED)

cat("=== TESTING RETURN FORECAST EVALUATION WITH REAL DATA ===\n\n")

# Load data
cat("Loading data...\n")
raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1, stringsAsFactors = FALSE)
raw_price_data$Date <- as.Date(rownames(raw_price_data))

# Test on EURUSD
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
cat("  Fit structure check:\n")
cat("    - Has manual_fit:", !is.null(fit$manual_fit), "\n")
cat("    - Has sigma:", !is.null(fit$sigma), " (length:", length(fit$sigma), ")\n")
cat("    - Has residuals:", !is.null(fit$residuals), " (length:", length(fit$residuals), ")\n")
cat("    - Has coef:", !is.null(fit$coef), "\n")

# Load NF residuals
nf_residual_file <- paste0("outputs/manual/nf_models/", model_type, "_", asset_name, "_synthetic_residuals.csv")
nf_residuals <- NULL

if (file.exists(nf_residual_file)) {
  cat("Loading NF residuals from:", nf_residual_file, "\n")
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
  cat("  NF residuals stats: mean=", round(mean(nf_residuals), 6), 
      ", sd=", round(sd(nf_residuals), 6), "\n")
} else {
  cat("  [WARNING] NF residuals not found, using standard residuals\n")
  nf_residuals <- engine_residuals(fit, standardize = TRUE)
}

# Test generate_multiple_paths directly
cat("\n=== TEST 1: generate_multiple_paths ===\n")
test_horizon <- 20
test_returns_subset <- test_returns[1:test_horizon]

path_result <- tryCatch({
  generate_multiple_paths(
    fit = fit,
    nf_residuals = nf_residuals,
    horizon = test_horizon,
    model_type = model_type,
    submodel = NULL,
    engine = "manual",
    n_paths = 10  # Small number for testing
  )
}, error = function(e) {
  cat("ERROR: generate_multiple_paths failed:", e$message, "\n")
  traceback()
  NULL
})

if (is.null(path_result)) {
  cat("  [FAIL] generate_multiple_paths returned NULL\n")
} else {
  cat("  [PASS] generate_multiple_paths succeeded\n")
  cat("    Valid paths:", path_result$n_valid_paths, "\n")
  cat("    Point forecast length:", length(path_result$point_forecast), "\n")
  cat("    Sigma forecast length:", length(path_result$sigma_forecast), "\n")
}

# Test evaluate_return_forecasts
cat("\n=== TEST 2: evaluate_return_forecasts ===\n")
eval_result <- tryCatch({
  evaluate_return_forecasts(
    fit = fit,
    nf_residuals = nf_residuals,
    actual_returns = test_returns_subset,
    horizon = test_horizon,
    model_type = model_type,
    submodel = NULL,
    engine = "manual",
    n_paths = 10  # Small number for testing
  )
}, error = function(e) {
  cat("ERROR: evaluate_return_forecasts failed:", e$message, "\n")
  traceback()
  NULL
})

if (is.null(eval_result)) {
  cat("  [FAIL] evaluate_return_forecasts returned NULL\n")
  quit(status = 1)
} else {
  cat("  [PASS] evaluate_return_forecasts succeeded\n")
  cat("    MSE:", eval_result$mse, "\n")
  cat("    MAE:", eval_result$mae, "\n")
  cat("    LogLik:", eval_result$loglik, "\n")
  cat("    NPaths:", eval_result$n_valid_paths, "\n")
  
  if (is.na(eval_result$mse) || eval_result$n_valid_paths == 0) {
    cat("  [WARNING] Result has NA values or no valid paths\n")
  }
}

cat("\n=== TEST COMPLETE ===\n")
