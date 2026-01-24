#!/usr/bin/env Rscript
# Fit eGARCH models for missing assets: USDZAR, MSFT, AMZN
# This script attempts to fit eGARCH models that previously failed

# Load required libraries
library(xts)
library(PerformanceAnalytics)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Load centralized seed configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
  set.seed(REPRODUCIBILITY_SEED)
} else {
  set.seed(123)
}

# Load manual engine
source("scripts/engines/engine_selector.R")
source("scripts/manual/manual_optimized_config.R")

cat("=== FITTING MISSING eGARCH MODELS ===\n")
cat("Target assets: USDZAR, MSFT, AMZN\n\n")

# =============================================================================
# DATA LOADING
# =============================================================================

cat("1. Loading data...\n")

raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1)
raw_price_data$Date <- lubridate::ymd(rownames(raw_price_data))
rownames(raw_price_data) <- NULL
raw_price_data <- raw_price_data %>% dplyr::select(Date, everything())

price_data_matrix <- raw_price_data[, !(names(raw_price_data) %in% "Date")]

# Target assets
target_assets <- c("USDZAR", "MSFT", "AMZN")
target_assets <- target_assets[target_assets %in% names(price_data_matrix)]

cat("Processing", length(target_assets), "assets:", paste(target_assets, collapse = ", "), "\n\n")

# Convert to XTS and calculate returns
target_returns <- list()
for (asset in target_assets) {
  if (asset %in% names(price_data_matrix)) {
    prices <- price_data_matrix[[asset]]
    dates <- raw_price_data$Date
    price_xts <- xts(prices, order.by = dates)
    returns <- diff(log(price_xts))
    returns <- returns[!is.na(returns)]
    target_returns[[asset]] <- returns
    cat("  Loaded", asset, "-", length(returns), "returns\n")
  }
}

# =============================================================================
# eGARCH FITTING WITH RETRY LOGIC
# =============================================================================

cat("\n2. Fitting eGARCH models with enhanced retry logic...\n")

# Get eGARCH config
egarch_config <- get_manual_model_config()[["eGARCH"]]

# Enhanced fitting function with multiple attempts
fit_egarch_with_retry <- function(returns_data, asset_name, max_attempts = 3) {
  returns_vec <- as.numeric(returns_data)
  returns_vec <- returns_vec[!is.na(returns_vec)]
  
  if (length(returns_vec) < 100) {
    cat("  ERROR: Insufficient data for", asset_name, "\n")
    return(NULL)
  }
  
  cat("  Fitting eGARCH for", asset_name, "-", length(returns_vec), "observations\n")
  
  for (attempt in 1:max_attempts) {
    tryCatch({
      cat("    Attempt", attempt, "...\n")
      
      # Fit eGARCH model
      garch_fit <- engine_fit(
        model = egarch_config$model,
        returns = returns_vec,
        dist = egarch_config$distribution,
        submodel = egarch_config$submodel,
        engine = "manual"
      )
      
      # Check convergence
      if (engine_converged(garch_fit)) {
        cat("    [SUCCESS] eGARCH converged for", asset_name, "\n")
        cat("      Coefficients:", paste(names(garch_fit$coef), round(garch_fit$coef, 4), sep = "=", collapse = ", "), "\n")
        cat("      Log-likelihood:", round(garch_fit$loglik, 2), "\n")
        
        # Extract residuals
        residuals <- engine_residuals(garch_fit, standardize = TRUE)
        
        return(list(
          fit = garch_fit,
          residuals = residuals,
          asset = asset_name,
          success = TRUE
        ))
      } else {
        cat("    [FAILED] eGARCH did not converge for", asset_name, "(attempt", attempt, ")\n")
        if (attempt < max_attempts) {
          cat("    Retrying with different initialization...\n")
          # Add small random perturbation to data to help optimization
          returns_vec <- returns_vec + rnorm(length(returns_vec), 0, sd(returns_vec) * 0.0001)
        }
      }
    }, error = function(e) {
      cat("    [ERROR] Attempt", attempt, "failed:", e$message, "\n")
      if (attempt < max_attempts) {
        cat("    Retrying...\n")
      }
    })
  }
  
  cat("  [FAILED] All attempts failed for", asset_name, "\n")
  return(NULL)
}

# Fit eGARCH for each target asset
results <- list()
for (asset in target_assets) {
  cat("\nProcessing", asset, "...\n")
  result <- fit_egarch_with_retry(target_returns[[asset]], asset)
  if (!is.null(result)) {
    results[[asset]] <- result
  }
}

# =============================================================================
# SAVE RESIDUALS
# =============================================================================

cat("\n3. Saving residuals...\n")

residuals_dir <- "outputs/manual/residuals_by_model/eGARCH"
if (!dir.exists(residuals_dir)) {
  dir.create(residuals_dir, recursive = TRUE)
}

success_count <- 0
for (asset in names(results)) {
  result <- results[[asset]]
  if (!is.null(result) && !is.null(result$residuals)) {
    # Combine residuals from all windows (for consistency with main script)
    all_residuals <- as.numeric(result$residuals)
    
    if (length(all_residuals) > 0) {
      residuals_df <- data.frame(residuals = all_residuals)
      residuals_file <- file.path(residuals_dir, paste0(asset, "_Manual_Optimized_residuals.csv"))
      write.csv(residuals_df, residuals_file, row.names = FALSE)
      cat("  [OK] Saved residuals for", asset, "-", length(all_residuals), "residuals\n")
      success_count <- success_count + 1
    }
  }
}

cat("\n=== SUMMARY ===\n")
cat("Assets processed:", length(target_assets), "\n")
cat("Successful fits:", success_count, "\n")
cat("Failed fits:", length(target_assets) - success_count, "\n")

if (success_count > 0) {
  cat("\n[OK] Residual files saved. Next step: Run NF training to generate synthetic residuals.\n")
} else {
  cat("\n[WARNING] No successful fits. eGARCH may not converge for these assets.\n")
}

cat("\nDone.\n")
