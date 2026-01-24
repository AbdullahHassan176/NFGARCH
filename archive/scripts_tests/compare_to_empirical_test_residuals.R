#!/usr/bin/env Rscript
# Compare NF-GARCH and Standard GARCH residuals to EMPIRICAL TEST SET residuals
# This provides stronger evidence than comparing NF vs Standard residuals
# 
# What this does:
# 1. Fit GARCH on training set (65% split)
# 2. Forecast volatility for test set (35% split)
# 3. Calculate empirical test residuals: (test_returns - mean_forecast) / sigma_forecast
# 4. Compare NF-generated residuals vs empirical test residuals
# 5. Compare Standard GARCH residuals vs empirical test residuals
# 6. Report which matches empirical test data better

# Load centralized seed configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
  set.seed(REPRODUCIBILITY_SEED)
} else {
  set.seed(123)  # Fallback if config not available
}

library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(xts)

# Load transport package for Wasserstein distance
if (!require(transport)) {
  install.packages("transport")
  library(transport)
}

cat("=== COMPARING TO EMPIRICAL TEST SET RESIDUALS ===\n\n")
cat("This script provides STRONGER evidence by comparing model residuals\n")
cat("to actual empirical test set residuals (not just to each other).\n\n")

# =============================================================================
# Helper Functions (from calculate_distributional_metrics.R)
# =============================================================================

calculate_ks_distance <- function(actual, predicted) {
  tryCatch({
    ks_test <- ks.test(actual, predicted)
    return(ks_test$statistic)
  }, error = function(e) {
    return(NA)
  })
}

calculate_wasserstein_distance <- function(actual, predicted) {
  tryCatch({
    if (requireNamespace("transport", quietly = TRUE)) {
      wd <- transport::wasserstein1d(actual, predicted)
      return(wd)
    }
  }, error = function(e) {
    # Continue to manual calculation
  })
  
  # Manual calculation
  tryCatch({
    sorted_actual <- sort(actual)
    sorted_pred <- sort(predicted)
    n <- length(sorted_actual)
    m <- length(sorted_pred)
    
    min_len <- min(n, m)
    if (min_len == 0) return(NA)
    
    sorted_actual <- sort(sorted_actual)[1:min_len]
    sorted_pred <- sort(sorted_pred)[1:min_len]
    
    wd <- mean(abs(sorted_actual - sorted_pred), na.rm = TRUE)
    return(wd)
  }, error = function(e2) {
    return(NA)
  })
}

calculate_skewness <- function(data) {
  tryCatch({
    n <- length(data)
    if (n < 3) return(NA)
    mean_data <- mean(data, na.rm = TRUE)
    sd_data <- sd(data, na.rm = TRUE)
    if (sd_data == 0) return(NA)
    skew <- (n / ((n - 1) * (n - 2))) * sum(((data - mean_data) / sd_data)^3, na.rm = TRUE)
    return(skew)
  }, error = function(e) {
    return(NA)
  })
}

calculate_kurtosis <- function(data) {
  tryCatch({
    n <- length(data)
    if (n < 4) return(NA)
    mean_data <- mean(data, na.rm = TRUE)
    sd_data <- sd(data, na.rm = TRUE)
    if (sd_data == 0) return(NA)
    kurt <- (n * (n + 1) / ((n - 1) * (n - 2) * (n - 3))) * 
            sum(((data - mean_data) / sd_data)^4, na.rm = TRUE) - 
            3 * (n - 1)^2 / ((n - 2) * (n - 3))
    return(kurt)
  }, error = function(e) {
    return(NA)
  })
}

# =============================================================================
# Load Data and Setup
# =============================================================================

cat("Loading data...\n")
source("scripts/engines/engine_selector.R")
source("scripts/utils/safety_functions.R")
source("scripts/manual_garch/forecast_manual.R")

# Load price data
raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1)
date_index <- as.Date(rownames(raw_price_data))
rownames(raw_price_data) <- NULL

# Define assets
equity_tickers <- c("NVDA", "MSFT", "AMZN")
fx_names <- c("EURUSD", "GBPUSD", "USDZAR")

all_asset_names <- c(fx_names, equity_tickers)
all_returns <- list()

# Calculate returns
for (ticker in all_asset_names) {
  prices <- raw_price_data[[ticker]]
  returns <- diff(log(prices))[-1]
  all_returns[[ticker]] <- returns
}

cat("Loaded", length(all_returns), "assets\n\n")

# Model configurations - process in batches, start with simpler models
model_configs <- list(
  sGARCH = list(model = "sGARCH", distribution = "sstd", submodel = NULL),
  gjrGARCH = list(model = "gjrGARCH", distribution = "sstd", submodel = NULL),
  TGARCH = list(model = "TGARCH", distribution = "sstd", submodel = NULL),
  eGARCH = list(model = "eGARCH", distribution = "sstd", submodel = NULL)
)

# Process models in batches (skip eGARCH initially as it has convergence issues)
models_to_process <- names(model_configs)[1:3]  # Start with sGARCH, gjrGARCH, TGARCH
cat("Processing models in batch:", paste(models_to_process, collapse = ", "), "\n")
cat("(Skipping eGARCH due to convergence issues)\n\n")

# =============================================================================
# Load NF Residuals
# =============================================================================

cat("Loading NF-generated residuals...\n")
nf_residuals_dir <- "outputs/manual/nf_models"
nf_residuals_map <- list()

for (model_name in names(model_configs)) {
  for (asset_name in all_asset_names) {
    nf_key <- paste0(model_name, "_", asset_name)
    nf_file <- file.path(nf_residuals_dir, paste0(nf_key, "_synthetic_residuals.csv"))
    
    if (file.exists(nf_file)) {
      tryCatch({
        nf_data <- read.csv(nf_file, header = FALSE)
        nf_residuals <- as.numeric(nf_data[[1]])
        
        # Skip header if present
        if (is.character(nf_residuals[1]) && grepl("residual", nf_residuals[1], ignore.case = TRUE)) {
          nf_residuals <- nf_residuals[-1]
        }
        
        nf_residuals <- nf_residuals[!is.na(nf_residuals)]
        if (length(nf_residuals) > 10) {
          nf_residuals_map[[nf_key]] <- nf_residuals
        }
      }, error = function(e) {
        cat("  [WARNING] Could not load NF residuals for", nf_key, ":", e$message, "\n")
      })
    }
  }
}

cat("Loaded NF residuals for", length(nf_residuals_map), "model-asset combinations\n\n")

# =============================================================================
# Main Analysis: Compare to Empirical Test Residuals
# =============================================================================

cat("=== MAIN ANALYSIS: COMPARING TO EMPIRICAL TEST RESIDUALS ===\n\n")

results_list <- list()

for (asset_idx in 1:length(all_returns)) {
  asset_name <- all_asset_names[asset_idx]
  returns_data <- all_returns[[asset_idx]]
  
  cat("\nProcessing asset:", asset_name, "\n")
  cat("  Total observations:", length(returns_data), "\n")
  
  # Create train/test split (65/35)
  # But limit test size to reasonable forecast horizon (max 100 observations for stability)
  n_obs <- length(returns_data)
  train_size <- floor(n_obs * 0.65)
  test_size_full <- n_obs - train_size
  test_size <- min(test_size_full, 100)  # Limit to 100 for more stable forecasting
  
  train_returns <- returns_data[1:train_size]
  # Use last test_size observations from test set
  test_returns <- returns_data[(n_obs - test_size + 1):n_obs]
  
  cat("  Train size:", train_size, "| Test size:", test_size, "(limited from", test_size_full, ")\n")
  
  for (model_name in models_to_process) {
    cfg <- model_configs[[model_name]]
    nf_key <- paste0(model_name, "_", asset_name)
    
    cat("  Model:", model_name, "\n")
    
    tryCatch({
      # Fit GARCH on training set
      fit <- engine_fit(
        model = cfg$model,
        returns = as.numeric(train_returns),
        dist = cfg$distribution,
        submodel = cfg$submodel,
        engine = "manual"
      )
      
      if (!engine_converged(fit)) {
        cat("    [SKIP] Model did not converge\n")
        next
      }
      
      # Forecast volatility for test set
      # Use manual_forecast directly on the manual_fit object
      if (is.null(fit$manual_fit)) {
        cat("    [SKIP] No manual_fit object found\n")
        next
      }
      
      # Try predict method first, then manual_forecast
      forecast <- NULL
      
      # Debug: Check fit structure
      cat("    [DEBUG] Fit structure: has predict =", !is.null(fit$manual_fit$predict),
          "| has sigma =", !is.null(fit$manual_fit$sigma) && length(fit$manual_fit$sigma) > 0,
          "| sigma length =", if(!is.null(fit$manual_fit$sigma)) length(fit$manual_fit$sigma) else 0,
          "| has coef =", !is.null(fit$manual_fit$coef), "\n")
      
      if (!is.null(fit$manual_fit$predict)) {
        forecast <- tryCatch({
          result <- fit$manual_fit$predict(test_size)
          # Check if result is valid (not all NA)
          if (!is.null(result) && !is.null(result$sigma) && length(result$sigma) > 0) {
            n_na <- sum(is.na(result$sigma))
            if (n_na == length(result$sigma)) {
              cat("    [WARNING] predict() returned all NA sigma, trying manual_forecast\n")
              NULL
            } else {
              result
            }
          } else {
            cat("    [WARNING] predict() returned invalid sigma, trying manual_forecast\n")
            NULL
          }
        }, error = function(e) {
          cat("    [WARNING] predict() failed, trying manual_forecast:", e$message, "\n")
          NULL
        })
      }
      
      if (is.null(forecast)) {
        forecast <- tryCatch({
          result <- manual_forecast(fit$manual_fit, test_size)
          # Check if result is valid (not all NA)
          if (!is.null(result) && !is.null(result$sigma) && length(result$sigma) > 0) {
            n_na <- sum(is.na(result$sigma))
            if (n_na == length(result$sigma)) {
              cat("    [WARNING] manual_forecast returned all NA sigma, using fallback\n")
              NULL
            } else {
              result
            }
          } else {
            cat("    [WARNING] manual_forecast returned invalid sigma, using fallback\n")
            NULL
          }
        }, error = function(e) {
          cat("    [WARNING] manual_forecast failed:", e$message, "\n")
          NULL
        })
      }
      
      # Fallback: use last sigma value (simple constant forecast)
      if (is.null(forecast) || is.null(forecast$sigma) || length(forecast$sigma) == 0) {
        if (!is.null(fit$manual_fit$sigma) && length(fit$manual_fit$sigma) > 0) {
          last_sigma <- tail(fit$manual_fit$sigma, 1)
          mu_idx <- grep("mu", names(fit$manual_fit$coef))
          mu <- if (length(mu_idx) > 0) fit$manual_fit$coef[mu_idx[1]] else 0
          forecast <- list(
            sigma = rep(last_sigma, test_size),
            mean = rep(mu, test_size)
          )
          cat("    [INFO] Using fallback forecast (constant sigma =", round(last_sigma, 6), ")\n")
        } else {
          cat("    [SKIP] Could not generate forecast (no sigma in fit)\n")
          next
        }
      }
      
      forecast_sigma <- forecast$sigma
      forecast_mean <- forecast$mean
      
      # Debug: Print what we got
      cat("    [DEBUG] Forecast sigma: length =", length(forecast_sigma), 
          "| is.null =", is.null(forecast_sigma),
          "| all NA =", if(!is.null(forecast_sigma) && length(forecast_sigma) > 0) all(is.na(forecast_sigma)) else "N/A",
          "| first few =", if(!is.null(forecast_sigma) && length(forecast_sigma) > 0) paste(head(forecast_sigma, 3), collapse=",") else "N/A", "\n")
      cat("    [DEBUG] Forecast mean: length =", if(!is.null(forecast_mean)) length(forecast_mean) else "NULL",
          "| is.null =", is.null(forecast_mean),
          "| all NA =", if(!is.null(forecast_mean) && length(forecast_mean) > 0) all(is.na(forecast_mean)) else "N/A",
          "| first few =", if(!is.null(forecast_mean) && length(forecast_mean) > 0) paste(head(forecast_mean, 3), collapse=",") else "N/A", "\n")
      
      # Check if forecast is invalid and use fallback immediately
      use_fallback <- FALSE
      if (is.null(forecast_sigma) || length(forecast_sigma) == 0) {
        use_fallback <- TRUE
        cat("    [DEBUG] Forecast sigma is NULL or empty\n")
      } else if (length(forecast_sigma) > 0 && all(is.na(forecast_sigma))) {
        use_fallback <- TRUE
        cat("    [DEBUG] Forecast sigma is all NA\n")
      } else if (length(forecast_sigma) != test_size) {
        use_fallback <- TRUE
        cat("    [DEBUG] Forecast sigma length mismatch:", length(forecast_sigma), "vs", test_size, "\n")
      }
      
      if (use_fallback) {
        if (!is.null(fit$manual_fit$sigma) && length(fit$manual_fit$sigma) > 0) {
          last_sigma <- tail(fit$manual_fit$sigma, 1)
          mu_idx <- grep("mu", names(fit$manual_fit$coef))
          mu <- if (length(mu_idx) > 0) fit$manual_fit$coef[mu_idx[1]] else 0
          forecast_sigma <- rep(last_sigma, test_size)
          forecast_mean <- rep(mu, test_size)
          cat("    [INFO] Using fallback forecast (constant sigma =", round(last_sigma, 6), ", mu =", round(mu, 6), ")\n")
        } else {
          cat("    [SKIP] Cannot create fallback (no sigma in fit object)\n")
          next
        }
      } else {
        # Ensure forecast_mean is correct BEFORE checking for NAs
        # Fix if NULL, empty, wrong length, or all NA
        if (is.null(forecast_mean) || length(forecast_mean) == 0 || 
            length(forecast_mean) != length(forecast_sigma) ||
            (length(forecast_mean) > 0 && all(is.na(forecast_mean)))) {
          mu_idx <- grep("mu", names(fit$manual_fit$coef))
          mu <- if (length(mu_idx) > 0) fit$manual_fit$coef[mu_idx[1]] else 0
          forecast_mean <- rep(mu, length(forecast_sigma))
          cat("    [INFO] Fixed forecast mean (using mu =", round(mu, 6), ")\n")
        }
        
        # Forecast looks valid, but check for partial NAs
        n_na_sigma <- sum(is.na(forecast_sigma))
        n_na_mean <- sum(is.na(forecast_mean))
        cat("    [DEBUG] Before NA check: n_na_sigma =", n_na_sigma, "| n_na_mean =", n_na_mean,
            "| forecast_sigma length =", length(forecast_sigma),
            "| forecast_mean length =", if(!is.null(forecast_mean)) length(forecast_mean) else "NULL", "\n")
        if (n_na_sigma > 0 || n_na_mean > 0) {
          # Remove NA values
          valid_idx <- !is.na(forecast_sigma) & !is.na(forecast_mean)
          cat("    [DEBUG] valid_idx: length =", length(valid_idx), "| sum =", sum(valid_idx), "\n")
          if (sum(valid_idx) < 10) {
            cat("    [SKIP] Too many NA values in forecast (only", sum(valid_idx), "valid)\n")
            next
          }
          forecast_sigma <- forecast_sigma[valid_idx]
          forecast_mean <- forecast_mean[valid_idx]
          test_returns <- test_returns[valid_idx]
          cat("    [WARNING] Removed", sum(!valid_idx), "NA values from forecast\n")
        }
      }
      
      # Ensure forecast_mean is correct (double-check after NA removal)
      if (is.null(forecast_mean) || length(forecast_mean) == 0 || length(forecast_mean) != length(forecast_sigma)) {
        mu_idx <- grep("mu", names(fit$manual_fit$coef))
        mu <- if (length(mu_idx) > 0) fit$manual_fit$coef[mu_idx[1]] else 0
        forecast_mean <- rep(mu, length(forecast_sigma))
        cat("    [INFO] Fixed forecast mean (using mu =", round(mu, 6), ")\n")
      }
      
      n_nonpos_sigma <- sum(forecast_sigma <= 0, na.rm = TRUE)
      if (n_nonpos_sigma > 0) {
        # Remove non-positive sigma values
        valid_idx <- forecast_sigma > 0
        if (sum(valid_idx) < 10) {
          cat("    [SKIP] Too many non-positive sigma values (only", sum(valid_idx), "valid)\n")
          next
        }
        forecast_sigma <- forecast_sigma[valid_idx]
        forecast_mean <- forecast_mean[valid_idx]
        test_returns <- test_returns[valid_idx]
        cat("    [WARNING] Removed", sum(!valid_idx), "non-positive sigma values\n")
      }
      
      cat("    Forecast: sigma range [", round(min(forecast_sigma), 6), ",", round(max(forecast_sigma), 6), 
          "] | mean =", round(mean(forecast_mean), 6), "| n =", length(forecast_sigma), "\n")
      
      # Ensure same length
      min_len <- min(length(test_returns), length(forecast_sigma), length(forecast_mean))
      if (min_len < 10) {
        cat("    [SKIP] Insufficient forecast length:", min_len, "\n")
        next
      }
      
      test_returns_trimmed <- test_returns[1:min_len]
      forecast_sigma_trimmed <- forecast_sigma[1:min_len]
      forecast_mean_trimmed <- forecast_mean[1:min_len]
      
      # Calculate EMPIRICAL TEST RESIDUALS
      # This is what actually happened in the test set
      empirical_test_residuals <- (test_returns_trimmed - forecast_mean_trimmed) / forecast_sigma_trimmed
      n_before_filter <- length(empirical_test_residuals)
      n_infinite <- sum(!is.finite(empirical_test_residuals))
      empirical_test_residuals <- empirical_test_residuals[is.finite(empirical_test_residuals)]
      
      if (length(empirical_test_residuals) < 10) {
        cat("    [SKIP] Insufficient empirical test residuals (", length(empirical_test_residuals), 
            " after filtering,", n_infinite, "infinite/NA out of", n_before_filter, ")\n")
        if (n_infinite > 0) {
          cat("      Sample of non-finite values:", head(empirical_test_residuals[!is.finite(empirical_test_residuals)], 5), "\n")
        }
        next
      }
      
      # Standardize empirical test residuals
      empirical_test_residuals_std <- (empirical_test_residuals - mean(empirical_test_residuals)) / sd(empirical_test_residuals)
      
      cat("    Empirical test residuals: n =", length(empirical_test_residuals_std), 
          "| mean =", round(mean(empirical_test_residuals_std), 4),
          "| sd =", round(sd(empirical_test_residuals_std), 4), "\n")
      
      # Get Standard GARCH residuals (from training set)
      standard_residuals <- engine_residuals(fit, standardize = TRUE)
      standard_residuals <- standard_residuals[!is.na(standard_residuals)]
      
      if (length(standard_residuals) < 10) {
        cat("    [SKIP] Insufficient standard residuals\n")
        next
      }
      
      # Standardize standard residuals
      standard_residuals_std <- (standard_residuals - mean(standard_residuals)) / sd(standard_residuals)
      
      # Sample same number as test set for fair comparison
      n_compare <- min(length(empirical_test_residuals_std), length(standard_residuals_std), 1000)
      standard_residuals_sample <- sample(standard_residuals_std, n_compare, replace = FALSE)
      empirical_test_sample <- empirical_test_residuals_std[1:n_compare]
      
      # Compare Standard GARCH vs Empirical Test
      ks_standard_vs_empirical <- calculate_ks_distance(empirical_test_sample, standard_residuals_sample)
      wasserstein_standard_vs_empirical <- calculate_wasserstein_distance(empirical_test_sample, standard_residuals_sample)
      skew_diff_standard <- abs(calculate_skewness(empirical_test_sample) - calculate_skewness(standard_residuals_sample))
      kurt_diff_standard <- abs(calculate_kurtosis(empirical_test_sample) - calculate_kurtosis(standard_residuals_sample))
      
      # Get NF-generated residuals
      nf_residuals <- NULL
      if (nf_key %in% names(nf_residuals_map)) {
        nf_residuals <- nf_residuals_map[[nf_key]]
        nf_residuals <- nf_residuals[!is.na(nf_residuals)]
        
        if (length(nf_residuals) >= 10) {
          # Standardize NF residuals
          nf_residuals_std <- (nf_residuals - mean(nf_residuals)) / sd(nf_residuals)
          
          # Sample same number as test set
          nf_residuals_sample <- sample(nf_residuals_std, n_compare, replace = FALSE)
          
          # Compare NF-GARCH vs Empirical Test
          ks_nf_vs_empirical <- calculate_ks_distance(empirical_test_sample, nf_residuals_sample)
          wasserstein_nf_vs_empirical <- calculate_wasserstein_distance(empirical_test_sample, nf_residuals_sample)
          skew_diff_nf <- abs(calculate_skewness(empirical_test_sample) - calculate_skewness(nf_residuals_sample))
          kurt_diff_nf <- abs(calculate_kurtosis(empirical_test_sample) - calculate_kurtosis(nf_residuals_sample))
          
          # Determine which is better (lower is better for distances)
          ks_winner <- ifelse(ks_nf_vs_empirical < ks_standard_vs_empirical, "NF", "Standard")
          wasserstein_winner <- ifelse(wasserstein_nf_vs_empirical < wasserstein_standard_vs_empirical, "NF", "Standard")
          skew_winner <- ifelse(skew_diff_nf < skew_diff_standard, "NF", "Standard")
          kurt_winner <- ifelse(kurt_diff_nf < kurt_diff_standard, "NF", "Standard")
          
          cat("    [COMPARISON TO EMPIRICAL TEST RESIDUALS]\n")
          cat("      KS Distance:      Standard =", round(ks_standard_vs_empirical, 4), 
              "| NF =", round(ks_nf_vs_empirical, 4), "| Winner:", ks_winner, "\n")
          cat("      Wasserstein:      Standard =", round(wasserstein_standard_vs_empirical, 4), 
              "| NF =", round(wasserstein_nf_vs_empirical, 4), "| Winner:", wasserstein_winner, "\n")
          cat("      Skewness Diff:    Standard =", round(skew_diff_standard, 4), 
              "| NF =", round(skew_diff_nf, 4), "| Winner:", skew_winner, "\n")
          cat("      Kurtosis Diff:    Standard =", round(kurt_diff_standard, 4), 
              "| NF =", round(kurt_diff_nf, 4), "| Winner:", kurt_winner, "\n")
          
          # Store results
          results_list[[length(results_list) + 1]] <- data.frame(
            Asset = asset_name,
            Model = model_name,
            Test_Size = length(empirical_test_residuals_std),
            # Empirical test residual properties
            Empirical_Skewness = calculate_skewness(empirical_test_sample),
            Empirical_Kurtosis = calculate_kurtosis(empirical_test_sample),
            # Standard GARCH vs Empirical
            KS_Standard_vs_Empirical = ks_standard_vs_empirical,
            Wasserstein_Standard_vs_Empirical = wasserstein_standard_vs_empirical,
            Skew_Diff_Standard = skew_diff_standard,
            Kurt_Diff_Standard = kurt_diff_standard,
            # NF-GARCH vs Empirical
            KS_NF_vs_Empirical = ks_nf_vs_empirical,
            Wasserstein_NF_vs_Empirical = wasserstein_nf_vs_empirical,
            Skew_Diff_NF = skew_diff_nf,
            Kurt_Diff_NF = kurt_diff_nf,
            # Winners
            KS_Winner = ks_winner,
            Wasserstein_Winner = wasserstein_winner,
            Skew_Winner = skew_winner,
            Kurt_Winner = kurt_winner,
            # Improvement indicators
            NF_Better_KS = ks_nf_vs_empirical < ks_standard_vs_empirical,
            NF_Better_Wasserstein = wasserstein_nf_vs_empirical < wasserstein_standard_vs_empirical,
            NF_Better_Skew = skew_diff_nf < skew_diff_standard,
            NF_Better_Kurt = kurt_diff_nf < kurt_diff_standard
          )
        } else {
          cat("    [SKIP] Insufficient NF residuals\n")
        }
      } else {
        cat("    [SKIP] NF residuals not found for", nf_key, "\n")
      }
      
    }, error = function(e) {
      cat("    [ERROR]", e$message, "\n")
    })
  }
}

# =============================================================================
# Aggregate Results
# =============================================================================

if (length(results_list) == 0) {
  cat("\n[ERROR] No results generated. Check data availability.\n")
  quit(status = 1)
}

results_df <- do.call(rbind, results_list)

cat("\n\n=== SUMMARY RESULTS ===\n\n")

# Overall win rates
cat("Overall Win Rates (NF vs Standard in matching empirical test residuals):\n")
cat("  KS Distance:      NF wins", sum(results_df$NF_Better_KS, na.rm = TRUE), 
    "out of", sum(!is.na(results_df$NF_Better_KS)), 
    "(", round(100 * mean(results_df$NF_Better_KS, na.rm = TRUE), 1), "%)\n")
cat("  Wasserstein:       NF wins", sum(results_df$NF_Better_Wasserstein, na.rm = TRUE), 
    "out of", sum(!is.na(results_df$NF_Better_Wasserstein)), 
    "(", round(100 * mean(results_df$NF_Better_Wasserstein, na.rm = TRUE), 1), "%)\n")
cat("  Skewness Match:    NF wins", sum(results_df$NF_Better_Skew, na.rm = TRUE), 
    "out of", sum(!is.na(results_df$NF_Better_Skew)), 
    "(", round(100 * mean(results_df$NF_Better_Skew, na.rm = TRUE), 1), "%)\n")
cat("  Kurtosis Match:    NF wins", sum(results_df$NF_Better_Kurt, na.rm = TRUE), 
    "out of", sum(!is.na(results_df$NF_Better_Kurt)), 
    "(", round(100 * mean(results_df$NF_Better_Kurt, na.rm = TRUE), 1), "%)\n\n")

# Average distances
cat("Average Distances to Empirical Test Residuals:\n")
cat("  KS Distance:\n")
cat("    Standard GARCH:", round(mean(results_df$KS_Standard_vs_Empirical, na.rm = TRUE), 4), "\n")
cat("    NF-GARCH:      ", round(mean(results_df$KS_NF_vs_Empirical, na.rm = TRUE), 4), "\n")
cat("  Wasserstein Distance:\n")
cat("    Standard GARCH:", round(mean(results_df$Wasserstein_Standard_vs_Empirical, na.rm = TRUE), 4), "\n")
cat("    NF-GARCH:      ", round(mean(results_df$Wasserstein_NF_vs_Empirical, na.rm = TRUE), 4), "\n\n")

# By model
cat("Results by Model:\n")
for (model_name in unique(results_df$Model)) {
  model_results <- results_df[results_df$Model == model_name, ]
  cat("\n  ", model_name, ":\n")
  cat("    NF Better KS:      ", round(100 * mean(model_results$NF_Better_KS, na.rm = TRUE), 1), "%\n")
  cat("    NF Better Wasserstein:", round(100 * mean(model_results$NF_Better_Wasserstein, na.rm = TRUE), 1), "%\n")
  cat("    NF Better Skew:    ", round(100 * mean(model_results$NF_Better_Skew, na.rm = TRUE), 1), "%\n")
  cat("    NF Better Kurt:    ", round(100 * mean(model_results$NF_Better_Kurt, na.rm = TRUE), 1), "%\n")
}

# =============================================================================
# Save Results
# =============================================================================

output_dir <- "outputs/evaluation"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_file <- file.path(output_dir, "comparison_to_empirical_test_residuals.xlsx")

# Create workbook
wb <- createWorkbook()

# Full results
addWorksheet(wb, "Full_Results")
writeData(wb, "Full_Results", results_df)

# Summary by model
summary_by_model <- results_df %>%
  group_by(Model) %>%
  summarise(
    n = n(),
    NF_Wins_KS = sum(NF_Better_KS, na.rm = TRUE),
    NF_Wins_Wasserstein = sum(NF_Better_Wasserstein, na.rm = TRUE),
    NF_Wins_Skew = sum(NF_Better_Skew, na.rm = TRUE),
    NF_Wins_Kurt = sum(NF_Better_Kurt, na.rm = TRUE),
    Mean_KS_Standard = mean(KS_Standard_vs_Empirical, na.rm = TRUE),
    Mean_KS_NF = mean(KS_NF_vs_Empirical, na.rm = TRUE),
    Mean_Wasserstein_Standard = mean(Wasserstein_Standard_vs_Empirical, na.rm = TRUE),
    Mean_Wasserstein_NF = mean(Wasserstein_NF_vs_Empirical, na.rm = TRUE),
    .groups = "drop"
  )

addWorksheet(wb, "Summary_by_Model")
writeData(wb, "Summary_by_Model", summary_by_model)

# Summary by asset
summary_by_asset <- results_df %>%
  group_by(Asset) %>%
  summarise(
    n = n(),
    NF_Wins_KS = sum(NF_Better_KS, na.rm = TRUE),
    NF_Wins_Wasserstein = sum(NF_Better_Wasserstein, na.rm = TRUE),
    NF_Wins_Skew = sum(NF_Better_Skew, na.rm = TRUE),
    NF_Wins_Kurt = sum(NF_Better_Kurt, na.rm = TRUE),
    Mean_KS_Standard = mean(KS_Standard_vs_Empirical, na.rm = TRUE),
    Mean_KS_NF = mean(KS_NF_vs_Empirical, na.rm = TRUE),
    Mean_Wasserstein_Standard = mean(Wasserstein_Standard_vs_Empirical, na.rm = TRUE),
    Mean_Wasserstein_NF = mean(Wasserstein_NF_vs_Empirical, na.rm = TRUE),
    .groups = "drop"
  )

addWorksheet(wb, "Summary_by_Asset")
writeData(wb, "Summary_by_Asset", summary_by_asset)

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("\n\nResults saved to:", output_file, "\n")
cat("=== ANALYSIS COMPLETE ===\n")
