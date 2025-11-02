#!/usr/bin/env Rscript
# Calculate Distributional Metrics for GARCH Models
# Required metrics: KS distance, Wasserstein distance, Tail index, Skewness, Kurtosis

library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

# Load transport package for Wasserstein distance
if (!require(transport)) {
  install.packages("transport")
  library(transport)
}

cat("=== CALCULATING DISTRIBUTIONAL METRICS ===\n\n")

# =============================================================================
# Helper Functions
# =============================================================================

# Calculate Kolmogorov-Smirnov distance
calculate_ks_distance <- function(actual, predicted) {
  tryCatch({
    ks_test <- ks.test(actual, predicted)
    return(ks_test$statistic)
  }, error = function(e) {
    return(NA)
  })
}

# Calculate Wasserstein-1 distance
calculate_wasserstein_distance <- function(actual, predicted) {
  tryCatch({
    # Try to use transport package if available
    if (requireNamespace("transport", quietly = TRUE)) {
      wd <- transport::wasserstein1d(actual, predicted)
      return(wd)
    }
  }, error = function(e) {
    # Continue to manual calculation
  })
  
  # Manual calculation (always available)
  tryCatch({
    sorted_actual <- sort(actual)
    sorted_pred <- sort(predicted)
    n <- length(sorted_actual)
    m <- length(sorted_pred)
    
    # Ensure same length for comparison
    min_len <- min(n, m)
    if (min_len == 0) return(NA)
    
    # Simple approximation: sort both, align, take mean absolute difference
    sorted_actual <- sort(sorted_actual)[1:min_len]
    sorted_pred <- sort(sorted_pred)[1:min_len]
    
    wd <- mean(abs(sorted_actual - sorted_pred), na.rm = TRUE)
    return(wd)
  }, error = function(e2) {
    return(NA)
  })
}

# Calculate tail index (Hill estimator)
calculate_tail_index <- function(data, k = NULL) {
  tryCatch({
    # Sort absolute values
    abs_data <- abs(data)
    sorted <- sort(abs_data, decreasing = TRUE)
    
    # Use top k observations (default: top 10%)
    if (is.null(k)) {
      k <- max(floor(length(sorted) * 0.1), 5)
    }
    k <- min(k, length(sorted) - 1)
    
    if (k < 2) return(NA)
    
    # Hill estimator
    log_threshold <- log(sorted[k + 1])
    log_ratios <- log(sorted[1:k]) - log_threshold
    
    # Tail index (inverse of Hill estimator)
    hill_estimator <- mean(log_ratios)
    if (hill_estimator <= 0) return(NA)
    
    tail_index <- 1 / hill_estimator
    return(tail_index)
  }, error = function(e) {
    return(NA)
  })
}

# Calculate skewness
calculate_skewness <- function(data) {
  tryCatch({
    # Try moments package if available
    if (requireNamespace("moments", quietly = TRUE)) {
      return(moments::skewness(data, na.rm = TRUE))
    }
  }, error = function(e) {
    # Continue to manual calculation
  })
  
  # Manual calculation (always available)
  tryCatch({
    mean_val <- mean(data, na.rm = TRUE)
    sd_val <- sd(data, na.rm = TRUE)
    if (is.na(sd_val) || sd_val == 0) return(NA)
    skew <- mean(((data - mean_val) / sd_val)^3, na.rm = TRUE)
    return(skew)
  }, error = function(e) {
    return(NA)
  })
}

# Calculate kurtosis
calculate_kurtosis <- function(data) {
  tryCatch({
    # Try moments package if available
    if (requireNamespace("moments", quietly = TRUE)) {
      return(moments::kurtosis(data, na.rm = TRUE))
    }
  }, error = function(e) {
    # Continue to manual calculation
  })
  
  # Manual calculation (always available)
  tryCatch({
    mean_val <- mean(data, na.rm = TRUE)
    sd_val <- sd(data, na.rm = TRUE)
    if (is.na(sd_val) || sd_val == 0) return(NA)
    kurt <- mean(((data - mean_val) / sd_val)^4, na.rm = TRUE) - 3  # Excess kurtosis
    return(kurt)
  }, error = function(e) {
    return(NA)
  })
}

# =============================================================================
# Load Data
# =============================================================================

cat("Loading results...\n")

# Load NF-GARCH results
nf_results_file <- "results/consolidated/NF_GARCH_Results_manual.xlsx"
if (!file.exists(nf_results_file)) {
  cat("[WARNING] NF-GARCH results not found. Calculating metrics from available data.\n")
  nf_results <- NULL
} else {
  nf_chrono <- read.xlsx(nf_results_file, sheet = "Chrono_Split_NF_GARCH")
  nf_tscv <- read.xlsx(nf_results_file, sheet = "TS_CV_NF_GARCH")
  cat("[OK] Loaded NF-GARCH results\n")
}

# Load standard GARCH results (if available)
standard_results_file <- "results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx"
if (file.exists(standard_results_file)) {
  standard_results <- read.xlsx(standard_results_file, sheet = "Combined_Results")
  cat("[OK] Loaded standard GARCH results\n")
} else {
  standard_results <- NULL
}

# Load residuals for distributional analysis
residuals_dir <- "outputs/manual/residuals_by_model"
nf_residuals_dir <- "outputs/manual/nf_models"

# =============================================================================
# Calculate Distributional Metrics
# =============================================================================

cat("\n=== CALCULATING DISTRIBUTIONAL METRICS ===\n")

distributional_results <- list()

# Process each model and asset combination
models <- c("sGARCH", "eGARCH", "TGARCH", "gjrGARCH")
assets <- c("EURUSD", "GBPUSD", "USDZAR", "NVDA", "MSFT", "AMZN")

for (model_name in models) {
  for (asset_name in assets) {
    cat("\nProcessing:", model_name, "-", asset_name, "\n")
    
    # Load standard residuals
    standard_residual_file <- file.path(residuals_dir, model_name, 
                                       paste0(asset_name, "_Manual_Optimized_residuals.csv"))
    
    # Load NF residuals
    nf_residual_file <- file.path(nf_residuals_dir, 
                                  paste0(model_name, "_", asset_name, "_synthetic_residuals.csv"))
    
    metrics <- data.frame(
      Model = model_name,
      Asset = asset_name,
      KS_distance = NA,
      Wasserstein_distance = NA,
      Tail_index = NA,
      Skewness = NA,
      Kurtosis = NA
    )
    
    # Calculate metrics from standard residuals
    if (file.exists(standard_residual_file)) {
      standard_residuals <- read.csv(standard_residual_file, header = FALSE)[[1]]
      standard_residuals <- as.numeric(standard_residuals[!is.na(standard_residuals)])
      
      if (length(standard_residuals) > 10) {
        metrics$Tail_index <- calculate_tail_index(standard_residuals)
        metrics$Skewness <- calculate_skewness(standard_residuals)
        metrics$Kurtosis <- calculate_kurtosis(standard_residuals)
      }
    }
    
    # Compare standard vs NF residuals if both available
    if (file.exists(standard_residual_file) && file.exists(nf_residual_file)) {
      tryCatch({
        # Read CSV files (handle headers if present)
        standard_data <- read.csv(standard_residual_file, header = FALSE)
        nf_data <- read.csv(nf_residual_file, header = FALSE)
        
        # Extract first column (handle header row if present)
        standard_residuals <- standard_data[[1]]
        nf_residuals <- nf_data[[1]]
        
        # Skip header row if it's character (e.g., "residual" or "synthetic_residuals")
        if (is.character(standard_residuals[1]) && 
            (standard_residuals[1] == "residual" || 
             standard_residuals[1] == "synthetic_residuals" ||
             grepl("residual", standard_residuals[1], ignore.case = TRUE))) {
          standard_residuals <- standard_residuals[-1]
        }
        if (is.character(nf_residuals[1]) && 
            (nf_residuals[1] == "residual" || 
             nf_residuals[1] == "synthetic_residuals" ||
             grepl("residual", nf_residuals[1], ignore.case = TRUE))) {
          nf_residuals <- nf_residuals[-1]
        }
        
        standard_residuals <- as.numeric(standard_residuals[!is.na(standard_residuals)])
        nf_residuals <- as.numeric(nf_residuals[!is.na(nf_residuals)])
        
        if (length(standard_residuals) > 10 && length(nf_residuals) > 10) {
          # Standardize both
          standard_residuals <- (standard_residuals - mean(standard_residuals)) / sd(standard_residuals)
          nf_residuals <- (nf_residuals - mean(nf_residuals)) / sd(nf_residuals)
          
          metrics$KS_distance <- calculate_ks_distance(standard_residuals, nf_residuals)
          metrics$Wasserstein_distance <- calculate_wasserstein_distance(standard_residuals, nf_residuals)
        } else {
          cat("  [WARNING] Insufficient data for", model_name, "-", asset_name, "\n")
        }
      }, error = function(e) {
        cat("  [WARNING] Error calculating KS/Wasserstein for", model_name, "-", asset_name, ":", e$message, "\n")
      })
    } else {
      if (!file.exists(standard_residual_file)) {
        cat("  [WARNING] Standard residuals missing:", standard_residual_file, "\n")
      }
      if (!file.exists(nf_residual_file)) {
        cat("  [WARNING] NF residuals missing:", nf_residual_file, "\n")
      }
    }
    
    distributional_results[[paste(model_name, asset_name, sep = "_")]] <- metrics
  }
}

# Combine results
distributional_df <- bind_rows(distributional_results)

cat("\n[OK] Distributional metrics calculated\n")
cat("  Total model-asset combinations:", nrow(distributional_df), "\n")

# =============================================================================
# Summary Statistics
# =============================================================================

cat("\n=== SUMMARY STATISTICS ===\n")

summary_stats <- distributional_df %>%
  group_by(Model) %>%
  summarise(
    mean_KS = mean(KS_distance, na.rm = TRUE),
    median_KS = median(KS_distance, na.rm = TRUE),
    mean_Wasserstein = mean(Wasserstein_distance, na.rm = TRUE),
    median_Wasserstein = median(Wasserstein_distance, na.rm = TRUE),
    mean_Tail_index = mean(Tail_index, na.rm = TRUE),
    mean_Skewness = mean(Skewness, na.rm = TRUE),
    mean_Kurtosis = mean(Kurtosis, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)

# =============================================================================
# Save Results
# =============================================================================

cat("\n=== SAVING RESULTS ===\n")

output_file <- "results/consolidated/Distributional_Metrics.xlsx"
wb <- createWorkbook()

addWorksheet(wb, "Distributional_Metrics")
writeData(wb, "Distributional_Metrics", distributional_df)

addWorksheet(wb, "Summary_Statistics")
writeData(wb, "Summary_Statistics", summary_stats)

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("[OK] Results saved to:", output_file, "\n")
cat("\n=== DISTRIBUTIONAL METRICS CALCULATION COMPLETE ===\n")

