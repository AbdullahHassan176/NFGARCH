#!/usr/bin/env Rscript
# Calculate Distributional Metrics for GARCH Models
# Required metrics: KS distance, Wasserstein distance, Tail index, Skewness, Kurtosis

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

# Load transport package for Wasserstein distance
if (!require(transport)) {
  install.packages("transport")
  library(transport)
}

# Load split-specific configuration (handles --split parameter)
source("scripts/evaluation/evaluation_split_config.R")

# Load manual config for asset lists
source("scripts/manual/manual_optimized_config.R")

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
nf_results_file <- file.path(RESULTS_BASE, "consolidated", paste0("NF_GARCH_Results_", EVAL_SPLIT_MODE, ".xlsx"))
if (!file.exists(nf_results_file)) {
  cat("[WARNING] NF-GARCH results not found. Calculating metrics from available data.\n")
  nf_results <- NULL
} else {
  nf_chrono <- read.xlsx(nf_results_file, sheet = "Chrono_Split_NF_GARCH")
  sheets_nf <- getSheetNames(nf_results_file)
  nf_tscv <- if ("TS_CV_NF_GARCH" %in% sheets_nf) {
    read.xlsx(nf_results_file, sheet = "TS_CV_NF_GARCH")
  } else {
    data.frame()
  }
  cat("[OK] Loaded NF-GARCH results\n")
}

# Load standard GARCH results (if available)
standard_results_file <- file.path(RESULTS_BASE, "consolidated", "NF_vs_Standard_GARCH_Comparison.xlsx")
if (file.exists(standard_results_file)) {
  standard_results <- read.xlsx(standard_results_file, sheet = "Combined_Results")
  cat("[OK] Loaded standard GARCH results\n")
} else {
  standard_results <- NULL
}

# Load residuals for distributional analysis - use split-aware paths
residuals_dir <- EVAL_PATHS$residuals
nf_residuals_dir <- EVAL_PATHS$nf_models

# =============================================================================
# Calculate Distributional Metrics
# =============================================================================

cat("\n=== CALCULATING DISTRIBUTIONAL METRICS ===\n")

distributional_results <- list()

# Process each model and asset combination
models <- c("sGARCH", "eGARCH", "TGARCH", "gjrGARCH")
# Get assets from centralized config
assets <- get_manual_assets()

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
      Tail_index_Std = NA,
      Skewness_Std = NA,
      Kurtosis_Std = NA,
      Tail_index_NF = NA,
      Skewness_NF = NA,
      Kurtosis_NF = NA
    )
    
    # Load and process standard residuals
    standard_residuals <- NULL
    if (file.exists(standard_residual_file)) {
      tryCatch({
        standard_data <- read.csv(standard_residual_file, header = FALSE)
        standard_residuals <- standard_data[[1]]
        
        # Skip header row if it's character
        if (is.character(standard_residuals[1]) && 
            (standard_residuals[1] == "residual" || 
             standard_residuals[1] == "synthetic_residuals" ||
             grepl("residual", standard_residuals[1], ignore.case = TRUE))) {
          standard_residuals <- standard_residuals[-1]
        }
        
        standard_residuals <- as.numeric(standard_residuals[!is.na(standard_residuals)])
        
        if (length(standard_residuals) > 10) {
          # Standardize
          standard_residuals_std <- (standard_residuals - mean(standard_residuals)) / sd(standard_residuals)
          
          # Calculate properties of standard residuals
          metrics$Tail_index_Std <- calculate_tail_index(standard_residuals_std)
          metrics$Skewness_Std <- calculate_skewness(standard_residuals_std)
          metrics$Kurtosis_Std <- calculate_kurtosis(standard_residuals_std)
        }
      }, error = function(e) {
        cat("  [WARNING] Error loading standard residuals:", e$message, "\n")
      })
    }
    
    # Load and process NF residuals
    nf_residuals <- NULL
    if (file.exists(nf_residual_file)) {
      tryCatch({
        nf_data <- read.csv(nf_residual_file, header = FALSE)
        nf_residuals <- nf_data[[1]]
        
        # Skip header row if it's character
        if (is.character(nf_residuals[1]) && 
            (nf_residuals[1] == "residual" || 
             nf_residuals[1] == "synthetic_residuals" ||
             grepl("residual", nf_residuals[1], ignore.case = TRUE))) {
          nf_residuals <- nf_residuals[-1]
        }
        
        nf_residuals <- as.numeric(nf_residuals[!is.na(nf_residuals)])
        
        if (length(nf_residuals) > 10) {
          # Standardize
          nf_residuals_std <- (nf_residuals - mean(nf_residuals)) / sd(nf_residuals)
          
          # Calculate properties of NF residuals
          metrics$Tail_index_NF <- calculate_tail_index(nf_residuals_std)
          metrics$Skewness_NF <- calculate_skewness(nf_residuals_std)
          metrics$Kurtosis_NF <- calculate_kurtosis(nf_residuals_std)
        }
      }, error = function(e) {
        cat("  [WARNING] Error loading NF residuals:", e$message, "\n")
      })
    }
    
    # Compare standard vs NF residuals if both available
    if (!is.null(standard_residuals) && !is.null(nf_residuals) && 
        length(standard_residuals) > 10 && length(nf_residuals) > 10) {
      tryCatch({
        # Standardize both for comparison
        standard_residuals_std <- (standard_residuals - mean(standard_residuals)) / sd(standard_residuals)
        nf_residuals_std <- (nf_residuals - mean(nf_residuals)) / sd(nf_residuals)
        
        metrics$KS_distance <- calculate_ks_distance(standard_residuals_std, nf_residuals_std)
        metrics$Wasserstein_distance <- calculate_wasserstein_distance(standard_residuals_std, nf_residuals_std)
      }, error = function(e) {
        cat("  [WARNING] Error calculating KS/Wasserstein for", model_name, "-", asset_name, ":", e$message, "\n")
      })
    } else {
      if (is.null(standard_residuals) || length(standard_residuals) <= 10) {
        cat("  [WARNING] Standard residuals missing or insufficient:", standard_residual_file, "\n")
      }
      if (is.null(nf_residuals) || length(nf_residuals) <= 10) {
        cat("  [WARNING] NF residuals missing or insufficient:", nf_residual_file, "\n")
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
    mean_Tail_index_Std = mean(Tail_index_Std, na.rm = TRUE),
    mean_Skewness_Std = mean(Skewness_Std, na.rm = TRUE),
    mean_Kurtosis_Std = mean(Kurtosis_Std, na.rm = TRUE),
    mean_Tail_index_NF = mean(Tail_index_NF, na.rm = TRUE),
    mean_Skewness_NF = mean(Skewness_NF, na.rm = TRUE),
    mean_Kurtosis_NF = mean(Kurtosis_NF, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)

# =============================================================================
# Save Results
# =============================================================================

cat("\n=== SAVING RESULTS ===\n")

output_file <- file.path(RESULTS_BASE, "consolidated", "Distributional_Metrics.xlsx")
wb <- createWorkbook()

addWorksheet(wb, "Distributional_Metrics")
writeData(wb, "Distributional_Metrics", distributional_df)

addWorksheet(wb, "Summary_Statistics")
writeData(wb, "Summary_Statistics", summary_stats)

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("[OK] Results saved to:", output_file, "\n")
cat("\n=== DISTRIBUTIONAL METRICS CALCULATION COMPLETE ===\n")

