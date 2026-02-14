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
nf_results_file <- paste(RESULTS_BASE, "consolidated", paste0("NF_GARCH_Results_", EVAL_SPLIT_MODE, ".xlsx"), sep="/")
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
standard_results_file <- paste(RESULTS_BASE, "consolidated", "NF_vs_Standard_GARCH_Comparison.xlsx", sep="/")
if (file.exists(standard_results_file)) {
  standard_results <- read.xlsx(standard_results_file, sheet = "Combined_Results")
  cat("[OK] Loaded standard GARCH results\n")
} else {
  standard_results <- NULL
}

# Load residuals for distributional analysis - use split-aware paths
residuals_dir <- EVAL_PATHS$residuals
nf_residuals_dir <- EVAL_PATHS$nf_models

# Map folder/config names to base model (e.g. sGARCH_std -> sGARCH, TGARCH_std -> TGARCH)
base_model_from_name <- function(name) {
  sub("_(std|norm|sstd)$", "", name, ignore.case = TRUE)
}

# Discover standard residual files: residuals_by_model/{model_folder}/{asset}_Manual_Optimized_residuals.csv
# Returns list of (base_model, asset) -> path (one path per pair; if multiple folders map to same base, first wins)
discover_standard_residuals <- function() {
  out <- list()
  if (!dir.exists(residuals_dir)) return(out)
  subdirs <- list.dirs(residuals_dir, full.names = FALSE, recursive = FALSE)
  for (sub in subdirs) {
    base <- base_model_from_name(sub)
    dir_path <- file.path(residuals_dir, sub)
    files <- list.files(dir_path, pattern = "_Manual_Optimized_residuals\\.csv$", full.names = TRUE)
    for (f in files) {
      asset <- sub("_Manual_Optimized_residuals\\.csv$", "", basename(f))
      key <- paste(base, asset, sep = "_")
      if (!key %in% names(out)) out[[key]] <- list(base_model = base, asset = asset, path = f)
    }
  }
  out
}

# Discover NF residual files: nf_models/*_synthetic_residuals.csv
# Supports MODEL_ASSET (e.g. eGARCH_AMZN) and MODEL_DIST_ASSET (e.g. sGARCH_std_AMZN, sGARCH_norm_AMZN)
discover_nf_residuals <- function() {
  out <- list()
  if (!dir.exists(nf_residuals_dir)) return(out)
  files <- list.files(nf_residuals_dir, pattern = "_synthetic_residuals\\.csv$", full.names = TRUE)
  for (f in files) {
    fname_clean <- stringr::str_replace(basename(f), "_synthetic_residuals\\.csv$", "")
    parts <- strsplit(fname_clean, "_")[[1]]
    if (length(parts) == 2) {
      base <- parts[1]
      asset <- parts[2]
    } else if (length(parts) >= 3) {
      base <- base_model_from_name(paste(parts[1:(length(parts)-1)], collapse = "_"))
      asset <- parts[length(parts)]
    } else next
    key <- paste(base, asset, sep = "_")
    if (!key %in% names(out)) out[[key]] <- list(base_model = base, asset = asset, path = f)
  }
  out
}

# Load residual vector from CSV (handles header row)
load_residuals <- function(path) {
  tryCatch({
    data <- read.csv(path, header = FALSE, stringsAsFactors = FALSE)
    vec <- data[[1]]
    if (is.character(vec[1]) && grepl("residual", vec[1], ignore.case = TRUE))
      vec <- vec[-1]
    vec <- as.numeric(vec[!is.na(vec)])
    if (length(vec) > 10) vec else NULL
  }, error = function(e) NULL)
}

# =============================================================================
# Calculate Distributional Metrics
# =============================================================================

cat("\n=== CALCULATING DISTRIBUTIONAL METRICS ===\n")

standard_map <- discover_standard_residuals()
nf_map <- discover_nf_residuals()
cat("  Discovered", length(standard_map), "standard residual files\n")
cat("  Discovered", length(nf_map), "NF residual files\n")

# Process each (base_model, asset) that has both standard and NF residuals
distributional_results <- list()
keys_std <- names(standard_map)
keys_nf <- names(nf_map)
keys_both <- intersect(keys_std, keys_nf)
cat("  Pairs with both standard and NF:", length(keys_both), "\n")

for (key in keys_both) {
  std_info <- standard_map[[key]]
  nf_info <- nf_map[[key]]
  model_name <- std_info$base_model
  asset_name <- std_info$asset
  cat("\nProcessing:", model_name, "-", asset_name, "\n")
  
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
  
  standard_residuals <- load_residuals(std_info$path)
  nf_residuals <- load_residuals(nf_info$path)
  
  if (!is.null(standard_residuals)) {
    standard_residuals_std <- (standard_residuals - mean(standard_residuals)) / sd(standard_residuals)
    metrics$Tail_index_Std <- calculate_tail_index(standard_residuals_std)
    metrics$Skewness_Std <- calculate_skewness(standard_residuals_std)
    metrics$Kurtosis_Std <- calculate_kurtosis(standard_residuals_std)
  }
  
  if (!is.null(nf_residuals)) {
    nf_residuals_std <- (nf_residuals - mean(nf_residuals)) / sd(nf_residuals)
    metrics$Tail_index_NF <- calculate_tail_index(nf_residuals_std)
    metrics$Skewness_NF <- calculate_skewness(nf_residuals_std)
    metrics$Kurtosis_NF <- calculate_kurtosis(nf_residuals_std)
  }
  
  if (!is.null(standard_residuals) && !is.null(nf_residuals) &&
      length(standard_residuals) > 10 && length(nf_residuals) > 10) {
    tryCatch({
      standard_residuals_std <- (standard_residuals - mean(standard_residuals)) / sd(standard_residuals)
      nf_residuals_std <- (nf_residuals - mean(nf_residuals)) / sd(nf_residuals)
      metrics$KS_distance <- calculate_ks_distance(standard_residuals_std, nf_residuals_std)
      metrics$Wasserstein_distance <- calculate_wasserstein_distance(standard_residuals_std, nf_residuals_std)
    }, error = function(e) {
      cat("  [WARNING] Error calculating KS/Wasserstein:", e$message, "\n")
    })
  }
  
  distributional_results[[key]] <- metrics
}

# Also add rows for (model, asset) pairs that have only standard or only NF (so we don't drop assets)
# Use keys that appear in either set but not both, to record what we have
all_keys <- unique(c(keys_std, keys_nf))
for (key in all_keys) {
  if (key %in% names(distributional_results)) next
  info <- standard_map[[key]]
  if (is.null(info)) info <- nf_map[[key]]
  if (is.null(info)) next
  model_name <- info$base_model
  asset_name <- info$asset
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
  if (key %in% keys_std) {
    std_res <- load_residuals(standard_map[[key]]$path)
    if (!is.null(std_res) && length(std_res) > 10) {
      std_std <- (std_res - mean(std_res)) / sd(std_res)
      metrics$Tail_index_Std <- calculate_tail_index(std_std)
      metrics$Skewness_Std <- calculate_skewness(std_std)
      metrics$Kurtosis_Std <- calculate_kurtosis(std_std)
    }
  }
  if (key %in% keys_nf) {
    nf_res <- load_residuals(nf_map[[key]]$path)
    if (!is.null(nf_res) && length(nf_res) > 10) {
      nf_std <- (nf_res - mean(nf_res)) / sd(nf_res)
      metrics$Tail_index_NF <- calculate_tail_index(nf_std)
      metrics$Skewness_NF <- calculate_skewness(nf_std)
      metrics$Kurtosis_NF <- calculate_kurtosis(nf_std)
    }
  }
  distributional_results[[key]] <- metrics
}

# Combine results
distributional_df <- bind_rows(distributional_results)
if (nrow(distributional_df) == 0) {
  distributional_df <- data.frame(
    Model = character(), Asset = character(),
    KS_distance = numeric(), Wasserstein_distance = numeric(),
    Tail_index_Std = numeric(), Skewness_Std = numeric(), Kurtosis_Std = numeric(),
    Tail_index_NF = numeric(), Skewness_NF = numeric(), Kurtosis_NF = numeric()
  )
}

cat("\n[OK] Distributional metrics calculated\n")
cat("  Total model-asset combinations:", nrow(distributional_df), "\n")

# =============================================================================
# Summary Statistics
# =============================================================================

cat("\n=== SUMMARY STATISTICS ===\n")

summary_stats <- if (nrow(distributional_df) > 0) {
  distributional_df %>%
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
} else {
  data.frame(
    Model = character(), mean_KS = numeric(), median_KS = numeric(),
    mean_Wasserstein = numeric(), median_Wasserstein = numeric(),
    mean_Tail_index_Std = numeric(), mean_Skewness_Std = numeric(), mean_Kurtosis_Std = numeric(),
    mean_Tail_index_NF = numeric(), mean_Skewness_NF = numeric(), mean_Kurtosis_NF = numeric()
  )
}

print(summary_stats)

# =============================================================================
# Save Results
# =============================================================================

cat("\n=== SAVING RESULTS ===\n")

output_file <- paste(RESULTS_BASE, "consolidated", "Distributional_Metrics.xlsx", sep="/")
wb <- createWorkbook()

addWorksheet(wb, "Distributional_Metrics")
writeData(wb, "Distributional_Metrics", distributional_df)

addWorksheet(wb, "Summary_Statistics")
writeData(wb, "Summary_Statistics", summary_stats)

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("[OK] Results saved to:", output_file, "\n")
cat("\n=== DISTRIBUTIONAL METRICS CALCULATION COMPLETE ===\n")

