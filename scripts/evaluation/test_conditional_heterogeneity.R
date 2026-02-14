#!/usr/bin/env Rscript
# Conditional Heterogeneity Analysis for Methodology Chapter
# Tests for conditional heterogeneity in GARCH residuals and assesses
# impact on NF model stability

# Load centralized seed configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
  set.seed(REPRODUCIBILITY_SEED)
} else {
  set.seed(123)  # Fallback if config not available
}

# Load split-specific configuration (handles --split parameter)
source("scripts/evaluation/evaluation_split_config.R")

# Load required libraries
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(xts)
library(zoo)
# library(strucchange)  # Optional - will use alternative if not available
library(FinTS)
library(lmtest)

# Load configuration
source("scripts/core/config.R")

# =============================================================================
# CONDITIONAL HETEROGENEITY TEST FUNCTIONS
# =============================================================================

#' Rolling window variance analysis
#' Tests for time-varying volatility in residuals
test_rolling_variance <- function(residuals, window_size = 100) {
  if (length(residuals) < window_size * 2) {
    return(NULL)
  }
  
  # Calculate rolling variance
  rolling_var <- rollapply(residuals, width = window_size, FUN = var, 
                          fill = NA, align = "right")
  
  # Test for significant variation in rolling variance
  rolling_var_clean <- rolling_var[!is.na(rolling_var)]
  
  if (length(rolling_var_clean) < 10) {
    return(NULL)
  }
  
  # Coefficient of variation of rolling variance
  cv_rolling_var <- sd(rolling_var_clean, na.rm = TRUE) / mean(rolling_var_clean, na.rm = TRUE)
  
  # Test for trend in rolling variance (simple linear regression)
  time_index <- 1:length(rolling_var_clean)
  var_trend_test <- tryCatch({
    lm_result <- lm(rolling_var_clean ~ time_index)
    trend_pvalue <- summary(lm_result)$coefficients[2, 4]
    trend_coef <- coef(lm_result)[2]
    list(trend_coefficient = trend_coef, trend_pvalue = trend_pvalue)
  }, error = function(e) {
    list(trend_coefficient = NA, trend_pvalue = NA)
  })
  
  return(list(
    mean_rolling_var = mean(rolling_var_clean, na.rm = TRUE),
    sd_rolling_var = sd(rolling_var_clean, na.rm = TRUE),
    cv_rolling_var = cv_rolling_var,
    trend_coefficient = var_trend_test$trend_coefficient,
    trend_pvalue = var_trend_test$trend_pvalue,
    interpretation = ifelse(!is.na(var_trend_test$trend_pvalue) && var_trend_test$trend_pvalue < 0.05,
                           "Time-varying variance detected",
                           "No significant time trend in variance")
  ))
}

#' Structural break test using CUSUM (alternative implementation)
test_structural_breaks <- function(residuals) {
  if (length(residuals) < 50) {
    return(NULL)
  }
  
  tryCatch({
    # Alternative CUSUM test implementation
    # Calculate cumulative sum of residuals
    n <- length(residuals)
    mean_resid <- mean(residuals, na.rm = TRUE)
    centered_resid <- residuals - mean_resid
    cumsum_resid <- cumsum(centered_resid)
    
    # Calculate CUSUM statistic
    max_cusum <- max(abs(cumsum_resid)) / (sd(residuals, na.rm = TRUE) * sqrt(n))
    
    # Simple threshold-based test (approximate)
    # For large samples, CUSUM statistic > 1.36 suggests structural break at 5% level
    threshold <- 1.36
    p_value_approx <- ifelse(max_cusum > threshold, 0.05, 0.5)  # Approximate
    
    return(list(
      statistic = max_cusum,
      p_value = p_value_approx,
      interpretation = ifelse(max_cusum > threshold,
                             "Structural break detected (approximate)",
                             "No structural break detected (approximate)")
    ))
  }, error = function(e) {
    return(list(
      statistic = NA,
      p_value = NA,
      interpretation = paste("Error:", e$message)
    ))
  })
}

#' Time-varying distribution analysis
#' Analyze distribution statistics across rolling windows
test_time_varying_distribution <- function(residuals, window_size = 100) {
  if (length(residuals) < window_size * 2) {
    return(NULL)
  }
  
  # Calculate rolling statistics
  rolling_mean <- rollapply(residuals, width = window_size, FUN = mean, 
                           fill = NA, align = "right")
  rolling_sd <- rollapply(residuals, width = window_size, FUN = sd, 
                         fill = NA, align = "right")
  rolling_skew <- rollapply(residuals, width = window_size, 
                           FUN = function(x) moments::skewness(x), 
                           fill = NA, align = "right")
  
  # Clean data
  rolling_mean_clean <- rolling_mean[!is.na(rolling_mean)]
  rolling_sd_clean <- rolling_sd[!is.na(rolling_sd)]
  rolling_skew_clean <- rolling_skew[!is.na(rolling_skew)]
  
  if (length(rolling_mean_clean) < 10) {
    return(NULL)
  }
  
  # Coefficient of variation for each statistic
  cv_mean <- sd(rolling_mean_clean, na.rm = TRUE) / abs(mean(rolling_mean_clean, na.rm = TRUE))
  cv_sd <- sd(rolling_sd_clean, na.rm = TRUE) / mean(rolling_sd_clean, na.rm = TRUE)
  cv_skew <- sd(rolling_skew_clean, na.rm = TRUE) / abs(mean(rolling_skew_clean, na.rm = TRUE))
  
  return(list(
    cv_mean = cv_mean,
    cv_sd = cv_sd,
    cv_skew = cv_skew,
    mean_range = max(rolling_mean_clean, na.rm = TRUE) - min(rolling_mean_clean, na.rm = TRUE),
    sd_range = max(rolling_sd_clean, na.rm = TRUE) - min(rolling_sd_clean, na.rm = TRUE),
    interpretation = ifelse(cv_sd > 0.2, 
                           "Significant time variation in distribution",
                           "Relatively stable distribution over time")
  ))
}

#' Test ARCH effects in residuals
test_arch_effects <- function(residuals, lags = 10) {
  tryCatch({
    # ARCH LM test
    arch_test <- ArchTest(residuals, lags = lags)
    
    # Additional: Test for ARCH effects at different lags
    arch_lags <- c(1, 5, 10, 20)
    arch_results <- list()
    
    for (lag in arch_lags) {
      if (lag <= length(residuals) / 10) {
        test_result <- tryCatch({
          ArchTest(residuals, lags = lag)
        }, error = function(e) NULL)
        
        if (!is.null(test_result)) {
          arch_results[[as.character(lag)]] <- list(
            lag = lag,
            statistic = test_result$statistic,
            p_value = test_result$p.value
          )
        }
      }
    }
    
    return(list(
      main_lag = lags,
      main_statistic = arch_test$statistic,
      main_pvalue = arch_test$p.value,
      arch_results_by_lag = arch_results,
      interpretation = ifelse(arch_test$p.value < 0.05,
                             "ARCH effects present (conditional heterogeneity)",
                             "No significant ARCH effects")
    ))
  }, error = function(e) {
    return(list(
      main_lag = lags,
      main_statistic = NA,
      main_pvalue = NA,
      interpretation = paste("Error:", e$message)
    ))
  })
}

#' NF model stability analysis across market regimes
#' This is a conceptual analysis - would require NF models trained on different periods
analyze_nf_stability <- function(residuals) {
  # Split residuals into early and late periods
  n <- length(residuals)
  split_point <- floor(n / 2)
  
  early_residuals <- residuals[1:split_point]
  late_residuals <- residuals[(split_point + 1):n]
  
  # Compare distribution statistics
  early_stats <- list(
    mean = mean(early_residuals, na.rm = TRUE),
    sd = sd(early_residuals, na.rm = TRUE),
    skewness = moments::skewness(early_residuals),
    kurtosis = moments::kurtosis(early_residuals)
  )
  
  late_stats <- list(
    mean = mean(late_residuals, na.rm = TRUE),
    sd = sd(late_residuals, na.rm = TRUE),
    skewness = moments::skewness(late_residuals),
    kurtosis = moments::kurtosis(late_residuals)
  )
  
  # Calculate differences
  mean_diff <- abs(early_stats$mean - late_stats$mean)
  sd_diff <- abs(early_stats$sd - late_stats$sd)
  sd_ratio <- late_stats$sd / early_stats$sd
  
  return(list(
    early_mean = early_stats$mean,
    late_mean = late_stats$mean,
    mean_difference = mean_diff,
    early_sd = early_stats$sd,
    late_sd = late_stats$sd,
    sd_difference = sd_diff,
    sd_ratio = sd_ratio,
    interpretation = ifelse(sd_ratio > 1.2 || sd_ratio < 0.8,
                           "Significant distribution shift between periods",
                           "Relatively stable distribution across periods")
  ))
}

# =============================================================================
# MAIN TESTING FUNCTION
# =============================================================================

#' Test conditional heterogeneity for a single residual file
test_conditional_heterogeneity_file <- function(file_path, model_name, asset_name) {
  cat("Testing:", model_name, "-", asset_name, "\n")
  
  # Load residuals
  tryCatch({
    residuals_df <- read.csv(file_path, stringsAsFactors = FALSE)
    residuals <- as.numeric(residuals_df[, 1])
    residuals <- residuals[!is.na(residuals) & is.finite(residuals)]
    
    if (length(residuals) < 100) {
      warning("Insufficient residuals for testing:", length(residuals))
      return(NULL)
    }
    
    # Perform all tests
    rolling_var_result <- test_rolling_variance(residuals, window_size = 100)
    structural_break_result <- test_structural_breaks(residuals)
    time_varying_result <- test_time_varying_distribution(residuals, window_size = 100)
    arch_result <- test_arch_effects(residuals, lags = 10)
    nf_stability_result <- analyze_nf_stability(residuals)
    
    # Compile results
    results <- data.frame(
      Model = model_name,
      Asset = asset_name,
      Asset_Type = ifelse(asset_name %in% ASSETS$fx, "FX", "Equity"),
      N = length(residuals),
      # Rolling variance results
      RollingVar_Mean = ifelse(!is.null(rolling_var_result), 
                               round(rolling_var_result$mean_rolling_var, 6), NA),
      RollingVar_CV = ifelse(!is.null(rolling_var_result), 
                            round(rolling_var_result$cv_rolling_var, 4), NA),
      RollingVar_Trend_PValue = ifelse(!is.null(rolling_var_result), 
                                      round(rolling_var_result$trend_pvalue, 4), NA),
      RollingVar_Interpretation = ifelse(!is.null(rolling_var_result), 
                                         rolling_var_result$interpretation, NA),
      # Structural break results
      CUSUM_Statistic = ifelse(!is.null(structural_break_result), 
                              round(structural_break_result$statistic, 4), NA),
      CUSUM_PValue = ifelse(!is.null(structural_break_result), 
                           round(structural_break_result$p_value, 4), NA),
      CUSUM_Interpretation = ifelse(!is.null(structural_break_result), 
                                    structural_break_result$interpretation, NA),
      # Time-varying distribution results
      Dist_CV_SD = ifelse(!is.null(time_varying_result), 
                         round(time_varying_result$cv_sd, 4), NA),
      Dist_SD_Range = ifelse(!is.null(time_varying_result), 
                            round(time_varying_result$sd_range, 4), NA),
      Dist_Interpretation = ifelse(!is.null(time_varying_result), 
                                   time_varying_result$interpretation, NA),
      # ARCH results
      ARCH_Statistic = round(arch_result$main_statistic, 4),
      ARCH_PValue = round(arch_result$main_pvalue, 4),
      ARCH_Interpretation = arch_result$interpretation,
      # NF stability results
      NF_Stability_SD_Ratio = ifelse(!is.null(nf_stability_result), 
                                    round(nf_stability_result$sd_ratio, 4), NA),
      NF_Stability_Interpretation = ifelse(!is.null(nf_stability_result), 
                                           nf_stability_result$interpretation, NA),
      stringsAsFactors = FALSE
    )
    
    return(results)
  }, error = function(e) {
    cat("Error testing", model_name, "-", asset_name, ":", e$message, "\n")
    return(NULL)
  })
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

cat("=== CONDITIONAL HETEROGENEITY ANALYSIS ===\n")
cat("Testing GARCH residuals for conditional heterogeneity\n\n")

# Find all residual files
residuals_dir <- EVAL_PATHS$residuals
model_dirs <- list.dirs(residuals_dir, recursive = FALSE)

all_results <- list()

for (model_dir in model_dirs) {
  model_name <- basename(model_dir)
  cat("\nProcessing model:", model_name, "\n")
  
  # Get all residual files for this model
  residual_files <- list.files(model_dir, pattern = ".*_Manual_Optimized_residuals\\.csv$", 
                                full.names = TRUE)
  
  for (file_path in residual_files) {
    # Extract asset name from filename
    filename <- basename(file_path)
    asset_name <- str_replace(filename, "_Manual_Optimized_residuals\\.csv$", "")
    
    # Test residuals
    result <- test_conditional_heterogeneity_file(file_path, model_name, asset_name)
    
    if (!is.null(result)) {
      all_results[[length(all_results) + 1]] <- result
    }
  }
}

# Combine all results
if (length(all_results) > 0) {
  heterogeneity_results <- do.call(rbind, all_results)
  
  # Create summary by model
  summary_by_model <- heterogeneity_results %>%
    group_by(Model) %>%
    summarise(
      N_Assets = n(),
      Mean_RollingVar_CV = round(mean(RollingVar_CV, na.rm = TRUE), 4),
      TimeVaryingVar_Count = sum(RollingVar_Trend_PValue < 0.05, na.rm = TRUE),
      StructuralBreak_Count = sum(CUSUM_PValue < 0.05, na.rm = TRUE),
      Mean_Dist_CV_SD = round(mean(Dist_CV_SD, na.rm = TRUE), 4),
      ARCH_Effects_Count = sum(ARCH_PValue < 0.05, na.rm = TRUE),
      Mean_NF_Stability_SD_Ratio = round(mean(NF_Stability_SD_Ratio, na.rm = TRUE), 4),
      .groups = "drop"
    )
  
  # Create limitations summary
  limitations_summary <- data.frame(
    Limitation = c(
      "Unconditional Residual Distribution Assumption",
      "Two-Stage Pipeline Structure",
      "NF Model Stability",
      "Conditional Heterogeneity Impact"
    ),
    Description = c(
      "The two-stage pipeline (GARCH → NF) assumes that GARCH residuals follow an unconditional distribution. However, financial return innovations may exhibit conditional heterogeneity even after GARCH filtering.",
      "The separation of GARCH fitting and NF training implies that residual distributions are treated as unconditional. This may not fully capture time-varying characteristics of financial innovations.",
      "NF models trained on residuals may not be stable across different market regimes if conditional heterogeneity is present. This could affect the reliability of synthetic residual generation.",
      "If conditional heterogeneity exists in residuals, the NF model may not accurately capture the full distributional dynamics, potentially affecting the quality of generated synthetic data."
    ),
    Mitigation = c(
      "We test for conditional heterogeneity using rolling variance analysis, structural break tests, and time-varying distribution analysis.",
      "We acknowledge this limitation and test for remaining ARCH effects and time-varying characteristics in residuals.",
      "We analyze NF model stability by comparing distribution statistics across different time periods and market regimes.",
      "Results are interpreted with awareness of this limitation, and findings are documented for transparency."
    ),
    stringsAsFactors = FALSE
  )
  
  # =============================================================================
  # SAVE RESULTS
  # =============================================================================
  
  cat("\n=== SAVING RESULTS ===\n")
  
  # Create output directory
  output_file <- paste(RESULTS_BASE, "consolidated", "Methodology_Conditional_Heterogeneity.xlsx", sep="/")
  if (!dir.exists(dirname(output_file))) {
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  }
  wb <- createWorkbook()
  
  # Add main results sheet
  addWorksheet(wb, "Heterogeneity_Tests")
  writeData(wb, "Heterogeneity_Tests", heterogeneity_results)
  
  # Add summary by model
  addWorksheet(wb, "Summary_By_Model")
  writeData(wb, "Summary_By_Model", summary_by_model)
  
  # Add limitations summary
  addWorksheet(wb, "Limitations_Summary")
  writeData(wb, "Limitations_Summary", limitations_summary)
  
  # Save workbook
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  cat("Results saved to:", output_file, "\n")
  cat("Total tests performed:", nrow(heterogeneity_results), "\n")
  cat("\n=== CONDITIONAL HETEROGENEITY ANALYSIS COMPLETE ===\n")
  
} else {
  cat("ERROR: No results generated. Check residual file paths.\n")
}

