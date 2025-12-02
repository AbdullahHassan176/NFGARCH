#!/usr/bin/env Rscript
# Residual Stationarity Testing for Methodology Chapter
# Tests GARCH residuals for stationarity using ADF, KPSS, Ljung-Box, and ARCH tests

# Load required libraries
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(tseries)
library(FinTS)
library(lmtest)

# Load configuration
source("scripts/core/config.R")

# =============================================================================
# STATIONARITY TEST FUNCTIONS
# =============================================================================

#' Perform ADF (Augmented Dickey-Fuller) test for unit roots
#' Null hypothesis: series has unit root (non-stationary)
#' Alternative: series is stationary
test_adf <- function(residuals, max_lag = NULL) {
  tryCatch({
    if (is.null(max_lag)) {
      # Use automatic lag selection
      test_result <- adf.test(residuals)
    } else {
      test_result <- adf.test(residuals, k = max_lag)
    }
    
    return(list(
      statistic = test_result$statistic,
      p_value = test_result$p.value,
      lag = ifelse(is.null(max_lag), "auto", max_lag),
      alternative = "stationary",
      interpretation = ifelse(test_result$p.value < 0.05, 
                              "Stationary (reject H0)", 
                              "Non-stationary (fail to reject H0)")
    ))
  }, error = function(e) {
    return(list(
      statistic = NA,
      p_value = NA,
      lag = NA,
      alternative = "stationary",
      interpretation = paste("Error:", e$message)
    ))
  })
}

#' Perform KPSS test for trend stationarity
#' Null hypothesis: series is stationary
#' Alternative: series has unit root (non-stationary)
test_kpss <- function(residuals) {
  tryCatch({
    test_result <- kpss.test(residuals, null = "Trend")
    
    return(list(
      statistic = test_result$statistic,
      p_value = test_result$p.value,
      null_hypothesis = "stationary",
      interpretation = ifelse(test_result$p.value < 0.05, 
                              "Non-stationary (reject H0)", 
                              "Stationary (fail to reject H0)")
    ))
  }, error = function(e) {
    return(list(
      statistic = NA,
      p_value = NA,
      null_hypothesis = "stationary",
      interpretation = paste("Error:", e$message)
    ))
  })
}

#' Perform Ljung-Box test for serial correlation
#' Null hypothesis: no serial correlation
test_ljung_box <- function(residuals, lag = 10) {
  tryCatch({
    test_result <- Box.test(residuals, lag = lag, type = "Ljung-Box")
    
    return(list(
      statistic = test_result$statistic,
      p_value = test_result$p.value,
      lag = lag,
      null_hypothesis = "no serial correlation",
      interpretation = ifelse(test_result$p.value < 0.05, 
                              "Serial correlation present (reject H0)", 
                              "No serial correlation (fail to reject H0)")
    ))
  }, error = function(e) {
    return(list(
      statistic = NA,
      p_value = NA,
      lag = lag,
      null_hypothesis = "no serial correlation",
      interpretation = paste("Error:", e$message)
    ))
  })
}

#' Perform ARCH LM test for remaining heteroskedasticity
#' Null hypothesis: no ARCH effects (homoskedastic)
test_arch <- function(residuals, lags = 10) {
  tryCatch({
    test_result <- ArchTest(residuals, lags = lags)
    
    return(list(
      statistic = test_result$statistic,
      p_value = test_result$p.value,
      lags = lags,
      null_hypothesis = "no ARCH effects",
      interpretation = ifelse(test_result$p.value < 0.05, 
                              "ARCH effects present (reject H0)", 
                              "No ARCH effects (fail to reject H0)")
    ))
  }, error = function(e) {
    return(list(
      statistic = NA,
      p_value = NA,
      lags = lags,
      null_hypothesis = "no ARCH effects",
      interpretation = paste("Error:", e$message)
    ))
  })
}

#' Calculate summary statistics for residuals
calculate_residual_stats <- function(residuals) {
  residuals_clean <- residuals[!is.na(residuals) & is.finite(residuals)]
  
  if (length(residuals_clean) < 10) {
    return(list(
      n = length(residuals_clean),
      mean = NA,
      sd = NA,
      min = NA,
      max = NA,
      skewness = NA,
      kurtosis = NA,
      acf_lag1 = NA
    ))
  }
  
  return(list(
    n = length(residuals_clean),
    mean = mean(residuals_clean),
    sd = sd(residuals_clean),
    min = min(residuals_clean),
    max = max(residuals_clean),
    skewness = moments::skewness(residuals_clean),
    kurtosis = moments::kurtosis(residuals_clean),
    acf_lag1 = tryCatch(acf(residuals_clean, lag.max = 1, plot = FALSE)$acf[2], 
                        error = function(e) NA)
  ))
}

# =============================================================================
# MAIN TESTING FUNCTION
# =============================================================================

#' Test stationarity for a single residual file
test_residual_file <- function(file_path, model_name, asset_name) {
  cat("Testing:", model_name, "-", asset_name, "\n")
  
  # Load residuals
  tryCatch({
    residuals_df <- read.csv(file_path, stringsAsFactors = FALSE)
    # Get first column (residuals)
    residuals <- as.numeric(residuals_df[, 1])
    residuals <- residuals[!is.na(residuals) & is.finite(residuals)]
    
    if (length(residuals) < 50) {
      warning("Insufficient residuals for testing:", length(residuals))
      return(NULL)
    }
    
    # Perform all tests
    adf_result <- test_adf(residuals)
    kpss_result <- test_kpss(residuals)
    ljung_box_result <- test_ljung_box(residuals, lag = 10)
    arch_result <- test_arch(residuals, lags = 10)
    stats <- calculate_residual_stats(residuals)
    
    # Compile results
    results <- data.frame(
      Model = model_name,
      Asset = asset_name,
      Asset_Type = ifelse(asset_name %in% ASSETS$fx, "FX", "Equity"),
      N = stats$n,
      Mean = round(stats$mean, 6),
      SD = round(stats$sd, 6),
      Min = round(stats$min, 4),
      Max = round(stats$max, 4),
      Skewness = round(stats$skewness, 4),
      Kurtosis = round(stats$kurtosis, 4),
      ACF_Lag1 = round(stats$acf_lag1, 4),
      ADF_Statistic = round(adf_result$statistic, 4),
      ADF_PValue = round(adf_result$p_value, 4),
      ADF_Interpretation = adf_result$interpretation,
      KPSS_Statistic = round(kpss_result$statistic, 4),
      KPSS_PValue = round(kpss_result$p_value, 4),
      KPSS_Interpretation = kpss_result$interpretation,
      LjungBox_Statistic = round(ljung_box_result$statistic, 4),
      LjungBox_PValue = round(ljung_box_result$p_value, 4),
      LjungBox_Interpretation = ljung_box_result$interpretation,
      ARCH_Statistic = round(arch_result$statistic, 4),
      ARCH_PValue = round(arch_result$p_value, 4),
      ARCH_Interpretation = arch_result$interpretation,
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

cat("=== RESIDUAL STATIONARITY TESTING ===\n")
cat("Testing GARCH residuals for stationarity\n\n")

# Find all residual files
residuals_dir <- "outputs/manual/residuals_by_model"
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
    result <- test_residual_file(file_path, model_name, asset_name)
    
    if (!is.null(result)) {
      all_results[[length(all_results) + 1]] <- result
    }
  }
}

# Combine all results
if (length(all_results) > 0) {
  stationarity_results <- do.call(rbind, all_results)
  
  # Create summary statistics by model
  summary_by_model <- stationarity_results %>%
    group_by(Model) %>%
    summarise(
      N_Assets = n(),
      Mean_ADF_PValue = round(mean(ADF_PValue, na.rm = TRUE), 4),
      Stationary_ADF_Count = sum(ADF_PValue < 0.05, na.rm = TRUE),
      Mean_KPSS_PValue = round(mean(KPSS_PValue, na.rm = TRUE), 4),
      Stationary_KPSS_Count = sum(KPSS_PValue >= 0.05, na.rm = TRUE),
      Mean_LjungBox_PValue = round(mean(LjungBox_PValue, na.rm = TRUE), 4),
      NoSerialCorr_Count = sum(LjungBox_PValue >= 0.05, na.rm = TRUE),
      Mean_ARCH_PValue = round(mean(ARCH_PValue, na.rm = TRUE), 4),
      NoARCH_Count = sum(ARCH_PValue >= 0.05, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create summary by asset
  summary_by_asset <- stationarity_results %>%
    group_by(Asset, Asset_Type) %>%
    summarise(
      N_Models = n(),
      Mean_ADF_PValue = round(mean(ADF_PValue, na.rm = TRUE), 4),
      Mean_KPSS_PValue = round(mean(KPSS_PValue, na.rm = TRUE), 4),
      Mean_LjungBox_PValue = round(mean(LjungBox_PValue, na.rm = TRUE), 4),
      Mean_ARCH_PValue = round(mean(ARCH_PValue, na.rm = TRUE), 4),
      .groups = "drop"
    )
  
  # Create diagnostic summary
  diagnostic_summary <- data.frame(
    Test = c("ADF Test", "KPSS Test", "Ljung-Box Test", "ARCH LM Test"),
    Null_Hypothesis = c("Non-stationary (unit root)", 
                        "Stationary", 
                        "No serial correlation",
                        "No ARCH effects"),
    Alternative = c("Stationary",
                    "Non-stationary (unit root)",
                    "Serial correlation present",
                    "ARCH effects present"),
    Interpretation_Threshold = c("p < 0.05: Stationary",
                                   "p < 0.05: Non-stationary",
                                   "p < 0.05: Serial correlation",
                                   "p < 0.05: ARCH effects"),
    stringsAsFactors = FALSE
  )
  
  # =============================================================================
  # SAVE RESULTS
  # =============================================================================
  
  cat("\n=== SAVING RESULTS ===\n")
  
  # Create output directory
  if (!dir.exists("results/consolidated")) {
    dir.create("results/consolidated", recursive = TRUE, showWarnings = FALSE)
  }
  
  output_file <- "results/consolidated/Methodology_Residual_Stationarity.xlsx"
  wb <- createWorkbook()
  
  # Add main results sheet
  addWorksheet(wb, "Stationarity_Tests")
  writeData(wb, "Stationarity_Tests", stationarity_results)
  
  # Add summary by model
  addWorksheet(wb, "Summary_By_Model")
  writeData(wb, "Summary_By_Model", summary_by_model)
  
  # Add summary by asset
  addWorksheet(wb, "Summary_By_Asset")
  writeData(wb, "Summary_By_Asset", summary_by_asset)
  
  # Add diagnostic summary
  addWorksheet(wb, "Diagnostic_Summary")
  writeData(wb, "Diagnostic_Summary", diagnostic_summary)
  
  # Save workbook
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  cat("Results saved to:", output_file, "\n")
  cat("Total tests performed:", nrow(stationarity_results), "\n")
  cat("\n=== STATIONARITY TESTING COMPLETE ===\n")
  
} else {
  cat("ERROR: No results generated. Check residual file paths.\n")
}

