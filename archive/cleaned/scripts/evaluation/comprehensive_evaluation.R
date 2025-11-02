#!/usr/bin/env Rscript
# Comprehensive Evaluation for Manual Pipeline Results
# Evaluates NF-GARCH models using available results from manual pipeline

library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(moments)

# Source utilities
source("scripts/utils/safety_functions.R")
source("scripts/utils/conflict_resolution.R")

cat("=== COMPREHENSIVE EVALUATION FOR MANUAL PIPELINE ===\n\n")

# Load results from manual pipeline
results_file <- "results/consolidated/NF_GARCH_Results_manual.xlsx"

if (!file.exists(results_file)) {
  stop("Results file not found: ", results_file)
}

cat("Loading results from:", results_file, "\n")

# Load all sheets
sheets <- getSheetNames(results_file)
cat("Available sheets:", paste(sheets, collapse = ", "), "\n\n")

# Load main results
if ("Chrono_Split_NF_GARCH" %in% sheets) {
  chrono_results <- read.xlsx(results_file, sheet = "Chrono_Split_NF_GARCH")
  cat("Chrono split results: ", nrow(chrono_results), " rows\n")
} else {
  chrono_results <- data.frame()
}

if ("TS_CV_NF_GARCH" %in% sheets) {
  tscv_results <- read.xlsx(results_file, sheet = "TS_CV_NF_GARCH")
  cat("TS CV results: ", nrow(tscv_results), " rows\n")
} else {
  tscv_results <- data.frame()
}

# Load GARCH fitting summary
garch_summary_file <- "outputs/manual/garch_fitting/model_summary.csv"
if (file.exists(garch_summary_file)) {
  garch_summary <- read.csv(garch_summary_file)
  cat("GARCH fitting summary: ", nrow(garch_summary), " models\n")
} else {
  garch_summary <- data.frame()
}

# =============================================================================
# 1. PERFORMANCE METRICS ANALYSIS
# =============================================================================

cat("\n=== PERFORMANCE METRICS ANALYSIS ===\n")

if (nrow(chrono_results) > 0) {
  # Calculate summary statistics
  perf_summary <- chrono_results %>%
    group_by(Model) %>%
    summarise(
      n_models = n(),
      mean_AIC = mean(AIC, na.rm = TRUE),
      mean_BIC = mean(BIC, na.rm = TRUE),
      mean_MSE = mean(MSE, na.rm = TRUE),
      mean_MAE = mean(MAE, na.rm = TRUE),
      mean_LogLik = mean(LogLikelihood, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(mean_AIC)
  
  cat("\nModel Performance Summary (Chrono Split):\n")
  print(perf_summary)
  
  # Best model by metric
  best_mse <- perf_summary %>% arrange(mean_MSE) %>% slice(1)
  best_mae <- perf_summary %>% arrange(mean_MAE) %>% slice(1)
  best_aic <- perf_summary %>% arrange(mean_AIC) %>% slice(1)
  
  cat("\nBest Models:\n")
  cat("  Lowest MSE: ", best_mse$Model, " (MSE = ", round(best_mse$mean_MSE, 6), ")\n", sep = "")
  cat("  Lowest MAE: ", best_mae$Model, " (MAE = ", round(best_mae$mean_MAE, 6), ")\n", sep = "")
  cat("  Lowest AIC: ", best_aic$Model, " (AIC = ", round(best_aic$mean_AIC, 2), ")\n", sep = "")
}

if (nrow(tscv_results) > 0) {
  # TS CV summary
  tscv_summary <- tscv_results %>%
    group_by(Model) %>%
    summarise(
      n_windows = n(),
      mean_AIC = mean(AIC, na.rm = TRUE),
      mean_MSE = mean(MSE, na.rm = TRUE),
      mean_MAE = mean(MAE, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nTS CV Performance Summary:\n")
  print(tscv_summary)
}

# =============================================================================
# 2. MODEL CONVERGENCE ANALYSIS
# =============================================================================

cat("\n=== MODEL CONVERGENCE ANALYSIS ===\n")

if (nrow(garch_summary) > 0) {
  convergence_rate <- mean(garch_summary$Converged == TRUE, na.rm = TRUE)
  cat("Overall convergence rate: ", round(convergence_rate * 100, 2), "%\n", sep = "")
  
  convergence_by_model <- garch_summary %>%
    group_by(Model) %>%
    summarise(
      n_total = n(),
      n_converged = sum(Converged == TRUE, na.rm = TRUE),
      convergence_rate = mean(Converged == TRUE, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nConvergence by Model:\n")
  print(convergence_by_model)
}

# =============================================================================
# 3. ASSET-SPECIFIC ANALYSIS
# =============================================================================

cat("\n=== ASSET-SPECIFIC ANALYSIS ===\n")

if (nrow(chrono_results) > 0 && "Asset" %in% names(chrono_results)) {
  asset_summary <- chrono_results %>%
    group_by(Asset) %>%
    summarise(
      n_models = n(),
      best_model = Model[which.min(MSE)],
      best_mse = min(MSE, na.rm = TRUE),
      mean_mse = mean(MSE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(mean_mse)
  
  cat("\nPerformance by Asset:\n")
  print(asset_summary)
}

# =============================================================================
# 4. COMPARISON: CHRONO vs TS CV
# =============================================================================

cat("\n=== CHRONOLOGICAL vs TIME-SERIES CV COMPARISON ===\n")

if (nrow(chrono_results) > 0 && nrow(tscv_results) > 0) {
  comparison <- inner_join(
    chrono_results %>% group_by(Model) %>% summarise(Chrono_MSE = mean(MSE, na.rm = TRUE), .groups = "drop"),
    tscv_results %>% group_by(Model) %>% summarise(TS_CV_MSE = mean(MSE, na.rm = TRUE), .groups = "drop"),
    by = "Model"
  ) %>%
    mutate(
      MSE_diff = TS_CV_MSE - Chrono_MSE,
      MSE_improvement_pct = ((Chrono_MSE - TS_CV_MSE) / Chrono_MSE) * 100
    )
  
  cat("\nMSE Comparison:\n")
  print(comparison)
  
  better_tscv <- sum(comparison$MSE_improvement_pct > 0, na.rm = TRUE)
  cat("\nModels performing better with TS CV: ", better_tscv, " / ", nrow(comparison), "\n", sep = "")
}

# =============================================================================
# 5. CREATE EVALUATION WORKBOOK
# =============================================================================

cat("\n=== CREATING EVALUATION WORKBOOK ===\n")

wb <- createWorkbook()

# Summary sheet
addWorksheet(wb, "Summary")
summary_data <- data.frame(
  Metric = c(
    "Total Models Evaluated",
    "Chrono Split Results",
    "TS CV Results",
    "Overall Convergence Rate",
    "Best Model (MSE)",
    "Best Model (AIC)"
  ),
  Value = c(
    if (nrow(chrono_results) > 0) nrow(chrono_results) else 0,
    if (nrow(chrono_results) > 0) "Yes" else "No",
    if (nrow(tscv_results) > 0) "Yes" else "No",
    if (nrow(garch_summary) > 0) paste0(round(convergence_rate * 100, 2), "%") else "N/A",
    if (nrow(perf_summary) > 0) best_mse$Model else "N/A",
    if (nrow(perf_summary) > 0) best_aic$Model else "N/A"
  )
)
writeData(wb, "Summary", summary_data)

# Performance summary
if (exists("perf_summary") && nrow(perf_summary) > 0) {
  addWorksheet(wb, "Performance_Summary")
  writeData(wb, "Performance_Summary", perf_summary)
}

# Chrono results
if (nrow(chrono_results) > 0) {
  addWorksheet(wb, "Chrono_Results")
  writeData(wb, "Chrono_Results", chrono_results)
}

# TS CV results
if (nrow(tscv_results) > 0) {
  addWorksheet(wb, "TS_CV_Results")
  writeData(wb, "TS_CV_Results", tscv_results)
}

# Convergence analysis
if (nrow(garch_summary) > 0 && exists("convergence_by_model")) {
  addWorksheet(wb, "Convergence_Analysis")
  writeData(wb, "Convergence_Analysis", convergence_by_model)
}

# Asset analysis
if (exists("asset_summary") && nrow(asset_summary) > 0) {
  addWorksheet(wb, "Asset_Analysis")
  writeData(wb, "Asset_Analysis", asset_summary)
}

# Comparison
if (exists("comparison") && nrow(comparison) > 0) {
  addWorksheet(wb, "Chrono_vs_TS_CV")
  writeData(wb, "Chrono_vs_TS_CV", comparison)
}

# Save workbook
eval_output <- "results/consolidated/Comprehensive_Evaluation.xlsx"
saveWorkbook(wb, eval_output, overwrite = TRUE)

cat("\n✓ Evaluation workbook saved to:", eval_output, "\n")

cat("\n=== EVALUATION COMPLETE ===\n")

