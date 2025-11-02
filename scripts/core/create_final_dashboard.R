#!/usr/bin/env Rscript
# Create Final Dashboard from Manual Pipeline Results
# Consolidates all evaluation results into a comprehensive dashboard

library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)

cat("=== CREATING FINAL DASHBOARD ===\n\n")

# =============================================================================
# 1. Load All Available Results
# =============================================================================

cat("Loading results...\n")

# Load NF-GARCH simulation results
results_file <- "results/consolidated/NF_GARCH_Results_manual.xlsx"
if (file.exists(results_file)) {
  chrono_results <- read.xlsx(results_file, sheet = "Chrono_Split_NF_GARCH")
  tscv_results <- read.xlsx(results_file, sheet = "TS_CV_NF_GARCH")
  cat("[OK] Loaded simulation results\n")
  cat("  - Chrono results: ", nrow(chrono_results), " rows\n")
  cat("  - TS CV results: ", nrow(tscv_results), " rows\n")
} else {
  chrono_results <- data.frame()
  tscv_results <- data.frame()
}

# Load GARCH fitting summary
garch_summary_file <- "outputs/manual/garch_fitting/model_summary.csv"
if (file.exists(garch_summary_file)) {
  garch_summary <- read.csv(garch_summary_file)
  cat("[OK] Loaded GARCH fitting summary: ", nrow(garch_summary), " models\n")
} else {
  garch_summary <- data.frame()
}

# Check NF models training summary
nf_summary_file <- "outputs/manual/nf_models/training_summary.json"
if (file.exists(nf_summary_file)) {
  cat("[OK] NF training summary exists\n")
} else {
  cat("[WARNING] NF training summary not found\n")
}

# =============================================================================
# 2. Performance Analysis
# =============================================================================

cat("\n=== PERFORMANCE ANALYSIS ===\n")

# Chronological Split Performance
if (nrow(chrono_results) > 0) {
  chrono_perf <- chrono_results %>%
    group_by(Model) %>%
    summarise(
      n_assets = n(),
      mean_AIC = mean(AIC, na.rm = TRUE),
      mean_BIC = mean(BIC, na.rm = TRUE),
      mean_MSE = mean(MSE, na.rm = TRUE),
      mean_MAE = mean(MAE, na.rm = TRUE),
      mean_LogLik = mean(LogLikelihood, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(mean_AIC)
  
  cat("\nChronological Split Performance:\n")
  print(chrono_perf)
}

# TS CV Performance
if (nrow(tscv_results) > 0) {
  tscv_perf <- tscv_results %>%
    group_by(Model) %>%
    summarise(
      n_windows = n(),
      mean_AIC = mean(AIC, na.rm = TRUE),
      mean_MSE = mean(MSE, na.rm = TRUE),
      mean_MAE = mean(MAE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(mean_AIC)
  
  cat("\nTime-Series CV Performance:\n")
  print(tscv_perf)
}

# =============================================================================
# 3. Asset Analysis
# =============================================================================

cat("\n=== ASSET ANALYSIS ===\n")

if (nrow(chrono_results) > 0 && "Asset" %in% names(chrono_results)) {
  # Summary showing best model per asset
  asset_perf <- chrono_results %>%
    group_by(Asset) %>%
    summarise(
      n_models = n(),
      best_model = Model[which.min(MSE)],
      best_mse = min(MSE, na.rm = TRUE),
      mean_mse = mean(MSE, na.rm = TRUE),
      mean_mae = mean(MAE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(mean_mse)
  
  cat("\nPerformance by Asset (Best Model Summary):\n")
  print(asset_perf)
  
  # Detailed breakdown showing ALL models per asset
  asset_detailed <- chrono_results %>%
    select(Asset, Model, AIC, BIC, MSE, MAE, LogLikelihood) %>%
    arrange(Asset, MSE) %>%
    mutate(
      Model_Rank = row_number(),
      .by = Asset
    )
  
  cat("\nDetailed Asset Analysis (All Models):\n")
  print(asset_detailed)
}

# =============================================================================
# 4. Model Comparison
# =============================================================================

cat("\n=== MODEL COMPARISON ===\n")

if (nrow(chrono_results) > 0 && nrow(tscv_results) > 0) {
  model_comparison <- chrono_results %>%
    group_by(Model) %>%
    summarise(
      Chrono_MSE = mean(MSE, na.rm = TRUE),
      Chrono_MAE = mean(MAE, na.rm = TRUE),
      Chrono_AIC = mean(AIC, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      tscv_results %>%
        group_by(Model) %>%
        summarise(
          TS_CV_MSE = mean(MSE, na.rm = TRUE),
          TS_CV_MAE = mean(MAE, na.rm = TRUE),
          TS_CV_AIC = mean(AIC, na.rm = TRUE),
          n_windows = n(),
          .groups = "drop"
        ),
      by = "Model"
    ) %>%
    mutate(
      MSE_improvement = ifelse(!is.na(TS_CV_MSE), (Chrono_MSE - TS_CV_MSE) / abs(Chrono_MSE) * 100, NA),
      MAE_improvement = ifelse(!is.na(TS_CV_MAE), (Chrono_MAE - TS_CV_MAE) / abs(Chrono_MAE) * 100, NA)
    )
  
  cat("\nChrono vs TS CV Comparison:\n")
  print(model_comparison)
}

# =============================================================================
# 5. Convergence Analysis
# =============================================================================

cat("\n=== CONVERGENCE ANALYSIS ===\n")

if (nrow(garch_summary) > 0) {
  # Check what columns exist
  conv_col <- if ("Converged" %in% names(garch_summary)) {
    "Converged"
  } else if ("converged" %in% names(garch_summary)) {
    "converged"
  } else if ("Success" %in% names(garch_summary)) {
    "Success"
  } else {
    NULL
  }
  
  if (!is.null(conv_col)) {
    overall_conv <- mean(garch_summary[[conv_col]] == TRUE, na.rm = TRUE)
    cat("Overall convergence rate: ", round(overall_conv * 100, 2), "%\n", sep = "")
    
    if ("Model" %in% names(garch_summary) || "model" %in% names(garch_summary)) {
      model_col <- if ("Model" %in% names(garch_summary)) "Model" else "model"
      conv_by_model <- garch_summary %>%
        group_by(!!sym(model_col)) %>%
        summarise(
          n_total = n(),
          n_converged = sum(!!sym(conv_col) == TRUE, na.rm = TRUE),
          conv_rate = mean(!!sym(conv_col) == TRUE, na.rm = TRUE),
          .groups = "drop"
        )
      
      cat("\nConvergence by Model:\n")
      print(conv_by_model)
    }
  } else {
    cat("[WARNING] Convergence column not found in summary\n")
  }
}

# =============================================================================
# 6. Create Final Dashboard Workbook
# =============================================================================

cat("\n=== CREATING FINAL DASHBOARD ===\n")

wb <- createWorkbook()

# Executive Summary
addWorksheet(wb, "Executive_Summary")
exec_summary <- data.frame(
  Metric = c(
    "Pipeline Status",
    "Total Models Evaluated",
    "Chrono Split Results",
    "TS CV Results (Optimized)",
    "Best Model (Lowest AIC)",
    "Best Model (Lowest MSE)",
    "Overall Convergence Rate"
  ),
  Value = c(
    "Completed",
    if (nrow(chrono_results) > 0) nrow(chrono_results) else 0,
    if (nrow(chrono_results) > 0) "Yes" else "No",
    if (nrow(tscv_results) > 0) paste0("Yes (", nrow(tscv_results), " windows)") else "No",
    if (exists("chrono_perf") && nrow(chrono_perf) > 0) chrono_perf$Model[1] else "N/A",
    if (exists("chrono_perf") && nrow(chrono_perf) > 0) chrono_perf$Model[which.min(chrono_perf$mean_MSE)] else "N/A",
    if (exists("overall_conv")) paste0(round(overall_conv * 100, 2), "%") else "N/A"
  )
)
writeData(wb, "Executive_Summary", exec_summary)

# Performance Metrics
if (exists("chrono_perf") && nrow(chrono_perf) > 0) {
  addWorksheet(wb, "Performance_Chrono")
  writeData(wb, "Performance_Chrono", chrono_perf)
}

if (exists("tscv_perf") && nrow(tscv_perf) > 0) {
  addWorksheet(wb, "Performance_TS_CV")
  writeData(wb, "Performance_TS_CV", tscv_perf)
}

# Asset Analysis
if (exists("asset_perf") && nrow(asset_perf) > 0) {
  addWorksheet(wb, "Asset_Analysis")
  writeData(wb, "Asset_Analysis", asset_perf)
}

# Detailed Asset Analysis (all models per asset)
if (exists("asset_detailed") && nrow(asset_detailed) > 0) {
  addWorksheet(wb, "Asset_Detailed")
  writeData(wb, "Asset_Detailed", asset_detailed)
}

# Model Comparison
if (exists("model_comparison") && nrow(model_comparison) > 0) {
  addWorksheet(wb, "Model_Comparison")
  writeData(wb, "Model_Comparison", model_comparison)
}

# Convergence Analysis
if (nrow(garch_summary) > 0) {
  addWorksheet(wb, "Convergence_Analysis")
  writeData(wb, "Convergence_Analysis", garch_summary)
}

# Detailed Results
if (nrow(chrono_results) > 0) {
  addWorksheet(wb, "Detailed_Chrono_Results")
  writeData(wb, "Detailed_Chrono_Results", chrono_results)
}

if (nrow(tscv_results) > 0) {
  addWorksheet(wb, "Detailed_TS_CV_Results")
  writeData(wb, "Detailed_TS_CV_Results", tscv_results)
}

# GARCH Fitting Summary
if (nrow(garch_summary) > 0) {
  addWorksheet(wb, "GARCH_Fitting_Summary")
  writeData(wb, "GARCH_Fitting_Summary", garch_summary)
}

# =============================================================================
# 7. Load Additional Metrics (if available)
# =============================================================================

cat("\n=== LOADING ADDITIONAL METRICS ===\n")

# Distributional Metrics
dist_file <- "results/consolidated/Distributional_Metrics.xlsx"
if (file.exists(dist_file)) {
  dist_metrics <- read.xlsx(dist_file, sheet = "Distributional_Metrics")
  dist_summary <- read.xlsx(dist_file, sheet = "Summary_Statistics")
  
  addWorksheet(wb, "Distributional_Fit")
  writeData(wb, "Distributional_Fit", dist_metrics)
  
  addWorksheet(wb, "Distributional_Summary")
  writeData(wb, "Distributional_Summary", dist_summary)
  
  cat("[OK] Loaded distributional metrics\n")
}

# Stylized Facts
stylized_file <- "results/consolidated/Stylized_Facts.xlsx"
if (file.exists(stylized_file)) {
  stylized_facts <- read.xlsx(stylized_file, sheet = "Stylized_Facts")
  stylized_summary <- read.xlsx(stylized_file, sheet = "Summary_By_Asset_Class")
  
  addWorksheet(wb, "Stylized_Facts")
  writeData(wb, "Stylized_Facts", stylized_facts)
  
  addWorksheet(wb, "Stylized_Facts_Summary")
  writeData(wb, "Stylized_Facts_Summary", stylized_summary)
  
  cat("[OK] Loaded stylized facts\n")
}

# VaR Backtesting
var_file <- "results/consolidated/VaR_Backtesting.xlsx"
if (file.exists(var_file)) {
  var_results <- read.xlsx(var_file, sheet = "VaR_Backtesting")
  var_summary <- read.xlsx(var_file, sheet = "Summary_Statistics")
  
  addWorksheet(wb, "Risk_Calibration")
  writeData(wb, "Risk_Calibration", var_results)
  
  addWorksheet(wb, "VaR_Summary")
  writeData(wb, "VaR_Summary", var_summary)
  
  cat("[OK] Loaded VaR backtesting results\n")
}

# Stress Testing
stress_file <- "results/consolidated/Stress_Testing.xlsx"
if (file.exists(stress_file)) {
  stress_results <- read.xlsx(stress_file, sheet = "Stress_Test_Results")
  stress_summary <- read.xlsx(stress_file, sheet = "Summary_Statistics")
  
  addWorksheet(wb, "Stress_Testing")
  writeData(wb, "Stress_Testing", stress_results)
  
  addWorksheet(wb, "Stress_Summary")
  writeData(wb, "Stress_Summary", stress_summary)
  
  # Load forecast evaluation under stress if available
  stress_sheets <- getSheetNames(stress_file)
  if ("Forecast_Under_Stress" %in% stress_sheets) {
    forecast_under_stress <- read.xlsx(stress_file, sheet = "Forecast_Under_Stress")
    addWorksheet(wb, "Forecast_Under_Stress")
    writeData(wb, "Forecast_Under_Stress", forecast_under_stress)
  }
  
  if ("Forecast_Summary" %in% stress_sheets) {
    forecast_summary_stress <- read.xlsx(stress_file, sheet = "Forecast_Summary")
    addWorksheet(wb, "Forecast_Summary_Stress")
    writeData(wb, "Forecast_Summary_Stress", forecast_summary_stress)
  }
  
  cat("[OK] Loaded stress testing results\n")
}

# NF vs Standard Comparison (for asset-class analysis and Wilcoxon)
comparison_file <- "results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx"
if (file.exists(comparison_file)) {
  # Try to load asset class summary
  tryCatch({
    asset_class_summary <- read.xlsx(comparison_file, sheet = "Asset_Class_Summary")
    best_by_class <- read.xlsx(comparison_file, sheet = "Best_Model_By_Class")
    wilcoxon_test <- read.xlsx(comparison_file, sheet = "Wilcoxon_Test")
    
    addWorksheet(wb, "Asset_Class_Analysis")
    writeData(wb, "Asset_Class_Analysis", asset_class_summary)
    
    addWorksheet(wb, "Best_Model_By_Class")
    writeData(wb, "Best_Model_By_Class", best_by_class)
    
    addWorksheet(wb, "Statistical_Significance")
    writeData(wb, "Statistical_Significance", wilcoxon_test)
    
    cat("[OK] Loaded comparison analysis\n")
  }, error = function(e) {
    cat("[WARNING] Some comparison sheets not available:", e$message, "\n")
  })
}

# =============================================================================
# 8. Update Executive Summary with Additional Metrics
# =============================================================================

cat("\n=== UPDATING EXECUTIVE SUMMARY ===\n")

# Add distributional metrics summary
if (exists("dist_summary") && nrow(dist_summary) > 0) {
  best_ks_model <- dist_summary$Model[which.min(dist_summary$mean_KS)]
  exec_summary <- rbind(exec_summary, 
    data.frame(
      Metric = "Best KS Distance Model",
      Value = ifelse(exists("best_ks_model"), best_ks_model, "N/A")
    )
  )
}

# Add VaR summary
if (exists("var_summary") && nrow(var_summary) > 0) {
  avg_exceedance_rate <- mean(var_summary$mean_exceedance_rate, na.rm = TRUE)
  exec_summary <- rbind(exec_summary,
    data.frame(
      Metric = "Average VaR Exceedance Rate (95%)",
      Value = ifelse(!is.na(avg_exceedance_rate), paste0(round(avg_exceedance_rate * 100, 2), "%"), "N/A")
    )
  )
}

# Update executive summary sheet with additional metrics
if (exists("dist_summary") && nrow(dist_summary) > 0 && exists("exec_summary")) {
  best_ks_model <- dist_summary$Model[which.min(dist_summary$mean_KS)]
  exec_summary <- rbind(exec_summary, 
    data.frame(
      Metric = "Best KS Distance Model",
      Value = ifelse(exists("best_ks_model") && !is.null(best_ks_model), as.character(best_ks_model), "N/A"),
      stringsAsFactors = FALSE
    )
  )
}

if (exists("var_summary") && nrow(var_summary) > 0 && exists("exec_summary")) {
  avg_exceedance_rate <- mean(var_summary$mean_exceedance_rate, na.rm = TRUE)
  exec_summary <- rbind(exec_summary,
    data.frame(
      Metric = "Average VaR Exceedance Rate (95%)",
      Value = ifelse(!is.na(avg_exceedance_rate), paste0(round(avg_exceedance_rate * 100, 2), "%"), "N/A"),
      stringsAsFactors = FALSE
    )
  )
}

# Update executive summary in workbook
if (exists("exec_summary")) {
  removeWorksheet(wb, "Executive_Summary")
  addWorksheet(wb, "Executive_Summary")
  writeData(wb, "Executive_Summary", exec_summary)
}

# Save workbook
dashboard_file <- "results/consolidated/Final_Dashboard.xlsx"
saveWorkbook(wb, dashboard_file, overwrite = TRUE)

cat("\n[OK] Final dashboard saved to:", dashboard_file, "\n")
cat("\n=== DASHBOARD CREATION COMPLETE ===\n")

