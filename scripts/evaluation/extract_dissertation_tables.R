#!/usr/bin/env Rscript
# Extract Dissertation Tables and Summaries
# Generates LaTeX tables and summaries from results files

library(openxlsx)
library(dplyr)
library(tidyr)

cat("=== EXTRACTING DISSERTATION TABLES ===\n\n")

# =============================================================================
# 1. Load All Results Files
# =============================================================================

cat("Loading results files...\n")

# Final Dashboard
dashboard_file <- "results/consolidated/Final_Dashboard.xlsx"
nf_comparison_file <- "results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx"
var_file <- "results/consolidated/VaR_Backtesting.xlsx"
stress_file <- "results/consolidated/Stress_Testing.xlsx"
stylized_file <- "results/consolidated/Stylized_Facts.xlsx"
dist_file <- "results/consolidated/Distributional_Metrics.xlsx"

# Create output directory
output_dir <- "results/dissertation_tables"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# =============================================================================
# 2. Stylised Facts Summary
# =============================================================================

cat("\n1. Extracting Stylised Facts...\n")

if (file.exists(stylized_file)) {
  stylized_facts <- read.xlsx(stylized_file, sheet = "Stylized_Facts")
  stylized_summary <- read.xlsx(stylized_file, sheet = "Summary_By_Asset_Class")
  
  # Summary statistics by asset class
  stylized_table <- stylized_facts %>%
    select(Asset, Asset_Class, Volatility_clustering_persistence, 
           Leverage_effect, Gain_loss_asymmetry_ratio, Skewness) %>%
    group_by(Asset_Class) %>%
    summarise(
      Mean_Volatility_Clustering = mean(Volatility_clustering_persistence, na.rm = TRUE),
      Mean_Leverage_Effect = mean(Leverage_effect, na.rm = TRUE),
      Mean_Gain_Loss_Asymmetry = mean(Gain_loss_asymmetry_ratio, na.rm = TRUE),
      Mean_Skewness = mean(Skewness, na.rm = TRUE),
      .groups = "drop"
    )
  
  write.csv(stylized_table, file.path(output_dir, "stylized_facts_summary.csv"), row.names = FALSE)
  cat("  [OK] Stylised facts summary saved\n")
}

# =============================================================================
# 3. Baseline GARCH Model Performance
# =============================================================================

cat("\n2. Extracting Baseline GARCH Performance...\n")

if (file.exists(dashboard_file)) {
  sheets <- getSheetNames(dashboard_file)
  
  # Try to get chronological split results (standard GARCH only)
  if ("Performance_Chrono" %in% sheets) {
    chrono_perf <- read.xlsx(dashboard_file, sheet = "Performance_Chrono")
    
    # Data is already aggregated, just rename columns for clarity
    baseline_summary <- chrono_perf %>%
      select(Model, N_Assets = n_assets, 
             Mean_MSE = mean_MSE, Mean_MAE = mean_MAE,
             Mean_AIC = mean_AIC, Mean_BIC = mean_BIC,
             Mean_LogLik = mean_LogLik) %>%
      arrange(Mean_MSE)
    
    write.csv(baseline_summary, file.path(output_dir, "baseline_garch_performance.csv"), row.names = FALSE)
    cat("  [OK] Baseline GARCH performance saved\n")
  }
  
  # Asset class analysis
  if ("Asset_Class_Analysis" %in% sheets) {
    asset_class <- read.xlsx(dashboard_file, sheet = "Asset_Class_Analysis")
    write.csv(asset_class, file.path(output_dir, "asset_class_analysis.csv"), row.names = FALSE)
    cat("  [OK] Asset class analysis saved\n")
  }
}

# =============================================================================
# 4. NF-GARCH vs Standard GARCH Comparison
# =============================================================================

cat("\n3. Extracting NF-GARCH vs Standard Comparison...\n")

if (file.exists(nf_comparison_file)) {
  sheets <- getSheetNames(nf_comparison_file)
  
  # Overall comparison
  if ("Combined_Results" %in% sheets) {
    combined <- read.xlsx(nf_comparison_file, sheet = "Combined_Results")
    
    # Overall summary by source
    overall_comparison <- combined %>%
      group_by(Source) %>%
      summarise(
        N_Observations = n(),
        Mean_MSE = mean(MSE, na.rm = TRUE),
        Median_MSE = median(MSE, na.rm = TRUE),
        Mean_MAE = mean(MAE, na.rm = TRUE),
        Median_MAE = median(MAE, na.rm = TRUE),
        Mean_AIC = mean(AIC, na.rm = TRUE),
        Median_AIC = median(AIC, na.rm = TRUE),
        Mean_BIC = mean(BIC, na.rm = TRUE),
        Median_BIC = median(BIC, na.rm = TRUE),
        .groups = "drop"
      )
    
    write.csv(overall_comparison, file.path(output_dir, "nf_vs_standard_overall.csv"), row.names = FALSE)
    cat("  [OK] Overall NF vs Standard comparison saved\n")
    
    # By model comparison
    model_comparison <- combined %>%
      group_by(Model, Source) %>%
      summarise(
        N = n(),
        Mean_MSE = mean(MSE, na.rm = TRUE),
        Mean_MAE = mean(MAE, na.rm = TRUE),
        Mean_AIC = mean(AIC, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(names_from = Source, values_from = c(Mean_MSE, Mean_MAE, Mean_AIC)) %>%
      mutate(
        MSE_Improvement_Pct = ifelse(!is.na(Mean_MSE_Standard) & !is.na(Mean_MSE_NF_GARCH) & Mean_MSE_Standard > 0,
                                     (Mean_MSE_Standard - Mean_MSE_NF_GARCH) / Mean_MSE_Standard * 100, NA),
        MAE_Improvement_Pct = ifelse(!is.na(Mean_MAE_Standard) & !is.na(Mean_MAE_NF_GARCH) & Mean_MAE_Standard > 0,
                                     (Mean_MAE_Standard - Mean_MAE_NF_GARCH) / Mean_MAE_Standard * 100, NA)
      )
    
    write.csv(model_comparison, file.path(output_dir, "nf_vs_standard_by_model.csv"), row.names = FALSE)
    cat("  [OK] NF vs Standard by model saved\n")
  }
  
  # Win rate
  if ("Win_Rate_Analysis" %in% sheets) {
    win_rate <- read.xlsx(nf_comparison_file, sheet = "Win_Rate_Analysis")
    write.csv(win_rate, file.path(output_dir, "nf_win_rate.csv"), row.names = FALSE)
    cat("  [OK] Win rate analysis saved\n")
  }
  
  # Wilcoxon test
  if ("Wilcoxon_Test" %in% sheets) {
    wilcoxon <- read.xlsx(nf_comparison_file, sheet = "Wilcoxon_Test")
    write.csv(wilcoxon, file.path(output_dir, "wilcoxon_test_results.csv"), row.names = FALSE)
    cat("  [OK] Wilcoxon test results saved\n")
  }
}

# =============================================================================
# 5. Distributional Diagnostics
# =============================================================================

cat("\n4. Extracting Distributional Metrics...\n")

if (file.exists(dist_file)) {
  dist_metrics <- read.xlsx(dist_file, sheet = "Distributional_Metrics")
  dist_summary <- read.xlsx(dist_file, sheet = "Summary_Statistics")
  
  # Summary by model
  dist_by_model <- dist_metrics %>%
    group_by(Model) %>%
    summarise(
      Mean_KS = mean(KS_distance, na.rm = TRUE),
      Median_KS = median(KS_distance, na.rm = TRUE),
      Mean_Wasserstein = mean(Wasserstein_distance, na.rm = TRUE),
      Median_Wasserstein = median(Wasserstein_distance, na.rm = TRUE),
      Mean_Tail_Index = mean(Tail_index, na.rm = TRUE),
      Mean_Skewness = mean(Skewness, na.rm = TRUE),
      Mean_Kurtosis = mean(Kurtosis, na.rm = TRUE),
      .groups = "drop"
    )
  
  write.csv(dist_by_model, file.path(output_dir, "distributional_metrics_by_model.csv"), row.names = FALSE)
  write.csv(dist_summary, file.path(output_dir, "distributional_summary.csv"), row.names = FALSE)
  cat("  [OK] Distributional metrics saved\n")
}

# =============================================================================
# 6. VaR Backtesting
# =============================================================================

cat("\n5. Extracting VaR Backtesting Results...\n")

if (file.exists(var_file)) {
  var_results <- read.xlsx(var_file, sheet = "VaR_Backtesting")
  var_summary <- read.xlsx(var_file, sheet = "Summary_Statistics")
  
  # Summary by model and confidence level
  var_by_model <- var_results %>%
    group_by(Model, Confidence_Level) %>%
    summarise(
      N_Assets = n(),
      Mean_Exceedance_Rate = mean(Exceedance_Rate, na.rm = TRUE),
      Expected_Rate = mean(Expected_Rate, na.rm = TRUE),
      Mean_Kupiec_pvalue = mean(Kupiec_pvalue, na.rm = TRUE),
      Mean_Christoffersen_pvalue = mean(Christoffersen_pvalue, na.rm = TRUE),
      .groups = "drop"
    )
  
  write.csv(var_by_model, file.path(output_dir, "var_backtesting_by_model.csv"), row.names = FALSE)
  write.csv(var_summary, file.path(output_dir, "var_summary.csv"), row.names = FALSE)
  cat("  [OK] VaR backtesting results saved\n")
}

# =============================================================================
# 7. Stress Testing
# =============================================================================

cat("\n6. Extracting Stress Testing Results...\n")

if (file.exists(stress_file)) {
  sheets <- getSheetNames(stress_file)
  
  if ("Stress_Test_Results" %in% sheets) {
    stress_results <- read.xlsx(stress_file, sheet = "Stress_Test_Results")
    
    # Summary by scenario type
    stress_summary <- stress_results %>%
      group_by(Scenario_Type, Scenario_Name) %>%
      summarise(
        N_Assets = n(),
        Mean_Volatility = mean(Volatility, na.rm = TRUE),
        Mean_Max_Drawdown = mean(Max_Drawdown, na.rm = TRUE),
        Mean_Volatility_Increase_Pct = mean(Volatility_Increase_Pct, na.rm = TRUE),
        .groups = "drop"
      )
    
    write.csv(stress_summary, file.path(output_dir, "stress_testing_summary.csv"), row.names = FALSE)
    cat("  [OK] Stress testing summary saved\n")
  }
  
  # Forecast evaluation under stress
  if ("Forecast_Under_Stress" %in% sheets) {
    forecast_stress <- read.xlsx(stress_file, sheet = "Forecast_Under_Stress")
    
    # Summary by crisis
    crisis_forecast <- forecast_stress %>%
      filter(Scenario_Type == "Historical_Crisis") %>%
      group_by(Scenario_Name, Model) %>%
      summarise(
        N = n(),
        NF_GARCH_MSE = mean(NF_GARCH_MSE, na.rm = TRUE),
        Standard_GARCH_MSE = mean(Standard_GARCH_MSE, na.rm = TRUE),
        NF_GARCH_MAE = mean(NF_GARCH_MAE, na.rm = TRUE),
        Standard_GARCH_MAE = mean(Standard_GARCH_MAE, na.rm = TRUE),
        MSE_Improvement_Pct = mean(MSE_Improvement_Pct, na.rm = TRUE),
        .groups = "drop"
      )
    
    write.csv(crisis_forecast, file.path(output_dir, "crisis_forecast_performance.csv"), row.names = FALSE)
    cat("  [OK] Crisis forecast performance saved\n")
  }
}

cat("\n=== EXTRACTION COMPLETE ===\n")
cat("Tables saved to:", output_dir, "\n")

