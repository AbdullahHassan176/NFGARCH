#!/usr/bin/env Rscript
# Analyze Density Forecast Metrics (Predictive Log-Likelihood)

library(openxlsx)
library(dplyr)

cat("=== DENSITY FORECAST METRICS ANALYSIS ===\n\n")

# Load NF-GARCH results (has PredictiveLogLik)
nf_results <- read.xlsx("results/consolidated/NF_GARCH_Results_manual.xlsx", sheet = "Chrono_Split_NF_GARCH")

cat("NF-GARCH Density Forecast Metrics:\n")
cat("  Total models with density metrics: ", sum(!is.na(nf_results$PredictiveLogLik)), "\n")
cat("  Mean Predictive Log-Likelihood: ", round(mean(nf_results$PredictiveLogLik, na.rm = TRUE), 2), "\n")
cat("  Median Predictive Log-Likelihood: ", round(median(nf_results$PredictiveLogLik, na.rm = TRUE), 2), "\n")
cat("  Min: ", round(min(nf_results$PredictiveLogLik, na.rm = TRUE), 2), "\n")
cat("  Max: ", round(max(nf_results$PredictiveLogLik, na.rm = TRUE), 2), "\n")
cat("  SD: ", round(sd(nf_results$PredictiveLogLik, na.rm = TRUE), 2), "\n")
cat("  Mean NPaths: ", round(mean(nf_results$NPaths, na.rm = TRUE), 0), "\n\n")

# By Model Type
cat("=== DENSITY METRICS BY MODEL TYPE ===\n\n")
by_model <- nf_results %>%
  group_by(Model, Distribution) %>%
  summarise(
    n = n(),
    Mean_PredictiveLogLik = round(mean(PredictiveLogLik, na.rm = TRUE), 2),
    Median_PredictiveLogLik = round(median(PredictiveLogLik, na.rm = TRUE), 2),
    Mean_NPaths = round(mean(NPaths, na.rm = TRUE), 0),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_PredictiveLogLik))

print(by_model)
cat("\n")

# By Asset
cat("=== DENSITY METRICS BY ASSET ===\n\n")
by_asset <- nf_results %>%
  group_by(Asset) %>%
  summarise(
    n = n(),
    Mean_PredictiveLogLik = round(mean(PredictiveLogLik, na.rm = TRUE), 2),
    Median_PredictiveLogLik = round(median(PredictiveLogLik, na.rm = TRUE), 2),
    Mean_NPaths = round(mean(NPaths, na.rm = TRUE), 0),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_PredictiveLogLik))

print(by_asset)
cat("\n")

# Check if standard GARCH has density metrics
cat("=== CHECKING STANDARD GARCH DENSITY METRICS ===\n")
comp_file <- "results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx"
if (file.exists(comp_file)) {
  comp_sheets <- names(loadWorkbook(comp_file))
  cat("  Comparison file sheets: ", paste(comp_sheets, collapse = ", "), "\n")
  
  # Check if standard GARCH was evaluated with density metrics
  # Standard GARCH typically doesn't have PredictiveLogLik in the comparison
  # because it uses single-path forecasts, not multi-path density forecasts
  cat("\n  Note: Standard GARCH models in the comparison use single-path forecasts,\n")
  cat("        so they don't have PredictiveLogLik (density forecast metric).\n")
  cat("        PredictiveLogLik requires multiple simulation paths (1000 paths)\n")
  cat("        to evaluate the full predictive distribution.\n\n")
  
  cat("  NF-GARCH Advantage:\n")
  cat("    - Uses 1000 simulation paths per forecast\n")
  cat("    - Provides full predictive distribution\n")
  cat("    - Enables density forecast evaluation (PredictiveLogLik)\n")
  cat("    - Captures uncertainty in forecasts\n\n")
  
  cat("  Standard GARCH Limitation:\n")
  cat("    - Uses single-path forecasts\n")
  cat("    - Only provides point forecasts (MSE, MAE)\n")
  cat("    - Cannot evaluate density forecast quality\n")
  cat("    - Does not capture forecast uncertainty\n\n")
}

# Best density forecasts
cat("=== BEST DENSITY FORECASTS (Top 10 by PredictiveLogLik) ===\n\n")
best_density <- nf_results %>%
  arrange(desc(PredictiveLogLik)) %>%
  head(10) %>%
  select(Asset, Model, Distribution, PredictiveLogLik, NPaths, MSE, MAE)

print(best_density)
cat("\n")

cat("=== INTERPRETATION ===\n")
cat("Predictive Log-Likelihood measures how well the model's predictive\n")
cat("distribution matches the actual observed returns.\n\n")
cat("Higher values = Better density forecasts\n")
cat("Positive values = Model assigns higher probability to observed returns\n")
cat("Negative values = Model assigns lower probability (but may still be good relative to others)\n\n")

cat("NF-GARCH provides density forecasts (full predictive distributions)\n")
cat("while Standard GARCH only provides point forecasts.\n")
cat("This is a fundamental advantage of NF-GARCH for risk management\n")
cat("and uncertainty quantification.\n")
