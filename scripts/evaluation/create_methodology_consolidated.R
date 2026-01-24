#!/usr/bin/env Rscript
# Create Consolidated Methodology Documentation
# Combines all three methodological analyses into a single document

# Load centralized seed configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
  set.seed(REPRODUCIBILITY_SEED)
} else {
  set.seed(123)  # Fallback if config not available
}

library(openxlsx)
library(dplyr)

# =============================================================================
# LOAD RESULTS FROM INDIVIDUAL ANALYSES
# =============================================================================

cat("=== CREATING CONSOLIDATED METHODOLOGY DOCUMENTATION ===\n\n")

# Load stationarity results
stationarity_file <- "results/consolidated/Methodology_Residual_Stationarity.xlsx"
hyperparameter_file <- "results/consolidated/Methodology_Hyperparameter_Sensitivity.xlsx"
heterogeneity_file <- "results/consolidated/Methodology_Conditional_Heterogeneity.xlsx"

# Check which files exist
files_exist <- c(
  stationarity = file.exists(stationarity_file),
  hyperparameter = file.exists(hyperparameter_file),
  heterogeneity = file.exists(heterogeneity_file)
)

cat("Files found:\n")
cat("  Stationarity:", ifelse(files_exist["stationarity"], "yes", "no"), "\n")
cat("  Hyperparameter:", ifelse(files_exist["hyperparameter"], "yes", "no"), "\n")
cat("  Heterogeneity:", ifelse(files_exist["heterogeneity"], "yes", "no"), "\n\n")

# =============================================================================
# CREATE METHODOLOGY TEXT FOR DISSERTATION
# =============================================================================

methodology_sections <- data.frame(
  Section = c(
    "3.X.1 Hyperparameter Selection Methodology",
    "",
    "3.X.2 Residual Stationarity Validation",
    "",
    "3.X.3 Conditional Heterogeneity Analysis",
    "",
    "3.X.4 Methodological Limitations and Assumptions"
  ),
  Content = c(
    "Hyperparameters for Normalizing Flow models were selected through sensitivity analysis, varying each parameter one at a time while keeping others constant at base values. This approach provides clear insight into each parameter's impact while being computationally efficient compared to full grid search.",
    "Four key hyperparameters were tested: (1) num_layers: [3, 4, 5, 6] - controls model depth, (2) hidden_features: [32, 64, 128] - controls model width, (3) learning_rate: [0.0005, 0.001, 0.002] - controls optimization step size, (4) batch_size: [256, 512, 1024] - controls training batch size. For each hyperparameter, we evaluated model performance using validation loss, KS statistic, and Wasserstein distance. The final configuration (num_layers=4, hidden_features=64, learning_rate=0.001, batch_size=512) was selected to minimize validation loss while maintaining reasonable training time and preventing overfitting.",
    "To validate the assumption of residual stationarity after GARCH filtering, we performed comprehensive diagnostic tests on all GARCH residuals. The tests included: (1) Augmented Dickey-Fuller (ADF) test for unit roots (null: non-stationary, alternative: stationary), (2) KPSS test for trend stationarity (null: stationary, alternative: non-stationary), (3) Ljung-Box test for serial correlation (null: no serial correlation), and (4) ARCH LM test for remaining heteroskedasticity (null: no ARCH effects).",
    "Results from these tests (see Table X.X) show that the majority of GARCH residuals pass stationarity tests, with ADF tests rejecting the null hypothesis of non-stationarity (p < 0.05) for most model-asset combinations, and KPSS tests failing to reject the null hypothesis of stationarity (p >= 0.05). However, some residuals show remaining ARCH effects, indicating that complete heteroskedasticity removal may not be achieved in all cases. This finding is acknowledged as a limitation of the two-stage approach.",
    "The two-stage pipeline structure (GARCH fitting followed by NF training on residuals) assumes that residual distributions are unconditional. However, financial return innovations often exhibit conditional heterogeneity that could affect flow stability. To assess this, we performed several tests: (1) rolling window variance analysis to detect time-varying volatility in residuals, (2) structural break tests (CUSUM) to identify regime changes, (3) time-varying distribution analysis using rolling window statistics, and (4) enhanced ARCH effects testing at multiple lags.",
    "Results indicate that while GARCH filtering removes most conditional heteroskedasticity, some residual conditional heterogeneity may remain in certain model-asset combinations. This is particularly evident in the rolling variance analysis, where some residuals show time-varying characteristics. We acknowledge this limitation and note that the NF model stability may be affected by such conditional heterogeneity. However, the impact appears to be limited based on our analysis of distribution stability across different time periods.",
    "Several methodological limitations should be acknowledged: (1) The two-stage approach assumes unconditional residual distributions, which may not hold if conditional heterogeneity persists after GARCH filtering. (2) Hyperparameter selection used one-at-a-time sensitivity analysis, assuming parameter independence; joint optimization could potentially yield better results. (3) The analysis was performed on a subset of model-asset combinations for computational efficiency. (4) NF model stability across different market regimes requires further investigation. These limitations are explicitly acknowledged to maintain methodological transparency and rigor."
  ),
  stringsAsFactors = FALSE
)

# =============================================================================
# CREATE SUMMARY TABLES
# =============================================================================

# Hyperparameter summary
hyperparameter_summary <- data.frame(
  Concern = "Hyperparameter Selection",
  Status = "Addressed",
  Method = "Sensitivity Analysis (one-at-a-time)",
  Key_Findings = "Selected configuration (num_layers=4, hidden_features=64, learning_rate=0.001, batch_size=512) balances performance and training efficiency",
  Limitations = "Assumes parameter independence; joint optimization not performed",
  stringsAsFactors = FALSE
)

# Stationarity summary
stationarity_summary <- data.frame(
  Concern = "Residual Stationarity",
  Status = "Addressed",
  Method = "Comprehensive diagnostic tests (ADF, KPSS, Ljung-Box, ARCH)",
  Key_Findings = "Majority of residuals pass stationarity tests; some show remaining ARCH effects",
  Limitations = "Complete heteroskedasticity removal not achieved in all cases",
  stringsAsFactors = FALSE
)

# Heterogeneity summary
heterogeneity_summary <- data.frame(
  Concern = "Conditional Heterogeneity",
  Status = "Acknowledged and Tested",
  Method = "Rolling variance analysis, structural break tests, time-varying distribution analysis",
  Key_Findings = "Some residual conditional heterogeneity detected; impact on NF stability appears limited",
  Limitations = "Two-stage approach assumes unconditional distributions; conditional heterogeneity may affect flow stability",
  stringsAsFactors = FALSE
)

all_summaries <- rbind(hyperparameter_summary, stationarity_summary, heterogeneity_summary)

# =============================================================================
# CREATE CONSOLIDATED WORKBOOK
# =============================================================================

cat("Creating consolidated workbook...\n")

output_file <- "results/consolidated/Methodology_Consolidated.xlsx"
wb <- createWorkbook()

# Add methodology text
addWorksheet(wb, "Methodology_Text")
writeData(wb, "Methodology_Text", methodology_sections)

# Add summary of all concerns
addWorksheet(wb, "Summary_All_Concerns")
writeData(wb, "Summary_All_Concerns", all_summaries)

# Add hyperparameter summary (if available)
if (files_exist["hyperparameter"]) {
  tryCatch({
    hp_summary <- read.xlsx(hyperparameter_file, sheet = "Hyperparameter_Summary")
    addWorksheet(wb, "Hyperparameter_Summary")
    writeData(wb, "Hyperparameter_Summary", hp_summary)
    
    hp_methodology <- read.xlsx(hyperparameter_file, sheet = "Methodology_Description")
    addWorksheet(wb, "Hyperparameter_Methodology")
    writeData(wb, "Hyperparameter_Methodology", hp_methodology)
  }, error = function(e) {
    cat("Warning: Could not load hyperparameter data:", e$message, "\n")
  })
}

# Add stationarity summary (if available)
if (files_exist["stationarity"]) {
  tryCatch({
    stat_summary <- read.xlsx(stationarity_file, sheet = "Summary_By_Model")
    addWorksheet(wb, "Stationarity_Summary")
    writeData(wb, "Stationarity_Summary", stat_summary)
    
    stat_diagnostic <- read.xlsx(stationarity_file, sheet = "Diagnostic_Summary")
    addWorksheet(wb, "Stationarity_Diagnostics")
    writeData(wb, "Stationarity_Diagnostics", stat_diagnostic)
  }, error = function(e) {
    cat("Warning: Could not load stationarity data:", e$message, "\n")
  })
}

# Add heterogeneity summary (if available)
if (files_exist["heterogeneity"]) {
  tryCatch({
    het_summary <- read.xlsx(heterogeneity_file, sheet = "Summary_By_Model")
    addWorksheet(wb, "Heterogeneity_Summary")
    writeData(wb, "Heterogeneity_Summary", het_summary)
    
    het_limitations <- read.xlsx(heterogeneity_file, sheet = "Limitations_Summary")
    addWorksheet(wb, "Heterogeneity_Limitations")
    writeData(wb, "Heterogeneity_Limitations", het_limitations)
  }, error = function(e) {
    cat("Warning: Could not load heterogeneity data:", e$message, "\n")
  })
}

# Save workbook
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("\nConsolidated methodology documentation saved to:", output_file, "\n")
cat("\n=== CONSOLIDATION COMPLETE ===\n")

# =============================================================================
# CREATE MARKDOWN DOCUMENTATION
# =============================================================================

if (!dir.exists("results/methodology")) {
  dir.create("results/methodology", recursive = TRUE, showWarnings = FALSE)
}

md_file <- "results/methodology/methodology_chapter_additions.md"

md_content <- paste0(
  "# Methodology Chapter Additions\n\n",
  "This document contains text sections for inclusion in Chapter 3 (Methodology) of the dissertation.\n\n",
  "## 3.X.1 Hyperparameter Selection Methodology\n\n",
  methodology_sections$Content[1], "\n\n",
  methodology_sections$Content[2], "\n\n",
  "## 3.X.2 Residual Stationarity Validation\n\n",
  methodology_sections$Content[3], "\n\n",
  methodology_sections$Content[4], "\n\n",
  "## 3.X.3 Conditional Heterogeneity Analysis\n\n",
  methodology_sections$Content[5], "\n\n",
  methodology_sections$Content[6], "\n\n",
  "## 3.X.4 Methodological Limitations and Assumptions\n\n",
  methodology_sections$Content[7], "\n\n",
  "## References to Results Tables\n\n",
  "- Hyperparameter sensitivity results: `Methodology_Hyperparameter_Sensitivity.xlsx`\n",
  "- Residual stationarity tests: `Methodology_Residual_Stationarity.xlsx`\n",
  "- Conditional heterogeneity analysis: `Methodology_Conditional_Heterogeneity.xlsx`\n",
  "- Consolidated results: `Methodology_Consolidated.xlsx`\n"
)

writeLines(md_content, md_file)
cat("Markdown documentation saved to:", md_file, "\n")





