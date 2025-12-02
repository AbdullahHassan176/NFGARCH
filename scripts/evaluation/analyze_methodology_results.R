#!/usr/bin/env Rscript
# Analyze Methodology Results
# Checks for errors and provides analysis of all methodology results

library(openxlsx)
library(dplyr)

cat("=== ANALYZING METHODOLOGY RESULTS ===\n\n")

# =============================================================================
# CHECK FILES EXIST
# =============================================================================

files_to_check <- c(
  "results/consolidated/Methodology_Residual_Stationarity.xlsx",
  "results/consolidated/Methodology_Hyperparameter_Sensitivity.xlsx",
  "results/consolidated/Methodology_Conditional_Heterogeneity.xlsx",
  "results/consolidated/Methodology_Consolidated.xlsx"
)

cat("Checking files...\n")
for (file in files_to_check) {
  if (file.exists(file)) {
    size <- file.info(file)$size
    cat("  ✓", basename(file), "-", round(size/1024, 2), "KB\n")
  } else {
    cat("  ✗", basename(file), "- MISSING!\n")
  }
}

# =============================================================================
# ANALYZE STATIONARITY RESULTS
# =============================================================================

cat("\n=== RESIDUAL STATIONARITY ANALYSIS ===\n")

if (file.exists(files_to_check[1])) {
  tryCatch({
    sheets <- getSheetNames(files_to_check[1])
    cat("Sheets:", paste(sheets, collapse=", "), "\n")
    
    # Read main results
    data <- read.xlsx(files_to_check[1], sheet = "Stationarity_Tests")
    cat("\nTotal tests:", nrow(data), "\n")
    cat("Models tested:", length(unique(data$Model)), "\n")
    cat("Assets tested:", length(unique(data$Asset)), "\n\n")
    
    # Analyze ADF results
    adf_stationary <- sum(data$ADF_PValue < 0.05, na.rm = TRUE)
    adf_total <- sum(!is.na(data$ADF_PValue))
    cat("ADF Test Results:\n")
    cat("  Stationary (p < 0.05):", adf_stationary, "out of", adf_total, 
        paste0("(", round(100*adf_stationary/adf_total, 1), "%)\n"))
    
    # Analyze KPSS results
    kpss_stationary <- sum(data$KPSS_PValue >= 0.05, na.rm = TRUE)
    kpss_total <- sum(!is.na(data$KPSS_PValue))
    cat("KPSS Test Results:\n")
    cat("  Stationary (p >= 0.05):", kpss_stationary, "out of", kpss_total,
        paste0("(", round(100*kpss_stationary/kpss_total, 1), "%)\n"))
    
    # Analyze Ljung-Box results
    lb_no_corr <- sum(data$LjungBox_PValue >= 0.05, na.rm = TRUE)
    lb_total <- sum(!is.na(data$LjungBox_PValue))
    cat("Ljung-Box Test Results:\n")
    cat("  No serial correlation (p >= 0.05):", lb_no_corr, "out of", lb_total,
        paste0("(", round(100*lb_no_corr/lb_total, 1), "%)\n"))
    
    # Analyze ARCH results
    arch_no_effects <- sum(data$ARCH_PValue >= 0.05, na.rm = TRUE)
    arch_total <- sum(!is.na(data$ARCH_PValue))
    cat("ARCH LM Test Results:\n")
    cat("  No ARCH effects (p >= 0.05):", arch_no_effects, "out of", arch_total,
        paste0("(", round(100*arch_no_effects/arch_total, 1), "%)\n"))
    
    # Summary by model
    if ("Summary_By_Model" %in% sheets) {
      cat("\nSummary by Model:\n")
      summary_by_model <- read.xlsx(files_to_check[1], sheet = "Summary_By_Model")
      print(summary_by_model)
    }
    
  }, error = function(e) {
    cat("ERROR reading stationarity file:", e$message, "\n")
  })
} else {
  cat("File not found!\n")
}

# =============================================================================
# ANALYZE HYPERPARAMETER RESULTS
# =============================================================================

cat("\n=== HYPERPARAMETER SENSITIVITY ANALYSIS ===\n")

if (file.exists(files_to_check[2])) {
  tryCatch({
    sheets <- getSheetNames(files_to_check[2])
    cat("Sheets:", paste(sheets, collapse=", "), "\n\n")
    
    # Read summary
    summary <- read.xlsx(files_to_check[2], sheet = "Hyperparameter_Summary")
    cat("Hyperparameters Tested:\n")
    for (i in 1:nrow(summary)) {
      cat("  ", summary$Parameter[i], ":\n")
      cat("    Current Value:", summary$Current_Value[i], "\n")
      cat("    Test Values:", summary$Test_Values[i], "\n")
      cat("    Method:", summary$Selection_Method[i], "\n")
    }
    
    # Read methodology description
    if ("Methodology_Description" %in% sheets) {
      methodology <- read.xlsx(files_to_check[2], sheet = "Methodology_Description")
      cat("\nMethodology Sections:", nrow(methodology), "\n")
    }
    
  }, error = function(e) {
    cat("ERROR reading hyperparameter file:", e$message, "\n")
  })
} else {
  cat("File not found!\n")
}

# =============================================================================
# ANALYZE CONDITIONAL HETEROGENEITY RESULTS
# =============================================================================

cat("\n=== CONDITIONAL HETEROGENEITY ANALYSIS ===\n")

if (file.exists(files_to_check[3])) {
  tryCatch({
    sheets <- getSheetNames(files_to_check[3])
    cat("Sheets:", paste(sheets, collapse=", "), "\n")
    
    # Read main results
    data <- read.xlsx(files_to_check[3], sheet = "Heterogeneity_Tests")
    cat("\nTotal tests:", nrow(data), "\n")
    
    # Analyze rolling variance
    time_varying_var <- sum(data$RollingVar_Trend_PValue < 0.05, na.rm = TRUE)
    tv_total <- sum(!is.na(data$RollingVar_Trend_PValue))
    cat("\nRolling Variance Analysis:\n")
    cat("  Time-varying variance detected:", time_varying_var, "out of", tv_total,
        paste0("(", round(100*time_varying_var/tv_total, 1), "%)\n"))
    
    # Analyze structural breaks
    structural_breaks <- sum(data$CUSUM_PValue < 0.05, na.rm = TRUE)
    sb_total <- sum(!is.na(data$CUSUM_PValue))
    cat("Structural Break Tests:\n")
    cat("  Structural breaks detected:", structural_breaks, "out of", sb_total,
        paste0("(", round(100*structural_breaks/sb_total, 1), "%)\n"))
    
    # Analyze ARCH effects
    arch_effects <- sum(data$ARCH_PValue < 0.05, na.rm = TRUE)
    arch_total <- sum(!is.na(data$ARCH_PValue))
    cat("ARCH Effects:\n")
    cat("  ARCH effects present:", arch_effects, "out of", arch_total,
        paste0("(", round(100*arch_effects/arch_total, 1), "%)\n"))
    
    # Analyze NF stability
    unstable <- sum(data$NF_Stability_SD_Ratio > 1.2 | data$NF_Stability_SD_Ratio < 0.8, na.rm = TRUE)
    stable_total <- sum(!is.na(data$NF_Stability_SD_Ratio))
    cat("NF Model Stability:\n")
    cat("  Significant distribution shifts:", unstable, "out of", stable_total,
        paste0("(", round(100*unstable/stable_total, 1), "%)\n"))
    
    # Summary by model
    if ("Summary_By_Model" %in% sheets) {
      cat("\nSummary by Model:\n")
      summary_by_model <- read.xlsx(files_to_check[3], sheet = "Summary_By_Model")
      print(summary_by_model)
    }
    
  }, error = function(e) {
    cat("ERROR reading heterogeneity file:", e$message, "\n")
  })
} else {
  cat("File not found!\n")
}

# =============================================================================
# CHECK CONSOLIDATED FILE
# =============================================================================

cat("\n=== CONSOLIDATED FILE CHECK ===\n")

if (file.exists(files_to_check[4])) {
  tryCatch({
    sheets <- getSheetNames(files_to_check[4])
    cat("Sheets:", paste(sheets, collapse=", "), "\n")
    cat("Total sheets:", length(sheets), "\n")
    
    # Check methodology text
    if ("Methodology_Text" %in% sheets) {
      text_data <- read.xlsx(files_to_check[4], sheet = "Methodology_Text")
      cat("\nMethodology sections:", nrow(text_data), "\n")
    }
    
    # Check summary
    if ("Summary_All_Concerns" %in% sheets) {
      summary <- read.xlsx(files_to_check[4], sheet = "Summary_All_Concerns")
      cat("Concerns addressed:", nrow(summary), "\n")
      cat("\nConcerns:\n")
      for (i in 1:nrow(summary)) {
        cat("  ", i, ".", summary$Concern[i], "-", summary$Status[i], "\n")
      }
    }
    
  }, error = function(e) {
    cat("ERROR reading consolidated file:", e$message, "\n")
  })
} else {
  cat("File not found!\n")
}

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n=== FINAL SUMMARY ===\n")
cat("All methodology analyses completed successfully.\n")
cat("Results are consolidated in results/consolidated/ as XLSX files.\n")
cat("Methodology text is available in results/methodology/methodology_chapter_additions.md\n")
cat("\n=== ANALYSIS COMPLETE ===\n")

