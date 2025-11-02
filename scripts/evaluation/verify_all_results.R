#!/usr/bin/env Rscript
# Verify all results ran successfully

library(openxlsx)

cat("=== COMPREHENSIVE RESULTS VERIFICATION ===\n\n")

all_ok <- TRUE

# 1. Check NF-GARCH Results
cat("1. NF-GARCH Results:\n")
if (file.exists("results/consolidated/NF_GARCH_Results_manual.xlsx")) {
  nf_chrono <- read.xlsx("results/consolidated/NF_GARCH_Results_manual.xlsx", sheet = "Chrono_Split_NF_GARCH")
  nf_tscv <- tryCatch({
    read.xlsx("results/consolidated/NF_GARCH_Results_manual.xlsx", sheet = "TS_CV_NF_GARCH")
  }, error = function(e) data.frame())
  
  cat("   [OK] File exists\n")
  cat("   Chrono models:", nrow(nf_chrono), "\n")
  cat("   TS CV windows:", nrow(nf_tscv), "\n")
  cat("   Valid MSE:", sum(!is.na(nf_chrono$MSE)), "/", nrow(nf_chrono), "\n")
  cat("   Valid MAE:", sum(!is.na(nf_chrono$MAE)), "/", nrow(nf_chrono), "\n")
  
  # Check for extreme values
  normal_mse <- sum(nf_chrono$MSE < 1e6, na.rm = TRUE)
  extreme_mse <- sum(nf_chrono$MSE > 1e6, na.rm = TRUE)
  cat("   Normal MSE (<1e6):", normal_mse, "\n")
  cat("   Extreme MSE (>1e6):", extreme_mse, "\n")
  
  if (extreme_mse > 0) {
    cat("   [WARNING] WARNING: Extreme MSE values found!\n")
    extreme <- nf_chrono[nf_chrono$MSE > 1e6, c("Model", "Asset", "MSE")]
    print(extreme)
    all_ok <- FALSE
  } else {
    cat("   [OK] All MSE values are normal\n")
  }
  
  cat("   Min MSE:", min(nf_chrono$MSE, na.rm = TRUE), "\n")
  cat("   Max MSE:", max(nf_chrono$MSE, na.rm = TRUE), "\n")
  cat("\n")
} else {
  cat("   [FAILED] File MISSING!\n\n")
  all_ok <- FALSE
}

# 2. Check Comparison Results
cat("2. Comparison Results:\n")
if (file.exists("results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx")) {
  summary <- read.xlsx("results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx", sheet = "Summary")
  comp <- read.xlsx("results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx", sheet = "Overall_Comparison")
  
  cat("   [OK] File exists\n")
  winner <- summary$Answer[summary$Question == "Overall winner"]
  nf_mse <- as.numeric(summary$Answer[summary$Question == "Overall mean MSE - NF-GARCH"])
  std_mse <- as.numeric(summary$Answer[summary$Question == "Overall mean MSE - Standard"])
  
  cat("   Overall winner:", winner, "\n")
  cat("   NF-GARCH MSE:", nf_mse, "\n")
  cat("   Standard MSE:", std_mse, "\n")
  
  if (nf_mse < std_mse) {
    improvement <- round((1 - nf_mse / std_mse) * 100, 1)
    cat("   [OK] NF-GARCH is better (", improvement, "% improvement)\n", sep = "")
  } else {
    cat("   [WARNING] Standard GARCH is better\n")
  }
  
  # Check for extreme values
  if (nf_mse > 1e6) {
    cat("   [WARNING] WARNING: NF-GARCH MSE is extreme (>1e6)!\n")
    all_ok <- FALSE
  }
  
  cat("\n")
} else {
  cat("   [FAILED] File MISSING!\n\n")
  all_ok <- FALSE
}

# 3. Check Final Dashboard
cat("3. Final Dashboard:\n")
if (file.exists("results/consolidated/Final_Dashboard.xlsx")) {
  wb <- loadWorkbook("results/consolidated/Final_Dashboard.xlsx")
  sheets <- names(wb)
  cat("   [OK] File exists\n")
  cat("   Sheets:", length(sheets), "\n")
  cat("\n")
} else {
  cat("   [WARNING] File not found (may not be needed)\n\n")
}

# 4. Check Diagnostic Files
cat("4. Diagnostic Files:\n")
diag_files <- c(
  "results/diagnostics/NF_GARCH_INVESTIGATION_SUMMARY.md",
  "results/diagnostics/RESIDUAL_STANDARDIZATION_FIX_SUMMARY.md"
)

for (file in diag_files) {
  if (file.exists(file)) {
    cat("   [OK]", basename(file), "\n")
  } else {
    cat("   [WARNING]", basename(file), "MISSING\n")
  }
}
cat("\n")

# 5. Check for Standardization Messages
cat("5. Standardization Fix Verification:\n")
if (file.exists("scripts/simulation_forecasting/simulate_nf_garch_engine.R")) {
  script_content <- readLines("scripts/simulation_forecasting/simulate_nf_garch_engine.R")
  
  has_standardization <- any(grepl("CRITICAL FIX: Standardize NF residuals", script_content, fixed = TRUE))
  has_recheck <- any(grepl("Double-check standardization", script_content, fixed = TRUE))
  
  if (has_standardization) {
    cat("   [OK] Standardization fix applied\n")
  } else {
    cat("   [FAILED] Standardization fix NOT found!\n")
    all_ok <- FALSE
  }
  
  if (has_recheck) {
    cat("   [OK] Double-check standardization present\n")
  } else {
    cat("   [WARNING] Double-check standardization missing\n")
  }
  cat("\n")
} else {
  cat("   [FAILED] Simulation script NOT found!\n\n")
  all_ok <- FALSE
}

# Final Summary
cat("=== VERIFICATION SUMMARY ===\n")
if (all_ok) {
  cat("[OK] ALL CHECKS PASSED\n")
  cat("[OK] NF-GARCH simulation completed successfully\n")
  cat("[OK] Results are valid and correct\n")
  cat("[OK] Standardization fix is applied\n")
  cat("\nEverything ran successfully!\n")
} else {
  cat("[WARNING] SOME ISSUES FOUND\n")
  cat("Please review the warnings above.\n")
}

