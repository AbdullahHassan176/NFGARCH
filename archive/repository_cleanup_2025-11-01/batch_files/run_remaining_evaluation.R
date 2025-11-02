#!/usr/bin/env Rscript
# Run Remaining Evaluation Scripts Using Existing Outputs
# Execute this script in R Studio to complete the evaluation phase

cat("\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("RUNNING REMAINING EVALUATION SCRIPTS\n")
cat("Using Existing Outputs (No Re-running Required)\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("\n")

# Set working directory
if (basename(getwd()) != "Financial-SDG-GARCH") {
  if (file.exists("scripts")) {
    cat("✓ Working directory OK\n")
  } else {
    cat("⚠ WARNING: Please set working directory to project root\n")
    cat("  Use: setwd('C:/Github/Financial-SDG-GARCH')\n")
    stop("Working directory issue")
  }
} else {
  cat("✓ Working directory: ", getwd(), "\n")
}

# Load required libraries
cat("\nLoading required libraries...\n")
required_packages <- c("openxlsx", "dplyr", "tidyr", "stringr", "xts")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]

if (length(missing_packages) > 0) {
  cat("⚠ Installing missing packages:", paste(missing_packages, collapse=", "), "\n")
  install.packages(missing_packages, dependencies = TRUE)
}

invisible(lapply(required_packages, library, character.only = TRUE))
cat("✓ All required libraries loaded\n")

# Optional packages (warnings if missing, but scripts will work)
optional_packages <- c("transport", "moments")
for (pkg in optional_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("⚠ Optional package not found:", pkg, "(scripts will use manual calculation)\n")
  }
}

cat("\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("STEP 1: Update NF vs Standard Comparison (Wilcoxon, Asset-Class)\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("\n")

tryCatch({
  source("scripts/evaluation/compare_nf_vs_standard_garch.R")
  cat("\n✅ Step 1 completed successfully\n")
}, error = function(e) {
  cat("\n⚠ ERROR in Step 1:", e$message, "\n")
  cat("Continuing to next step...\n")
})

cat("\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("STEP 2: Calculate Distributional Metrics\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("\n")

tryCatch({
  source("scripts/evaluation/calculate_distributional_metrics.R")
  cat("\n✅ Step 2 completed successfully\n")
}, error = function(e) {
  cat("\n⚠ ERROR in Step 2:", e$message, "\n")
  cat("Continuing to next step...\n")
})

cat("\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("STEP 3: Calculate Stylized Facts\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("\n")

tryCatch({
  source("scripts/evaluation/calculate_stylized_facts.R")
  cat("\n✅ Step 3 completed successfully\n")
}, error = function(e) {
  cat("\n⚠ ERROR in Step 3:", e$message, "\n")
  cat("Continuing to next step...\n")
})

cat("\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("STEP 4: VaR Backtesting\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("\n")

tryCatch({
  source("scripts/evaluation/var_backtesting_comprehensive.R")
  cat("\n✅ Step 4 completed successfully\n")
}, error = function(e) {
  cat("\n⚠ ERROR in Step 4:", e$message, "\n")
  cat("Continuing to next step...\n")
})

cat("\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("STEP 5: Stress Testing\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("\n")

tryCatch({
  source("scripts/evaluation/stress_testing_comprehensive.R")
  cat("\n✅ Step 5 completed successfully\n")
}, error = function(e) {
  cat("\n⚠ ERROR in Step 5:", e$message, "\n")
  cat("Continuing to next step...\n")
})

cat("\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("STEP 6: Create Final Dashboard\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("\n")

tryCatch({
  source("scripts/core/create_final_dashboard.R")
  cat("\n✅ Step 6 completed successfully\n")
}, error = function(e) {
  cat("\n⚠ ERROR in Step 6:", e$message, "\n")
  cat("Dashboard creation failed - check previous steps\n")
})

cat("\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("EVALUATION COMPLETE\n")
cat("=", paste0(rep("=", 70), collapse=""), "\n")
cat("\n")

# Verify output files
cat("Verifying output files...\n\n")
output_files <- c(
  "results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx",
  "results/consolidated/Distributional_Metrics.xlsx",
  "results/consolidated/Stylized_Facts.xlsx",
  "results/consolidated/VaR_Backtesting.xlsx",
  "results/consolidated/Stress_Testing.xlsx",
  "results/consolidated/Final_Dashboard.xlsx"
)

for (file in output_files) {
  if (file.exists(file)) {
    cat("✓", file, "\n")
  } else {
    cat("✗", file, "(MISSING)\n")
  }
}

cat("\n")
cat("✅ All remaining evaluation scripts completed!\n")
cat("📊 Results saved to: results/consolidated/\n")
cat("📋 Dashboard: results/consolidated/Final_Dashboard.xlsx\n")
cat("\n")

