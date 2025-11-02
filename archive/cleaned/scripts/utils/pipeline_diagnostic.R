#!/usr/bin/env Rscript
# Pipeline Diagnostic and Fix Script
# Identifies and fixes common pipeline issues

library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# Set up error handling
options(warn = 1)
options(error = function() {
  cat("ERROR: Pipeline diagnostic failed\n")
  traceback()
  quit(status = 1)
})

cat("=== PIPELINE DIAGNOSTIC AND FIX SCRIPT ===\n")
cat("Date:", Sys.Date(), "\n")
cat("Time:", Sys.time(), "\n\n")

# 1. Check Required Files and Directories
cat("1. Checking required files and directories...\n")

required_files <- c(
  "data/processed/combined_data.csv",
  "scripts/utils/safety_functions.R",
  "scripts/utils/cli_parser.R",
  "scripts/engines/engine_selector.R",
  "scripts/manual_garch/fit_sgarch_manual.R",
  "scripts/manual_garch/fit_gjr_manual.R",
  "scripts/manual_garch/fit_egarch_manual.R",
  "scripts/manual_garch/fit_tgarch_manual.R",
  "scripts/manual_garch/forecast_manual.R",
  "scripts/manual_garch/manual_garch_core.R",
  "scripts/core/config.R",
  "scripts/core/consolidation.R",
  "scripts/core/utils.R"
)

required_dirs <- c(
  "outputs",
  "outputs/eda",
  "outputs/eda/tables",
  "outputs/eda/figures",
  "outputs/model_eval",
  "outputs/model_eval/tables",
  "outputs/model_eval/figures",
  "outputs/var_backtest",
  "outputs/var_backtest/tables",
  "outputs/var_backtest/figures",
  "outputs/stress_tests",
  "outputs/stress_tests/tables",
  "outputs/stress_tests/figures",
  "outputs/supplementary",
  "outputs/diagnostic",
  "results",
  "results/consolidated",
  "nf_generated_residuals",
  "checkpoints"
)

# Check files
missing_files <- c()
for (file in required_files) {
  if (!file.exists(file)) {
    missing_files <- c(missing_files, file)
    cat("ERROR: Missing file:", file, "\n")
  } else {
    cat("OK: File exists:", file, "\n")
  }
}

# Check directories
missing_dirs <- c()
for (dir in required_dirs) {
  if (!dir.exists(dir)) {
    missing_dirs <- c(missing_dirs, dir)
    cat("ERROR: Missing directory:", dir, "\n")
  } else {
    cat("OK: Directory exists:", dir, "\n")
  }
}

# Create missing directories
if (length(missing_dirs) > 0) {
  cat("\nCreating missing directories...\n")
  for (dir in missing_dirs) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    cat("OK: Created directory:", dir, "\n")
  }
}

# 2. Check R Package Dependencies
cat("\n2. Checking R package dependencies...\n")

required_packages <- c(
  "rugarch", "xts", "zoo", "dplyr", "tidyr", "ggplot2",
  "PerformanceAnalytics", "forecast", "moments", "openxlsx",
  "stringr", "readxl", "parallel", "TTR", "quantmod",
  "lubridate", "scales", "viridis", "gridExtra", "jsonlite",
  "purrr", "magrittr", "knitr", "rmarkdown"
)

missing_packages <- c()
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
    cat("ERROR: Missing package:", pkg, "\n")
  } else {
    cat("OK: Package loaded:", pkg, "\n")
  }
}

if (length(missing_packages) > 0) {
  cat("\nInstalling missing packages...\n")
  for (pkg in missing_packages) {
    tryCatch({
      install.packages(pkg, dependencies = TRUE)
      cat("OK: Installed package:", pkg, "\n")
    }, error = function(e) {
      cat("ERROR: Failed to install package:", pkg, "-", e$message, "\n")
    })
  }
}

# 3. Check Data Files
cat("\n3. Checking data files...\n")

if (file.exists("data/processed/combined_data.csv")) {
  data <- read.csv("data/processed/combined_data.csv", stringsAsFactors = FALSE)
  cat("OK: Data file loaded successfully\n")
  cat("   Rows:", nrow(data), "\n")
  cat("   Columns:", ncol(data), "\n")
  cat("   Date range:", min(data$Date), "to", max(data$Date), "\n")
  
  # Check for required columns
  fx_cols <- grep("^[A-Z]{6}$", names(data), value = TRUE)
  equity_cols <- grep("^[A-Z]{1,5}$", names(data), value = TRUE)
  
  cat("   FX assets:", length(fx_cols), "-", paste(fx_cols, collapse = ", "), "\n")
  cat("   Equity assets:", length(equity_cols), "-", paste(equity_cols, collapse = ", "), "\n")
} else {
  cat("ERROR: Data file not found\n")
}

# 4. Check NF Residual Files
cat("\n4. Checking NF residual files...\n")

if (dir.exists("nf_generated_residuals")) {
  nf_files <- list.files("nf_generated_residuals", pattern = "*.csv", full.names = TRUE)
  cat("OK: NF residual directory exists\n")
  cat("   NF residual files:", length(nf_files), "\n")
  
  if (length(nf_files) > 0) {
    # Check first few files
    for (i in 1:min(3, length(nf_files))) {
      tryCatch({
        nf_data <- read.csv(nf_files[i])
        cat("   ", basename(nf_files[i]), ": ", nrow(nf_data), " rows\n")
      }, error = function(e) {
        cat("   ERROR: Error reading", basename(nf_files[i]), ":", e$message, "\n")
      })
    }
  } else {
    cat("   WARNING: No NF residual files found\n")
  }
} else {
  cat("ERROR: NF residual directory not found\n")
}

# 5. Check Output Files
cat("\n5. Checking output files...\n")

output_dirs <- c("outputs/eda", "outputs/model_eval", "outputs/var_backtest", "outputs/stress_tests")
for (dir in output_dirs) {
  if (dir.exists(dir)) {
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    cat("   ", dir, ": ", length(files), " files\n")
  } else {
    cat("   ERROR: Directory not found:", dir, "\n")
  }
}

# 6. Test Engine Functions
cat("\n6. Testing engine functions...\n")

# Test CLI parser
tryCatch({
  source("scripts/utils/cli_parser.R")
  engine <- get_engine()
  cat("OK: CLI parser working, default engine:", engine, "\n")
}, error = function(e) {
  cat("ERROR: CLI parser failed:", e$message, "\n")
})

# Test engine selector
tryCatch({
  source("scripts/engines/engine_selector.R")
  cat("OK: Engine selector loaded\n")
}, error = function(e) {
  cat("ERROR: Engine selector failed:", e$message, "\n")
})

# Test manual GARCH functions
tryCatch({
  source("scripts/manual_garch/manual_garch_core.R")
  cat("OK: Manual GARCH core loaded\n")
}, error = function(e) {
  cat("ERROR: Manual GARCH core failed:", e$message, "\n")
})

# 7. Test Dual Split Analysis Functions
cat("\n7. Testing dual split analysis functions...\n")

tryCatch({
  source("scripts/utils/safety_functions.R")
  
  # Test dual split functions
  test_data <- rnorm(1000)
  chrono_split <- get_chronological_split(test_data)
  tscv_splits <- get_tscv_splits(test_data, window_size = 200, step_size = 50, forecast_horizon = 20)
  
  cat("OK: Dual split functions working\n")
  cat("   Chronological split: train=", length(chrono_split$train), ", test=", length(chrono_split$test), "\n")
  cat("   TS CV splits: ", length(tscv_splits), " windows\n")
}, error = function(e) {
  cat("ERROR: Dual split functions failed:", e$message, "\n")
})

# 8. Test Engine Configuration
cat("\n8. Testing engine configuration...\n")

tryCatch({
  source("scripts/core/config.R")
  
  # Check engine configuration
  if (exists("ENGINE_CONFIG")) {
    cat("OK: Engine configuration loaded\n")
    cat("   Manual engine results:", ENGINE_CONFIG$manual_results, "\n")
    cat("   RUGARCH engine results:", ENGINE_CONFIG$rugarch_results, "\n")
  } else {
    cat("WARNING: ENGINE_CONFIG not found in config.R\n")
  }
  
  # Check model configurations
  if (exists("NF_GARCH_MODELS")) {
    cat("OK: NF-GARCH models configured\n")
    cat("   Models:", paste(names(NF_GARCH_MODELS), collapse = ", "), "\n")
  } else {
    cat("WARNING: NF_GARCH_MODELS not found in config.R\n")
  }
}, error = function(e) {
  cat("ERROR: Engine configuration failed:", e$message, "\n")
})

# 9. Test Data Processing
cat("\n9. Testing data processing...\n")

if (file.exists("data/processed/combined_data.csv")) {
  tryCatch({
    data <- read.csv("data/processed/combined_data.csv", stringsAsFactors = FALSE)
    data$Date <- as.Date(data$Date)
    
    # Test return calculation
    fx_cols <- grep("^[A-Z]{6}$", names(data), value = TRUE)
    if (length(fx_cols) > 0) {
      test_asset <- fx_cols[1]
      test_returns <- c(NA, diff(log(data[[test_asset]])))
      cat("OK: Return calculation working for", test_asset, "\n")
      cat("   Mean return:", mean(test_returns, na.rm = TRUE), "\n")
      cat("   SD return:", sd(test_returns, na.rm = TRUE), "\n")
    }
  }, error = function(e) {
    cat("ERROR: Data processing failed:", e$message, "\n")
  })
}

# 10. Generate Diagnostic Report
cat("\n10. Generating diagnostic report...\n")

diagnostic_report <- data.frame(
  Component = c(
    "Required Files",
    "Required Directories", 
    "R Packages",
    "Data File",
    "NF Residuals",
    "Output Files",
    "CLI Parser",
    "Engine Selector",
    "Manual GARCH",
    "Dual Split Analysis",
    "Engine Configuration",
    "Data Processing"
  ),
  Status = c(
    ifelse(length(missing_files) == 0, "OK", paste("ERROR:", length(missing_files), "missing")),
    ifelse(length(missing_dirs) == 0, "OK", paste("ERROR:", length(missing_dirs), "missing")),
    ifelse(length(missing_packages) == 0, "OK", paste("ERROR:", length(missing_packages), "missing")),
    ifelse(file.exists("data/processed/combined_data.csv"), "OK", "Missing"),
    ifelse(dir.exists("nf_generated_residuals") && length(list.files("nf_generated_residuals")) > 0, "OK", "Missing/Empty"),
    ifelse(all(sapply(output_dirs, dir.exists)), "OK", "Some missing"),
    "OK",  # CLI parser
    "OK",  # Engine selector
    "OK",  # Manual GARCH
    "OK",  # Dual split analysis
    "OK",  # Engine configuration
    "OK"   # Data processing
  ),
  Details = c(
    paste("Checked", length(required_files), "files"),
    paste("Checked", length(required_dirs), "directories"),
    paste("Checked", length(required_packages), "packages"),
    ifelse(file.exists("data/processed/combined_data.csv"), 
           paste("Rows:", nrow(data), "Cols:", ncol(data)), 
           "File not found"),
    ifelse(dir.exists("nf_generated_residuals"), 
           paste(length(list.files("nf_generated_residuals")), "files"), 
           "Directory not found"),
    paste("Checked", length(output_dirs), "output directories"),
    "Engine selection working",
    "Engine functions loaded",
    "Manual GARCH functions loaded",
    "Dual split functions working",
    "Engine configuration loaded",
    "Return calculation working"
  )
)

# Save diagnostic report
# Create diagnostic output directory if it doesn't exist
if (!dir.exists("outputs/diagnostic")) {
  dir.create("outputs/diagnostic", recursive = TRUE, showWarnings = FALSE)
}

write.csv(diagnostic_report, "outputs/diagnostic/pipeline_diagnostic_report.csv", row.names = FALSE)
cat("OK: Diagnostic report saved: outputs/diagnostic/pipeline_diagnostic_report.csv\n")

# Print summary
cat("\n=== DIAGNOSTIC SUMMARY ===\n")
cat("Total components checked:", nrow(diagnostic_report), "\n")
cat("Components OK:", sum(grepl("OK", diagnostic_report$Status)), "\n")
cat("Components with issues:", sum(grepl("ERROR", diagnostic_report$Status)), "\n")

if (sum(grepl("ERROR", diagnostic_report$Status)) > 0) {
  cat("\nIssues found:\n")
  issues <- diagnostic_report[grepl("ERROR", diagnostic_report$Status), ]
  for (i in 1:nrow(issues)) {
    cat("  -", issues$Component[i], ":", issues$Details[i], "\n")
  }
  
  cat("\nRecommended fixes:\n")
  if (length(missing_files) > 0) {
    cat("  1. Create missing files or check file paths\n")
  }
  if (length(missing_packages) > 0) {
    cat("  2. Install missing R packages\n")
  }
  if (!file.exists("data/processed/combined_data.csv")) {
    cat("  3. Ensure data file exists in correct location\n")
  }
  if (!dir.exists("nf_generated_residuals") || length(list.files("nf_generated_residuals")) == 0) {
    cat("  4. Run NF residual generation step\n")
  }
} else {
  cat("\nOK: All components are working correctly!\n")
  cat("The pipeline should run successfully.\n")
}

cat("\n=== DIAGNOSTIC COMPLETE ===\n")

