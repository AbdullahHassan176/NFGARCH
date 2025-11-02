#!/usr/bin/env Rscript
# Dual Engine Consolidation Module
# This script consolidates results from both rugarch and manual engines
# ensuring proper like-for-like comparison between standard GARCH and NF-GARCH models

# Load required libraries
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# Load configuration and utilities
source("scripts/core/config.R")
source("scripts/core/utils.R")

# =============================================================================
# DUAL ENGINE CONSOLIDATION FUNCTIONS
# =============================================================================

# Main dual engine consolidation function
consolidate_dual_engine_results <- function() {
  cat("=== DUAL ENGINE CONSOLIDATION ===\n")
  cat("Consolidating results from both rugarch and manual engines\n")
  
  # Initialize results storage
  all_results <- list()
  
  # Load rugarch results
  cat("Loading rugarch engine results...\n")
  rugarch_results <- load_engine_results("rugarch")
  
  # Load manual results
  cat("Loading manual engine results...\n")
  manual_results <- load_engine_results("manual")
  
  # Combine results with engine labeling
  all_results$rugarch <- rugarch_results
  all_results$manual <- manual_results
  
  # Create comprehensive comparison
  comparison_results <- create_engine_comparison(rugarch_results, manual_results)
  all_results$comparison <- comparison_results
  
  # Save consolidated results
  save_dual_engine_results(all_results)
  
  cat("OK: Dual engine consolidation completed\n")
  return(all_results)
}

# Load results for a specific engine
load_engine_results <- function(engine) {
  results <- list()
  
  # Define engine-specific paths
  if (engine == "rugarch") {
    base_path <- "results/rugarch_results"
    consolidated_path <- file.path(base_path, "consolidated")
    plots_path <- file.path(base_path, "plots")
  } else if (engine == "manual") {
    base_path <- "results/manual_results"
    consolidated_path <- file.path(base_path, "consolidated")
    plots_path <- file.path(base_path, "plots")
  } else {
    stop("Unknown engine: ", engine)
  }
  
  # Load GARCH model fitting results
  garch_file <- file.path(consolidated_path, paste0("Initial_GARCH_Model_Fitting", ifelse(engine == "manual", "_Manual", ""), ".xlsx"))
  if (file.exists(garch_file)) {
    tryCatch({
      sheets <- excel_sheets(garch_file)
      for (sheet in sheets) {
        data <- read_excel(garch_file, sheet = sheet)
        data$Engine <- engine
        data$Model_Type <- "Standard_GARCH"
        results[[paste0("garch_", sheet)]] <- data
      }
      cat("OK: Loaded", engine, "GARCH results\n")
    }, error = function(e) {
      cat("WARNING: Could not load", engine, "GARCH results:", e$message, "\n")
    })
  }
  
  # Load NF-GARCH results
  nf_garch_file <- file.path(consolidated_path, paste0("NF_GARCH_Results_", engine, ".xlsx"))
  if (file.exists(nf_garch_file)) {
    tryCatch({
      sheets <- excel_sheets(nf_garch_file)
      for (sheet in sheets) {
        data <- read_excel(nf_garch_file, sheet = sheet)
        data$Engine <- engine
        data$Model_Type <- "NF_GARCH"
        results[[paste0("nf_garch_", sheet)]] <- data
      }
      cat("OK: Loaded", engine, "NF-GARCH results\n")
    }, error = function(e) {
      cat("WARNING: Could not load", engine, "NF-GARCH results:", e$message, "\n")
    })
  }
  
  # Load additional results from outputs directory
  outputs_path <- "outputs"
  if (dir.exists(outputs_path)) {
    # Load model evaluation results
    model_eval_path <- file.path(outputs_path, "model_eval", "tables")
    if (dir.exists(model_eval_path)) {
      eval_files <- list.files(model_eval_path, pattern = ".*\\.csv", full.names = TRUE)
      for (file in eval_files) {
        tryCatch({
          data <- read.csv(file)
          data$Engine <- engine
          results[[paste0("eval_", basename(file))]] <- data
        }, error = function(e) {
          cat("WARNING: Could not load evaluation file", file, ":", e$message, "\n")
        })
      }
    }
    
    # Load VaR backtesting results
    var_path <- file.path(outputs_path, "var_backtest", "tables")
    if (dir.exists(var_path)) {
      var_files <- list.files(var_path, pattern = ".*\\.csv", full.names = TRUE)
      for (file in var_files) {
        tryCatch({
          data <- read.csv(file)
          data$Engine <- engine
          results[[paste0("var_", basename(file))]] <- data
        }, error = function(e) {
          cat("WARNING: Could not load VaR file", file, ":", e$message, "\n")
        })
      }
    }
    
    # Load stress testing results
    stress_path <- file.path(outputs_path, "stress_tests", "tables")
    if (dir.exists(stress_path)) {
      stress_files <- list.files(stress_path, pattern = ".*\\.csv", full.names = TRUE)
      for (file in stress_files) {
        tryCatch({
          data <- read.csv(file)
          data$Engine <- engine
          results[[paste0("stress_", basename(file))]] <- data
        }, error = function(e) {
          cat("WARNING: Could not load stress file", file, ":", e$message, "\n")
        })
      }
    }
  }
  
  return(results)
}

# Create comprehensive engine comparison
create_engine_comparison <- function(rugarch_results, manual_results) {
  cat("Creating engine comparison...\n")
  
  comparison <- list()
  
  # Compare GARCH model performance
  comparison$garch_performance <- compare_garch_performance(rugarch_results, manual_results)
  
  # Compare NF-GARCH performance
  comparison$nf_garch_performance <- compare_nf_garch_performance(rugarch_results, manual_results)
  
  # Create like-for-like comparison (manual GARCH vs NF-GARCH)
  comparison$like_for_like <- create_like_for_like_comparison(manual_results)
  
  # Create model mapping summary
  comparison$model_mapping <- create_model_mapping_summary()
  
  return(comparison)
}

# Compare GARCH model performance between engines
compare_garch_performance <- function(rugarch_results, manual_results) {
  # Extract GARCH performance data from both engines
  rugarch_garch <- extract_garch_performance(rugarch_results)
  manual_garch <- extract_garch_performance(manual_results)
  
  if (nrow(rugarch_garch) == 0 || nrow(manual_garch) == 0) {
    return(data.frame())
  }
  
  # Combine and compare
  rugarch_garch$Engine <- "rugarch"
  manual_garch$Engine <- "manual"
  
  combined <- bind_rows(rugarch_garch, manual_garch)
  
  # Create comparison summary
  comparison_summary <- combined %>%
    group_by(Model, Engine) %>%
    summarise(
      Avg_AIC = mean(AIC, na.rm = TRUE),
      Avg_BIC = mean(BIC, na.rm = TRUE),
      Avg_LogLik = mean(LogLikelihood, na.rm = TRUE),
      Avg_MSE = mean(MSE, na.rm = TRUE),
      Avg_MAE = mean(MAE, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = Engine,
      values_from = c(Avg_AIC, Avg_BIC, Avg_LogLik, Avg_MSE, Avg_MAE),
      names_sep = "_"
    ) %>%
    mutate(
      AIC_Diff = Avg_AIC_manual - Avg_AIC_rugarch,
      BIC_Diff = Avg_BIC_manual - Avg_BIC_rugarch,
      MSE_Diff = Avg_MSE_manual - Avg_MSE_rugarch,
      MAE_Diff = Avg_MAE_manual - Avg_MAE_rugarch,
      # Performance ranking
      Rugarch_Rank = rank(Avg_MSE_rugarch),
      Manual_Rank = rank(Avg_MSE_manual),
      Rank_Change = Manual_Rank - Rugarch_Rank
    ) %>%
    arrange(Rugarch_Rank)
  
  return(comparison_summary)
}

# Compare NF-GARCH performance between engines
compare_nf_garch_performance <- function(rugarch_results, manual_results) {
  # Extract NF-GARCH performance data from both engines
  rugarch_nf <- extract_nf_garch_performance(rugarch_results)
  manual_nf <- extract_nf_garch_performance(manual_results)
  
  if (nrow(rugarch_nf) == 0 || nrow(manual_nf) == 0) {
    return(data.frame())
  }
  
  # Combine and compare
  rugarch_nf$Engine <- "rugarch"
  manual_nf$Engine <- "manual"
  
  combined <- bind_rows(rugarch_nf, manual_nf)
  
  # Create comparison summary
  comparison_summary <- combined %>%
    group_by(Model, Engine) %>%
    summarise(
      Avg_AIC = mean(AIC, na.rm = TRUE),
      Avg_BIC = mean(BIC, na.rm = TRUE),
      Avg_LogLik = mean(LogLikelihood, na.rm = TRUE),
      Avg_MSE = mean(MSE, na.rm = TRUE),
      Avg_MAE = mean(MAE, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = Engine,
      values_from = c(Avg_AIC, Avg_BIC, Avg_LogLik, Avg_MSE, Avg_MAE),
      names_sep = "_"
    ) %>%
    mutate(
      AIC_Diff = Avg_AIC_manual - Avg_AIC_rugarch,
      BIC_Diff = Avg_BIC_manual - Avg_BIC_rugarch,
      MSE_Diff = Avg_MSE_manual - Avg_MSE_rugarch,
      MAE_Diff = Avg_MAE_manual - Avg_MAE_rugarch,
      # Performance ranking
      Rugarch_Rank = rank(Avg_MSE_rugarch),
      Manual_Rank = rank(Avg_MSE_manual),
      Rank_Change = Manual_Rank - Rugarch_Rank
    ) %>%
    arrange(Rugarch_Rank)
  
  return(comparison_summary)
}

# Create like-for-like comparison (manual GARCH vs NF-GARCH)
create_like_for_like_comparison <- function(manual_results) {
  cat("Creating like-for-like comparison (manual GARCH vs NF-GARCH)...\n")
  
  # Extract manual GARCH performance
  manual_garch <- extract_garch_performance(manual_results)
  manual_garch$Model_Type <- "Standard_GARCH"
  
  # Extract manual NF-GARCH performance
  manual_nf <- extract_nf_garch_performance(manual_results)
  manual_nf$Model_Type <- "NF_GARCH"
  
  if (nrow(manual_garch) == 0 || nrow(manual_nf) == 0) {
    return(data.frame())
  }
  
  # Combine for comparison
  combined <- bind_rows(manual_garch, manual_nf)
  
  # Create comparison summary
  comparison_summary <- combined %>%
    group_by(Model, Model_Type) %>%
    summarise(
      Avg_AIC = mean(AIC, na.rm = TRUE),
      Avg_BIC = mean(BIC, na.rm = TRUE),
      Avg_LogLik = mean(LogLikelihood, na.rm = TRUE),
      Avg_MSE = mean(MSE, na.rm = TRUE),
      Avg_MAE = mean(MAE, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = Model_Type,
      values_from = c(Avg_AIC, Avg_BIC, Avg_LogLik, Avg_MSE, Avg_MAE),
      names_sep = "_"
    ) %>%
    mutate(
      AIC_Improvement = Avg_AIC_Standard_GARCH - Avg_AIC_NF_GARCH,
      BIC_Improvement = Avg_BIC_Standard_GARCH - Avg_BIC_NF_GARCH,
      MSE_Improvement = Avg_MSE_Standard_GARCH - Avg_MSE_NF_GARCH,
      MAE_Improvement = Avg_MAE_Standard_GARCH - Avg_MAE_NF_GARCH,
      # Performance ranking
      Standard_Rank = rank(Avg_MSE_Standard_GARCH),
      NF_Rank = rank(Avg_MSE_NF_GARCH),
      Rank_Change = NF_Rank - Standard_Rank,
      # Improvement percentages
      MSE_Improvement_Pct = (MSE_Improvement / Avg_MSE_Standard_GARCH) * 100,
      MAE_Improvement_Pct = (MAE_Improvement / Avg_MAE_Standard_GARCH) * 100
    ) %>%
    arrange(Standard_Rank)
  
  return(comparison_summary)
}

# Create model mapping summary
create_model_mapping_summary <- function() {
  # Create a summary of how models are mapped between standard GARCH and NF-GARCH
  mapping <- data.frame(
    Standard_GARCH_Model = c("sGARCH_norm", "sGARCH_sstd", "eGARCH", "gjrGARCH", "TGARCH"),
    NF_GARCH_Model = c("NF--sGARCH", "NF--sGARCH", "NF--eGARCH", "NF--gjrGARCH", "NF--TGARCH"),
    Base_Model = c("sGARCH", "sGARCH", "eGARCH", "gjrGARCH", "TGARCH"),
    Description = c(
      "Standard GARCH with Normal Distribution",
      "Standard GARCH with Skewed Student-t Distribution", 
      "Exponential GARCH with Normal Distribution",
      "Glosten-Jagannathan-Runkle GARCH",
      "Threshold GARCH (fGARCH with submodel=TGARCH)"
    ),
    Engine_Used = c("manual", "manual", "manual", "manual", "manual"),
    Comparison_Type = c("like_for_like", "like_for_like", "like_for_like", "like_for_like", "like_for_like")
  )
  
  return(mapping)
}

# Extract GARCH performance data
extract_garch_performance <- function(results) {
  performance_data <- data.frame()
  
  for (key in names(results)) {
    if (grepl("^garch_", key) && is.data.frame(results[[key]])) {
      data <- results[[key]]
      if (nrow(data) > 0 && all(c("Model", "AIC", "BIC") %in% colnames(data))) {
        # Extract performance metrics
        perf_data <- data %>%
          select(Model, any_of(c("Asset", "AIC", "BIC", "LogLikelihood", "MSE", "MAE", "MSE..Forecast.vs.Actual.", "MAE..Forecast.vs.Actual."))) %>%
          mutate(
            MSE = if("MSE" %in% names(.)) MSE else if("MSE..Forecast.vs.Actual." %in% names(.)) `MSE..Forecast.vs.Actual.` else NA,
            MAE = if("MAE" %in% names(.)) MAE else if("MAE..Forecast.vs.Actual." %in% names(.)) `MAE..Forecast.vs.Actual.` else NA
          ) %>%
          select(Model, any_of(c("Asset")), AIC, BIC, LogLikelihood, MSE, MAE)
        
        performance_data <- bind_rows(performance_data, perf_data)
      }
    }
  }
  
  return(performance_data)
}

# Extract NF-GARCH performance data
extract_nf_garch_performance <- function(results) {
  performance_data <- data.frame()
  
  for (key in names(results)) {
    if (grepl("^nf_garch_", key) && is.data.frame(results[[key]])) {
      data <- results[[key]]
      if (nrow(data) > 0 && all(c("Model", "AIC", "BIC") %in% colnames(data))) {
        # Extract performance metrics
        perf_data <- data %>%
          select(Model, any_of(c("Asset", "AIC", "BIC", "LogLikelihood", "MSE", "MAE", "MSE..Forecast.vs.Actual.", "MAE..Forecast.vs.Actual."))) %>%
          mutate(
            MSE = if("MSE" %in% names(.)) MSE else if("MSE..Forecast.vs.Actual." %in% names(.)) `MSE..Forecast.vs.Actual.` else NA,
            MAE = if("MAE" %in% names(.)) MAE else if("MAE..Forecast.vs.Actual." %in% names(.)) `MAE..Forecast.vs.Actual.` else NA
          ) %>%
          select(Model, any_of(c("Asset")), AIC, BIC, LogLikelihood, MSE, MAE)
        
        performance_data <- bind_rows(performance_data, perf_data)
      }
    }
  }
  
  return(performance_data)
}

# Save dual engine results
save_dual_engine_results <- function(all_results) {
  cat("Saving dual engine results...\n")
  
  # Create output directory
  output_dir <- "results/consolidated"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Create comprehensive workbook
  wb <- createWorkbook()
  
  # Add engine comparison sheets
  if ("comparison" %in% names(all_results)) {
    comp <- all_results$comparison
    
    if ("garch_performance" %in% names(comp) && nrow(comp$garch_performance) > 0) {
      addWorksheet(wb, "GARCH_Engine_Comparison")
      writeData(wb, "GARCH_Engine_Comparison", comp$garch_performance)
    }
    
    if ("nf_garch_performance" %in% names(comp) && nrow(comp$nf_garch_performance) > 0) {
      addWorksheet(wb, "NF_GARCH_Engine_Comparison")
      writeData(wb, "NF_GARCH_Engine_Comparison", comp$nf_garch_performance)
    }
    
    if ("like_for_like" %in% names(comp) && nrow(comp$like_for_like) > 0) {
      addWorksheet(wb, "Like_For_Like_Comparison")
      writeData(wb, "Like_For_Like_Comparison", comp$like_for_like)
    }
    
    if ("model_mapping" %in% names(comp) && nrow(comp$model_mapping) > 0) {
      addWorksheet(wb, "Model_Mapping_Summary")
      writeData(wb, "Model_Mapping_Summary", comp$model_mapping)
    }
  }
  
  # Add rugarch results sheets
  if ("rugarch" %in% names(all_results)) {
    rugarch_data <- all_results$rugarch
    for (key in names(rugarch_data)) {
      if (is.data.frame(rugarch_data[[key]]) && nrow(rugarch_data[[key]]) > 0) {
        sheet_name <- paste0("Rugarch_", str_replace_all(key, "_", "_"))
        if (nchar(sheet_name) > 31) {
          sheet_name <- substr(sheet_name, 1, 31)
        }
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet_name, rugarch_data[[key]])
      }
    }
  }
  
  # Add manual results sheets
  if ("manual" %in% names(all_results)) {
    manual_data <- all_results$manual
    for (key in names(manual_data)) {
      if (is.data.frame(manual_data[[key]]) && nrow(manual_data[[key]]) > 0) {
        sheet_name <- paste0("Manual_", str_replace_all(key, "_", "_"))
        if (nchar(sheet_name) > 31) {
          sheet_name <- substr(sheet_name, 1, 31)
        }
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet_name, manual_data[[key]])
      }
    }
  }
  
  # Save workbook
  output_file <- file.path(output_dir, "Dissertation_Consolidated_Results_Dual_Engine.xlsx")
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  cat("OK: Dual engine results saved to:", output_file, "\n")
  return(output_file)
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

# Main execution function
main <- function() {
  tryCatch({
    result <- consolidate_dual_engine_results()
    cat("Dual engine consolidation completed successfully\n")
  }, error = function(e) {
    cat("ERROR: Dual engine consolidation failed:", e$message, "\n")
    quit(status = 1)
  })
}

# Run main function if script is executed directly
if (!interactive() && length(commandArgs(trailingOnly = TRUE)) > 0) {
  main()
}

