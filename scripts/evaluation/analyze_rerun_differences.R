# Analyze differences between rerun and original results
# Focus on identifying missing assets/models

library(openxlsx)

cat("========================================\n")
cat("ANALYZING RERUN vs ORIGINAL DIFFERENCES\n")
cat("========================================\n\n")

original_dir <- "results/consolidated"
rerun_dir <- "results/rerun"

# 1. Check NF-GARCH Results - which assets/models are missing?
cat("1. NF-GARCH Results - Missing Assets/Models\n")
cat("========================================\n\n")

orig_file <- file.path(original_dir, "NF_GARCH_Results_manual.xlsx")
rerun_file <- file.path(rerun_dir, "NF_GARCH_Results_manual.xlsx")

if (file.exists(orig_file) && file.exists(rerun_file)) {
  orig_chrono <- read.xlsx(orig_file, sheet = "Chrono_Split_NF_GARCH")
  rerun_chrono <- read.xlsx(rerun_file, sheet = "Chrono_Split_NF_GARCH")
  
  cat("Chrono Split Results:\n")
  cat(sprintf("  Original: %d rows\n", nrow(orig_chrono)))
  cat(sprintf("  Rerun: %d rows\n", nrow(rerun_chrono)))
  cat(sprintf("  Missing: %d rows\n\n", nrow(orig_chrono) - nrow(rerun_chrono)))
  
  # Identify unique combinations in original
  if ("Asset" %in% colnames(orig_chrono) && "Model" %in% colnames(orig_chrono)) {
    orig_combos <- paste(orig_chrono$Asset, orig_chrono$Model, sep = "|")
    rerun_combos <- paste(rerun_chrono$Asset, rerun_chrono$Model, sep = "|")
    missing_combos <- setdiff(orig_combos, rerun_combos)
    
    cat("Missing Asset|Model combinations:\n")
    for (combo in missing_combos[1:min(20, length(missing_combos))]) {
      cat(sprintf("  - %s\n", combo))
    }
    if (length(missing_combos) > 20) {
      cat(sprintf("  ... and %d more\n", length(missing_combos) - 20))
    }
    cat("\n")
  }
  
  orig_tscv <- read.xlsx(orig_file, sheet = "TS_CV_NF_GARCH")
  rerun_tscv <- read.xlsx(rerun_file, sheet = "TS_CV_NF_GARCH")
  
  cat("TS-CV Results:\n")
  cat(sprintf("  Original: %d rows\n", nrow(orig_tscv)))
  cat(sprintf("  Rerun: %d rows\n", nrow(rerun_tscv)))
  cat(sprintf("  Missing: %d rows\n\n", nrow(orig_tscv) - nrow(rerun_tscv)))
}

# 2. Check NF vs Standard - which models are missing?
cat("\n2. NF vs Standard Comparison - Missing Models\n")
cat("========================================\n\n")

orig_file2 <- file.path(original_dir, "NF_vs_Standard_GARCH_Comparison.xlsx")
rerun_file2 <- file.path(rerun_dir, "NF_vs_Standard_GARCH_Comparison.xlsx")

if (file.exists(orig_file2) && file.exists(rerun_file2)) {
  orig_model <- read.xlsx(orig_file2, sheet = "Model_Comparison")
  rerun_model <- read.xlsx(rerun_file2, sheet = "Model_Comparison")
  
  cat("Model Comparison:\n")
  cat(sprintf("  Original: %d rows\n", nrow(orig_model)))
  cat(sprintf("  Rerun: %d rows\n", nrow(rerun_model)))
  
  if ("Model" %in% colnames(orig_model)) {
    orig_models <- orig_model$Model
    rerun_models <- rerun_model$Model
    missing_models <- setdiff(orig_models, rerun_models)
    
    if (length(missing_models) > 0) {
      cat("Missing Models:\n")
      for (model in missing_models) {
        cat(sprintf("  - %s\n", model))
      }
    } else {
      cat("All models present (but fewer rows - check asset combinations)\n")
    }
  }
  cat("\n")
}

# 3. Check Final Dashboard
cat("\n3. Final Dashboard - Summary\n")
cat("========================================\n\n")

orig_file3 <- file.path(original_dir, "Final_Dashboard.xlsx")
rerun_file3 <- file.path(rerun_dir, "Final_Dashboard.xlsx")

if (file.exists(orig_file3) && file.exists(rerun_file3)) {
  orig_perf <- read.xlsx(orig_file3, sheet = "Performance_Chrono")
  rerun_perf <- read.xlsx(rerun_file3, sheet = "Performance_Chrono")
  
  cat("Performance Chrono:\n")
  cat(sprintf("  Original: %d rows\n", nrow(orig_perf)))
  cat(sprintf("  Rerun: %d rows\n", nrow(rerun_perf)))
  
  if ("Model" %in% colnames(orig_perf)) {
    orig_models <- unique(orig_perf$Model)
    rerun_models <- unique(rerun_perf$Model)
    missing_models <- setdiff(orig_models, rerun_models)
    
    if (length(missing_models) > 0) {
      cat("Missing Models:\n")
      for (model in missing_models) {
        cat(sprintf("  - %s\n", model))
      }
    }
  }
  cat("\n")
}

# 4. Check what's in common - compare values for matching rows
cat("\n4. Value Comparison for Matching Rows\n")
cat("========================================\n\n")

if (file.exists(orig_file) && file.exists(rerun_file)) {
  orig_chrono <- read.xlsx(orig_file, sheet = "Chrono_Split_NF_GARCH")
  rerun_chrono <- read.xlsx(rerun_file, sheet = "Chrono_Split_NF_GARCH")
  
  # Find matching rows
  if ("Asset" %in% colnames(orig_chrono) && "Model" %in% colnames(orig_chrono)) {
    orig_chrono$Key <- paste(orig_chrono$Asset, orig_chrono$Model, sep = "|")
    rerun_chrono$Key <- paste(rerun_chrono$Asset, rerun_chrono$Model, sep = "|")
    
    common_keys <- intersect(orig_chrono$Key, rerun_chrono$Key)
    
    if (length(common_keys) > 0) {
      cat(sprintf("Found %d matching Asset|Model combinations\n", length(common_keys)))
      
      # Compare MSE for matching rows
      numeric_cols <- c("MSE", "MAE", "AIC", "BIC", "LogLik")
      numeric_cols <- numeric_cols[numeric_cols %in% colnames(orig_chrono)]
      
      if (length(numeric_cols) > 0) {
        cat("\nComparing values for matching rows:\n")
        for (col in numeric_cols) {
          orig_vals <- orig_chrono[orig_chrono$Key %in% common_keys, col]
          rerun_vals <- rerun_chrono[rerun_chrono$Key %in% common_keys, col]
          
          # Match by key
          orig_sorted <- orig_vals[order(orig_chrono$Key[orig_chrono$Key %in% common_keys])]
          rerun_sorted <- rerun_vals[order(rerun_chrono$Key[rerun_chrono$Key %in% common_keys])]
          
          both_finite <- is.finite(orig_sorted) & is.finite(rerun_sorted)
          if (sum(both_finite) > 0) {
            diffs <- abs(orig_sorted[both_finite] - rerun_sorted[both_finite])
            rel_diffs <- diffs / (abs(rerun_sorted[both_finite]) + 1e-10)
            
            cat(sprintf("  %s: Max diff=%.6f, Max rel diff=%.2f%%, Mean rel diff=%.2f%%\n",
                       col, max(diffs, na.rm = TRUE), max(rel_diffs, na.rm = TRUE)*100,
                       mean(rel_diffs, na.rm = TRUE)*100))
          }
        }
      }
    }
  }
}

cat("\n\nAnalysis complete!\n")




