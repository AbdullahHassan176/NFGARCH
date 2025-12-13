# Compare Rerun Results with Original Results
# Systematic comparison of all outputs

# Load centralized seed configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
  set.seed(REPRODUCIBILITY_SEED)
} else {
  set.seed(123)  # Fallback if config not available
}

library(openxlsx)
library(dplyr)

cat("========================================\n")
cat("COMPARING RERUN vs ORIGINAL RESULTS\n")
cat("========================================\n\n")

# File paths
original_dir <- "results/consolidated"
rerun_dir <- "results/rerun"

# Results storage
comparison_results <- list()

# Function to compare two dataframes
compare_dataframes <- function(df1, df2, name) {
  cat(sprintf("Comparing %s...\n", name))
  
  # Check if both exist
  if (is.null(df1) && is.null(df2)) {
    return(list(match = TRUE, message = "Both NULL"))
  }
  if (is.null(df1) || is.null(df2)) {
    return(list(match = FALSE, message = sprintf("One is NULL: df1=%s, df2=%s", 
                                                  !is.null(df1), !is.null(df2))))
  }
  
  # Check dimensions
  if (nrow(df1) != nrow(df2) || ncol(df1) != ncol(df2)) {
    return(list(match = FALSE, 
                message = sprintf("Dimensions differ: Original (%d x %d) vs Rerun (%d x %d)",
                                  nrow(df1), ncol(df1), nrow(df2), ncol(df2))))
  }
  
  # Check column names
  if (!identical(colnames(df1), colnames(df2))) {
    return(list(match = FALSE, 
                message = sprintf("Column names differ"),
                details = list(original = colnames(df1), rerun = colnames(df2))))
  }
  
  # Compare numeric values
  numeric_cols <- sapply(df1, is.numeric)
  if (any(numeric_cols)) {
    max_diff <- 0
    max_diff_col <- NA
    total_diffs <- 0
    n_comparisons <- 0
    
    for (col in names(df1)[numeric_cols]) {
      if (!col %in% names(df2)) next
      
      val1 <- df1[[col]]
      val2 <- df2[[col]]
      
      # Handle NAs
      both_na <- is.na(val1) & is.na(val2)
      both_finite <- is.finite(val1) & is.finite(val2)
      
      # Compare finite values
      if (any(both_finite)) {
        diffs <- abs(val1[both_finite] - val2[both_finite])
        rel_diffs <- diffs / (abs(val2[both_finite]) + 1e-10)
        
        n_comparisons <- n_comparisons + sum(both_finite)
        total_diffs <- total_diffs + sum(diffs > 1e-6)
        
        if (length(diffs) > 0 && max(diffs, na.rm = TRUE) > max_diff) {
          max_diff <- max(diffs, na.rm = TRUE)
          max_diff_col <- col
        }
      }
      
      # Check NA mismatches
      na_mismatch <- (is.na(val1) & !is.na(val2)) | (!is.na(val1) & is.na(val2))
      if (any(na_mismatch, na.rm = TRUE)) {
        total_diffs <- total_diffs + sum(na_mismatch, na.rm = TRUE)
      }
    }
    
    if (total_diffs == 0) {
      return(list(match = TRUE, message = "Perfect match"))
    } else {
      return(list(match = FALSE,
                  message = sprintf("%d differences found (%.2f%% of %d comparisons)",
                                   total_diffs, 
                                   total_diffs/n_comparisons*100,
                                   n_comparisons),
                  max_absolute_diff = max_diff,
                  max_diff_column = max_diff_col,
                  n_differences = total_diffs,
                  n_comparisons = n_comparisons))
    }
  }
  
  # For non-numeric, compare as strings
  if (identical(df1, df2)) {
    return(list(match = TRUE, message = "Perfect match"))
  } else {
    return(list(match = FALSE, message = "Non-numeric values differ"))
  }
}

# Function to read and compare Excel file
compare_excel_file <- function(filename, sheet_name = NULL) {
  cat(sprintf("\n--- %s ---\n", filename))
  
  original_file <- file.path(original_dir, filename)
  rerun_file <- file.path(rerun_dir, filename)
  
  if (!file.exists(original_file)) {
    return(list(match = NA, message = sprintf("Original file not found: %s", original_file)))
  }
  
  if (!file.exists(rerun_file)) {
    return(list(match = NA, message = sprintf("Rerun file not found: %s", rerun_file)))
  }
  
  # Read original
  tryCatch({
    if (is.null(sheet_name)) {
      original_sheets <- getSheetNames(original_file)
      if (length(original_sheets) == 1) {
        original_data <- read.xlsx(original_file, sheet = 1)
      } else {
        # Multiple sheets - compare each
        results <- list()
        for (sheet in original_sheets) {
          if (file.exists(rerun_file)) {
            rerun_sheets <- getSheetNames(rerun_file)
            if (sheet %in% rerun_sheets) {
              orig_sheet <- read.xlsx(original_file, sheet = sheet)
              rerun_sheet <- read.xlsx(rerun_file, sheet = sheet)
              results[[sheet]] <- compare_dataframes(orig_sheet, rerun_sheet, 
                                                      sprintf("%s[%s]", filename, sheet))
            }
          }
        }
        return(results)
      }
    } else {
      original_data <- read.xlsx(original_file, sheet = sheet_name)
    }
  }, error = function(e) {
    return(list(match = NA, message = sprintf("Error reading original: %s", e$message)))
  })
  
  # Read rerun
  tryCatch({
    if (is.null(sheet_name)) {
      rerun_sheets <- getSheetNames(rerun_file)
      if (length(rerun_sheets) == 1) {
        rerun_data <- read.xlsx(rerun_file, sheet = 1)
      } else {
        # Already handled above
        rerun_data <- NULL
      }
    } else {
      rerun_data <- read.xlsx(rerun_file, sheet = sheet_name)
    }
  }, error = function(e) {
    return(list(match = NA, message = sprintf("Error reading rerun: %s", e$message)))
  })
  
  if (exists("results")) {
    return(results)
  }
  
  # Compare
  if (exists("original_data") && exists("rerun_data")) {
    return(compare_dataframes(original_data, rerun_data, filename))
  } else {
    return(list(match = NA, message = "Could not read data"))
  }
}

# 1. NF-GARCH Results
cat("\n1. NF-GARCH Results\n")
comparison_results[["NF_GARCH_Results"]] <- compare_excel_file("NF_GARCH_Results_manual.xlsx")

# 2. NF vs Standard Comparison
cat("\n2. NF vs Standard Comparison\n")
comparison_results[["NF_vs_Standard"]] <- compare_excel_file("NF_vs_Standard_GARCH_Comparison.xlsx")

# 3. Stress Testing
cat("\n3. Stress Testing\n")
comparison_results[["Stress_Testing"]] <- compare_excel_file("Stress_Testing.xlsx")

# 4. Stylized Facts
cat("\n4. Stylized Facts\n")
comparison_results[["Stylized_Facts"]] <- compare_excel_file("Stylized_Facts.xlsx")

# 5. VaR Backtesting
cat("\n5. VaR Backtesting\n")
comparison_results[["VaR_Backtesting"]] <- compare_excel_file("VaR_Backtesting.xlsx")

# 6. Final Dashboard
cat("\n6. Final Dashboard\n")
comparison_results[["Final_Dashboard"]] <- compare_excel_file("Final_Dashboard.xlsx")

# 7. Distributional Metrics (if exists in original)
if (file.exists(file.path(original_dir, "Distributional_Metrics.xlsx"))) {
  cat("\n7. Distributional Metrics\n")
  comparison_results[["Distributional_Metrics"]] <- compare_excel_file("Distributional_Metrics.xlsx")
}

# Print summary
cat("\n\n========================================\n")
cat("COMPARISON SUMMARY\n")
cat("========================================\n\n")

for (name in names(comparison_results)) {
  result <- comparison_results[[name]]
  
  if (is.list(result) && "match" %in% names(result)) {
    if (result$match) {
      cat(sprintf("[MATCH] %s: %s\n", name, result$message))
    } else if (is.na(result$match)) {
      cat(sprintf("[SKIP] %s: %s\n", name, result$message))
    } else {
      cat(sprintf("[DIFF] %s: %s\n", name, result$message))
      if ("max_absolute_diff" %in% names(result)) {
        cat(sprintf("       Max difference: %.6f (column: %s)\n", 
                   result$max_absolute_diff, result$max_diff_column))
      }
      if ("n_differences" %in% names(result)) {
        cat(sprintf("       Differences: %d out of %d comparisons\n",
                   result$n_differences, result$n_comparisons))
      }
    }
  } else if (is.list(result)) {
    # Multiple sheets
    cat(sprintf("[MULTI] %s: %d sheets\n", name, length(result)))
    for (sheet_name in names(result)) {
      sheet_result <- result[[sheet_name]]
      if (sheet_result$match) {
        cat(sprintf("        [MATCH] %s: %s\n", sheet_name, sheet_result$message))
      } else {
        cat(sprintf("        [DIFF] %s: %s\n", sheet_name, sheet_result$message))
      }
    }
  }
}

# Detailed analysis for key differences
cat("\n\n========================================\n")
cat("DETAILED DIFFERENCE ANALYSIS\n")
cat("========================================\n\n")

# For files with differences, show sample differences
for (name in names(comparison_results)) {
  result <- comparison_results[[name]]
  
  if (is.list(result) && "match" %in% names(result) && !result$match && !is.na(result$match)) {
    cat(sprintf("\n--- %s ---\n", name))
    
    filename <- switch(name,
                      "NF_GARCH_Results" = "NF_GARCH_Results_manual.xlsx",
                      "NF_vs_Standard" = "NF_vs_Standard_GARCH_Comparison.xlsx",
                      "Stress_Testing" = "Stress_Testing.xlsx",
                      "Stylized_Facts" = "Stylized_Facts.xlsx",
                      "VaR_Backtesting" = "VaR_Backtesting.xlsx",
                      "Final_Dashboard" = "Final_Dashboard.xlsx",
                      "Distributional_Metrics" = "Distributional_Metrics.xlsx",
                      name)
    
    if (file.exists(file.path(original_dir, filename)) && 
        file.exists(file.path(rerun_dir, filename))) {
      
      orig_data <- read.xlsx(file.path(original_dir, filename), sheet = 1)
      rerun_data <- read.xlsx(file.path(rerun_dir, filename), sheet = 1)
      
      # Find first few differences
      numeric_cols <- sapply(orig_data, is.numeric)
      diff_count <- 0
      
      for (col in names(orig_data)[numeric_cols]) {
        if (col %in% names(rerun_data) && diff_count < 5) {
          val1 <- orig_data[[col]]
          val2 <- rerun_data[[col]]
          
          both_finite <- is.finite(val1) & is.finite(val2)
          if (any(both_finite)) {
            diffs <- abs(val1[both_finite] - val2[both_finite]) > 1e-6
            if (any(diffs, na.rm = TRUE)) {
              idx <- which(diffs)[1]
              cat(sprintf("  Column '%s', Row %d: Original=%.6f, Rerun=%.6f, Diff=%.6f\n",
                         col, which(both_finite)[idx],
                         val1[both_finite][idx],
                         val2[both_finite][idx],
                         abs(val1[both_finite][idx] - val2[both_finite][idx])))
              diff_count <- diff_count + 1
            }
          }
        }
      }
    }
  }
}

cat("\n\nComparison complete!\n")




