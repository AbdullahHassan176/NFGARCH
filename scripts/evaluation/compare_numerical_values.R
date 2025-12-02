# Compare Numerical Values Between Current Branch and Main Branch
# Check if results are identical across every data point

library(openxlsx)
library(dplyr)

cat("========================================\n")
cat("NUMERICAL VALUE COMPARISON\n")
cat("Current Branch vs Main Branch\n")
cat("========================================\n\n")

# Function to compare two dataframes numerically
compare_numerical_values <- function(df_main, df_current, name) {
  cat(sprintf("\n--- %s ---\n", name))
  
  # Check if both exist
  if (is.null(df_main) || is.null(df_current)) {
    cat("  [SKIP] One or both dataframes are NULL\n")
    return(NULL)
  }
  
  # Check dimensions
  if (nrow(df_main) != nrow(df_current) || ncol(df_main) != ncol(df_current)) {
    cat(sprintf("  [DIFFERENT STRUCTURE] Main: %d x %d, Current: %d x %d\n",
               nrow(df_main), ncol(df_main), nrow(df_current), ncol(df_current)))
    return(list(match = FALSE, type = "structure"))
  }
  
  # Check column names
  if (!identical(colnames(df_main), colnames(df_current))) {
    missing_in_current <- setdiff(colnames(df_main), colnames(df_current))
    missing_in_main <- setdiff(colnames(df_current), colnames(df_main))
    if (length(missing_in_current) > 0) {
      cat(sprintf("  [MISSING COLUMNS IN CURRENT] %s\n", paste(missing_in_current, collapse = ", ")))
    }
    if (length(missing_in_main) > 0) {
      cat(sprintf("  [MISSING COLUMNS IN MAIN] %s\n", paste(missing_in_main, collapse = ", ")))
    }
    return(list(match = FALSE, type = "columns"))
  }
  
  # Compare numeric columns
  numeric_cols <- sapply(df_main, is.numeric)
  total_diffs <- 0
  total_comparisons <- 0
  max_abs_diff <- 0
  max_rel_diff <- 0
  max_diff_col <- NA
  max_diff_row <- NA
  diff_details <- list()
  
  for (col in names(df_main)[numeric_cols]) {
    if (!col %in% names(df_current)) next
    
    val_main <- df_main[[col]]
    val_current <- df_current[[col]]
    
    # Handle NAs
    both_na <- is.na(val_main) & is.na(val_current)
    both_finite <- is.finite(val_main) & is.finite(val_current)
    
    # Check NA mismatches
    na_mismatch <- (is.na(val_main) & !is.na(val_current)) | (!is.na(val_main) & is.na(val_current))
    if (any(na_mismatch, na.rm = TRUE)) {
      n_mismatch <- sum(na_mismatch, na.rm = TRUE)
      cat(sprintf("  [NA MISMATCH] Column '%s': %d NA mismatches\n", col, n_mismatch))
      total_diffs <- total_diffs + n_mismatch
      diff_details[[col]] <- list(na_mismatches = n_mismatch)
    }
    
    # Compare finite values
    if (any(both_finite)) {
      val_main_finite <- val_main[both_finite]
      val_current_finite <- val_current[both_finite]
      
      abs_diffs <- abs(val_main_finite - val_current_finite)
      rel_diffs <- abs_diffs / (abs(val_current_finite) + 1e-10)
      
      # Count differences (tolerance: 1e-10 for absolute, 1e-8 for relative)
      significant_diffs <- abs_diffs > 1e-10 & rel_diffs > 1e-8
      n_diffs <- sum(significant_diffs, na.rm = TRUE)
      
      total_comparisons <- total_comparisons + sum(both_finite)
      total_diffs <- total_diffs + n_diffs
      
      if (n_diffs > 0) {
        max_abs_diff_idx <- which.max(abs_diffs)
        if (abs_diffs[max_abs_diff_idx] > max_abs_diff) {
          max_abs_diff <- abs_diffs[max_abs_diff_idx]
          max_rel_diff <- rel_diffs[max_abs_diff_idx]
          max_diff_col <- col
          max_diff_row <- which(both_finite)[max_abs_diff_idx]
        }
        
        # Store first few differences
        if (n_diffs <= 5) {
          diff_idx <- which(significant_diffs)
          diff_details[[col]] <- list(
            n_diffs = n_diffs,
            examples = data.frame(
              row = which(both_finite)[diff_idx],
              main = val_main_finite[diff_idx],
              current = val_current_finite[diff_idx],
              abs_diff = abs_diffs[diff_idx],
              rel_diff = rel_diffs[diff_idx]
            )
          )
        } else {
          diff_details[[col]] <- list(
            n_diffs = n_diffs,
            max_abs_diff = max(abs_diffs[significant_diffs], na.rm = TRUE),
            max_rel_diff = max(rel_diffs[significant_diffs], na.rm = TRUE)
          )
        }
      }
    }
  }
  
  # Compare non-numeric columns
  non_numeric_cols <- names(df_main)[!numeric_cols]
  for (col in non_numeric_cols) {
    if (!col %in% names(df_current)) next
    
    val_main <- df_main[[col]]
    val_current <- df_current[[col]]
    
    # Convert to character for comparison
    main_char <- as.character(val_main)
    current_char <- as.character(val_current)
    
    mismatches <- main_char != current_char & !(is.na(val_main) & is.na(val_current))
    if (any(mismatches, na.rm = TRUE)) {
      n_mismatch <- sum(mismatches, na.rm = TRUE)
      cat(sprintf("  [NON-NUMERIC MISMATCH] Column '%s': %d mismatches\n", col, n_mismatch))
      total_diffs <- total_diffs + n_mismatch
      
      # Show first few examples
      if (n_mismatch <= 5) {
        mismatch_idx <- which(mismatches)[1:min(5, n_mismatch)]
        for (idx in mismatch_idx) {
          cat(sprintf("    Row %d: Main='%s', Current='%s'\n", 
                     idx, main_char[idx], current_char[idx]))
        }
      }
    }
  }
  
  # Summary
  if (total_diffs == 0) {
    cat(sprintf("  [IDENTICAL] All %d comparisons match perfectly\n", total_comparisons))
    return(list(match = TRUE, total_comparisons = total_comparisons))
  } else {
    cat(sprintf("  [DIFFERENCES] %d differences found (%.4f%% of %d comparisons)\n",
               total_diffs, total_diffs/total_comparisons*100, total_comparisons))
    if (!is.na(max_diff_col)) {
      cat(sprintf("  Max difference: %.10e (relative: %.10e) in column '%s', row %d\n",
                 max_abs_diff, max_rel_diff, max_diff_col, max_diff_row))
      cat(sprintf("    Main value: %.10e\n", df_main[[max_diff_col]][max_diff_row]))
      cat(sprintf("    Current value: %.10e\n", df_current[[max_diff_col]][max_diff_row]))
    }
    return(list(match = FALSE, total_diffs = total_diffs, total_comparisons = total_comparisons,
               max_abs_diff = max_abs_diff, max_rel_diff = max_rel_diff,
               max_diff_col = max_diff_col, max_diff_row = max_diff_row,
               diff_details = diff_details))
  }
}

# Function to compare Excel file
compare_excel_file <- function(file_path, key_cols = c("Asset", "Model")) {
  cat(sprintf("\n========================================\n"))
  cat(sprintf("FILE: %s\n", basename(file_path)))
  cat(sprintf("========================================\n"))
  
  # Checkout from main
  system(sprintf('git checkout main -- %s 2>NUL', file_path), intern = FALSE)
  main_file <- file_path
  if (!file.exists(main_file)) {
    cat("  [SKIP] File not found in main branch\n")
    return(NULL)
  }
  
  # Get current file
  system(sprintf('git checkout HEAD -- %s 2>NUL', file_path), intern = FALSE)
  current_file <- file_path
  if (!file.exists(current_file)) {
    cat("  [SKIP] File not found in current branch\n")
    return(NULL)
  }
  
  # Read sheets
  main_sheets <- getSheetNames(main_file)
  current_sheets <- getSheetNames(current_file)
  
  results <- list()
  
  for (sheet in intersect(main_sheets, current_sheets)) {
    cat(sprintf("\n--- Sheet: %s ---\n", sheet))
    
    main_data <- read.xlsx(main_file, sheet = sheet)
    current_data <- read.xlsx(current_file, sheet = sheet)
    
    # Sort by key columns for consistent comparison
    if (length(intersect(key_cols, colnames(main_data))) > 0) {
      sort_cols <- intersect(key_cols, colnames(main_data))
      main_data <- main_data %>% arrange(across(all_of(sort_cols)))
      current_data <- current_data %>% arrange(across(all_of(sort_cols)))
    }
    
    result <- compare_numerical_values(main_data, current_data, sheet)
    results[[sheet]] <- result
  }
  
  # Restore current file
  system(sprintf('git checkout HEAD -- %s 2>NUL', file_path), intern = FALSE)
  
  return(results)
}

# Compare key files
all_results <- list()

cat("\n1. NF-GARCH Results\n")
all_results[["NF_GARCH"]] <- compare_excel_file("results/consolidated/NF_GARCH_Results_manual.xlsx",
                                                 key_cols = c("Asset", "Model"))

cat("\n\n2. NF vs Standard Comparison\n")
all_results[["NF_vs_Standard"]] <- compare_excel_file("results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx",
                                                       key_cols = c("Asset", "Model"))

cat("\n\n3. Final Dashboard\n")
all_results[["Final_Dashboard"]] <- compare_excel_file("results/consolidated/Final_Dashboard.xlsx",
                                                       key_cols = c("Asset", "Model"))

cat("\n\n4. Stress Testing\n")
all_results[["Stress_Testing"]] <- compare_excel_file("results/consolidated/Stress_Testing.xlsx",
                                                       key_cols = c("Asset", "Model", "Crisis_Type"))

cat("\n\n5. Stylized Facts\n")
all_results[["Stylized_Facts"]] <- compare_excel_file("results/consolidated/Stylized_Facts.xlsx",
                                                       key_cols = c("Asset", "Model"))

cat("\n\n6. VaR Backtesting\n")
all_results[["VaR_Backtesting"]] <- compare_excel_file("results/consolidated/VaR_Backtesting.xlsx",
                                                        key_cols = c("Asset", "Model"))

# Overall summary
cat("\n\n========================================\n")
cat("OVERALL SUMMARY\n")
cat("========================================\n\n")

total_files <- 0
identical_files <- 0
files_with_diffs <- 0

for (file_name in names(all_results)) {
  if (is.null(all_results[[file_name]])) next
  
  total_files <- total_files + 1
  file_all_identical <- TRUE
  
  for (sheet_name in names(all_results[[file_name]])) {
    sheet_result <- all_results[[file_name]][[sheet_name]]
    if (!is.null(sheet_result) && !is.null(sheet_result$match)) {
      if (!sheet_result$match) {
        file_all_identical <- FALSE
        break
      }
    }
  }
  
  if (file_all_identical) {
    identical_files <- identical_files + 1
    cat(sprintf("[IDENTICAL] %s\n", file_name))
  } else {
    files_with_diffs <- files_with_diffs + 1
    cat(sprintf("[DIFFERENCES] %s\n", file_name))
  }
}

cat(sprintf("\n\nFiles: %d total, %d identical, %d with differences\n",
           total_files, identical_files, files_with_diffs))

if (files_with_diffs == 0) {
  cat("\n✓ PERFECT REPLICATION: All results are identical!\n")
  cat("  If someone reran your code right now, they would replicate\n")
  cat("  every single data point in your paper.\n")
} else {
  cat("\n⚠ PARTIAL REPLICATION: Some differences found.\n")
  cat("  Review the details above to see what differs.\n")
}

cat("\n")




