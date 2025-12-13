# Compare Current Branch Results with Main Branch
# Identify missing models, assets, and data

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
cat("COMPARING CURRENT BRANCH vs MAIN BRANCH\n")
cat("========================================\n\n")

# Temporary directory for main branch files
temp_dir <- file.path(getwd(), "temp_main_comparison")
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

# Function to get unique combinations of key columns
get_combinations <- function(df, key_cols) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame())
  }
  
  # Find which key columns exist
  available_cols <- key_cols[key_cols %in% colnames(df)]
  
  if (length(available_cols) == 0) {
    return(data.frame())
  }
  
  # Get unique combinations
  unique_combos <- df %>%
    select(all_of(available_cols)) %>%
    distinct() %>%
    arrange(across(everything()))
  
  return(unique_combos)
}

# Function to compare files
compare_files <- function(file_path, key_cols = c("Asset", "Model", "Asset_Class")) {
  cat(sprintf("\n--- %s ---\n", basename(file_path)))
  
  current_file <- file_path
  main_file <- file.path(temp_dir, basename(file_path))
  
  # Check if file exists in current branch
  if (!file.exists(current_file)) {
    cat(sprintf("  [MISSING] File not found in current branch\n"))
    return(NULL)
  }
  
  # Check if we already have the main branch file
  if (!file.exists(main_file)) {
    cat(sprintf("  [SKIP] File not found in main branch\n"))
    return(NULL)
  }
  
  # Read both files
  tryCatch({
    # Read sheets
    main_sheets <- getSheetNames(main_file)
    current_sheets <- getSheetNames(current_file)
    
    # Check for missing sheets
    missing_sheets <- setdiff(main_sheets, current_sheets)
    if (length(missing_sheets) > 0) {
      cat(sprintf("  [MISSING SHEETS] %s\n", paste(missing_sheets, collapse = ", ")))
    }
    
    # Compare each common sheet
    common_sheets <- intersect(main_sheets, current_sheets)
    all_missing <- list()
    
    for (sheet in common_sheets) {
      cat(sprintf("\n  Sheet: %s\n", sheet))
      
      main_data <- read.xlsx(main_file, sheet = sheet)
      current_data <- read.xlsx(current_file, sheet = sheet)
      
      # Get combinations from main
      main_combos <- get_combinations(main_data, key_cols)
      current_combos <- get_combinations(current_data, key_cols)
      
      if (nrow(main_combos) == 0) {
        cat(sprintf("    [SKIP] No key columns found (available: %s)\n", 
                   paste(colnames(main_data)[1:min(5, ncol(main_data))], collapse = ", ")))
        next
      }
      
      # Find missing combinations
      if (nrow(current_combos) == 0) {
        cat(sprintf("    [ALL MISSING] All %d combinations from main are missing\n", 
                   nrow(main_combos)))
        if (nrow(main_combos) <= 20) {
          print(main_combos)
        } else {
          cat(sprintf("    (Showing first 10 of %d)\n", nrow(main_combos)))
          print(head(main_combos, 10))
        }
        all_missing[[sheet]] <- main_combos
        next
      }
      
      # Merge to find missing
      merged <- main_combos %>%
        anti_join(current_combos, by = colnames(main_combos))
      
      if (nrow(merged) > 0) {
        cat(sprintf("    [MISSING] %d combinations missing from current branch:\n", 
                   nrow(merged)))
        if (nrow(merged) <= 20) {
          print(merged)
        } else {
          cat(sprintf("    (Showing first 10 of %d)\n", nrow(merged)))
          print(head(merged, 10))
        }
        all_missing[[sheet]] <- merged
      } else {
        cat(sprintf("    [OK] All %d combinations present\n", nrow(main_combos)))
      }
      
      # Check row counts
      cat(sprintf("    Main: %d rows, Current: %d rows", 
                 nrow(main_data), nrow(current_data)))
      if (nrow(main_data) != nrow(current_data)) {
        cat(sprintf(" (Difference: %d)\n", nrow(main_data) - nrow(current_data)))
      } else {
        cat("\n")
      }
    }
    
    return(all_missing)
    
  }, error = function(e) {
    cat(sprintf("  [ERROR] %s\n", e$message))
    return(NULL)
  })
}

# First, checkout files from main branch to temp directory
cat("Extracting files from main branch...\n")
files_to_check <- c(
  "results/consolidated/NF_GARCH_Results_manual.xlsx",
  "results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx",
  "results/consolidated/Final_Dashboard.xlsx",
  "results/consolidated/Stress_Testing.xlsx",
  "results/consolidated/Stylized_Facts.xlsx",
  "results/consolidated/VaR_Backtesting.xlsx"
)

for (file_path in files_to_check) {
  # Use git show to extract file
  temp_file <- file.path(temp_dir, basename(file_path))
  git_cmd <- sprintf('git show main:%s', file_path)
  
  # Try to extract file
  result <- tryCatch({
    file_content <- system(git_cmd, intern = TRUE, ignore.stderr = TRUE)
    if (length(file_content) > 0 && !any(grepl("error|fatal", file_content, ignore.case = TRUE))) {
      # For binary files, we need a different approach
      # Use git checkout to a temp location
      system(sprintf('git show main:%s > "%s"', file_path, temp_file), intern = FALSE)
    }
    NULL
  }, error = function(e) NULL)
}

# Actually, let's use a simpler approach - checkout files to temp location
cat("Using git checkout to extract files...\n")
system(sprintf('git checkout main -- %s 2>NUL', paste(files_to_check, collapse = " ")), intern = FALSE)

# Move files to temp directory
for (file_path in files_to_check) {
  if (file.exists(file_path)) {
    file.copy(file_path, file.path(temp_dir, basename(file_path)), overwrite = TRUE)
    # Restore current branch file
    system(sprintf('git checkout HEAD -- %s 2>NUL', file_path), intern = FALSE)
  }
}

# Now compare
cat("\n1. NF-GARCH Results\n")
nf_results <- compare_files("results/consolidated/NF_GARCH_Results_manual.xlsx", 
                           key_cols = c("Asset", "Model"))

cat("\n2. NF vs Standard Comparison\n")
nf_vs_std <- compare_files("results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx", 
                          key_cols = c("Asset", "Model", "Asset_Class"))

cat("\n3. Final Dashboard\n")
dashboard <- compare_files("results/consolidated/Final_Dashboard.xlsx", 
                          key_cols = c("Asset", "Model", "Asset_Class"))

cat("\n4. Stress Testing\n")
stress <- compare_files("results/consolidated/Stress_Testing.xlsx", 
                       key_cols = c("Asset", "Model", "Crisis_Type"))

cat("\n5. Stylized Facts\n")
stylized <- compare_files("results/consolidated/Stylized_Facts.xlsx", 
                         key_cols = c("Asset", "Model"))

cat("\n6. VaR Backtesting\n")
var_bt <- compare_files("results/consolidated/VaR_Backtesting.xlsx", 
                       key_cols = c("Asset", "Model"))

cat("\n\n========================================\n")
cat("SUMMARY\n")
cat("========================================\n\n")

# Cleanup
unlink(temp_dir, recursive = TRUE)

cat("Comparison complete!\n")
cat("Review the output above for missing models, assets, or data.\n")

