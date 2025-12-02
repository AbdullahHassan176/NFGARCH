# Check which assets/models were processed in rerun vs original

library(openxlsx)

cat("========================================\n")
cat("COVERAGE ANALYSIS: RERUN vs ORIGINAL\n")
cat("========================================\n\n")

original_dir <- "results/consolidated"
rerun_dir <- "results/rerun"

# Get all assets and models from original
orig_file <- file.path(original_dir, "NF_GARCH_Results_manual.xlsx")
rerun_file <- file.path(rerun_dir, "NF_GARCH_Results_manual.xlsx")

if (file.exists(orig_file) && file.exists(rerun_file)) {
  orig_chrono <- read.xlsx(orig_file, sheet = "Chrono_Split_NF_GARCH")
  rerun_chrono <- read.xlsx(rerun_file, sheet = "Chrono_Split_NF_GARCH")
  
  # Get unique assets and models
  orig_assets <- unique(orig_chrono$Asset)
  rerun_assets <- unique(rerun_chrono$Asset)
  
  orig_models <- unique(orig_chrono$Model)
  rerun_models <- unique(rerun_chrono$Model)
  
  cat("ASSETS:\n")
  cat("  Original: ", length(orig_assets), " assets\n")
  cat("    ", paste(orig_assets, collapse = ", "), "\n\n")
  
  cat("  Rerun: ", length(rerun_assets), " assets\n")
  cat("    ", paste(rerun_assets, collapse = ", "), "\n\n")
  
  missing_assets <- setdiff(orig_assets, rerun_assets)
  if (length(missing_assets) > 0) {
    cat("  Missing assets: ", paste(missing_assets, collapse = ", "), "\n\n")
  } else {
    cat("  All assets present\n\n")
  }
  
  cat("MODELS:\n")
  cat("  Original: ", paste(orig_models, collapse = ", "), "\n")
  cat("  Rerun: ", paste(rerun_models, collapse = ", "), "\n\n")
  
  missing_models <- setdiff(orig_models, rerun_models)
  if (length(missing_models) > 0) {
    cat("  Missing models: ", paste(missing_models, collapse = ", "), "\n\n")
  } else {
    cat("  All models present\n\n")
  }
  
  # Check asset-model combinations
  cat("ASSET-MODEL COMBINATIONS:\n")
  orig_combos <- paste(orig_chrono$Asset, orig_chrono$Model, sep = "|")
  rerun_combos <- paste(rerun_chrono$Asset, rerun_chrono$Model, sep = "|")
  
  orig_unique <- unique(orig_combos)
  rerun_unique <- unique(rerun_combos)
  
  cat("  Original: ", length(orig_unique), " unique combinations\n")
  cat("  Rerun: ", length(rerun_unique), " unique combinations\n")
  cat("  Missing: ", length(orig_unique) - length(rerun_unique), " combinations\n\n")
  
  missing_combos <- setdiff(orig_unique, rerun_unique)
  if (length(missing_combos) > 0) {
    cat("Missing combinations:\n")
    for (combo in sort(missing_combos)) {
      cat(sprintf("  - %s\n", combo))
    }
  }
}

# Check GARCH fitting results
cat("\n\n========================================\n")
cat("GARCH FITTING COVERAGE\n")
cat("========================================\n\n")

if (file.exists(file.path(original_dir, "Initial_GARCH_Model_Fitting.xlsx"))) {
  orig_garch <- read.xlsx(file.path(original_dir, "Initial_GARCH_Model_Fitting.xlsx"), sheet = 1)
  
  if ("Asset" %in% colnames(orig_garch) && "Model" %in% colnames(orig_garch)) {
    garch_assets <- unique(orig_garch$Asset)
    garch_models <- unique(orig_garch$Model)
    
    cat("GARCH Fitting Results:\n")
    cat("  Assets: ", length(garch_assets), "\n")
    cat("  Models: ", paste(garch_models, collapse = ", "), "\n")
    
    # Check which models per asset
    cat("\nModels per asset in original:\n")
    for (asset in sort(garch_assets)) {
      asset_models <- unique(orig_garch$Model[orig_garch$Asset == asset])
      cat(sprintf("  %s: %s\n", asset, paste(asset_models, collapse = ", ")))
    }
  }
}

cat("\n\nAnalysis complete!\n")




