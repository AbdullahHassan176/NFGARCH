# Test script to verify the comparison tables fix works correctly
# This tests the problematic code paths without running the full simulation

# Load required libraries
library(dplyr)
library(tidyr)
library(openxlsx)

cat("=== Testing Comparison Tables Fix ===\n\n")

# Test 1: Simulate nf_results_chrono list with data.frames
cat("Test 1: Creating nf_results_chrono with data.frames...\n")
nf_results_chrono <- list()

# Add some sample data.frames (simulating what fit_nf_garch returns)
for (i in 1:5) {
  nf_results_chrono[[i]] <- data.frame(
    Model = c("sGARCH", "eGARCH", "TGARCH")[((i-1) %% 3) + 1],
    Distribution = c("norm", "sstd")[((i-1) %% 2) + 1],
    Asset = c("EURUSD", "GBPUSD", "NVDA", "MSFT", "AMZN")[i],
    AIC = rnorm(1, 1000, 100),
    BIC = rnorm(1, 1100, 100),
    LogLikelihood = rnorm(1, -500, 50),
    MSE = abs(rnorm(1, 0.001, 0.0005)),
    MAE = abs(rnorm(1, 0.01, 0.005)),
    PredictiveLogLik = rnorm(1, -10, 2),
    NPaths = 1000,
    SplitType = "Chrono"
  )
}

cat("  Created", length(nf_results_chrono), "sample results\n")

# Test 2: Filter and combine using bind_rows (the fix)
cat("\nTest 2: Filtering and combining with bind_rows...\n")
nf_results_chrono <- nf_results_chrono[sapply(nf_results_chrono, function(x) !is.null(x) && is.data.frame(x))]

if (length(nf_results_chrono) > 0) {
  nf_results_df <- bind_rows(nf_results_chrono)
  
  # Ensure it's a data.frame
  if (!is.data.frame(nf_results_df) || nrow(nf_results_df) == 0) {
    stop("ERROR: Failed to create nf_results_df data.frame")
  }
  
  cat("  ✓ nf_results_df created successfully\n")
  cat("  ✓ Is data.frame:", is.data.frame(nf_results_df), "\n")
  cat("  ✓ Rows:", nrow(nf_results_df), "\n")
  cat("  ✓ Columns:", paste(names(nf_results_df), collapse=", "), "\n")
} else {
  stop("ERROR: nf_results_chrono is empty after filtering")
}

# Test 3: Create comparison tables (the problematic code)
cat("\nTest 3: Creating comparison tables with group_by...\n")
tryCatch({
  chrono_summary <- nf_results_df %>%
    group_by(Model, Distribution) %>%
    summarise(
      Avg_AIC = mean(AIC, na.rm = TRUE),
      Avg_BIC = mean(BIC, na.rm = TRUE),
      Avg_LogLik = mean(LogLikelihood, na.rm = TRUE),
      Avg_MSE = mean(MSE, na.rm = TRUE),
      Avg_MAE = mean(MAE, na.rm = TRUE),
      Models_Count = n(),
      .groups = 'drop'
    ) %>%
    mutate(Split_Type = "Chronological")
  
  cat("  ✓ chrono_summary created successfully\n")
  cat("  ✓ Rows:", nrow(chrono_summary), "\n")
  print(chrono_summary)
}, error = function(e) {
  stop("ERROR in Test 3: ", e$message)
})

# Test 4: Simulate TS CV results
cat("\nTest 4: Simulating TS CV results...\n")
Fitted_NFGARCH_TS_CV_models <- data.frame()

# Add some sample TS CV results
for (i in 1:3) {
  ts_cv_result <- data.frame(
    Model = c("sGARCH", "eGARCH", "TGARCH")[i],
    Distribution = "norm",
    Asset = c("EURUSD", "GBPUSD", "NVDA")[i],
    AIC = rnorm(1, 1000, 100),
    BIC = rnorm(1, 1100, 100),
    LogLikelihood = rnorm(1, -500, 50),
    MSE = abs(rnorm(1, 0.001, 0.0005)),
    MAE = abs(rnorm(1, 0.01, 0.005)),
    PredictiveLogLik = rnorm(1, -10, 2),
    NPaths = 1000,
    SplitType = "TS_CV"
  )
  
  Fitted_NFGARCH_TS_CV_models <- bind_rows(Fitted_NFGARCH_TS_CV_models, ts_cv_result)
}

cat("  ✓ Fitted_NFGARCH_TS_CV_models created\n")
cat("  ✓ Is data.frame:", is.data.frame(Fitted_NFGARCH_TS_CV_models), "\n")
cat("  ✓ Rows:", nrow(Fitted_NFGARCH_TS_CV_models), "\n")

# Test 5: Create TS CV summary
cat("\nTest 5: Creating TS CV summary...\n")
tryCatch({
  tscv_summary <- Fitted_NFGARCH_TS_CV_models %>%
    group_by(Model, Distribution) %>%
    summarise(
      Avg_AIC = mean(AIC, na.rm = TRUE),
      Avg_BIC = mean(BIC, na.rm = TRUE),
      Avg_LogLik = mean(LogLikelihood, na.rm = TRUE),
      Avg_MSE = mean(MSE, na.rm = TRUE),
      Avg_MAE = mean(MAE, na.rm = TRUE),
      Windows_Processed = n(),
      .groups = 'drop'
    ) %>%
    mutate(Split_Type = "Time_Series_CV")
  
  cat("  ✓ tscv_summary created successfully\n")
  cat("  ✓ Rows:", nrow(tscv_summary), "\n")
  print(tscv_summary)
}, error = function(e) {
  stop("ERROR in Test 5: ", e$message)
})

# Test 6: Create comparison table (the most complex part)
cat("\nTest 6: Creating comparison table with pivot_wider...\n")
tryCatch({
  comparison_table <- bind_rows(chrono_summary, tscv_summary) %>%
    pivot_wider(
      names_from = Split_Type,
      values_from = c(Avg_AIC, Avg_BIC, Avg_LogLik, Avg_MSE, Avg_MAE),
      names_sep = "_"
    ) %>%
    mutate(
      AIC_Diff = Avg_AIC_Time_Series_CV - Avg_AIC_Chronological,
      BIC_Diff = Avg_BIC_Time_Series_CV - Avg_BIC_Chronological,
      MSE_Diff = Avg_MSE_Time_Series_CV - Avg_MSE_Chronological,
      MAE_Diff = Avg_MAE_Time_Series_CV - Avg_MAE_Chronological
    )
  
  cat("  ✓ comparison_table created successfully\n")
  cat("  ✓ Rows:", nrow(comparison_table), "\n")
  print(comparison_table)
}, error = function(e) {
  stop("ERROR in Test 6: ", e$message)
})

# Test 7: Test the rbind -> bind_rows fix for TS CV results
cat("\nTest 7: Testing bind_rows fix for TS CV results (simulating the problematic code path)...\n")
Fitted_FX_NFGARCH_TS_CV_models <- list()
Fitted_EQ_NFGARCH_TS_CV_models <- list()

# Simulate FX results structure
Fitted_FX_NFGARCH_TS_CV_models[["sGARCH_norm"]] <- list(
  "EURUSD" = data.frame(Model = "sGARCH", Distribution = "norm", Asset = "EURUSD", MSE = 0.001, MAE = 0.01),
  "GBPUSD" = data.frame(Model = "sGARCH", Distribution = "norm", Asset = "GBPUSD", MSE = 0.0012, MAE = 0.011)
)

# Simulate Equity results structure
Fitted_EQ_NFGARCH_TS_CV_models[["sGARCH_norm"]] <- list(
  "NVDA" = data.frame(Model = "sGARCH", Distribution = "norm", Asset = "NVDA", MSE = 0.0015, MAE = 0.012)
)

# Test the flattening code (with the fix)
Fitted_NFGARCH_TS_CV_models_test <- data.frame()

for (model_name in names(Fitted_FX_NFGARCH_TS_CV_models)) {
  fx_results <- tryCatch({
    fx_list <- Fitted_FX_NFGARCH_TS_CV_models[[model_name]]
    if (is.null(fx_list) || length(fx_list) == 0) {
      return(NULL)
    }
    fx_list_with_asset <- lapply(names(fx_list), function(asset_name) {
      df <- fx_list[[asset_name]]
      if (!is.null(df) && nrow(df) > 0) {
        df$Asset <- asset_name
        df$AssetType <- "FX"
      }
      return(df)
    })
    # Use bind_rows instead of rbind (THE FIX)
    fx_list_with_asset <- fx_list_with_asset[!sapply(fx_list_with_asset, is.null)]
    if (length(fx_list_with_asset) > 0) {
      bind_rows(fx_list_with_asset)
    } else {
      NULL
    }
  }, error = function(e) {
    message("WARNING: FX NF-GARCH CV results failed for: ", model_name, " - ", e$message)
    return(NULL)
  })
  
  if (!is.null(fx_results) && is.data.frame(fx_results) && nrow(fx_results) > 0) {
    Fitted_NFGARCH_TS_CV_models_test <- bind_rows(Fitted_NFGARCH_TS_CV_models_test, fx_results)
  }
}

cat("  ✓ TS CV flattening with bind_rows works\n")
cat("  ✓ Is data.frame:", is.data.frame(Fitted_NFGARCH_TS_CV_models_test), "\n")
cat("  ✓ Rows:", nrow(Fitted_NFGARCH_TS_CV_models_test), "\n")

# Test 8: Verify the final structure can be used in group_by
cat("\nTest 8: Verifying final structure works with group_by...\n")
tryCatch({
  final_test <- Fitted_NFGARCH_TS_CV_models_test %>%
    group_by(Model, Distribution) %>%
    summarise(
      Avg_MSE = mean(MSE, na.rm = TRUE),
      Avg_MAE = mean(MAE, na.rm = TRUE),
      .groups = 'drop'
    )
  
  cat("  ✓ Final group_by operation works\n")
  print(final_test)
}, error = function(e) {
  stop("ERROR in Test 8: ", e$message)
})

cat("\n=== ALL TESTS PASSED ===\n")
cat("The fixes are working correctly. Safe to run full simulation.\n")
