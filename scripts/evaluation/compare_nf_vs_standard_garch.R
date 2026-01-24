#!/usr/bin/env Rscript
# Compare NF-GARCH vs Standard GARCH Models
# Evaluates whether NF-injected models outperform standard GARCH models

# Load centralized seed configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
  set.seed(REPRODUCIBILITY_SEED)
} else {
  set.seed(123)  # Fallback if config not available
}

library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(xts)

cat("=== NF-GARCH vs STANDARD GARCH COMPARISON ===\n\n")

# Load NF-GARCH results
nf_results_file <- "results/consolidated/NF_GARCH_Results_manual.xlsx"
if (!file.exists(nf_results_file)) {
  stop("NF-GARCH results file not found: ", nf_results_file)
}

cat("Loading NF-GARCH results from:", nf_results_file, "\n")
nf_chrono <- read.xlsx(nf_results_file, sheet = "Chrono_Split_NF_GARCH")
sheets_nf <- openxlsx::getSheetNames(nf_results_file)
if ("TS_CV_NF_GARCH" %in% sheets_nf) {
  nf_tscv <- read.xlsx(nf_results_file, sheet = "TS_CV_NF_GARCH")
} else {
  nf_tscv <- data.frame()
}
cat("NF-GARCH results loaded:\n")
cat("  - Chrono: ", nrow(nf_chrono), " models\n")
cat("  - TS CV: ", nrow(nf_tscv), " windows\n\n")

# =============================================================================
# Create Standard GARCH baseline (using same GARCH fits but with regular residuals)
# =============================================================================

cat("=== CREATING STANDARD GARCH BASELINE ===\n")

# Load GARCH fitting summary (optional, for log message only; Standard re-fits in this script)
if (file.exists("outputs/manual/garch_fitting/model_summary.csv")) {
  garch_summary <- read.csv("outputs/manual/garch_fitting/model_summary.csv")
  cat("GARCH fitting summary loaded: ", nrow(garch_summary), " models\n")
} else {
  garch_summary <- data.frame()
  cat("GARCH fitting summary not found (optional).\n")
}

# Load engine utilities and return forecast evaluation (must match NF-GARCH: 1000-path point forecast)
source("scripts/engines/engine_selector.R")
source("scripts/utils/safety_functions.R")
source("scripts/utils/return_forecast_evaluation.R")

# Load data to simulate standard GARCH models
raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1)
raw_price_data$Date <- as.Date(rownames(raw_price_data))
rownames(raw_price_data) <- NULL
raw_price_data <- raw_price_data %>% dplyr::select(Date, everything())

# Extract optimized assets
equity_tickers <- c("NVDA", "MSFT", "AMZN")
fx_names <- c("EURUSD", "GBPUSD", "USDZAR")

price_data_matrix <- raw_price_data[, !(names(raw_price_data) %in% "Date")]
date_index <- raw_price_data$Date

# Calculate returns
equity_xts <- lapply(equity_tickers, function(ticker) {
  if (ticker %in% names(price_data_matrix)) {
    xts(price_data_matrix[[ticker]], order.by = date_index)
  } else {
    NULL
  }
})
names(equity_xts) <- equity_tickers
equity_xts <- equity_xts[!sapply(equity_xts, is.null)]

fx_xts <- lapply(fx_names, function(ticker) {
  if (ticker %in% names(price_data_matrix)) {
    xts(price_data_matrix[[ticker]], order.by = date_index)
  } else {
    NULL
  }
})
names(fx_xts) <- fx_names
fx_xts <- fx_xts[!sapply(fx_xts, is.null)]

# Calculate returns
equity_returns <- lapply(equity_xts, function(x) diff(log(x))[-1])
fx_returns <- lapply(fx_xts, function(x) diff(log(x))[-1])

all_returns <- c(equity_returns, fx_returns)
all_asset_names <- c(equity_tickers, fx_names)

# Model configurations - align with NF-GARCH: sGARCH_norm, sGARCH_sstd, eGARCH, TGARCH, gjrGARCH
model_configs <- list(
  sGARCH_norm = list(model = "sGARCH", distribution = "norm", submodel = NULL),
  sGARCH_sstd = list(model = "sGARCH", distribution = "sstd", submodel = NULL),
  eGARCH      = list(model = "eGARCH", distribution = "sstd", submodel = NULL),
  TGARCH      = list(model = "TGARCH", distribution = "sstd", submodel = NULL),
  gjrGARCH    = list(model = "gjrGARCH", distribution = "sstd", submodel = NULL)
)

cat("Running standard GARCH simulations (using regular residuals)...\n")

# Standard GARCH simulation (using regular residuals from fitted models)
standard_garch_results <- list()

for (asset_idx in seq_along(all_returns)) {
  asset_name <- all_asset_names[asset_idx]
  returns_data <- all_returns[[asset_idx]]
  
  cat("\nProcessing asset:", asset_name, "\n")
  
  for (model_name in names(model_configs)) {
    cfg <- model_configs[[model_name]]
    cat("  Standard GARCH:", model_name, "\n")
    
    tryCatch({
      # Same 65/35 train/test split as NF-GARCH (simulate_nf_garch_engine.R)
      ret_vec <- as.numeric(returns_data)
      n_obs <- length(ret_vec)
      split_idx <- floor(n_obs * 0.65)
      train_returns <- ret_vec[1:split_idx]
      test_returns <- ret_vec[(split_idx + 1):n_obs]
      if (length(test_returns) < 5L) next

      # Fit on train only (NF-GARCH is also fit on train only)
      fit <- engine_fit(
        model = cfg$model,
        returns = train_returns,
        dist = cfg$distribution,
        submodel = cfg$submodel,
        engine = "manual"
      )

      if (engine_converged(fit)) {
        standard_residuals <- engine_residuals(fit, standardize = TRUE)
        # Use evaluate_return_forecasts with n_paths=1000 so point forecast = mean of paths,
        # matching NF-GARCH. (Previously: one engine_path = one random path, inflating Standard MSE.)
        eval_result <- evaluate_return_forecasts(
          fit = fit,
          nf_residuals = standard_residuals,
          actual_returns = test_returns,
          horizon = length(test_returns),
          model_type = cfg$model,
          submodel = cfg$submodel,
          engine = "manual",
          n_paths = 1000L
        )
        if (is.null(eval_result) || is.na(eval_result$mse) || eval_result$n_valid_paths < 1L) next

        ic <- engine_infocriteria(fit)
        standard_garch_results[[length(standard_garch_results) + 1L]] <- data.frame(
          Model = cfg$model,
          Distribution = cfg$distribution,
          Asset = asset_name,
          AIC = ic["AIC"],
          BIC = ic["BIC"],
          LogLikelihood = ic["LogLikelihood"],
          MSE = eval_result$mse,
          MAE = eval_result$mae,
          PredictiveLogLik = eval_result$loglik,
          NPaths = eval_result$n_valid_paths,
          Source = "Standard",
          SplitType = "Chrono"
        )
      }
    }, error = function(e) {
      cat("    Error:", e$message, "\n")
    })
  }
}

standard_df <- do.call(rbind, standard_garch_results)
cat("\nStandard GARCH results: ", nrow(standard_df), " models\n")

# =============================================================================
# Compare NF-GARCH vs Standard GARCH
# =============================================================================

cat("\n=== COMPARING NF-GARCH vs STANDARD GARCH ===\n")

# Prepare NF-GARCH results for comparison (rename to avoid hyphen issues)
nf_chrono$Source <- "NF_GARCH"

# Combine results (drop SplitType if present, for clean bind)
combined_results <- bind_rows(
  standard_df %>% select(-any_of("SplitType")),
  nf_chrono %>% select(-any_of("SplitType"))
)

# Performance comparison by (Model, Distribution)
comparison_by_model <- combined_results %>%
  group_by(Model, Distribution, Source) %>%
  summarise(
    n_assets = n(),
    mean_AIC = mean(AIC, na.rm = TRUE),
    mean_BIC = mean(BIC, na.rm = TRUE),
    mean_MSE = mean(MSE, na.rm = TRUE),
    mean_MAE = mean(MAE, na.rm = TRUE),
    mean_LogLik = mean(LogLikelihood, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Source,
    values_from = c(mean_AIC, mean_BIC, mean_MSE, mean_MAE, mean_LogLik),
    names_sep = "_"
  ) %>%
  mutate(
    AIC_improvement = ifelse(!is.na(mean_AIC_Standard) & !is.na(mean_AIC_NF_GARCH),
                            mean_AIC_Standard - mean_AIC_NF_GARCH, NA),
    MSE_improvement = ifelse(!is.na(mean_MSE_Standard) & !is.na(mean_MSE_NF_GARCH) & abs(mean_MSE_Standard) > 0,
                            (mean_MSE_Standard - mean_MSE_NF_GARCH) / abs(mean_MSE_Standard) * 100, NA),
    MAE_improvement = ifelse(!is.na(mean_MAE_Standard) & !is.na(mean_MAE_NF_GARCH) & abs(mean_MAE_Standard) > 0,
                            (mean_MAE_Standard - mean_MAE_NF_GARCH) / abs(mean_MAE_Standard) * 100, NA)
  )

cat("\nPerformance Comparison by Model:\n")
print(comparison_by_model)

# Overall comparison
overall_comparison <- combined_results %>%
  group_by(Source) %>%
  summarise(
    n_models = n(),
    mean_AIC = mean(AIC, na.rm = TRUE),
    mean_MSE = mean(MSE, na.rm = TRUE),
    mean_MAE = mean(MAE, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nOverall Performance:\n")
print(overall_comparison)

# Win rate analysis: pair NF vs Standard by (Model, Distribution, Asset)
win_rate <- combined_results %>%
  group_by(Model, Distribution, Asset) %>%
  summarise(
    nf_mse = MSE[Source == "NF_GARCH"][1],
    std_mse = MSE[Source == "Standard"][1],
    nf_better = ifelse(length(nf_mse) > 0 && length(std_mse) > 0 && !is.na(nf_mse) && !is.na(std_mse),
                       nf_mse < std_mse, NA),
    .groups = "drop"
  ) %>%
  filter(!is.na(nf_better)) %>%
  group_by(Model, Distribution) %>%
  summarise(
    total_comparisons = n(),
    nf_wins = sum(nf_better, na.rm = TRUE),
    win_rate = mean(nf_better, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat("\nWin Rate Analysis (NF-GARCH vs Standard GARCH):\n")
print(win_rate)

# =============================================================================
# Wilcoxon Signed-Rank Test (Statistical Significance)
# =============================================================================

cat("\n=== WILCOXON SIGNED-RANK TEST ===\n")

if (nrow(combined_results) > 0) {
  wilcoxon_results <- list()
  model_dist <- combined_results %>% distinct(Model, Distribution)
  
  for (i in seq_len(nrow(model_dist))) {
    m <- model_dist$Model[i]
    d <- model_dist$Distribution[i]
    model_data <- combined_results %>%
      filter(Model == m, Distribution == d) %>%
      select(Asset, Source, MSE, MAE, AIC) %>%
      group_by(Asset, Source) %>%
      summarise(MSE = mean(MSE, na.rm = TRUE), MAE = mean(MAE, na.rm = TRUE), AIC = mean(AIC, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = Source, values_from = c(MSE, MAE, AIC))
    
    if (nrow(model_data) > 0 && "MSE_Standard" %in% names(model_data) && "MSE_NF_GARCH" %in% names(model_data)) {
      mse_standard <- as.numeric(model_data$MSE_Standard)
      mse_nf <- as.numeric(model_data$MSE_NF_GARCH)
      valid_pairs <- !is.na(mse_standard) & !is.na(mse_nf)
      mse_standard <- mse_standard[valid_pairs]
      mse_nf <- mse_nf[valid_pairs]
      
      if (length(mse_standard) > 3) {
        wilcoxon_test <- wilcox.test(mse_nf, mse_standard, paired = TRUE, alternative = "less")
        key <- paste(m, d, sep = "_")
        wilcoxon_results[[key]] <- data.frame(
          Model = m,
          Distribution = d,
          Test_Type = "MSE (NF < Standard)",
          Statistic = wilcoxon_test$statistic,
          Pvalue = wilcoxon_test$p.value,
          Significant = wilcoxon_test$p.value < 0.05,
          Alternative = wilcoxon_test$alternative
        )
      }
    }
  }
  
  if (length(wilcoxon_results) > 0) {
    wilcoxon_df <- bind_rows(wilcoxon_results)
    cat("\nWilcoxon Test Results:\n")
    print(wilcoxon_df)
  } else {
    wilcoxon_df <- data.frame()
  }
} else {
  wilcoxon_df <- data.frame()
  cat("[WARNING] Insufficient data for Wilcoxon test\n")
}

# =============================================================================
# Asset-Class Aggregation (FX vs Equity)
# =============================================================================

cat("\n=== ASSET-CLASS ANALYSIS ===\n")

# Define asset classes
fx_assets <- c("EURUSD", "GBPUSD", "USDZAR")
equity_assets <- c("NVDA", "MSFT", "AMZN")

if (nrow(combined_results) > 0) {
  # Add asset class
  combined_results$Asset_Class <- ifelse(
    combined_results$Asset %in% fx_assets, "FX",
    ifelse(combined_results$Asset %in% equity_assets, "Equity", "Unknown")
  )
  
  # Aggregate by asset class
  asset_class_summary <- combined_results %>%
    group_by(Asset_Class, Source) %>%
    summarise(
      n_assets = n_distinct(Asset),
      mean_MSE = mean(MSE, na.rm = TRUE),
      median_MSE = median(MSE, na.rm = TRUE),
      mean_MAE = mean(MAE, na.rm = TRUE),
      median_MAE = median(MAE, na.rm = TRUE),
      mean_AIC = mean(AIC, na.rm = TRUE),
      median_AIC = median(AIC, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Asset_Class, Source)
  
  cat("\nAsset Class Summary:\n")
  print(asset_class_summary)
  
  # Best model per asset class
  best_by_class <- combined_results %>%
    group_by(Asset_Class, Asset, Source) %>%
    summarise(
      mean_MSE = mean(MSE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(Asset_Class, Asset) %>%
    summarise(
      best_source = Source[which.min(mean_MSE)],
      best_mse = min(mean_MSE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(Asset_Class, best_source) %>%
    summarise(
      n_wins = n(),
      win_rate = n() / n_distinct(combined_results$Asset[combined_results$Asset_Class == first(Asset_Class)]) * 100,
      .groups = "drop"
    )
  
  cat("\nBest Model by Asset Class:\n")
  print(best_by_class)
} else {
  asset_class_summary <- data.frame()
  best_by_class <- data.frame()
}

# =============================================================================
# Create Comparison Workbook
# =============================================================================

cat("\n=== CREATING COMPARISON WORKBOOK ===\n")

wb <- createWorkbook()

# Summary
addWorksheet(wb, "Summary")
summary_data <- data.frame(
  Question = c(
    "Do NF-GARCH models outperform standard GARCH?",
    "Overall mean MSE - Standard",
    "Overall mean MSE - NF-GARCH",
    "Overall mean AIC - Standard",
    "Overall mean AIC - NF-GARCH",
    "NF-GARCH wins (lower MSE)",
    "Standard GARCH wins (lower MSE)",
    "Overall winner"
  ),
  Answer = c(
    if (overall_comparison$mean_MSE[overall_comparison$Source == "NF_GARCH"] < 
        overall_comparison$mean_MSE[overall_comparison$Source == "Standard"]) "YES" else "NO",
    if (nrow(overall_comparison[overall_comparison$Source == "Standard",]) > 0) 
      overall_comparison$mean_MSE[overall_comparison$Source == "Standard"] else "N/A",
    if (nrow(overall_comparison[overall_comparison$Source == "NF_GARCH",]) > 0) 
      overall_comparison$mean_MSE[overall_comparison$Source == "NF_GARCH"] else "N/A",
    if (nrow(overall_comparison[overall_comparison$Source == "Standard",]) > 0) 
      overall_comparison$mean_AIC[overall_comparison$Source == "Standard"] else "N/A",
    if (nrow(overall_comparison[overall_comparison$Source == "NF_GARCH",]) > 0) 
      overall_comparison$mean_AIC[overall_comparison$Source == "NF_GARCH"] else "N/A",
    if (nrow(win_rate) > 0) sum(win_rate$nf_wins) else 0,
    if (nrow(win_rate) > 0) sum(win_rate$total_comparisons - win_rate$nf_wins) else 0,
    if (overall_comparison$mean_MSE[overall_comparison$Source == "NF_GARCH"] < 
        overall_comparison$mean_MSE[overall_comparison$Source == "Standard"]) "NF-GARCH" else "Standard GARCH"
  )
)
writeData(wb, "Summary", summary_data)

# Model comparison
if (nrow(comparison_by_model) > 0) {
  addWorksheet(wb, "Model_Comparison")
  writeData(wb, "Model_Comparison", comparison_by_model)
}

# Overall comparison
addWorksheet(wb, "Overall_Comparison")
writeData(wb, "Overall_Comparison", overall_comparison)

# Win rate
if (nrow(win_rate) > 0) {
  addWorksheet(wb, "Win_Rate_Analysis")
  writeData(wb, "Win_Rate_Analysis", win_rate)
}

# Combined results
addWorksheet(wb, "Combined_Results")
writeData(wb, "Combined_Results", combined_results)

# Wilcoxon test results
if (exists("wilcoxon_df") && nrow(wilcoxon_df) > 0) {
  addWorksheet(wb, "Wilcoxon_Test")
  writeData(wb, "Wilcoxon_Test", wilcoxon_df)
}

# Asset class analysis
if (exists("asset_class_summary") && nrow(asset_class_summary) > 0) {
  addWorksheet(wb, "Asset_Class_Summary")
  writeData(wb, "Asset_Class_Summary", asset_class_summary)
}

if (exists("best_by_class") && nrow(best_by_class) > 0) {
  addWorksheet(wb, "Best_Model_By_Class")
  writeData(wb, "Best_Model_By_Class", best_by_class)
}

# Save
comparison_file <- "results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx"
saveWorkbook(wb, comparison_file, overwrite = TRUE)

cat("\n[OK] Comparison workbook saved to:", comparison_file, "\n")
cat("\n=== COMPARISON COMPLETE ===\n")

