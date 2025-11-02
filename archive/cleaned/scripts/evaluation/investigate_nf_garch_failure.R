#!/usr/bin/env Rscript
# Investigate why NF-GARCH performed poorly
# Analyzes residual distributions and simulation logic

library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xts)

cat("=== INVESTIGATING NF-GARCH PERFORMANCE ISSUES ===\n\n")

# Load comparison results
comparison_file <- "results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx"
nf_results_file <- "results/consolidated/NF_GARCH_Results_manual.xlsx"

cat("Loading comparison results...\n")
combined_results <- read.xlsx(comparison_file, sheet = "Combined_Results")
nf_results <- read.xlsx(nf_results_file, sheet = "Chrono_Split_NF_GARCH")

cat("Loaded", nrow(combined_results), "total results\n")
cat("NF-GARCH results:", nrow(nf_results), "\n\n")

# =============================================================================
# 1. Check NF Residual Statistics
# =============================================================================

cat("=== 1. ANALYZING NF RESIDUAL DISTRIBUTIONS ===\n")

# Load a few NF residual files
nf_residual_files <- list.files("outputs/manual/nf_models", 
                                 pattern = "*_synthetic_residuals.csv",
                                 full.names = TRUE)

nf_residual_stats <- list()

for (file in head(nf_residual_files, 6)) {
  tryCatch({
    nf_resid <- read.csv(file)
    resid_vec <- as.numeric(nf_resid[,1])
    resid_vec <- resid_vec[!is.na(resid_vec)]
    
    model_asset <- gsub(".*/([^/]+)_synthetic_residuals.csv", "\\1", file)
    
    nf_residual_stats[[model_asset]] <- data.frame(
      model_asset = model_asset,
      n = length(resid_vec),
      mean = mean(resid_vec),
      sd = sd(resid_vec),
      min = min(resid_vec),
      max = max(resid_vec),
      q25 = quantile(resid_vec, 0.25),
      q75 = quantile(resid_vec, 0.75),
      kurtosis = ifelse(sd(resid_vec) > 0, 
                       mean((resid_vec - mean(resid_vec))^4) / sd(resid_vec)^4 - 3,
                       NA),
      skewness = ifelse(sd(resid_vec) > 0,
                       mean((resid_vec - mean(resid_vec))^3) / sd(resid_vec)^3,
                       NA)
    )
    cat("Loaded:", model_asset, "| Mean:", round(mean(resid_vec), 4), 
        "| SD:", round(sd(resid_vec), 4), "\n")
  }, error = function(e) {
    cat("Error loading", file, ":", e$message, "\n")
  })
}

nf_residual_df <- do.call(rbind, nf_residual_stats)
cat("\nNF Residual Statistics:\n")
print(nf_residual_df)

# =============================================================================
# 2. Generate Standard GARCH Residuals for Comparison
# =============================================================================

cat("\n=== 2. GENERATING STANDARD GARCH RESIDUALS ===\n")

source("scripts/engines/engine_selector.R")
source("scripts/utils/safety_functions.R")

# Load data
raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1)
raw_price_data$Date <- as.Date(rownames(raw_price_data))
rownames(raw_price_data) <- NULL
raw_price_data <- raw_price_data %>% dplyr::select(Date, everything())

price_data_matrix <- raw_price_data[, !(names(raw_price_data) %in% "Date")]
date_index <- raw_price_data$Date

equity_tickers <- c("NVDA", "MSFT", "AMZN")
fx_names <- c("EURUSD", "GBPUSD", "USDZAR")

equity_xts <- lapply(equity_tickers, function(ticker) {
  if (ticker %in% names(price_data_matrix)) {
    xts(price_data_matrix[[ticker]], order.by = date_index)
  } else NULL
})
names(equity_xts) <- equity_tickers
equity_xts <- equity_xts[!sapply(equity_xts, is.null)]

fx_xts <- lapply(fx_names, function(ticker) {
  if (ticker %in% names(price_data_matrix)) {
    xts(price_data_matrix[[ticker]], order.by = date_index)
  } else NULL
})
names(fx_xts) <- fx_names
fx_xts <- fx_xts[!sapply(fx_xts, is.null)]

equity_returns <- lapply(equity_xts, function(x) diff(log(x))[-1])
fx_returns <- lapply(fx_xts, function(x) diff(log(x))[-1])

all_returns <- c(equity_returns, fx_returns)
all_asset_names <- c(equity_tickers, fx_names)

model_configs <- list(
  sGARCH = list(model = "sGARCH", distribution = "norm", submodel = NULL),
  eGARCH = list(model = "eGARCH", distribution = "sstd", submodel = NULL),
  TGARCH = list(model = "TGARCH", distribution = "sstd", submodel = NULL)
)

standard_residual_stats <- list()

for (asset_idx in 1:length(all_returns)) {
  asset_name <- all_asset_names[asset_idx]
  returns_data <- all_returns[[asset_idx]]
  
  for (model_name in names(model_configs)) {
    cfg <- model_configs[[model_name]]
    model_asset <- paste0(model_name, "_", asset_name)
    
    tryCatch({
      fit <- engine_fit(
        model = cfg$model,
        returns = as.numeric(returns_data),
        dist = cfg$distribution,
        submodel = cfg$submodel,
        engine = "manual"
      )
      
      if (engine_converged(fit)) {
        std_resid <- engine_residuals(fit, standardize = TRUE)
        std_resid <- std_resid[!is.na(std_resid)]
        
        standard_residual_stats[[model_asset]] <- data.frame(
          model_asset = model_asset,
          n = length(std_resid),
          mean = mean(std_resid),
          sd = sd(std_resid),
          min = min(std_resid),
          max = max(std_resid),
          q25 = quantile(std_resid, 0.25),
          q75 = quantile(std_resid, 0.75),
          kurtosis = ifelse(sd(std_resid) > 0,
                           mean((std_resid - mean(std_resid))^4) / sd(std_resid)^4 - 3,
                           NA),
          skewness = ifelse(sd(std_resid) > 0,
                           mean((std_resid - mean(std_resid))^3) / sd(std_resid)^3,
                           NA)
        )
        cat("Generated standard residuals:", model_asset, "\n")
      }
    }, error = function(e) {
      cat("Error for", model_asset, ":", e$message, "\n")
    })
  }
}

standard_residual_df <- do.call(rbind, standard_residual_stats)

cat("\nStandard Residual Statistics:\n")
print(standard_residual_df)

# =============================================================================
# 3. Compare Residual Distributions
# =============================================================================

cat("\n=== 3. COMPARING RESIDUAL DISTRIBUTIONS ===\n")

# Match models and assets
nf_residual_df$type <- "NF"
standard_residual_df$type <- "Standard"

comparison_residuals <- bind_rows(nf_residual_df, standard_residual_df)

comparison_summary <- comparison_residuals %>%
  group_by(model_asset, type) %>%
  summarise(
    mean = mean(mean, na.rm = TRUE),
    sd = mean(sd, na.rm = TRUE),
    min = mean(min, na.rm = TRUE),
    max = mean(max, na.rm = TRUE),
    kurtosis = mean(kurtosis, na.rm = TRUE),
    skewness = mean(skewness, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = type,
    values_from = c(mean, sd, min, max, kurtosis, skewness),
    names_sep = "_"
  ) %>%
  mutate(
    mean_diff = mean_NF - mean_Standard,
    sd_ratio = sd_NF / sd_Standard,
    range_ratio = (max_NF - min_NF) / (max_Standard - min_Standard)
  )

cat("\nResidual Distribution Comparison:\n")
print(comparison_summary)

# =============================================================================
# 4. Analyze Simulation Issues
# =============================================================================

cat("\n=== 4. ANALYZING SIMULATION ISSUES ===\n")

# Check if NF residuals are causing extreme forecasts
cat("Checking for extreme forecast values in NF-GARCH results...\n")

extreme_nf <- nf_results %>%
  filter(!is.na(MSE)) %>%
  mutate(
    log_mse = log10(abs(MSE) + 1),
    log_mae = log10(abs(MAE) + 1),
    is_extreme = MSE > 1e10 | MAE > 1e10
  )

cat("\nExtreme NF-GARCH Results:\n")
print(extreme_nf %>% filter(is_extreme) %>% select(Model, Asset, MSE, MAE))

cat("\nNF-GARCH MSE Statistics (log10):\n")
print(summary(extreme_nf$log_mse))

# =============================================================================
# 5. Check Residual Scaling in Simulation
# =============================================================================

cat("\n=== 5. CHECKING RESIDUAL SCALING IN SIMULATION ===\n")

# Load one NF residual file and check its usage
test_nf_file <- "outputs/manual/nf_models/TGARCH_EURUSD_synthetic_residuals.csv"
if (file.exists(test_nf_file)) {
  test_nf_resid <- read.csv(test_nf_file)
  test_nf_vec <- as.numeric(test_nf_resid[,1])
  test_nf_vec <- test_nf_vec[!is.na(test_nf_vec)]
  
  cat("\nTest NF Residual (TGARCH_EURUSD):\n")
  cat("Raw: Mean =", mean(test_nf_vec), "SD =", sd(test_nf_vec), "\n")
  
  # Check if it's standardized
  test_nf_std <- (test_nf_vec - mean(test_nf_vec)) / sd(test_nf_vec)
  cat("After standardization: Mean =", mean(test_nf_std), "SD =", sd(test_nf_std), "\n")
  
  # Get corresponding standard residuals
  eurusd_returns <- fx_returns[["EURUSD"]]
  tgarch_fit <- engine_fit(
    model = "TGARCH",
    returns = as.numeric(eurusd_returns),
    dist = "sstd",
    submodel = NULL,
    engine = "manual"
  )
  
  if (engine_converged(tgarch_fit)) {
    std_resid <- engine_residuals(tgarch_fit, standardize = TRUE)
    std_resid <- std_resid[!is.na(std_resid)]
    
    cat("\nStandard Residual (TGARCH_EURUSD):\n")
    cat("Mean =", mean(std_resid), "SD =", sd(std_resid), "\n")
    
    # Compare distributions
    cat("\nDistribution Comparison:\n")
    cat("NF range:", range(test_nf_vec), "\n")
    cat("Standard range:", range(std_resid), "\n")
    cat("NF SD / Standard SD:", sd(test_nf_vec) / sd(std_resid), "\n")
  }
}

# =============================================================================
# 6. Create Diagnostic Visualizations
# =============================================================================

cat("\n=== 6. CREATING DIAGNOSTIC VISUALIZATIONS ===\n")

# Save diagnostic data
diagnostic_dir <- "results/diagnostics"
if (!dir.exists(diagnostic_dir)) {
  dir.create(diagnostic_dir, recursive = TRUE)
}

# Save comparison data
write.csv(comparison_residuals, 
          file.path(diagnostic_dir, "residual_comparison.csv"),
          row.names = FALSE)

write.csv(comparison_summary,
          file.path(diagnostic_dir, "residual_summary.csv"),
          row.names = FALSE)

write.csv(extreme_nf,
          file.path(diagnostic_dir, "extreme_nf_results.csv"),
          row.names = FALSE)

cat("\n✓ Diagnostic data saved to:", diagnostic_dir, "\n")
cat("\n=== INVESTIGATION COMPLETE ===\n")

# Create summary report
cat("\n=== SUMMARY OF FINDINGS ===\n")
cat("1. NF residual files found:", length(nf_residual_files), "\n")
cat("2. NF residual stats calculated:", nrow(nf_residual_df), "\n")
cat("3. Standard residual stats calculated:", nrow(standard_residual_df), "\n")
cat("4. Extreme NF-GARCH results:", sum(extreme_nf$is_extreme, na.rm = TRUE), "\n")
cat("\nCheck diagnostic files for detailed analysis.\n")

