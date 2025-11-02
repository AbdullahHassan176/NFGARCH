#!/usr/bin/env Rscript
# Create visualizations comparing NF-GARCH vs Standard GARCH
# Shows residual distributions and performance issues

library(openxlsx)
library(dplyr)
library(ggplot2)
library(xts)

cat("=== CREATING NF-GARCH VISUALIZATIONS ===\n\n")

# Load diagnostic data
diagnostic_dir <- "results/diagnostics"
if (!dir.exists(diagnostic_dir)) {
  dir.create(diagnostic_dir, recursive = TRUE)
}

# Load residual comparison
if (file.exists(file.path(diagnostic_dir, "residual_comparison.csv"))) {
  comparison_residuals <- read.csv(file.path(diagnostic_dir, "residual_comparison.csv"))
} else {
  cat("Running investigation first...\n")
  source("scripts/evaluation/investigate_nf_garch_failure.R")
  comparison_residuals <- read.csv(file.path(diagnostic_dir, "residual_comparison.csv"))
}

# Load NF vs Standard comparison
comparison_file <- "results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx"
combined_results <- read.xlsx(comparison_file, sheet = "Combined_Results")

# =============================================================================
# 1. Residual Distribution Comparison
# =============================================================================

cat("Creating residual distribution plots...\n")

# Load actual residual data for visualization
nf_residual_files <- list.files("outputs/manual/nf_models", 
                                  pattern = "*_synthetic_residuals.csv",
                                  full.names = TRUE)

# Sample a few for detailed visualization
plot_models <- c("TGARCH_EURUSD", "eGARCH_EURUSD", "TGARCH_NVDA", "eGARCH_NVDA")

nf_resid_data <- list()
for (file in nf_residual_files) {
  model_asset <- gsub(".*/([^/]+)_synthetic_residuals.csv", "\\1", file)
  if (model_asset %in% plot_models) {
    nf_resid <- read.csv(file)
    nf_resid_data[[model_asset]] <- data.frame(
      value = as.numeric(nf_resid[,1]),
      type = "NF",
      model_asset = model_asset
    )
  }
}

# Generate standard residuals for comparison
source("scripts/engines/engine_selector.R")
source("scripts/utils/safety_functions.R")

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

std_resid_data <- list()

for (model_asset in plot_models) {
  parts <- strsplit(model_asset, "_")[[1]]
  model_name <- parts[1]
  asset_name <- parts[2]
  
  if (asset_name %in% all_asset_names) {
    idx <- which(all_asset_names == asset_name)
    returns_data <- all_returns[[idx]]
    
    model_config <- list(
      sGARCH = list(model = "sGARCH", distribution = "norm", submodel = NULL),
      eGARCH = list(model = "eGARCH", distribution = "sstd", submodel = NULL),
      TGARCH = list(model = "TGARCH", distribution = "sstd", submodel = NULL)
    )
    
    if (model_name %in% names(model_config)) {
      cfg <- model_config[[model_name]]
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
          
          std_resid_data[[model_asset]] <- data.frame(
            value = std_resid,
            type = "Standard",
            model_asset = model_asset
          )
        }
      }, error = function(e) {
        cat("Error generating standard residuals for", model_asset, ":", e$message, "\n")
      })
    }
  }
}

# Combine for plotting
plot_data_list <- list()
for (model in plot_models) {
  if (model %in% names(nf_resid_data)) {
    plot_data_list[[length(plot_data_list) + 1]] <- nf_resid_data[[model]]
  }
  if (model %in% names(std_resid_data)) {
    plot_data_list[[length(plot_data_list) + 1]] <- std_resid_data[[model]]
  }
}

if (length(plot_data_list) > 0) {
  plot_data <- do.call(rbind, plot_data_list)
  
  # Plot 1: Residual distribution comparison
  p1 <- ggplot(plot_data, aes(x = value, fill = type)) +
    geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
    facet_wrap(~model_asset, scales = "free") +
    labs(
      title = "Residual Distribution Comparison: NF vs Standard GARCH",
      subtitle = "NF residuals show improper scaling (mean ≠ 0, SD ≠ 1) in several cases",
      x = "Residual Value",
      y = "Frequency",
      fill = "Residual Type"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(diagnostic_dir, "residual_distributions.png"), 
         plot = p1, width = 12, height = 8, dpi = 300)
  cat("✓ Saved: residual_distributions.png\n")
  
  # Plot 2: Q-Q plots
  p2 <- ggplot(plot_data, aes(sample = value, color = type)) +
    stat_qq(alpha = 0.6) +
    stat_qq_line() +
    facet_wrap(~model_asset, scales = "free") +
    labs(
      title = "Q-Q Plot Comparison: NF vs Standard GARCH Residuals",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles",
      color = "Residual Type"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(diagnostic_dir, "residual_qq_plots.png"), 
         plot = p2, width = 12, height = 8, dpi = 300)
  cat("✓ Saved: residual_qq_plots.png\n")
}

# =============================================================================
# 2. Performance Metrics Comparison
# =============================================================================

cat("\nCreating performance comparison plots...\n")

# Prepare performance data
perf_data <- combined_results %>%
  filter(!is.na(MSE)) %>%
  mutate(
    log_mse = log10(abs(MSE) + 1e-10),
    log_mae = log10(abs(MAE) + 1e-10),
    is_extreme = MSE > 1e10 | MAE > 1e10
  )

# Plot 3: MSE Comparison
p3 <- ggplot(perf_data, aes(x = Model, y = log_mse, fill = Source)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(
    name = "Log10(MSE)",
    breaks = seq(0, 250, by = 50)
  ) +
  labs(
    title = "MSE Comparison: NF-GARCH vs Standard GARCH",
    subtitle = "NF-GARCH shows extreme MSE values (>1e100) indicating scaling issues",
    x = "Model",
    fill = "Residual Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(diagnostic_dir, "mse_comparison.png"), 
       plot = p3, width = 10, height = 6, dpi = 300)
cat("✓ Saved: mse_comparison.png\n")

# Plot 4: MAE Comparison
p4 <- ggplot(perf_data, aes(x = Model, y = log_mae, fill = Source)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(
    name = "Log10(MAE)",
    breaks = seq(0, 120, by = 20)
  ) +
  labs(
    title = "MAE Comparison: NF-GARCH vs Standard GARCH",
    subtitle = "NF-GARCH shows extreme MAE values due to improper residual scaling",
    x = "Model",
    fill = "Residual Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(diagnostic_dir, "mae_comparison.png"), 
       plot = p4, width = 10, height = 6, dpi = 300)
cat("✓ Saved: mae_comparison.png\n")

# =============================================================================
# 3. Residual Statistics Summary
# =============================================================================

cat("\nCreating residual statistics summary...\n")

# Load residual summary if available
if (file.exists(file.path(diagnostic_dir, "residual_summary.csv"))) {
  resid_summary <- read.csv(file.path(diagnostic_dir, "residual_summary.csv"))
  
  # Plot 5: Mean and SD comparison
  library(tidyr)
  summary_long <- resid_summary %>%
    select(model_asset, mean_NF, mean_Standard, sd_NF, sd_Standard) %>%
    filter(!is.na(mean_NF) | !is.na(mean_Standard)) %>%
    pivot_longer(
      cols = c(mean_NF, mean_Standard, sd_NF, sd_Standard),
      names_to = c("metric", "type"),
      names_sep = "_"
    ) %>%
    filter(!is.na(value))
  
  p5 <- ggplot(summary_long, aes(x = model_asset, y = abs(value), fill = type)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
    facet_wrap(~metric, scales = "free_y") +
    labs(
      title = "Residual Statistics: Mean and SD Comparison",
      subtitle = "NF residuals should have mean ≈ 0 and SD ≈ 1",
      x = "Model-Asset",
      y = "Absolute Value",
      fill = "Type"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggsave(file.path(diagnostic_dir, "residual_stats_comparison.png"), 
         plot = p5, width = 14, height = 8, dpi = 300)
  cat("✓ Saved: residual_stats_comparison.png\n")
}

# =============================================================================
# 4. Create Summary Report
# =============================================================================

cat("\n=== CREATING SUMMARY REPORT ===\n")

# Find problematic models
problematic <- comparison_residuals %>%
  filter(type == "NF") %>%
  filter(abs(mean) > 5 | sd > 10 | sd < 0.5)

if (nrow(problematic) > 0) {
  cat("\n⚠️ PROBLEMATIC NF RESIDUALS (Not Properly Standardized):\n")
  print(problematic[, c("model_asset", "mean", "sd", "min", "max")])
}

# Summary statistics
cat("\n=== KEY FINDINGS ===\n")
cat("1. NF Residuals with Mean Issues (>5):", 
    sum(abs(comparison_residuals$mean[comparison_residuals$type == "NF"]) > 5, na.rm = TRUE), "\n")
cat("2. NF Residuals with SD Issues (>10 or <0.5):", 
    sum((comparison_residuals$sd[comparison_residuals$type == "NF"] > 10 | 
         comparison_residuals$sd[comparison_residuals$type == "NF"] < 0.5), na.rm = TRUE), "\n")
cat("3. Extreme NF-GARCH Results (>1e10):", 
    sum(perf_data$is_extreme[perf_data$Source == "NF_GARCH"], na.rm = TRUE), "\n")

cat("\n✓ All visualizations saved to:", diagnostic_dir, "\n")
cat("=== VISUALIZATION COMPLETE ===\n")

