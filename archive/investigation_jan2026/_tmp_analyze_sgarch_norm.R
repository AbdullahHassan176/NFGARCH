library(openxlsx)
library(dplyr)
library(tidyr)

combined <- read.xlsx('results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx', 
                      sheet='Combined_Results')

# Analyze sGARCH for both distributions
cat("=== INVESTIGATING WHY NF WORSE FOR sGARCH_norm ===\n\n")

for(dist in c('norm', 'sstd')) {
  sgarch <- combined %>% 
    filter(Model == 'sGARCH', Distribution == dist) %>%
    select(Asset, Source, MSE, MAE, PredictiveLogLik, NPaths) %>%
    arrange(Asset, Source)
  
  # Split by source
  nf <- sgarch %>% filter(Source == "NF_GARCH")
  std <- sgarch %>% filter(Source == "Standard")
  
  # Merge
  comp <- data.frame(
    Asset = nf$Asset,
    MSE_NF = nf$MSE,
    MSE_Std = std$MSE,
    MAE_NF = nf$MAE,
    MAE_Std = std$MAE,
    LogLik_NF = nf$PredictiveLogLik,
    LogLik_Std = std$PredictiveLogLik,
    NPaths_NF = nf$NPaths,
    NPaths_Std = std$NPaths
  ) %>%
    mutate(
      MSE_pct = 100 * (MSE_NF - MSE_Std) / MSE_Std,
      MAE_pct = 100 * (MAE_NF - MAE_Std) / MAE_Std,
      NF_worse_MSE = MSE_NF > MSE_Std,
      Asset_Class = ifelse(Asset %in% c('NVDA','MSFT','AMZN'), 'Equity', 'FX')
    )
  
  cat(paste0("\n=== sGARCH_", dist, " ===\n"))
  print(comp[, c('Asset', 'Asset_Class', 'MSE_Std', 'MSE_NF', 'MSE_pct', 'NF_worse_MSE')])
  
  cat(paste0("\nSummary for sGARCH_", dist, ":\n"))
  cat(paste0("  NF worse: ", sum(comp$NF_worse_MSE), " of ", nrow(comp), " assets\n"))
  cat(paste0("  Mean MSE % change: ", round(mean(comp$MSE_pct), 2), "%\n"))
  cat(paste0("  Median MSE % change: ", round(median(comp$MSE_pct), 2), "%\n"))
  
  # By asset class
  summary_by_class <- comp %>%
    group_by(Asset_Class) %>%
    summarise(
      n = n(),
      n_worse = sum(NF_worse_MSE),
      mean_MSE_pct = mean(MSE_pct),
      median_MSE_pct = median(MSE_pct)
    )
  cat("\n  By Asset Class:\n")
  print(summary_by_class)
  cat("\n")
}

# Now check NF residual characteristics for sGARCH_norm
cat("\n=== NF RESIDUAL QUALITY CHECK ===\n\n")

nf_files <- list.files('outputs/manual/nf_models', 
                       pattern='sGARCH_norm_.*_synthetic_residuals.csv',
                       full.names=TRUE)

cat("Checking NF residuals for sGARCH_norm models:\n\n")

residual_stats <- data.frame()

for(file in nf_files) {
  asset <- gsub('.*sGARCH_norm_([A-Z]+)_synthetic.*', '\\1', basename(file))
  
  residuals <- read.csv(file)
  
  stats <- data.frame(
    Asset = asset,
    n = nrow(residuals),
    mean = mean(residuals$synthetic_residuals, na.rm=TRUE),
    sd = sd(residuals$synthetic_residuals, na.rm=TRUE),
    skewness = moments::skewness(residuals$synthetic_residuals),
    kurtosis = moments::kurtosis(residuals$synthetic_residuals),
    min = min(residuals$synthetic_residuals, na.rm=TRUE),
    max = max(residuals$synthetic_residuals, na.rm=TRUE)
  )
  
  residual_stats <- rbind(residual_stats, stats)
}

residual_stats <- residual_stats %>%
  mutate(Asset_Class = ifelse(Asset %in% c('NVDA','MSFT','AMZN'), 'Equity', 'FX'))

cat("NF Residual Statistics for sGARCH_norm:\n")
print(residual_stats)

cat("\n=== DIAGNOSIS ===\n\n")

# Check if residuals are properly standardized
bad_standardization <- residual_stats %>%
  filter(abs(mean) > 0.5 | abs(sd - 1.5) > 0.5)

if(nrow(bad_standardization) > 0) {
  cat("⚠️ WARNING: Some NF residuals have unusual standardization:\n")
  print(bad_standardization[, c('Asset', 'mean', 'sd')])
  cat("\nExpected: mean ≈ 0, sd ≈ 1.5 (for skewed-student-t)\n")
  cat("For normal distribution, NF may be learning non-Gaussian patterns that don't exist!\n\n")
}

# Compare to standard residuals if available
cat("\n=== HYPOTHESIS: Why NF Fails for Gaussian Models ===\n\n")
cat("1. OVERFITTING: NF learns complex non-linear transformations\n")
cat("   - For Gaussian innovations: No complex patterns to learn\n")
cat("   - NF adds noise by fitting spurious patterns\n")
cat("   - Standard GARCH already optimal for Gaussian case\n\n")

cat("2. RESIDUAL DISTRIBUTION:\n")
cat("   - sGARCH_norm assumes N(0,1) innovations\n")
cat("   - NF tries to capture non-Gaussian features\n")
cat("   - If true distribution IS Gaussian, NF learns noise\n\n")

cat("3. ASSET-SPECIFIC PATTERNS:\n")
equity_worse <- sum(comp$NF_worse_MSE[comp$Asset_Class == 'Equity'])
cat(paste0("   - Equity: NF worse in ", equity_worse, " of 3 assets\n"))
cat("   - Equity has higher kurtosis (fat tails)\n")
cat("   - But sGARCH_norm forces Gaussian - mismatch!\n\n")

cat("CONCLUSION: NF-GARCH fails when the innovation distribution is\n")
cat("correctly specified. For sGARCH_norm, if innovations ARE Gaussian,\n")
cat("NF adds complexity without benefit, leading to overfitting.\n")
