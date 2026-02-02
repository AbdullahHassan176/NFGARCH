# Analysis 1: Residual Diagnostics
# Compare standard GARCH residuals vs NF synthetic residuals

library(FinTS)
library(moments)
library(tseries)
library(dplyr)

cat("=== ANALYSIS 1: RESIDUAL DIAGNOSTICS ===\n\n")
cat("Comparing Standard GARCH residuals vs NF synthetic residuals\n")
cat("Focus: ACF, ARCH effects, whiteness tests\n\n")

# Function to analyze residual quality
analyze_residual_quality <- function(asset, model = "sGARCH_norm") {
  
  cat(paste0("\n--- ", asset, " (", model, ") ---\n"))
  
  # Load standard GARCH residuals
  std_file <- paste0("outputs/manual/residuals_by_model/", 
                     model, "/", asset, "_Manual_Optimized_residuals.csv")
  
  # Load NF synthetic residuals  
  nf_file <- paste0("outputs/manual/nf_models/", 
                    model, "_", asset, "_synthetic_residuals.csv")
  
  # Check if files exist
  if(!file.exists(std_file)) {
    cat("  [SKIP] Standard residuals not found\n")
    return(NULL)
  }
  
  if(!file.exists(nf_file)) {
    cat("  [SKIP] NF residuals not found\n")
    return(NULL)
  }
  
  std_resid <- read.csv(std_file)
  nf_resid <- read.csv(nf_file)
  
  std_res <- std_resid$residuals
  nf_res <- nf_resid$synthetic_residuals
  
  # Remove NAs
  std_res <- std_res[!is.na(std_res)]
  nf_res <- nf_res[!is.na(nf_res)]
  
  # Autocorrelation
  std_acf <- acf(std_res, lag.max=10, plot=FALSE)
  nf_acf <- acf(nf_res, lag.max=10, plot=FALSE)
  
  # Ljung-Box test (p > 0.05 = good, no autocorrelation)
  std_lb <- tryCatch(Box.test(std_res, lag=10, type="Ljung-Box")$p.value, 
                     error=function(e) NA)
  nf_lb <- tryCatch(Box.test(nf_res, lag=10, type="Ljung-Box")$p.value, 
                    error=function(e) NA)
  
  # ARCH LM test (p > 0.05 = good, no ARCH effects)
  std_arch <- tryCatch(ArchTest(std_res, lags=5)$p.value, 
                       error=function(e) NA)
  nf_arch <- tryCatch(ArchTest(nf_res, lags=5)$p.value, 
                      error=function(e) NA)
  
  # Squared autocorrelation (ARCH effects)
  std_acf_sq <- acf(std_res^2, lag.max=5, plot=FALSE)
  nf_acf_sq <- acf(nf_res^2, lag.max=5, plot=FALSE)
  
  # Key diagnostics
  diagnostics <- data.frame(
    Asset = asset,
    Model = model,
    
    # Moments
    std_mean = mean(std_res),
    nf_mean = mean(nf_res),
    std_sd = sd(std_res),
    nf_sd = sd(nf_res),
    std_skew = skewness(std_res),
    nf_skew = skewness(nf_res),
    std_kurt = kurtosis(std_res),
    nf_kurt = kurtosis(nf_res),
    
    # Autocorrelation (lag 1)
    std_acf1 = std_acf$acf[2],
    nf_acf1 = nf_acf$acf[2],
    
    # Mean absolute ACF (lags 1-5)
    std_acf_mean = mean(abs(std_acf$acf[2:6])),
    nf_acf_mean = mean(abs(nf_acf$acf[2:6])),
    
    # Squared autocorrelation (lag 1)
    std_acf2_sq = std_acf_sq$acf[2],
    nf_acf2_sq = nf_acf_sq$acf[2],
    
    # Ljung-Box p-value
    std_lb_pval = std_lb,
    nf_lb_pval = nf_lb,
    
    # ARCH LM p-value
    std_arch_pval = std_arch,
    nf_arch_pval = nf_arch,
    
    # Quality flags (TRUE = good)
    std_white = !is.na(std_lb) && std_lb > 0.05,
    nf_white = !is.na(nf_lb) && nf_lb > 0.05,
    std_no_arch = !is.na(std_arch) && std_arch > 0.05,
    nf_no_arch = !is.na(nf_arch) && nf_arch > 0.05
  )
  
  cat(paste0("  Standard: ACF1=", round(std_acf$acf[2], 4), 
             ", LB p=", round(std_lb, 3), 
             ", ARCH p=", round(std_arch, 3), "\n"))
  cat(paste0("  NF:       ACF1=", round(nf_acf$acf[2], 4),
             ", LB p=", round(nf_lb, 3),
             ", ARCH p=", round(nf_arch, 3), "\n"))
  
  # Flag issues
  if(!is.na(nf_lb) && nf_lb < 0.05) {
    cat("  [WARNING] NF residuals show significant autocorrelation!\n")
  }
  if(!is.na(nf_arch) && nf_arch < 0.05) {
    cat("  [WARNING] NF residuals show residual ARCH effects!\n")
  }
  
  return(diagnostics)
}

# Run analysis for all assets and both models
assets <- c("NVDA", "MSFT", "AMZN", "EURUSD", "GBPUSD", "USDZAR")
models <- c("sGARCH_norm", "sGARCH_sstd")

results_list <- list()
for(model in models) {
  cat(paste0("\n### ", model, " ###\n"))
  for(asset in assets) {
    result <- analyze_residual_quality(asset, model)
    if(!is.null(result)) {
      results_list[[length(results_list) + 1]] <- result
    }
  }
}

# Combine results
results_df <- do.call(rbind, results_list)

# Save detailed results
write.csv(results_df, "analyses/results/analysis_1_residual_diagnostics_detailed.csv", 
          row.names=FALSE)

# Create summary
cat("\n\n=== SUMMARY ===\n\n")

summary <- results_df %>%
  group_by(Model) %>%
  summarise(
    n_assets = n(),
    
    # Standard residuals
    std_mean_acf1 = mean(abs(std_acf1), na.rm=TRUE),
    std_pct_white = 100 * mean(std_white, na.rm=TRUE),
    std_pct_no_arch = 100 * mean(std_no_arch, na.rm=TRUE),
    
    # NF residuals  
    nf_mean_acf1 = mean(abs(nf_acf1), na.rm=TRUE),
    nf_pct_white = 100 * mean(nf_white, na.rm=TRUE),
    nf_pct_no_arch = 100 * mean(nf_no_arch, na.rm=TRUE),
    
    # Comparison
    nf_worse_acf = nf_mean_acf1 > std_mean_acf1,
    nf_worse_white = nf_pct_white < std_pct_white,
    nf_worse_arch = nf_pct_no_arch < std_pct_no_arch
  )

print(summary)

write.csv(summary, "analyses/results/analysis_1_residual_diagnostics_summary.csv", 
          row.names=FALSE)

cat("\n=== INTERPRETATION ===\n\n")

for(i in 1:nrow(summary)) {
  model <- summary$Model[i]
  cat(paste0(model, ":\n"))
  cat(paste0("  Standard: ", round(summary$std_pct_white[i], 1), 
             "% pass whiteness, ", 
             round(summary$std_pct_no_arch[i], 1), "% no ARCH\n"))
  cat(paste0("  NF:       ", round(summary$nf_pct_white[i], 1), 
             "% pass whiteness, ", 
             round(summary$nf_pct_no_arch[i], 1), "% no ARCH\n"))
  
  if(summary$nf_worse_acf[i]) {
    cat("  [ISSUE] NF residuals have higher autocorrelation!\n")
  }
  if(summary$nf_worse_arch[i]) {
    cat("  [ISSUE] NF residuals have more ARCH effects!\n")
  }
  cat("\n")
}

cat("\n[COMPLETE] Analysis 1: Residual Diagnostics\n")
cat("Results saved to: analyses/results/analysis_1_*\n\n")
