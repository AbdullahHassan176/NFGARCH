# Analysis 4: Temporal Dynamics Analysis
# Test if NF changes time-series structure in harmful ways

suppressPackageStartupMessages(library(randtests))
suppressPackageStartupMessages(library(vrtest))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(moments))

cat("=== ANALYSIS 4: TEMPORAL DYNAMICS ANALYSIS ===\n\n")
cat("Testing if NF preserves temporal structure or adds spurious patterns\n")
cat("Tests: Runs test, Turning points, Variance ratio\n\n")

# Function to analyze temporal dynamics
analyze_temporal_dynamics <- function(asset, model = "sGARCH_norm") {
  
  cat(paste0("\n--- ", asset, " (", model, ") ---\n"))
  
  # Load residuals
  std_file <- paste0("outputs/manual/residuals_by_model/", 
                     model, "/", asset, "_Manual_Optimized_residuals.csv")
  nf_file <- paste0("outputs/manual/nf_models/", 
                    model, "_", asset, "_synthetic_residuals.csv")
  
  if(!file.exists(std_file) || !file.exists(nf_file)) {
    cat("  [SKIP] Files not found\n")
    return(NULL)
  }
  
  original <- read.csv(std_file)
  nf <- read.csv(nf_file)
  
  orig_res <- original$residuals[!is.na(original$residuals)]
  nf_res <- nf$synthetic_residuals[!is.na(nf$synthetic_residuals)]
  
  # Autocorrelation structure (sum of absolute ACF up to lag 20)
  orig_acf <- acf(orig_res, lag.max=20, plot=FALSE)
  nf_acf <- acf(nf_res, lag.max=20, plot=FALSE)
  
  orig_acf_sum <- sum(abs(orig_acf$acf[-1]))  # Exclude lag 0
  nf_acf_sum <- sum(abs(nf_acf$acf[-1]))
  
  # Runs test (randomness) - p > 0.05 = random, good
  orig_runs <- tryCatch({
    rt <- runs.test(as.factor(sign(orig_res)))
    list(stat = rt$statistic, pval = rt$p.value)
  }, error = function(e) list(stat = NA, pval = NA))
  
  nf_runs <- tryCatch({
    rt <- runs.test(as.factor(sign(nf_res)))
    list(stat = rt$statistic, pval = rt$p.value)
  }, error = function(e) list(stat = NA, pval = NA))
  
  # Turning points test - p > 0.05 = random, good
  orig_turning <- tryCatch({
    tt <- turning.point.test(orig_res)
    list(stat = tt$statistic, pval = tt$p.value)
  }, error = function(e) list(stat = NA, pval = NA))
  
  nf_turning <- tryCatch({
    tt <- turning.point.test(nf_res)
    list(stat = tt$statistic, pval = tt$p.value)
  }, error = function(e) list(stat = NA, pval = NA))
  
  # Variance ratio test (mean reversion)
  # VR = 1 indicates random walk
  orig_vr <- tryCatch({
    vr <- Auto.VR(orig_res)
    list(vr = vr$stat, pval = vr$pvalue)
  }, error = function(e) list(vr = NA, pval = NA))
  
  nf_vr <- tryCatch({
    vr <- Auto.VR(nf_res)
    list(vr = vr$stat, pval = vr$pvalue)
  }, error = function(e) list(vr = NA, pval = NA))
  
  # Spectral analysis - check for dominant frequencies
  orig_spectrum <- spectrum(orig_res, plot=FALSE)
  nf_spectrum <- spectrum(nf_res, plot=FALSE)
  
  # Dominant frequency strength (normalized)
  orig_dominant <- max(orig_spectrum$spec) / mean(orig_spectrum$spec)
  nf_dominant <- max(nf_spectrum$spec) / mean(nf_spectrum$spec)
  
  dynamics <- data.frame(
    Asset = asset,
    Model = model,
    
    # Autocorrelation structure
    orig_acf_sum = orig_acf_sum,
    nf_acf_sum = nf_acf_sum,
    acf_ratio = nf_acf_sum / orig_acf_sum,
    
    # Runs test (randomness)
    orig_runs_stat = orig_runs$stat,
    nf_runs_stat = nf_runs$stat,
    orig_runs_pval = orig_runs$pval,
    nf_runs_pval = nf_runs$pval,
    orig_random = !is.na(orig_runs$pval) && orig_runs$pval > 0.05,
    nf_random = !is.na(nf_runs$pval) && nf_runs$pval > 0.05,
    
    # Turning points
    orig_turning_pval = orig_turning$pval,
    nf_turning_pval = nf_turning$pval,
    orig_turning_random = !is.na(orig_turning$pval) && orig_turning$pval > 0.05,
    nf_turning_random = !is.na(nf_turning$pval) && nf_turning$pval > 0.05,
    
    # Variance ratio
    orig_vr = orig_vr$vr,
    nf_vr = nf_vr$vr,
    vr_diff = abs(nf_vr$vr - 1) - abs(orig_vr$vr - 1),  # Deviation from 1
    
    # Spectral properties
    orig_dominant_freq = orig_dominant,
    nf_dominant_freq = nf_dominant,
    freq_ratio = nf_dominant / orig_dominant,
    
    # Overall quality
    nf_worse_structure = nf_acf_sum > 1.2 * orig_acf_sum  # NF adds >20% more autocorrelation
  )
  
  cat(paste0("  ACF sum: Orig=", round(orig_acf_sum, 3), 
             ", NF=", round(nf_acf_sum, 3), 
             " (ratio=", round(dynamics$acf_ratio, 2), ")\n"))
  cat(paste0("  Runs test: Orig p=", round(orig_runs$pval, 3), 
             ", NF p=", round(nf_runs$pval, 3), "\n"))
  cat(paste0("  Turning points: Orig p=", round(orig_turning$pval, 3),
             ", NF p=", round(nf_turning$pval, 3), "\n"))
  
  if(dynamics$nf_worse_structure) {
    cat("  [WARNING] NF adds significant autocorrelation structure!\n")
  }
  if(!is.na(nf_runs$pval) && nf_runs$pval < 0.05 && orig_runs$pval >= 0.05) {
    cat("  [WARNING] NF introduces non-random patterns!\n")
  }
  
  return(dynamics)
}

# Run analysis
assets <- c("NVDA", "MSFT", "AMZN", "EURUSD", "GBPUSD", "USDZAR")
models <- c("sGARCH_norm", "sGARCH_sstd")

results_list <- list()
for(model in models) {
  cat(paste0("\n### ", model, " ###\n"))
  for(asset in assets) {
    result <- analyze_temporal_dynamics(asset, model)
    if(!is.null(result)) {
      results_list[[length(results_list) + 1]] <- result
    }
  }
}

# Combine results
results_df <- do.call(rbind, results_list)

# Save detailed results
write.csv(results_df, "analyses/results/analysis_4_temporal_dynamics_detailed.csv", 
          row.names=FALSE)

# Create summary
cat("\n\n=== SUMMARY ===\n\n")

summary <- results_df %>%
  group_by(Model) %>%
  summarise(
    n_assets = n(),
    
    # Autocorrelation
    mean_acf_ratio = mean(acf_ratio, na.rm=TRUE),
    pct_acf_worse = 100 * mean(acf_ratio > 1.2, na.rm=TRUE),
    
    # Randomness tests
    pct_orig_random_runs = 100 * mean(orig_random, na.rm=TRUE),
    pct_nf_random_runs = 100 * mean(nf_random, na.rm=TRUE),
    pct_orig_random_turning = 100 * mean(orig_turning_random, na.rm=TRUE),
    pct_nf_random_turning = 100 * mean(nf_turning_random, na.rm=TRUE),
    
    # Spectral
    mean_freq_ratio = mean(freq_ratio, na.rm=TRUE),
    
    # Overall
    pct_worse_structure = 100 * mean(nf_worse_structure, na.rm=TRUE)
  )

print(summary)

write.csv(summary, "analyses/results/analysis_4_temporal_dynamics_summary.csv", 
          row.names=FALSE)

cat("\n=== INTERPRETATION ===\n\n")

for(i in 1:nrow(summary)) {
  model <- summary$Model[i]
  cat(paste0(model, ":\n"))
  cat(paste0("  ACF ratio: ", round(summary$mean_acf_ratio[i], 3), 
             " (>1 = NF adds autocorrelation)\n"))
  cat(paste0("  Assets with worse structure: ", round(summary$pct_worse_structure[i], 1), 
             "%\n"))
  cat(paste0("  Randomness (runs): Orig=", round(summary$pct_orig_random_runs[i], 1), 
             "%, NF=", round(summary$pct_nf_random_runs[i], 1), "%\n"))
  cat(paste0("  Randomness (turning): Orig=", round(summary$pct_orig_random_turning[i], 1),
             "%, NF=", round(summary$pct_nf_random_turning[i], 1), "%\n"))
  
  if(summary$mean_acf_ratio[i] > 1.1) {
    cat("  [ISSUE] NF adds temporal structure (ACF ratio > 1.1)!\n")
  }
  if(summary$pct_nf_random_runs[i] < summary$pct_orig_random_runs[i] - 10) {
    cat("  [ISSUE] NF reduces randomness significantly!\n")
  }
  if(summary$pct_worse_structure[i] > 30) {
    cat("  [ISSUE] >30% of assets show degraded temporal structure!\n")
  }
  cat("\n")
}

# Compare norm vs sstd
if(nrow(summary) == 2) {
  cat("\n=== COMPARISON: norm vs sstd ===\n\n")
  
  norm_idx <- which(summary$Model == "sGARCH_norm")
  sstd_idx <- which(summary$Model == "sGARCH_sstd")
  
  if(length(norm_idx) > 0 && length(sstd_idx) > 0) {
    cat("Temporal Structure Degradation:\n")
    cat(paste0("  norm: ", round(summary$pct_worse_structure[norm_idx], 1), 
               "% of assets\n"))
    cat(paste0("  sstd: ", round(summary$pct_worse_structure[sstd_idx], 1), 
               "% of assets\n"))
    
    if(summary$pct_worse_structure[norm_idx] > summary$pct_worse_structure[sstd_idx]) {
      cat("  → sGARCH_norm: NF causes MORE temporal structure issues\n")
      cat("     This contributes to worse forecasting performance!\n")
    }
  }
}

cat("\n[COMPLETE] Analysis 4: Temporal Dynamics\n")
cat("Results saved to: analyses/results/analysis_4_*\n\n")
