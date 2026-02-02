# Analysis 3: Information Loss Analysis
# Measure if NF loses critical information during transformation

suppressPackageStartupMessages(library(entropy))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(moments))

cat("=== ANALYSIS 3: INFORMATION LOSS ANALYSIS ===\n\n")
cat("Measuring information preservation through NF transformation\n")
cat("Metrics: Entropy, KL divergence, distribution similarity\n\n")

# Function to analyze information loss
analyze_information_loss <- function(asset, model = "sGARCH_norm") {
  
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
  
  # Discretize for entropy calculation (20 bins)
  nbins <- 20
  
  # Create histograms
  orig_hist <- hist(orig_res, breaks=nbins, plot=FALSE)
  nf_hist <- hist(nf_res, breaks=nbins, plot=FALSE)
  
  # Normalize to probabilities
  orig_probs <- orig_hist$counts / sum(orig_hist$counts)
  nf_probs <- nf_hist$counts / sum(nf_hist$counts)
  
  # Add small constant to avoid log(0)
  epsilon <- 1e-10
  orig_probs <- orig_probs + epsilon
  nf_probs <- nf_probs + epsilon
  
  # Renormalize
  orig_probs <- orig_probs / sum(orig_probs)
  nf_probs <- nf_probs / sum(nf_probs)
  
  # Shannon Entropy
  orig_entropy <- -sum(orig_probs * log2(orig_probs))
  nf_entropy <- -sum(nf_probs * log2(nf_probs))
  
  # KL Divergence (how different distributions are)
  # KL(orig || nf) = sum(orig * log(orig/nf))
  kl_div <- sum(orig_probs * log(orig_probs / nf_probs))
  
  # Jensen-Shannon Divergence (symmetric version of KL)
  m_probs <- (orig_probs + nf_probs) / 2
  js_div <- 0.5 * sum(orig_probs * log(orig_probs / m_probs)) + 
            0.5 * sum(nf_probs * log(nf_probs / m_probs))
  
  # Kolmogorov-Smirnov statistic (empirical CDF difference)
  ks_test <- ks.test(orig_res, nf_res)
  ks_stat <- ks_test$statistic
  ks_pval <- ks_test$p.value
  
  # Wasserstein distance (Earth Mover's Distance)
  # Simple approximation using sorted values
  n_quantiles <- 100
  orig_quantiles <- quantile(orig_res, probs=seq(0, 1, length.out=n_quantiles))
  nf_quantiles <- quantile(nf_res, probs=seq(0, 1, length.out=n_quantiles))
  wasserstein <- mean(abs(orig_quantiles - nf_quantiles))
  
  # Moment differences
  mean_diff <- abs(mean(orig_res) - mean(nf_res))
  sd_diff <- abs(sd(orig_res) - sd(nf_res))
  skew_diff <- abs(moments::skewness(orig_res) - moments::skewness(nf_res))
  kurt_diff <- abs(moments::kurtosis(orig_res) - moments::kurtosis(nf_res))
  
  info <- data.frame(
    Asset = asset,
    Model = model,
    
    # Entropy (information content)
    orig_entropy = orig_entropy,
    nf_entropy = nf_entropy,
    entropy_ratio = nf_entropy / orig_entropy,
    entropy_loss_pct = 100 * (1 - nf_entropy / orig_entropy),
    
    # Distribution divergence
    kl_divergence = kl_div,
    js_divergence = js_div,
    ks_statistic = ks_stat,
    ks_pval = ks_pval,
    wasserstein_dist = wasserstein,
    
    # Moment preservation
    mean_diff = mean_diff,
    sd_diff = sd_diff,
    skew_diff = skew_diff,
    kurt_diff = kurt_diff,
    moment_preservation = 1 / (1 + mean_diff + sd_diff + skew_diff + kurt_diff),
    
    # Overall information quality
    distributions_similar = ks_pval > 0.05
  )
  
  cat(paste0("  Entropy: Orig=", round(orig_entropy, 3), 
             ", NF=", round(nf_entropy, 3), 
             ", Loss=", round(info$entropy_loss_pct, 1), "%\n"))
  cat(paste0("  KL divergence: ", round(kl_div, 4), 
             ", JS divergence: ", round(js_div, 4), "\n"))
  cat(paste0("  KS test: D=", round(ks_stat, 4), 
             ", p=", round(ks_pval, 4), 
             " (", ifelse(ks_pval > 0.05, "Similar", "Different"), ")\n"))
  
  if(info$entropy_loss_pct > 10) {
    cat("  [WARNING] Significant entropy loss (>10%)!\n")
  }
  if(kl_div > 0.1) {
    cat("  [WARNING] Large KL divergence - distributions differ substantially!\n")
  }
  
  return(info)
}

# Run analysis
assets <- c("NVDA", "MSFT", "AMZN", "EURUSD", "GBPUSD", "USDZAR")
models <- c("sGARCH_norm", "sGARCH_sstd")

results_list <- list()
for(model in models) {
  cat(paste0("\n### ", model, " ###\n"))
  for(asset in assets) {
    result <- analyze_information_loss(asset, model)
    if(!is.null(result)) {
      results_list[[length(results_list) + 1]] <- result
    }
  }
}

# Combine results
results_df <- do.call(rbind, results_list)

# Save detailed results
write.csv(results_df, "analyses/results/analysis_3_information_loss_detailed.csv", 
          row.names=FALSE)

# Create summary
cat("\n\n=== SUMMARY ===\n\n")

summary <- results_df %>%
  group_by(Model) %>%
  summarise(
    n_assets = n(),
    
    # Entropy
    mean_entropy_loss_pct = mean(entropy_loss_pct, na.rm=TRUE),
    median_entropy_loss_pct = median(entropy_loss_pct, na.rm=TRUE),
    pct_high_loss = 100 * mean(entropy_loss_pct > 10, na.rm=TRUE),
    
    # Distribution similarity
    mean_kl_div = mean(kl_divergence, na.rm=TRUE),
    mean_js_div = mean(js_divergence, na.rm=TRUE),
    mean_wasserstein = mean(wasserstein_dist, na.rm=TRUE),
    pct_similar = 100 * mean(distributions_similar, na.rm=TRUE),
    
    # Moment preservation
    mean_moment_preservation = mean(moment_preservation, na.rm=TRUE)
  )

print(summary)

write.csv(summary, "analyses/results/analysis_3_information_loss_summary.csv", 
          row.names=FALSE)

cat("\n=== INTERPRETATION ===\n\n")

for(i in 1:nrow(summary)) {
  model <- summary$Model[i]
  cat(paste0(model, ":\n"))
  cat(paste0("  Entropy loss: ", round(summary$mean_entropy_loss_pct[i], 2), 
             "% (median: ", round(summary$median_entropy_loss_pct[i], 2), "%)\n"))
  cat(paste0("  KL divergence: ", round(summary$mean_kl_div[i], 4), "\n"))
  cat(paste0("  Distribution similarity: ", round(summary$pct_similar[i], 1), 
             "% of assets\n"))
  cat(paste0("  Moment preservation: ", round(summary$mean_moment_preservation[i], 3), 
             " (1.0 = perfect)\n"))
  
  if(summary$mean_entropy_loss_pct[i] > 10) {
    cat("  [ISSUE] Significant average entropy loss!\n")
  }
  if(summary$mean_kl_div[i] > 0.1) {
    cat("  [ISSUE] Large distributional divergence!\n")
  }
  if(summary$pct_similar[i] < 50) {
    cat("  [ISSUE] Most assets show statistically different distributions!\n")
  }
  cat("\n")
}

# Compare sGARCH_norm vs sGARCH_sstd
if(nrow(summary) == 2) {
  cat("\n=== COMPARISON: norm vs sstd ===\n\n")
  
  norm_idx <- which(summary$Model == "sGARCH_norm")
  sstd_idx <- which(summary$Model == "sGARCH_sstd")
  
  if(length(norm_idx) > 0 && length(sstd_idx) > 0) {
    cat("Information Loss:\n")
    cat(paste0("  norm: ", round(summary$mean_entropy_loss_pct[norm_idx], 2), "%\n"))
    cat(paste0("  sstd: ", round(summary$mean_entropy_loss_pct[sstd_idx], 2), "%\n"))
    
    if(summary$mean_entropy_loss_pct[norm_idx] > summary$mean_entropy_loss_pct[sstd_idx]) {
      cat("  → sGARCH_norm has MORE information loss\n")
    } else {
      cat("  → sGARCH_sstd has MORE information loss\n")
    }
    
    cat("\nDistribution Divergence:\n")
    cat(paste0("  norm KL: ", round(summary$mean_kl_div[norm_idx], 4), "\n"))
    cat(paste0("  sstd KL: ", round(summary$mean_kl_div[sstd_idx], 4), "\n"))
    
    if(summary$mean_kl_div[norm_idx] > summary$mean_kl_div[sstd_idx]) {
      cat("  → sGARCH_norm: NF transforms distribution more drastically\n")
      cat("     This may explain worse forecasting performance!\n")
    }
  }
}

cat("\n[COMPLETE] Analysis 3: Information Loss\n")
cat("Results saved to: analyses/results/analysis_3_*\n\n")
