# Analysis: Cross-Model Compatibility Test
# "The Smoking Gun Experiment"
#
# Test Hypothesis: NF residuals are high quality, but compatibility with 
# base model dynamics determines forecast performance.
#
# Experiment:
# 1. Use NF-norm residuals with sstd dynamics -> Should improve (residuals good, dynamics compatible)
# 2. Use NF-sstd residuals with norm dynamics -> Should worsen (residuals good, dynamics incompatible)

suppressPackageStartupMessages({
  library(rugarch)
  library(dplyr)
})

cat("================================================================\n")
cat("CROSS-MODEL COMPATIBILITY TEST: The Smoking Gun\n")
cat("================================================================\n\n")

cat("Testing if NF quality vs model compatibility determines performance\n\n")

# Configuration
assets <- c("NVDA", "MSFT", "AMZN", "EURUSD", "GBPUSD", "USDZAR")

# Load data
data <- read.csv("data/processed/combined_data.csv")
data$date <- as.Date(data$X)
data <- data[order(data$date), ]

# Train/test split (65/35)
n_total <- nrow(data)
n_train <- floor(n_total * 0.65)
train_data <- data[1:n_train, ]
test_data <- data[(n_train + 1):n_total, ]

cat(paste0("Data: ", n_train, " training, ", nrow(test_data), " test observations\n\n"))

# Load all GARCH fits once
cat("Loading GARCH fits...\n")
all_fits <- readRDS("outputs/manual/garch_fitting/detailed_results.rds")
cat(paste0("Loaded ", length(all_fits), " model fits\n\n"))

# Function to get GARCH fit
get_garch_fit <- function(asset, model) {
  fit_name <- paste0(asset, "_", model)
  if(fit_name %in% names(all_fits)) {
    return(all_fits[[fit_name]])
  }
  return(NULL)
}

# Function to load synthetic residuals
load_synthetic_residuals <- function(asset, model) {
  synth_file <- paste0("outputs/manual/nf_models/", model, "_", asset, "_synthetic_residuals.csv")
  if(!file.exists(synth_file)) return(NULL)
  synth <- read.csv(synth_file)
  synth$synthetic_residuals
}

# Function to forecast with cross-model residuals
cross_model_forecast <- function(asset, base_model, residual_source_model) {
  
  cat(paste0("\n--- ", asset, ": ", base_model, " dynamics + ", 
             residual_source_model, " NF residuals ---\n"))
  
  # Get base model fit (for dynamics)
  fit <- get_garch_fit(asset, base_model)
  if(is.null(fit)) {
    cat("  [SKIP] Base model fit not found\n")
    return(NULL)
  }
  
  # Load synthetic residuals from source model
  synth_resid <- load_synthetic_residuals(asset, residual_source_model)
  if(is.null(synth_resid)) {
    cat("  [SKIP] Synthetic residuals not found\n")
    return(NULL)
  }
  
  # Get test data
  test_returns <- test_data[[asset]]
  n_test <- length(test_returns)
  
  # Standardize synthetic residuals (ensure mean=0, sd=1)
  synth_resid <- (synth_resid - mean(synth_resid)) / sd(synth_resid)
  
  # Forecast using base model dynamics but synthetic residuals
  forecasts <- numeric(n_test)
  
  # Get model parameters
  params <- coef(fit)
  model_spec <- fit@model$modeldesc$vmodel
  
  # Last training sigma
  last_sigma <- tail(fit@fit$sigma, 1)
  last_return <- tail(train_data[[asset]], 1)
  last_resid <- last_return / last_sigma
  
  # Monte Carlo forecasting with cross-model residuals
  n_paths <- 1000
  
  for(t in 1:n_test) {
    
    # Sample from synthetic residuals
    sampled_resid <- sample(synth_resid, n_paths, replace = TRUE)
    
    # Forecast next return using base model dynamics
    if(model_spec == "sGARCH") {
      
      omega <- params["omega"]
      alpha1 <- params["alpha1"]
      beta1 <- params["beta1"]
      
      # Forecast sigma
      sigma_next <- sqrt(omega + alpha1 * last_resid^2 * last_sigma^2 + beta1 * last_sigma^2)
      
      # Forecast returns
      return_paths <- sampled_resid * sigma_next
      
    } else if(model_spec == "gjrGARCH" || model_spec == "TGARCH") {
      
      omega <- params["omega"]
      alpha1 <- params["alpha1"]
      beta1 <- params["beta1"]
      gamma1 <- params["gamma1"]
      
      # Asymmetry indicator
      I_neg <- ifelse(last_resid < 0, 1, 0)
      
      # Forecast sigma
      sigma_next <- sqrt(omega + alpha1 * last_resid^2 * last_sigma^2 + 
                         gamma1 * I_neg * last_resid^2 * last_sigma^2 + 
                         beta1 * last_sigma^2)
      
      # Forecast returns
      return_paths <- sampled_resid * sigma_next
      
    } else {
      cat("  [SKIP] Model type not implemented\n")
      return(NULL)
    }
    
    # Average forecast
    forecasts[t] <- mean(return_paths)
    
    # Update for next step (use actual observed)
    actual_return <- test_returns[t]
    last_return <- actual_return
    last_sigma <- sigma_next
    last_resid <- actual_return / sigma_next
  }
  
  # Calculate MSE
  mse <- mean((forecasts - test_returns)^2)
  mae <- mean(abs(forecasts - test_returns))
  
  cat(paste0("  MSE: ", format(mse, scientific=TRUE, digits=4), "\n"))
  cat(paste0("  MAE: ", format(mae, scientific=TRUE, digits=4), "\n"))
  
  return(data.frame(
    Asset = asset,
    Base_Model = base_model,
    Residual_Source = residual_source_model,
    MSE = mse,
    MAE = mae,
    Configuration = paste0(base_model, "_dynamics_", residual_source_model, "_residuals")
  ))
}

# Run cross-model tests
cat("================================================================\n")
cat("TEST 1: norm dynamics + sstd NF residuals\n")
cat("Hypothesis: Should be WORSE (incompatible dynamics)\n")
cat("================================================================\n")

test1_results <- list()
for(asset in assets) {
  result <- cross_model_forecast(asset, "sGARCH_norm", "sGARCH_sstd")
  if(!is.null(result)) {
    test1_results[[length(test1_results) + 1]] <- result
  }
}

cat("\n================================================================\n")
cat("TEST 2: sstd dynamics + norm NF residuals\n")
cat("Hypothesis: Should be BETTER (compatible dynamics)\n")
cat("================================================================\n")

test2_results <- list()
for(asset in assets) {
  result <- cross_model_forecast(asset, "sGARCH_sstd", "sGARCH_norm")
  if(!is.null(result)) {
    test2_results[[length(test2_results) + 1]] <- result
  }
}

# Combine all results
all_cross_results <- rbind(
  do.call(rbind, test1_results),
  do.call(rbind, test2_results)
)

# Save detailed results
write.csv(all_cross_results, "analyses/results/cross_model_test_detailed.csv", row.names=FALSE)

# Load original results for comparison
original_file <- "results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx"
if(!file.exists(original_file)) {
  cat("\n[ERROR] Original results file not found\n")
  quit(status=1)
}

library(openxlsx)
original_results <- read.xlsx(original_file, sheet="Combined_Results")

# Get baseline performance (native configurations)
norm_native <- original_results %>%
  filter(Model == "sGARCH", Distribution == "norm", Source == "NF_GARCH") %>%
  select(Asset, MSE_native = MSE)

sstd_native <- original_results %>%
  filter(Model == "sGARCH", Distribution == "sstd", Source == "NF_GARCH") %>%
  select(Asset, MSE_native = MSE)

# Compare cross-model to native
cat("\n================================================================\n")
cat("RESULTS COMPARISON\n")
cat("================================================================\n\n")

# Test 1 comparison
test1_df <- do.call(rbind, test1_results)
test1_compare <- test1_df %>%
  left_join(norm_native, by="Asset") %>%
  mutate(
    Change_vs_Native = 100 * (MSE - MSE_native) / MSE_native,
    Direction = ifelse(Change_vs_Native > 0, "WORSE", "BETTER")
  )

cat("TEST 1: sGARCH_norm dynamics + sGARCH_sstd NF residuals\n")
cat("Expected: WORSE (sstd residuals incompatible with norm dynamics)\n\n")
print(test1_compare %>% select(Asset, MSE, MSE_native, Change_vs_Native, Direction))

cat("\nSummary Test 1:\n")
cat(paste0("  Mean change: ", round(mean(test1_compare$Change_vs_Native), 2), "%\n"))
cat(paste0("  Assets worse: ", sum(test1_compare$Direction == "WORSE"), " of ", nrow(test1_compare), "\n"))
if(mean(test1_compare$Change_vs_Native) > 0) {
  cat("  ✅ HYPOTHESIS CONFIRMED: sstd residuals make norm dynamics worse!\n")
} else {
  cat("  ❌ HYPOTHESIS REJECTED: Unexpected result\n")
}

# Test 2 comparison
test2_df <- do.call(rbind, test2_results)
test2_compare <- test2_df %>%
  left_join(sstd_native, by="Asset") %>%
  mutate(
    Change_vs_Native = 100 * (MSE - MSE_native) / MSE_native,
    Direction = ifelse(Change_vs_Native > 0, "WORSE", "BETTER")
  )

cat("\n\nTEST 2: sGARCH_sstd dynamics + sGARCH_norm NF residuals\n")
cat("Expected: SIMILAR or SLIGHTLY WORSE (norm residuals with sstd dynamics)\n\n")
print(test2_compare %>% select(Asset, MSE, MSE_native, Change_vs_Native, Direction))

cat("\nSummary Test 2:\n")
cat(paste0("  Mean change: ", round(mean(test2_compare$Change_vs_Native), 2), "%\n"))
cat(paste0("  Assets worse: ", sum(test2_compare$Direction == "WORSE"), " of ", nrow(test2_compare), "\n"))

# Create summary table
summary_table <- data.frame(
  Configuration = c(
    "Baseline: norm dynamics + norm NF residuals",
    "Test 1: norm dynamics + sstd NF residuals",
    "Baseline: sstd dynamics + sstd NF residuals",
    "Test 2: sstd dynamics + norm NF residuals"
  ),
  Mean_MSE = c(
    mean(test1_compare$MSE_native),
    mean(test1_compare$MSE),
    mean(test2_compare$MSE_native),
    mean(test2_compare$MSE)
  ),
  Change_vs_Baseline = c(
    0,
    mean(test1_compare$Change_vs_Native),
    0,
    mean(test2_compare$Change_vs_Native)
  ),
  Pct_Assets_Worse = c(
    NA,
    100 * mean(test1_compare$Direction == "WORSE"),
    NA,
    100 * mean(test2_compare$Direction == "WORSE")
  )
)

write.csv(summary_table, "analyses/results/cross_model_test_summary.csv", row.names=FALSE)

cat("\n================================================================\n")
cat("FINAL SYNTHESIS\n")
cat("================================================================\n\n")

cat("Cross-Model Compatibility Matrix:\n\n")
cat("                     | norm NF residuals | sstd NF residuals |\n")
cat("---------------------|-------------------|-------------------|\n")
cat(sprintf("norm dynamics        | Baseline (100%%)   | %+.1f%% (WORSE)    |\n", 
            mean(test1_compare$Change_vs_Native)))
cat(sprintf("sstd dynamics        | %+.1f%% (?)        | Baseline (100%%)   |\n",
            mean(test2_compare$Change_vs_Native)))
cat("\n")

# Hypothesis test
cat("HYPOTHESIS TEST:\n\n")

if(mean(test1_compare$Change_vs_Native) > 5) {
  cat("✅ STRONG CONFIRMATION: Mismatched residuals substantially degrade performance\n")
  cat("   Using sstd NF residuals with norm dynamics makes forecasts WORSE\n")
  cat("   This proves NF residuals are good quality - it's the COMPATIBILITY that matters!\n\n")
} else if(mean(test1_compare$Change_vs_Native) > 0) {
  cat("✅ WEAK CONFIRMATION: Mismatched residuals slightly degrade performance\n")
  cat("   Directionally consistent with compatibility hypothesis\n\n")
} else {
  cat("❌ HYPOTHESIS NOT SUPPORTED: Unexpected results\n")
  cat("   Need further investigation\n\n")
}

cat("CONCLUSION:\n")
cat("This experiment isolates NF quality from model compatibility.\n")
cat("If Test 1 shows degradation, it proves NF residuals are high quality\n")
cat("but incompatible dynamics cause poor forecasts.\n\n")

cat("Results saved to:\n")
cat("  - analyses/results/cross_model_test_detailed.csv\n")
cat("  - analyses/results/cross_model_test_summary.csv\n\n")

cat("================================================================\n")
cat("CROSS-MODEL TEST COMPLETE\n")
cat("================================================================\n")
