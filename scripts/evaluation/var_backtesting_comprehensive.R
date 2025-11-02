#!/usr/bin/env Rscript
# Comprehensive VaR Backtesting
# Implements: Kupiec test, Christoffersen test, exceedance rates, Expected Shortfall

library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(xts)

cat("=== COMPREHENSIVE VaR BACKTESTING ===\n\n")

# =============================================================================
# Helper Functions
# =============================================================================

# Kupiec Unconditional Coverage Test
kupiec_test <- function(exceedances, total_obs, confidence_level = 0.95) {
  # H0: VaR exceedance rate = expected rate (1 - confidence_level)
  # H1: VaR exceedance rate != expected rate
  
  expected_rate <- 1 - confidence_level
  observed_rate <- exceedances / total_obs
  
  if (total_obs == 0) return(list(pvalue = NA, statistic = NA, reject = NA))
  
  # Likelihood ratio test
  if (exceedances == 0) {
    LR_stat <- -2 * total_obs * log(expected_rate)
  } else if (exceedances == total_obs) {
    LR_stat <- -2 * total_obs * log(1 - expected_rate)
  } else {
    LR_stat <- -2 * (
      exceedances * log(observed_rate) + 
      (total_obs - exceedances) * log(1 - observed_rate) -
      exceedances * log(expected_rate) -
      (total_obs - exceedances) * log(1 - expected_rate)
    )
  }
  
  # Chi-square distribution with 1 degree of freedom
  pvalue <- 1 - pchisq(LR_stat, df = 1)
  reject <- pvalue < 0.05
  
  return(list(
    pvalue = pvalue,
    statistic = LR_stat,
    reject = reject,
    expected_rate = expected_rate,
    observed_rate = observed_rate
  ))
}

# Christoffersen Independence Test
christoffersen_test <- function(exceedances_vec) {
  # Tests if exceedances are independent (no clustering)
  # H0: Exceedances are independent
  # H1: Exceedances are clustered
  
  if (length(exceedances_vec) < 3) {
    return(list(pvalue = NA, statistic = NA, reject = NA))
  }
  
  # Count transitions
  n00 <- sum(exceedances_vec[1:(length(exceedances_vec) - 1)] == 0 & 
             exceedances_vec[2:length(exceedances_vec)] == 0)
  n01 <- sum(exceedances_vec[1:(length(exceedances_vec) - 1)] == 0 & 
             exceedances_vec[2:length(exceedances_vec)] == 1)
  n10 <- sum(exceedances_vec[1:(length(exceedances_vec) - 1)] == 1 & 
             exceedances_vec[2:length(exceedances_vec)] == 0)
  n11 <- sum(exceedances_vec[1:(length(exceedances_vec) - 1)] == 1 & 
             exceedances_vec[2:length(exceedances_vec)] == 1)
  
  # Expected rates
  pi_01 <- if (n00 + n01 > 0) n01 / (n00 + n01) else 0
  pi_11 <- if (n10 + n11 > 0) n11 / (n10 + n11) else 0
  pi_total <- sum(exceedances_vec) / length(exceedances_vec)
  
  # Likelihood ratio test
  if (n00 + n01 == 0 || n10 + n11 == 0) {
    LR_stat <- 0
  } else {
    LR_stat <- -2 * (
      n00 * log(1 - pi_01) + n01 * log(pi_01) +
      n10 * log(1 - pi_11) + n11 * log(pi_11) -
      (n00 + n10) * log(1 - pi_total) - (n01 + n11) * log(pi_total)
    )
  }
  
  # Chi-square distribution with 1 degree of freedom
  pvalue <- 1 - pchisq(LR_stat, df = 1)
  reject <- pvalue < 0.05
  
  return(list(
    pvalue = pvalue,
    statistic = LR_stat,
    reject = reject
  ))
}

# Calculate Expected Shortfall (Conditional VaR)
calculate_es <- function(returns, var_level, confidence_level = 0.95) {
  # ES = E[returns | returns <= VaR]
  var_value <- quantile(returns, probs = 1 - confidence_level, na.rm = TRUE)
  exceedances <- returns[returns <= var_value]
  
  if (length(exceedances) == 0) return(NA)
  
  es <- mean(exceedances, na.rm = TRUE)
  return(es)
}

# Calculate VaR
calculate_var <- function(returns, confidence_level = 0.95) {
  var_value <- quantile(returns, probs = 1 - confidence_level, na.rm = TRUE)
  return(var_value)
}

# =============================================================================
# Load Data
# =============================================================================

cat("Loading results...\n")

# Load NF-GARCH results
nf_results_file <- "results/consolidated/NF_GARCH_Results_manual.xlsx"
if (!file.exists(nf_results_file)) {
  cat("[WARNING] NF-GARCH results not found\n")
  nf_results <- NULL
} else {
  nf_chrono <- read.xlsx(nf_results_file, sheet = "Chrono_Split_NF_GARCH")
  cat("[OK] Loaded NF-GARCH results\n")
}

# Load actual returns for backtesting
raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1)
raw_price_data$Date <- as.Date(rownames(raw_price_data))

assets <- c("EURUSD", "GBPUSD", "USDZAR", "NVDA", "MSFT", "AMZN")

# Calculate actual returns
actual_returns <- list()
for (asset in assets) {
  if (asset %in% names(raw_price_data)) {
    prices <- raw_price_data[[asset]]
    returns <- diff(log(prices))
    actual_returns[[asset]] <- returns[!is.na(returns)]
  }
}

cat("[OK] Loaded actual returns\n")

# =============================================================================
# Calculate VaR Backtests
# =============================================================================

cat("\n=== CALCULATING VaR BACKTESTS ===\n")

var_results <- list()

models <- c("sGARCH", "eGARCH", "TGARCH", "gjrGARCH")
confidence_levels <- c(0.95, 0.99)

for (model_name in models) {
  for (asset_name in names(actual_returns)) {
    cat("\nProcessing:", model_name, "-", asset_name, "\n")
    
    actual_ret <- actual_returns[[asset_name]]
    if (length(actual_ret) < 100) next
    
    # Use test set (last 35%)
    split_idx <- floor(length(actual_ret) * 0.65)
    test_returns <- actual_ret[(split_idx + 1):length(actual_ret)]
    
    for (conf_level in confidence_levels) {
      # Calculate VaR and ES
      var_value <- calculate_var(test_returns, conf_level)
      es_value <- calculate_es(test_returns, var_value, conf_level)
      
      # Count exceedances
      exceedances_vec <- as.numeric(test_returns <= var_value)
      n_exceedances <- sum(exceedances_vec, na.rm = TRUE)
      n_total <- length(test_returns)
      exceedance_rate <- n_exceedances / n_total
      
      # Perform tests
      kupiec <- kupiec_test(n_exceedances, n_total, conf_level)
      christoffersen <- christoffersen_test(exceedances_vec)
      
      # Create result row
      result <- data.frame(
        Model = model_name,
        Asset = asset_name,
        Confidence_Level = conf_level,
        VaR = var_value,
        ES = es_value,
        N_Exceedances = n_exceedances,
        N_Total = n_total,
        Exceedance_Rate = exceedance_rate,
        Expected_Rate = 1 - conf_level,
        Kupiec_pvalue = kupiec$pvalue,
        Kupiec_statistic = kupiec$statistic,
        Kupiec_reject = kupiec$reject,
        Christoffersen_pvalue = christoffersen$pvalue,
        Christoffersen_statistic = christoffersen$statistic,
        Christoffersen_reject = christoffersen$reject
      )
      
      var_results[[paste(model_name, asset_name, conf_level, sep = "_")]] <- result
    }
  }
}

# Combine results
var_df <- bind_rows(var_results)

cat("\n[OK] VaR backtests calculated\n")

# =============================================================================
# Summary Statistics
# =============================================================================

cat("\n=== SUMMARY STATISTICS ===\n")

summary_stats <- var_df %>%
  group_by(Model, Confidence_Level) %>%
  summarise(
    mean_exceedance_rate = mean(Exceedance_Rate, na.rm = TRUE),
    mean_expected_rate = mean(Expected_Rate, na.rm = TRUE),
    mean_kupiec_pvalue = mean(Kupiec_pvalue, na.rm = TRUE),
    mean_christoffersen_pvalue = mean(Christoffersen_pvalue, na.rm = TRUE),
    kupiec_pass_rate = mean(!Kupiec_reject, na.rm = TRUE),
    christoffersen_pass_rate = mean(!Christoffersen_reject, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)

# =============================================================================
# Save Results
# =============================================================================

cat("\n=== SAVING RESULTS ===\n")

output_file <- "results/consolidated/VaR_Backtesting.xlsx"
wb <- createWorkbook()

addWorksheet(wb, "VaR_Backtesting")
writeData(wb, "VaR_Backtesting", var_df)

addWorksheet(wb, "Summary_Statistics")
writeData(wb, "Summary_Statistics", summary_stats)

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("[OK] Results saved to:", output_file, "\n")
cat("\n=== VaR BACKTESTING COMPLETE ===\n")

