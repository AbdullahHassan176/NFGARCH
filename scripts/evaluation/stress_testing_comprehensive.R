#!/usr/bin/env Rscript
# Comprehensive Stress Testing
# Historical crises and hypothetical shocks

library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(xts)

cat("=== COMPREHENSIVE STRESS TESTING ===\n\n")

# =============================================================================
# Load Data
# =============================================================================

cat("Loading data...\n")

raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1)
raw_price_data$Date <- as.Date(rownames(raw_price_data))

assets <- c("EURUSD", "GBPUSD", "USDZAR", "NVDA", "MSFT", "AMZN")

# Calculate returns
returns_data <- list()
for (asset in assets) {
  if (asset %in% names(raw_price_data)) {
    prices <- raw_price_data[[asset]]
    returns <- diff(log(prices))
    returns_data[[asset]] <- xts(returns[!is.na(returns)], order.by = raw_price_data$Date[2:length(raw_price_data$Date)][!is.na(returns)])
  }
}

cat("[OK] Loaded", length(returns_data), "assets\n")

# =============================================================================
# Historical Crisis Periods
# =============================================================================

cat("\n=== HISTORICAL CRISIS PERIODS ===\n")

# Define crisis periods
crisis_periods <- list(
  GFC_2008 = list(
    start = as.Date("2008-09-01"),
    end = as.Date("2009-03-31"),
    description = "2008 Global Financial Crisis"
  ),
  COVID_2020 = list(
    start = as.Date("2020-02-01"),
    end = as.Date("2020-04-30"),
    description = "COVID-19 Market Crash"
  )
)

# =============================================================================
# Hypothetical Shocks
# =============================================================================

cat("\n=== HYPOTHETICAL SHOCKS ===\n")

# Apply hypothetical shocks
apply_shock <- function(returns, shock_type, shock_magnitude) {
  n <- length(returns)
  
  if (shock_type == "price_drop") {
    # Sudden price drop
    shock_idx <- floor(n * 0.7)  # Apply at 70% of series
    shock_returns <- returns
    shock_returns[shock_idx] <- shock_returns[shock_idx] - shock_magnitude
    return(shock_returns)
  } else if (shock_type == "volatility_spike") {
    # Volatility spike
    shock_idx <- floor(n * 0.7)
    shock_returns <- returns
    shock_returns[shock_idx] <- shock_returns[shock_idx] * shock_magnitude
    return(shock_returns)
  } else if (shock_type == "mean_shift") {
    # Mean shift (prolonged)
    shock_idx_start <- floor(n * 0.6)
    shock_idx_end <- floor(n * 0.8)
    shock_returns <- returns
    shock_returns[shock_idx_start:shock_idx_end] <- shock_returns[shock_idx_start:shock_idx_end] - shock_magnitude
    return(shock_returns)
  }
  
  return(returns)
}

# =============================================================================
# Stress Test Analysis
# =============================================================================

cat("\n=== RUNNING STRESS TESTS ===\n")

stress_results <- list()

models <- c("sGARCH", "eGARCH", "TGARCH", "gjrGARCH")

for (asset_name in names(returns_data)) {
  cat("\nProcessing:", asset_name, "\n")
  
  returns <- returns_data[[asset_name]]
  
  # Historical crisis analysis
  for (crisis_name in names(crisis_periods)) {
    crisis <- crisis_periods[[crisis_name]]
    
    # Extract crisis period
    crisis_returns <- returns[index(returns) >= crisis$start & index(returns) <= crisis$end]
    
    if (length(crisis_returns) > 10) {
      # Calculate crisis metrics
      crisis_volatility <- sd(crisis_returns, na.rm = TRUE)
      crisis_mean <- mean(crisis_returns, na.rm = TRUE)
      crisis_max_drawdown <- min(cumsum(crisis_returns), na.rm = TRUE)
      
      stress_results[[paste(asset_name, crisis_name, sep = "_")]] <- data.frame(
        Asset = asset_name,
        Scenario_Type = "Historical_Crisis",
        Scenario_Name = crisis_name,
        Description = crisis$description,
        N_Observations = length(crisis_returns),
        Mean_Return = crisis_mean,
        Volatility = crisis_volatility,
        Max_Drawdown = crisis_max_drawdown,
        Start_Date = crisis$start,
        End_Date = crisis$end
      )
    }
  }
  
  # Hypothetical shocks
  hypothetical_shocks <- list(
    price_drop_30 = list(type = "price_drop", magnitude = 0.30, description = "30% price drop"),
    price_drop_50 = list(type = "price_drop", magnitude = 0.50, description = "50% price drop"),
    volatility_spike_2x = list(type = "volatility_spike", magnitude = 2.0, description = "2x volatility spike"),
    volatility_spike_3x = list(type = "volatility_spike", magnitude = 3.0, description = "3x volatility spike"),
    mean_shift_down = list(type = "mean_shift", magnitude = 0.01, description = "Negative mean shift")
  )
  
  for (shock_name in names(hypothetical_shocks)) {
    shock <- hypothetical_shocks[[shock_name]]
    
    # Apply shock
    shocked_returns <- apply_shock(as.numeric(returns), shock$type, shock$magnitude)
    
    if (length(shocked_returns) > 10) {
      # Calculate shocked metrics
      shock_volatility <- sd(shocked_returns, na.rm = TRUE)
      shock_mean <- mean(shocked_returns, na.rm = TRUE)
      shock_max_drawdown <- min(cumsum(shocked_returns), na.rm = TRUE)
      
      # Compare to baseline
      baseline_volatility <- sd(as.numeric(returns), na.rm = TRUE)
      baseline_mean <- mean(as.numeric(returns), na.rm = TRUE)
      
      volatility_increase <- (shock_volatility - baseline_volatility) / baseline_volatility * 100
      
      stress_results[[paste(asset_name, shock_name, sep = "_")]] <- data.frame(
        Asset = asset_name,
        Scenario_Type = "Hypothetical_Shock",
        Scenario_Name = shock_name,
        Description = shock$description,
        N_Observations = length(shocked_returns),
        Mean_Return = shock_mean,
        Volatility = shock_volatility,
        Max_Drawdown = shock_max_drawdown,
        Baseline_Volatility = baseline_volatility,
        Volatility_Increase_Pct = volatility_increase,
        Start_Date = NA,
        End_Date = NA
      )
    }
  }
}

# Combine results
stress_df <- bind_rows(stress_results)

cat("\n[OK] Stress tests completed\n")

# =============================================================================
# Summary Statistics
# =============================================================================

cat("\n=== SUMMARY STATISTICS ===\n")

summary_stats <- stress_df %>%
  group_by(Scenario_Type, Scenario_Name) %>%
  summarise(
    n_assets = n(),
    mean_volatility = mean(Volatility, na.rm = TRUE),
    mean_max_drawdown = mean(Max_Drawdown, na.rm = TRUE),
    mean_volatility_increase = mean(Volatility_Increase_Pct, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)

# =============================================================================
# Save Results
# =============================================================================

cat("\n=== SAVING RESULTS ===\n")

output_file <- "results/consolidated/Stress_Testing.xlsx"
wb <- createWorkbook()

addWorksheet(wb, "Stress_Test_Results")
writeData(wb, "Stress_Test_Results", stress_df)

addWorksheet(wb, "Summary_Statistics")
writeData(wb, "Summary_Statistics", summary_stats)

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("[OK] Results saved to:", output_file, "\n")
cat("\n=== STRESS TESTING COMPLETE ===\n")

