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

cat("\n[OK] Scenario characterization completed\n")

# =============================================================================
# Model Forecast Evaluation Under Stress
# =============================================================================

cat("\n=== EVALUATING MODEL FORECAST ACCURACY UNDER STRESS ===\n")

# Load required functions
source("scripts/engines/engine_selector.R")
source("scripts/utils/safety_functions.R")

# Load NF residuals for NF-GARCH evaluation
nf_residuals_map <- list()
nf_files <- list.files("outputs/manual/nf_models", pattern = "*_synthetic_residuals.csv", full.names = TRUE)

if (length(nf_files) > 0) {
  for (f in nf_files) {
    fname <- basename(f)
    fname_clean <- stringr::str_replace(fname, "_synthetic_residuals\\.csv$", "")
    parts <- strsplit(fname_clean, "_")[[1]]
    
    if (length(parts) >= 2) {
      model_name <- parts[1]
      asset_name <- parts[2]
      
      tryCatch({
        residuals_data <- read.csv(f)
        residual_values <- if ("residual" %in% names(residuals_data)) {
          residuals_data$residual
        } else if (ncol(residuals_data) > 0) {
          residuals_data[[1]]
        } else {
          next
        }
        
        residual_values <- as.numeric(residual_values)
        residual_values <- residual_values[!is.na(residual_values)]
        if (length(residual_values) > 0) {
          resid_mean <- mean(residual_values, na.rm = TRUE)
          resid_sd <- sd(residual_values, na.rm = TRUE)
          if (is.finite(resid_sd) && resid_sd > 0) {
            residual_values <- (residual_values - resid_mean) / resid_sd
            nf_residuals_map[[paste0(model_name, "_", asset_name)]] <- residual_values
          }
        }
      }, error = function(e) {
        cat("WARNING: Failed to load NF residuals from", fname, ":", e$message, "\n")
      })
    }
  }
}

cat("[OK] Loaded", length(nf_residuals_map), "NF residual files for stress testing\n")

# Model configurations
model_configs <- list(
  sGARCH = list(model = "sGARCH", distribution = "sstd", submodel = NULL),
  eGARCH = list(model = "eGARCH", distribution = "sstd", submodel = NULL),
  TGARCH = list(model = "TGARCH", distribution = "sstd", submodel = NULL),
  gjrGARCH = list(model = "gjrGARCH", distribution = "sstd", submodel = NULL)
)

# Evaluate forecast accuracy during stress periods
stress_forecast_results <- list()

for (asset_name in names(returns_data)) {
  cat("\nEvaluating forecast accuracy for:", asset_name, "\n")
  
  returns <- returns_data[[asset_name]]
  returns_vec <- as.numeric(returns)
  
  # Historical crisis evaluation
  for (crisis_name in names(crisis_periods)) {
    crisis <- crisis_periods[[crisis_name]]
    
    # Extract pre-crisis (training) and crisis (testing) periods
    crisis_start_idx <- which(index(returns) >= crisis$start)[1]
    if (is.na(crisis_start_idx) || crisis_start_idx < 500) next  # Need enough pre-crisis data
    
    # Pre-crisis training data (before crisis starts)
    train_returns <- returns_vec[1:(crisis_start_idx - 1)]
    
    # Crisis period test data
    crisis_returns <- returns[index(returns) >= crisis$start & index(returns) <= crisis$end]
    test_returns <- as.numeric(crisis_returns)
    
    if (length(test_returns) < 10 || length(train_returns) < 500) next
    
    # Evaluate each model
    for (model_name in names(model_configs)) {
      cfg <- model_configs[[model_name]]
      
      tryCatch({
        # Fit model on pre-crisis data
        fit <- engine_fit(
          model = cfg$model,
          returns = train_returns,
          dist = cfg$distribution,
          submodel = cfg$submodel,
          engine = "manual"
        )
        
        if (!engine_converged(fit)) next
        
        # Forecast during crisis period using NF residuals if available
        nf_key <- paste0(cfg$model, "_", asset_name)
        nf_resid <- if (nf_key %in% names(nf_residuals_map)) {
          nf_residuals_map[[nf_key]]
        } else {
          NULL
        }
        
        # Generate forecasts
        if (!is.null(nf_resid) && length(nf_resid) >= length(test_returns)) {
          # NF-GARCH forecast
          sim_result <- engine_path(
            fit,
            head(nf_resid, length(test_returns)),
            length(test_returns),
            cfg$model,
            cfg$submodel,
            engine = "manual"
          )
          nf_forecast <- sim_result$returns
          
          # Calculate NF-GARCH forecast accuracy
          nf_mse <- mean((test_returns - nf_forecast)^2, na.rm = TRUE)
          nf_mae <- mean(abs(test_returns - nf_forecast), na.rm = TRUE)
        } else {
          nf_forecast <- NULL
          nf_mse <- NA
          nf_mae <- NA
        }
        
        # Standard GARCH forecast (use fitted residuals)
        standard_resid <- engine_residuals(fit, standardize = TRUE)
        if (length(standard_resid) >= length(test_returns)) {
          sim_result_std <- engine_path(
            fit,
            head(standard_resid, length(test_returns)),
            length(test_returns),
            cfg$model,
            cfg$submodel,
            engine = "manual"
          )
          std_forecast <- sim_result_std$returns
          
          std_mse <- mean((test_returns - std_forecast)^2, na.rm = TRUE)
          std_mae <- mean(abs(test_returns - std_forecast), na.rm = TRUE)
        } else {
          std_mse <- NA
          std_mae <- NA
        }
        
        stress_forecast_results[[length(stress_forecast_results) + 1]] <- data.frame(
          Asset = asset_name,
          Scenario_Type = "Historical_Crisis",
          Scenario_Name = crisis_name,
          Model = cfg$model,
          NF_GARCH_MSE = nf_mse,
          NF_GARCH_MAE = nf_mae,
          Standard_GARCH_MSE = std_mse,
          Standard_GARCH_MAE = std_mae,
          MSE_Improvement_Pct = ifelse(!is.na(std_mse) && !is.na(nf_mse) && std_mse > 0, 
                                       (std_mse - nf_mse) / std_mse * 100, NA),
          N_Test_Periods = length(test_returns)
        )
        
      }, error = function(e) {
        cat("  WARNING: Failed to evaluate", model_name, "for", asset_name, "-", crisis_name, ":", e$message, "\n")
      })
    }
  }
}

# Combine forecast evaluation results
if (length(stress_forecast_results) > 0) {
  stress_forecast_df <- bind_rows(stress_forecast_results)
  cat("\n[OK] Forecast evaluation completed:", nrow(stress_forecast_df), "results\n")
} else {
  stress_forecast_df <- data.frame()
  cat("\n[WARNING] No forecast evaluation results generated\n")
}

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

# Add forecast evaluation under stress
if (nrow(stress_forecast_df) > 0) {
  addWorksheet(wb, "Forecast_Under_Stress")
  writeData(wb, "Forecast_Under_Stress", stress_forecast_df)
  
  # Summary of forecast performance under stress
  forecast_summary <- stress_forecast_df %>%
    group_by(Scenario_Name, Model) %>%
    summarise(
      n_assets = n(),
      mean_NF_GARCH_MSE = mean(NF_GARCH_MSE, na.rm = TRUE),
      mean_Standard_GARCH_MSE = mean(Standard_GARCH_MSE, na.rm = TRUE),
      mean_MSE_Improvement_Pct = mean(MSE_Improvement_Pct, na.rm = TRUE),
      .groups = "drop"
    )
  
  addWorksheet(wb, "Forecast_Summary")
  writeData(wb, "Forecast_Summary", forecast_summary)
}

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("[OK] Results saved to:", output_file, "\n")
cat("\n=== STRESS TESTING COMPLETE ===\n")

