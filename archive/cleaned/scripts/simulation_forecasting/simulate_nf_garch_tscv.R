#!/usr/bin/env Rscript
# NF-GARCH Simulation with Time Series Cross-Validation
# This script implements NF-GARCH models with proper TS CV for robust evaluation
# Supports both rugarch and manual engine implementations

# Load required libraries
library(rugarch)
library(xts)
library(zoo)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(openxlsx)

# Set up error handling
options(warn = 1)
options(error = function() {
  cat("ERROR: NF-GARCH TS CV simulation failed\n")
  traceback()
  quit(status = 1)
})

# Load configuration and engine selection utilities
tryCatch({
  source("scripts/utils/cli_parser.R")
  source("scripts/engines/engine_selector.R")
  source("scripts/utils/safety_functions.R")
}, error = function(e) {
  cat("ERROR: Failed to load utility scripts:", e$message, "\n")
  quit(status = 1)
})

# Display current configuration and engine selection
print_config()
engine <- get_engine()
cat("Using engine:", engine, "\n\n")

cat("Starting NF-GARCH simulation with TS CV using engine:", engine, "...\n")
set.seed(123)  # Ensure reproducibility

# Initialize pipeline
tryCatch({
  source("scripts/utils/conflict_resolution.R")
  initialize_pipeline()
}, error = function(e) {
  cat("WARNING: Pipeline initialization failed:", e$message, "\n")
})

# Data Import and Preprocessing
cat("Loading and preprocessing data...\n")

tryCatch({
  # Load data
  if (!file.exists("./data/processed/raw (FX + EQ).csv")) {
    stop("Data file not found: ./data/processed/raw (FX + EQ).csv")
  }
  
  raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1, stringsAsFactors = FALSE)
  raw_price_data$Date <- as.Date(rownames(raw_price_data))
  rownames(raw_price_data) <- NULL
  raw_price_data <- raw_price_data %>% dplyr::select(Date, everything())
  
  cat("OK: Data loaded successfully\n")
  cat("   Rows:", nrow(raw_price_data), "\n")
  cat("   Columns:", ncol(raw_price_data), "\n")
  
}, error = function(e) {
  cat("ERROR: Data loading failed:", e$message, "\n")
  quit(status = 1)
})

# Extract time index and price matrix for processing
date_index <- raw_price_data$Date
price_data_matrix <- raw_price_data[, !(names(raw_price_data) %in% "Date")]

# Define asset tickers for equity and foreign exchange instruments
equity_tickers <- c("NVDA", "MSFT", "PG", "CAT", "WMT", "AMZN")
fx_names <- c("EURUSD", "GBPUSD", "GBPCNY", "USDZAR", "GBPZAR", "EURZAR")

# Convert price series to XTS objects for time series analysis
equity_xts <- lapply(equity_tickers, function(ticker) {
  if (ticker %in% names(price_data_matrix)) {
    xts(price_data_matrix[[ticker]], order.by = date_index)
  } else {
    cat("WARNING: Asset", ticker, "not found in data\n")
    NULL
  }
})
names(equity_xts) <- equity_tickers
equity_xts <- equity_xts[!sapply(equity_xts, is.null)]

fx_xts <- lapply(fx_names, function(ticker) {
  if (ticker %in% names(price_data_matrix)) {
    xts(price_data_matrix[[ticker]], order.by = date_index)
  } else {
    cat("WARNING: Asset", ticker, "not found in data\n")
    NULL
  }
})
names(fx_xts) <- fx_names
fx_xts <- fx_xts[!sapply(fx_xts, is.null)]

cat("OK: Asset data prepared\n")
cat("   Equity assets:", length(equity_xts), "\n")
cat("   FX assets:", length(fx_xts), "\n")

# Calculate log returns for volatility modeling
CalculateReturns <- function(x) {
  if (inherits(x, "xts")) {
    diff(log(x))
  } else {
    diff(log(as.numeric(x)))
  }
}

equity_returns <- lapply(equity_xts, function(x) CalculateReturns(x)[-1, ])
fx_returns     <- lapply(fx_xts,     function(x) diff(log(x))[-1, ])

# Model Configuration
cat("Setting up model configurations...\n")

model_configs <- list(
  sGARCH_norm  = list(model = "sGARCH", distribution = "norm", submodel = NULL),
  sGARCH_sstd  = list(model = "sGARCH", distribution = "sstd", submodel = NULL),
  gjrGARCH     = list(model = "gjrGARCH", distribution = "sstd", submodel = NULL),
  eGARCH       = list(model = "eGARCH", distribution = "sstd", submodel = NULL),
  TGARCH       = list(model = "NF_tGARCH", distribution = "sstd", submodel = "TGARCH")
)

# Time Series Cross-Validation Implementation
cat("Implementing Time Series Cross-Validation...\n")

# TS CV function for NF-GARCH
ts_cross_validate_nfgarch <- function(returns, model_type, dist_type = "sstd", submodel = NULL, 
                                     nf_residuals, window_size = 500, step_size = 50, forecast_horizon = 20) {
  # Perform sliding window time-series cross-validation for NF-GARCH
  # This approach respects temporal ordering and provides robust performance estimates
  
  n <- nrow(returns)
  results <- list()
  
  for (start_idx in seq(1, n - window_size - forecast_horizon, by = step_size)) {
    # Define training and testing windows
    train_set <- returns[start_idx:(start_idx + window_size - 1)]
    test_set  <- returns[(start_idx + window_size):(start_idx + window_size + forecast_horizon - 1)]
    
    # Print progress information for monitoring
    message("Processing NF-GARCH window: ", start_idx, 
            " | Train size: ", nrow(train_set), 
            " | Test size: ", nrow(test_set),
            " | Train SD: ", round(sd(train_set, na.rm = TRUE), 6))
    
    # Fit GARCH model with error handling
    fit <- tryCatch({
      engine_fit(
        model = model_type, 
        returns = train_set, 
        dist = dist_type, 
        submodel = submodel, 
        engine = engine
      )
    }, error = function(e) {
      message("NF-GARCH fit error at index ", start_idx, ": ", e$message)
      return(NULL)
    })
    
    if (!is.null(fit) && engine_converged(fit)) {
      # Setup NF-GARCH simulation
      n_sim <- min(length(nf_residuals), length(test_set))
      if (n_sim < length(test_set)) {
        message("WARNING: NF residuals too short for window ", start_idx)
        next
      }
      
      # Use engine_path for NF-GARCH simulation
      sim_result <- tryCatch({
        engine_path(
          fit, 
          head(nf_residuals, n_sim), 
          n_sim, 
          model_type, 
          submodel, 
          engine
        )
      }, error = function(e) {
        message("NF-GARCH simulation error at index ", start_idx, ": ", e$message)
        return(NULL)
      })
      
      if (!is.null(sim_result)) {
        sim_returns <- sim_result$returns
        
        # Calculate performance metrics
        mse <- mean((test_set - sim_returns)^2, na.rm = TRUE)
        mae <- mean(abs(test_set - sim_returns), na.rm = TRUE)
        
        # Get model information criteria
        ic <- engine_infocriteria(fit)
        
        # Store results
        results[[length(results) + 1]] <- data.frame(
          WindowStart = start_idx,
          WindowEnd = start_idx + window_size - 1,
          TestStart = start_idx + window_size,
          TestEnd = start_idx + window_size + forecast_horizon - 1,
          Model = model_type,
          Distribution = dist_type,
          AIC = ic["AIC"],
          BIC = ic["BIC"],
          LogLikelihood = ic["LogLikelihood"],
          MSE = mse,
          MAE = mae,
          SplitType = "TS_CV"
        )
      }
    }
  }
  
  if (length(results) == 0) return(NULL)
  do.call(rbind, results)
}

# Load NF Residuals
cat("Loading NF residuals...\n")

tryCatch({
  nf_files <- list.files("nf_generated_residuals", pattern = "*.csv", full.names = TRUE)
  
  if (length(nf_files) == 0) {
    cat("WARNING: No NF residual files found\n")
    cat("Generating dummy residuals for testing...\n")
    
    # Generate dummy residuals for testing
    nf_residuals_map <- list()
    for (config_name in names(model_configs)) {
      for (asset in names(equity_returns)) {
        key <- paste0(config_name, "_equity_", asset, "_residuals_synthetic")
        nf_residuals_map[[key]] <- rnorm(1000, 0, 1)
      }
      for (asset in names(fx_returns)) {
        key <- paste0(config_name, "_fx_", asset, "_residuals_synthetic")
        nf_residuals_map[[key]] <- rnorm(1000, 0, 1)
      }
    }
  } else {
    # Parse model and asset from file names
    nf_residuals_map <- list()
    for (f in nf_files) {
      fname <- basename(f)
      key <- stringr::str_replace(fname, "\\.csv$", "")
      
      tryCatch({
        residuals_data <- read.csv(f)
        
        if ("residual" %in% names(residuals_data)) {
          nf_residuals_map[[key]] <- residuals_data$residual
        } else {
          nf_residuals_map[[key]] <- residuals_data[[1]]
        }
      }, error = function(e) {
        cat("WARNING: Failed to load NF residuals from", fname, ":", e$message, "\n")
      })
    }
  }
  
  cat("OK: Loaded", length(nf_residuals_map), "NF residual files\n")
  
}, error = function(e) {
  cat("ERROR: Failed to load NF residuals:", e$message, "\n")
  quit(status = 1)
})

# Helper to run all NF-GARCH CV models
run_all_nfgarch_cv_models <- function(returns_list, model_configs, nf_residuals_map, 
                                     window_size = 500, forecast_horizon = 40) {
  cv_results_all <- list()
  
  for (model_name in names(model_configs)) {
    cfg <- model_configs[[model_name]]
    message("Running NF-GARCH CV for model: ", model_name)
    
    result <- lapply(names(returns_list), function(asset_name) {
      # Find corresponding NF residuals
      possible_keys <- c(
        paste0(model_name, "_", asset_name, "_residuals_synthetic"),
        paste0(model_name, "_fx_", asset_name, "_residuals_synthetic"),
        paste0(model_name, "_equity_", asset_name, "_residuals_synthetic")
      )
      
      key <- NULL
      for (k in possible_keys) {
        if (k %in% names(nf_residuals_map)) {
          key <- k
          break
        }
      }
      
      if (is.null(key)) {
        message("ERROR: No NF residuals found for ", asset_name, " - ", model_name)
        return(NULL)
      }
      
      ts_cross_validate_nfgarch(
        returns_list[[asset_name]], 
        model_type = cfg$model, 
        dist_type = cfg$distribution, 
        submodel = cfg$submodel,
        nf_residuals = nf_residuals_map[[key]],
        window_size = window_size,
        forecast_horizon = forecast_horizon
      )
    })
    
    # Label by asset names
    names(result) <- names(returns_list)
    
    # Remove nulls
    result <- result[!sapply(result, is.null)]
    
    message("OK: NF-GARCH CV fits found for assets: ", paste(names(result), collapse = ", "))
    
    cv_results_all[[model_name]] <- result
  }
  
  return(cv_results_all)
}

# Check and ensure sufficient size and variability across each window
valid_fx_returns <- fx_returns[sapply(fx_returns, function(x) nrow(x) > 520 && sd(x, na.rm = TRUE) > 0)]
valid_equity_returns <- equity_returns[sapply(equity_returns, function(x) nrow(x) > 520 && sd(x, na.rm = TRUE) > 0)]

# Run NF-GARCH TS CV Analysis
cat("=== RUNNING NF-GARCH TIME SERIES CROSS-VALIDATION ===\n")

# FX NF-GARCH TS CV
cat("Processing FX assets with NF-GARCH TS CV...\n")
Fitted_FX_NFGARCH_TS_CV_models <- run_all_nfgarch_cv_models(
  valid_fx_returns, model_configs, nf_residuals_map
)

# Equity NF-GARCH TS CV
cat("Processing Equity assets with NF-GARCH TS CV...\n")
Fitted_EQ_NFGARCH_TS_CV_models <- run_all_nfgarch_cv_models(
  valid_equity_returns, model_configs, nf_residuals_map
)

# Flatten all NF-GARCH CV results into one data frame
Fitted_NFGARCH_TS_CV_models <- data.frame()

for (model_name in names(Fitted_FX_NFGARCH_TS_CV_models)) {
  fx_results <- tryCatch({
    do.call(rbind, Fitted_FX_NFGARCH_TS_CV_models[[model_name]])
  }, error = function(e) {
    message("WARNING: FX NF-GARCH CV results failed for: ", model_name, " - ", e$message)
    return(NULL)
  })
  
  eq_results <- tryCatch({
    do.call(rbind, Fitted_EQ_NFGARCH_TS_CV_models[[model_name]])
  }, error = function(e) {
    message("WARNING: EQ NF-GARCH CV results failed for: ", model_name, " - ", e$message)
    return(NULL)
  })
  
  if (!is.null(fx_results)) {
    fx_results$Asset <- names(Fitted_FX_NFGARCH_TS_CV_models[[model_name]])
    fx_results$AssetType <- "FX"
    Fitted_NFGARCH_TS_CV_models <- bind_rows(Fitted_NFGARCH_TS_CV_models, fx_results)
  }
  
  if (!is.null(eq_results)) {
    eq_results$Asset <- names(Fitted_EQ_NFGARCH_TS_CV_models[[model_name]])
    eq_results$AssetType <- "Equity"
    Fitted_NFGARCH_TS_CV_models <- bind_rows(Fitted_NFGARCH_TS_CV_models, eq_results)
  }
}

# Generate summary statistics
if (nrow(Fitted_NFGARCH_TS_CV_models) > 0) {
  nfgarch_cv_summary <- Fitted_NFGARCH_TS_CV_models %>%
    group_by(Model, Distribution, AssetType) %>%
    summarise(
      Avg_AIC = mean(AIC, na.rm = TRUE),
      Avg_BIC = mean(BIC, na.rm = TRUE),
      Avg_LogLik = mean(LogLikelihood, na.rm = TRUE),
      Avg_MSE = mean(MSE, na.rm = TRUE),
      Avg_MAE = mean(MAE, na.rm = TRUE),
      Windows_Processed = n(),
      .groups = 'drop'
    ) %>%
    arrange(Avg_MSE)
  
  # Save results
  output_file <- paste0("NF_GARCH_TS_CV_Results_", engine, ".xlsx")
  
  tryCatch({
    wb <- createWorkbook()
    
    # Add NF-GARCH TS CV results
    addWorksheet(wb, "NF_GARCH_TS_CV_Results")
    writeData(wb, "NF_GARCH_TS_CV_Results", Fitted_NFGARCH_TS_CV_models)
    
    # Add summary statistics
    addWorksheet(wb, "NF_GARCH_TS_CV_Summary")
    writeData(wb, "NF_GARCH_TS_CV_Summary", nfgarch_cv_summary)
    
    # Save workbook
    saveWorkbook(wb, output_file, overwrite = TRUE)
    
    cat("OK: NF-GARCH TS CV results saved to:", output_file, "\n")
    cat("   Total windows processed:", nrow(Fitted_NFGARCH_TS_CV_models), "\n")
    cat("   Models evaluated:", length(unique(Fitted_NFGARCH_TS_CV_models$Model)), "\n")
    cat("   Assets processed:", length(unique(Fitted_NFGARCH_TS_CV_models$Asset)), "\n")
    
  }, error = function(e) {
    cat("ERROR: Failed to save NF-GARCH TS CV results:", e$message, "\n")
  })
  
} else {
  cat("ERROR: No NF-GARCH TS CV results generated\n")
}

cat("\n=== NF-GARCH TIME SERIES CROSS-VALIDATION COMPLETE ===\n")
cat("Engine used:", engine, "\n")
cat("Models attempted:", length(names(model_configs)) * (length(valid_fx_returns) + length(valid_equity_returns)), "\n")
cat("Successful windows:", ifelse(nrow(Fitted_NFGARCH_TS_CV_models) > 0, nrow(Fitted_NFGARCH_TS_CV_models), 0), "\n")
