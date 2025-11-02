#!/usr/bin/env Rscript
# NF-GARCH Simulation and Forecasting
# This script implements Normalizing Flow-enhanced GARCH models for financial time series
# Uses manual engine implementation

# Load required libraries
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
  cat("ERROR: NF-GARCH simulation failed\n")
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

cat("Starting NF-GARCH simulation with engine:", engine, "...\n")
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
equity_tickers <- c("NVDA", "MSFT", "AMZN")
fx_names <- c("EURUSD", "GBPUSD", "USDZAR")

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

# Model Configuration and Data Splitting
cat("Setting up model configurations...\n")

# Model configurations - manual engine only
model_configs <- list(
  sGARCH_norm  = list(model = "sGARCH", distribution = "norm", submodel = NULL),
  sGARCH_sstd  = list(model = "sGARCH", distribution = "sstd", submodel = NULL),
  gjrGARCH     = list(model = "gjrGARCH", distribution = "sstd", submodel = NULL),
  eGARCH       = list(model = "eGARCH", distribution = "sstd", submodel = NULL),
  TGARCH       = list(model = "TGARCH", distribution = "sstd", submodel = NULL)
)

# Data Splitting for Model Training and Evaluation
get_split_index <- function(x, split_ratio = 0.65) {
  return(floor(nrow(x) * split_ratio))
}

# Create training and testing sets for both asset classes (Chronological Split)
fx_train_returns <- lapply(fx_returns, function(x) x[1:get_split_index(x)])
fx_test_returns  <- lapply(fx_returns, function(x) x[(get_split_index(x) + 1):nrow(x)])

equity_train_returns <- lapply(equity_returns, function(x) x[1:get_split_index(x)])
equity_test_returns  <- lapply(equity_returns, function(x) x[(get_split_index(x) + 1):nrow(x)])

# Time Series Cross-Validation Implementation for NF-GARCH
cat("Implementing Time Series Cross-Validation for NF-GARCH...\n")

# TS CV function for NF-GARCH with manual engine
# Optimized: Increased step_size to reduce windows, added max_windows limit
ts_cross_validate_nfgarch_manual <- function(returns, model_type, dist_type = "sstd", submodel = NULL, 
                                             nf_residuals, window_size = 500, step_size = 500, forecast_horizon = 20, max_windows = 3) {
  # Perform sliding window time-series cross-validation for NF-GARCH with manual engine
  # This approach respects temporal ordering and provides robust performance estimates
  
  n <- nrow(returns)
  results <- list()
  
  # Generate all possible window start indices
  all_start_indices <- seq(1, n - window_size - forecast_horizon, by = step_size)
  
  # Limit to max_windows if specified
  if (!is.null(max_windows) && length(all_start_indices) > max_windows) {
    # Select evenly spaced windows
    selected_indices <- round(seq(1, length(all_start_indices), length.out = max_windows))
    start_indices <- all_start_indices[selected_indices]
  } else {
    start_indices <- all_start_indices
  }
  
  cat("TS CV: Processing", length(start_indices), "windows (optimized from", length(all_start_indices), "possible)\n")
  
  for (start_idx in start_indices) {
    # Define training and testing windows
    train_set <- returns[start_idx:(start_idx + window_size - 1)]
    test_set  <- returns[(start_idx + window_size):(start_idx + window_size + forecast_horizon - 1)]
    
    # Print progress information for monitoring
    message("Processing NF-GARCH TS CV window: ", start_idx, 
            " | Train size: ", nrow(train_set), 
            " | Test size: ", nrow(test_set),
            " | Train SD: ", round(sd(train_set, na.rm = TRUE), 6))
    
    # Fit GARCH model with manual engine
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
      
      # CRITICAL FIX: Ensure NF residuals are standardized before use
      nf_resid_vec <- as.numeric(head(nf_residuals, n_sim))
      nf_resid_vec <- nf_resid_vec[!is.na(nf_resid_vec)]
      if (length(nf_resid_vec) < n_sim) {
        message("WARNING: NF residuals contain NAs for window ", start_idx)
        next
      }
      # Double-check standardization (should already be done, but ensure it)
      resid_mean <- mean(nf_resid_vec, na.rm = TRUE)
      resid_sd <- sd(nf_resid_vec, na.rm = TRUE)
      if (abs(resid_mean) > 0.1 || abs(resid_sd - 1) > 0.1) {
        nf_resid_vec <- (nf_resid_vec - resid_mean) / resid_sd
      }
      
      # Use engine_path for NF-GARCH simulation
      sim_result <- tryCatch({
        engine_path(
          fit, 
          nf_resid_vec, 
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

# GARCH Model Training
cat("Fitting GARCH models...\n")

Fitted_Chrono_Split_models <- list()

for (config_name in names(model_configs)) {
  cfg <- model_configs[[config_name]]
  
  cat("Fitting", config_name, "models...\n")
  
  # Fit models using the selected engine
  equity_chrono_split_fit <- lapply(names(equity_train_returns), function(asset) {
    tryCatch({
      engine_fit(model = cfg$model, returns = equity_train_returns[[asset]], 
                dist = cfg$distribution, submodel = cfg$submodel, engine = engine)
    }, error = function(e) {
      cat("WARNING: Failed to fit", config_name, "for", asset, ":", e$message, "\n")
      NULL
    })
  })
  names(equity_chrono_split_fit) <- names(equity_train_returns)
  
  fx_chrono_split_fit <- lapply(names(fx_train_returns), function(asset) {
    tryCatch({
      engine_fit(model = cfg$model, returns = fx_train_returns[[asset]], 
                dist = cfg$distribution, submodel = cfg$submodel, engine = engine)
    }, error = function(e) {
      cat("WARNING: Failed to fit", config_name, "for", asset, ":", e$message, "\n")
      NULL
    })
  })
  names(fx_chrono_split_fit) <- names(fx_train_returns)
  
  Fitted_Chrono_Split_models[[paste0("equity_", config_name)]] <- equity_chrono_split_fit
  Fitted_Chrono_Split_models[[paste0("fx_", config_name)]]     <- fx_chrono_split_fit
}

# Load NF Residuals
cat("Loading NF residuals...\n")

tryCatch({
  # Check both manual output directory and default directory
  nf_dirs <- c("outputs/manual/nf_models", "nf_generated_residuals")
  nf_files <- c()
  for (dir in nf_dirs) {
    if (dir.exists(dir)) {
      nf_files <- c(nf_files, list.files(dir, pattern = "*_synthetic_residuals.csv", full.names = TRUE))
    }
  }
  
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
    # Parse model and asset from file names (format: MODEL_ASSET_synthetic_residuals.csv)
    nf_residuals_map <- list()
    for (f in nf_files) {
      fname <- basename(f)
      # Extract model and asset from filename (e.g., "eGARCH_AMZN_synthetic_residuals.csv" -> "eGARCH" and "AMZN")
      fname_clean <- stringr::str_replace(fname, "_synthetic_residuals\\.csv$", "")
      parts <- strsplit(fname_clean, "_")[[1]]
      
      # Match against possible keys
      possible_keys <- c(
        paste0(parts[1], "_", parts[2], "_residuals_synthetic"),  # eGARCH_AMZN_residuals_synthetic
        paste0(parts[1], "_", parts[2]),                           # eGARCH_AMZN
        fname_clean,                                                # eGARCH_AMZN
        paste0(parts[1], "_fx_", parts[2], "_residuals_synthetic"), # eGARCH_fx_AMZN_residuals_synthetic
        paste0(parts[1], "_equity_", parts[2], "_residuals_synthetic") # eGARCH_equity_AMZN_residuals_synthetic
      )
      
      tryCatch({
        residuals_data <- read.csv(f)
        
        residual_values <- if ("residual" %in% names(residuals_data)) {
          residuals_data$residual
        } else if (ncol(residuals_data) > 0) {
          residuals_data[[1]]
        } else {
          next
        }
        
        # CRITICAL FIX: Standardize NF residuals (mean ≈ 0, SD ≈ 1)
        # This fixes the improper residual standardization issue
        residual_values <- as.numeric(residual_values)
        residual_values <- residual_values[!is.na(residual_values)]
        if (length(residual_values) > 0) {
          resid_mean <- mean(residual_values, na.rm = TRUE)
          resid_sd <- sd(residual_values, na.rm = TRUE)
          if (!is.finite(resid_sd) || resid_sd == 0) {
            cat("WARNING: NF residuals have zero/invalid variance for", fname_clean, "- skipping\n")
            next
          }
          residual_values <- (residual_values - resid_mean) / resid_sd
          cat("Standardized NF residuals for", fname_clean, ": Mean =", round(mean(residual_values), 6), "SD =", round(sd(residual_values), 6), "\n")
        }
        
        # Store under all possible keys for flexible lookup
        for (key in possible_keys) {
          nf_residuals_map[[key]] <- residual_values
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

# NF-GARCH Simulation
cat("Running NF-GARCH simulation...\n")

# Define NF-GARCH fitting function with robust error handling
fit_nf_garch <- function(asset_name, asset_returns, model_config, nf_resid) {
  tryCatch({
    # Use engine_fit
    fit <- engine_fit(
      model = model_config[["model"]], 
      returns = asset_returns, 
      dist = model_config[["distribution"]], 
      submodel = model_config[["submodel"]], 
      engine = engine
    )
    
    if (!engine_converged(fit)) {
      cat("ERROR: Fit failed for", asset_name, model_config[["model"]], "\n")
      return(NULL)
    }
    
    # Setup simulation
    n_sim <- floor(length(asset_returns) / 2)
    if (length(nf_resid) < n_sim) {
      cat("WARNING: NF residuals too short for", asset_name, "-", model_config[["model"]], "\n")
      return(NULL)
    }
    
    # CRITICAL FIX: Ensure NF residuals are standardized before use
    nf_resid_vec <- as.numeric(head(nf_resid, n_sim))
    nf_resid_vec <- nf_resid_vec[!is.na(nf_resid_vec)]
    if (length(nf_resid_vec) < n_sim) {
      cat("WARNING: NF residuals contain NAs for", asset_name, "-", model_config[["model"]], "\n")
      return(NULL)
    }
    # Double-check standardization (should already be done, but ensure it)
    resid_mean <- mean(nf_resid_vec, na.rm = TRUE)
    resid_sd <- sd(nf_resid_vec, na.rm = TRUE)
    if (abs(resid_mean) > 0.1 || abs(resid_sd - 1) > 0.1) {
      nf_resid_vec <- (nf_resid_vec - resid_mean) / resid_sd
      cat("Re-standardized NF residuals for", asset_name, model_config[["model"]], 
          ": Mean =", round(mean(nf_resid_vec), 6), "SD =", round(sd(nf_resid_vec), 6), "\n")
    }
    
    # Use engine_path for simulation
    sim_result <- engine_path(
      fit, 
      nf_resid_vec, 
      n_sim, 
      model_config[["model"]], 
      model_config[["submodel"]], 
      engine
    )
    sim_returns <- sim_result$returns
    
    fitted_values <- sim_returns
    mse <- mean((asset_returns - fitted_values)^2, na.rm = TRUE)
    mae <- mean(abs(asset_returns - fitted_values), na.rm = TRUE)
    
    # Get model information
    ic <- engine_infocriteria(fit)
    
    return(data.frame(
      Model = model_config[["model"]],
      Distribution = model_config[["distribution"]],
      Asset = asset_name,
      AIC = ic["AIC"],
      BIC = ic["BIC"],
      LogLikelihood = ic["LogLikelihood"],
      MSE = mse,
      MAE = mae,
      SplitType = "Chrono"
    ))
  }, error = function(e) {
    cat("ERROR: Error for", asset_name, model_config[["model"]], ":", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Run NF-GARCH Analysis - BOTH Chronological and TS CV
cat("=== CHRONOLOGICAL SPLIT NF-GARCH ANALYSIS ===\n")
nf_results_chrono <- list()

for (config_name in names(model_configs)) {
  cfg <- model_configs[[config_name]]
  
  cat("Processing", config_name, "(Chrono Split)...\n")
  
  # FX
  for (asset in names(fx_returns)) {
    possible_keys <- c(
      paste0(config_name, "_fx_", asset, "_residuals_synthetic"),
      paste0("fx_", asset, "_residuals_", config_name, "_residuals_synthetic_synthetic"),
      paste0(config_name, "_", asset, "_residuals_synthetic")
    )
    
    key <- NULL
    for (k in possible_keys) {
      if (k %in% names(nf_residuals_map)) {
        key <- k
        break
      }
    }
    
    if (is.null(key)) {
      cat("ERROR: Skipped:", asset, config_name, "- No synthetic residuals found.\n")
      next
    }
    
    cat("NF-GARCH (FX):", asset, config_name, "\n")
    r <- fit_nf_garch(asset, fx_returns[[asset]], cfg, nf_residuals_map[[key]])
    if (!is.null(r)) nf_results_chrono[[length(nf_results_chrono) + 1]] <- r
  }
  
  # Equity
  for (asset in names(equity_returns)) {
    possible_keys <- c(
      paste0(config_name, "_equity_", asset, "_residuals_synthetic"),
      paste0("equity_", asset, "_residuals_", config_name, "_residuals_synthetic_synthetic"),
      paste0(config_name, "_", asset, "_residuals_synthetic")
    )
    
    key <- NULL
    for (k in possible_keys) {
      if (k %in% names(nf_residuals_map)) {
        key <- k
        break
      }
    }
    
    if (is.null(key)) {
      cat("ERROR: Skipped:", asset, config_name, "- No synthetic residuals found.\n")
      next
    }
    
    cat("NF-GARCH (EQ):", asset, config_name, "\n")
    r <- fit_nf_garch(asset, equity_returns[[asset]], cfg, nf_residuals_map[[key]])
    if (!is.null(r)) nf_results_chrono[[length(nf_results_chrono) + 1]] <- r
  }
}

# Run NF-GARCH Time Series Cross-Validation (Manual Engine Focus)
cat("=== RUNNING NF-GARCH TIME SERIES CROSS-VALIDATION ===\n")

# Check and ensure sufficient size and variability across each window
valid_fx_returns <- fx_returns[sapply(fx_returns, function(x) nrow(x) > 520 && sd(x, na.rm = TRUE) > 0)]
valid_equity_returns <- equity_returns[sapply(equity_returns, function(x) nrow(x) > 520 && sd(x, na.rm = TRUE) > 0)]

# Helper to run all NF-GARCH CV models with manual engine
# Optimized: Reduced forecast_horizon and added max_windows limit
run_all_nfgarch_cv_models_manual <- function(returns_list, model_configs, nf_residuals_map, 
                                            window_size = 500, forecast_horizon = 20, max_windows = 3) {
  cv_results_all <- list()
  
  for (model_name in names(model_configs)) {
    cfg <- model_configs[[model_name]]
    message("Running NF-GARCH TS CV for model: ", model_name, " (", engine, " engine)")
    
    result <- lapply(names(returns_list), function(asset_name) {
      # Find corresponding NF residuals
      possible_keys <- c(
        paste0(model_name, "_", asset_name, "_residuals_synthetic"),
        paste0(model_name, "_fx_", asset_name, "_residuals_synthetic"),
        paste0(model_name, "_equity_", asset_name, "_residuals_synthetic"),
        paste0(model_name, "_", asset_name, "_residuals_synthetic.csv"),
        paste0(model_name, "_fx_", asset_name, "_residuals_synthetic.csv"),
        paste0(model_name, "_equity_", asset_name, "_residuals_synthetic.csv")
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
      
      ts_cross_validate_nfgarch_manual(
        returns_list[[asset_name]], 
        model_type = cfg$model, 
        dist_type = cfg$distribution, 
        submodel = cfg$submodel,
        nf_residuals = nf_residuals_map[[key]],
        window_size = window_size,
        forecast_horizon = forecast_horizon,
        max_windows = max_windows
      )
    })
    
    # Label by asset names
    names(result) <- names(returns_list)
    
    # Remove nulls
    result <- result[!sapply(result, is.null)]
    
    message("OK: NF-GARCH TS CV fits found for assets: ", paste(names(result), collapse = ", "))
    
    cv_results_all[[model_name]] <- result
  }
  
  return(cv_results_all)
}

# Run NF-GARCH TS CV Analysis
cat("Processing FX assets with NF-GARCH TS CV...\n")
Fitted_FX_NFGARCH_TS_CV_models <- run_all_nfgarch_cv_models_manual(
  valid_fx_returns, model_configs, nf_residuals_map
)

cat("Processing Equity assets with NF-GARCH TS CV...\n")
Fitted_EQ_NFGARCH_TS_CV_models <- run_all_nfgarch_cv_models_manual(
  valid_equity_returns, model_configs, nf_residuals_map
)

# Flatten all NF-GARCH CV results into one data frame
Fitted_NFGARCH_TS_CV_models <- data.frame()

for (model_name in names(Fitted_FX_NFGARCH_TS_CV_models)) {
  # FX results - add asset name to each data frame before combining
  fx_results <- tryCatch({
    fx_list <- Fitted_FX_NFGARCH_TS_CV_models[[model_name]]
    if (is.null(fx_list) || length(fx_list) == 0) {
      return(NULL)
    }
    # Add asset name to each asset's results before combining
    fx_list_with_asset <- lapply(names(fx_list), function(asset_name) {
      df <- fx_list[[asset_name]]
      if (!is.null(df) && nrow(df) > 0) {
        df$Asset <- asset_name
        df$AssetType <- "FX"
      }
      return(df)
    })
    # Combine all FX results
    do.call(rbind, fx_list_with_asset)
  }, error = function(e) {
    message("WARNING: FX NF-GARCH CV results failed for: ", model_name, " - ", e$message)
    return(NULL)
  })
  
  # Equity results - add asset name to each data frame before combining
  eq_results <- tryCatch({
    eq_list <- Fitted_EQ_NFGARCH_TS_CV_models[[model_name]]
    if (is.null(eq_list) || length(eq_list) == 0) {
      return(NULL)
    }
    # Add asset name to each asset's results before combining
    eq_list_with_asset <- lapply(names(eq_list), function(asset_name) {
      df <- eq_list[[asset_name]]
      if (!is.null(df) && nrow(df) > 0) {
        df$Asset <- asset_name
        df$AssetType <- "Equity"
      }
      return(df)
    })
    # Combine all Equity results
    do.call(rbind, eq_list_with_asset)
  }, error = function(e) {
    message("WARNING: EQ NF-GARCH CV results failed for: ", model_name, " - ", e$message)
    return(NULL)
  })
  
  if (!is.null(fx_results) && nrow(fx_results) > 0) {
    Fitted_NFGARCH_TS_CV_models <- bind_rows(Fitted_NFGARCH_TS_CV_models, fx_results)
  }
  
  if (!is.null(eq_results) && nrow(eq_results) > 0) {
    Fitted_NFGARCH_TS_CV_models <- bind_rows(Fitted_NFGARCH_TS_CV_models, eq_results)
  }
}

# Create Comparison Tables
cat("=== CREATING COMPARISON TABLES ===\n")

# Combine results
if (length(nf_results_chrono) > 0) {
  nf_results_df <- do.call(rbind, nf_results_chrono)
  
  # Create comparison tables
  comparison_tables <- list()
  
  # 1. Chronological Summary
  chrono_summary <- nf_results_df %>%
    group_by(Model, Distribution) %>%
    summarise(
      Avg_AIC = mean(AIC, na.rm = TRUE),
      Avg_BIC = mean(BIC, na.rm = TRUE),
      Avg_LogLik = mean(LogLikelihood, na.rm = TRUE),
      Avg_MSE = mean(MSE, na.rm = TRUE),
      Avg_MAE = mean(MAE, na.rm = TRUE),
      Models_Count = n(),
      .groups = 'drop'
    ) %>%
    mutate(Split_Type = "Chronological")
  
  # 2. TS CV Summary (if available)
  tscv_summary <- data.frame()
  if (nrow(Fitted_NFGARCH_TS_CV_models) > 0) {
    tscv_summary <- Fitted_NFGARCH_TS_CV_models %>%
      group_by(Model, Distribution) %>%
      summarise(
        Avg_AIC = mean(AIC, na.rm = TRUE),
        Avg_BIC = mean(BIC, na.rm = TRUE),
        Avg_LogLik = mean(LogLikelihood, na.rm = TRUE),
        Avg_MSE = mean(MSE, na.rm = TRUE),
        Avg_MAE = mean(MAE, na.rm = TRUE),
        Windows_Processed = n(),
        .groups = 'drop'
      ) %>%
      mutate(Split_Type = "Time_Series_CV")
  }
  
  # 3. Direct Comparison Table
  comparison_table <- data.frame()
  if (nrow(chrono_summary) > 0 && nrow(tscv_summary) > 0) {
    # Merge chronological and TS CV results for direct comparison
    comparison_table <- bind_rows(chrono_summary, tscv_summary) %>%
      pivot_wider(
        names_from = Split_Type,
        values_from = c(Avg_AIC, Avg_BIC, Avg_LogLik, Avg_MSE, Avg_MAE),
        names_sep = "_"
      ) %>%
      # Calculate differences
      mutate(
        AIC_Diff = Avg_AIC_Time_Series_CV - Avg_AIC_Chronological,
        BIC_Diff = Avg_BIC_Time_Series_CV - Avg_BIC_Chronological,
        MSE_Diff = Avg_MSE_Time_Series_CV - Avg_MSE_Chronological,
        MAE_Diff = Avg_MAE_Time_Series_CV - Avg_MAE_Chronological,
        # Performance ranking
        Chrono_Rank = rank(Avg_MSE_Chronological),
        TS_CV_Rank = rank(Avg_MSE_Time_Series_CV),
        Rank_Change = TS_CV_Rank - Chrono_Rank
      ) %>%
      arrange(Chrono_Rank)
  }
  
  # 4. Model Performance Comparison by Asset
  asset_comparison <- data.frame()
  if (nrow(Fitted_NFGARCH_TS_CV_models) > 0) {
    # Get asset-level comparison
    chrono_asset <- nf_results_df %>%
      group_by(Model, Asset) %>%
      summarise(
        Chrono_MSE = mean(MSE, na.rm = TRUE),
        Chrono_MAE = mean(MAE, na.rm = TRUE),
        .groups = 'drop'
      )
    
    tscv_asset <- Fitted_NFGARCH_TS_CV_models %>%
      group_by(Model, Asset) %>%
      summarise(
        TS_CV_MSE = mean(MSE, na.rm = TRUE),
        TS_CV_MAE = mean(MAE, na.rm = TRUE),
        Windows = n(),
        .groups = 'drop'
      )
    
    asset_comparison <- full_join(chrono_asset, tscv_asset, by = c("Model", "Asset")) %>%
      mutate(
        MSE_Improvement = Chrono_MSE - TS_CV_MSE,
        MAE_Improvement = Chrono_MAE - TS_CV_MAE,
        MSE_Improvement_Pct = (MSE_Improvement / Chrono_MSE) * 100,
        MAE_Improvement_Pct = (MAE_Improvement / Chrono_MAE) * 100
      ) %>%
      arrange(desc(MSE_Improvement_Pct))
  }
  
          # Save results with comprehensive comparison
          # Create consolidated directory if it doesn't exist
          if (!dir.exists("results/consolidated")) {
            dir.create("results/consolidated", recursive = TRUE, showWarnings = FALSE)
          }
          output_file <- paste0("results/consolidated/NF_GARCH_Results_", engine, ".xlsx")
  
  tryCatch({
    wb <- createWorkbook()
    
    # Add chronological split results
    addWorksheet(wb, "Chrono_Split_NF_GARCH")
    writeData(wb, "Chrono_Split_NF_GARCH", nf_results_df)
    
    # Add TS CV results if available
    if (nrow(Fitted_NFGARCH_TS_CV_models) > 0) {
      addWorksheet(wb, "TS_CV_NF_GARCH")
      writeData(wb, "TS_CV_NF_GARCH", Fitted_NFGARCH_TS_CV_models)
    }
    
    # Add comparison tables
    addWorksheet(wb, "Chrono_Summary")
    writeData(wb, "Chrono_Summary", chrono_summary)
    
    if (nrow(tscv_summary) > 0) {
      addWorksheet(wb, "TS_CV_Summary")
      writeData(wb, "TS_CV_Summary", tscv_summary)
    }
    
    if (nrow(comparison_table) > 0) {
      addWorksheet(wb, "Split_Comparison")
      writeData(wb, "Split_Comparison", comparison_table)
    }
    
    if (nrow(asset_comparison) > 0) {
      addWorksheet(wb, "Asset_Comparison")
      writeData(wb, "Asset_Comparison", asset_comparison)
    }
    
    # Add overall performance comparison
    if (nrow(comparison_table) > 0) {
      # Create performance ranking comparison
      performance_comparison <- comparison_table %>%
        select(Model, Distribution, 
               Chrono_MSE = Avg_MSE_Chronological, 
               TS_CV_MSE = Avg_MSE_Time_Series_CV,
               Chrono_Rank, TS_CV_Rank, Rank_Change) %>%
        mutate(
          Best_Method = ifelse(Chrono_MSE < TS_CV_MSE, "Chronological", "TS_CV"),
          Improvement = abs(Chrono_MSE - TS_CV_MSE),
          Improvement_Pct = (Improvement / pmin(Chrono_MSE, TS_CV_MSE)) * 100
        ) %>%
        arrange(Chrono_Rank)
      
      addWorksheet(wb, "Performance_Comparison")
      writeData(wb, "Performance_Comparison", performance_comparison)
    }
    
    # Save workbook
    saveWorkbook(wb, output_file, overwrite = TRUE)
    
    cat("OK: NF-GARCH results saved to:", output_file, "\n")
    cat("   Total chronological models:", nrow(nf_results_df), "\n")
    cat("   Successful chronological fits:", sum(!is.na(nf_results_df$AIC)), "\n")
    if (nrow(Fitted_NFGARCH_TS_CV_models) > 0) {
      cat("   TS CV windows processed:", nrow(Fitted_NFGARCH_TS_CV_models), "\n")
      cat("   Comparison tables created:", length(comparison_tables), "\n")
    }
    
    # Print summary statistics
    if (nrow(comparison_table) > 0) {
      cat("\n=== COMPARISON SUMMARY ===\n")
      cat("Models with better TS CV performance:", sum(comparison_table$TS_CV_Rank < comparison_table$Chrono_Rank), "\n")
      cat("Models with better Chrono performance:", sum(comparison_table$Chrono_Rank < comparison_table$TS_CV_Rank), "\n")
      cat("Average MSE improvement (TS CV vs Chrono):", round(mean(comparison_table$MSE_Diff, na.rm = TRUE), 6), "\n")
    }
    
  }, error = function(e) {
    cat("ERROR: Failed to save results:", e$message, "\n")
  })
  
} else {
  cat("ERROR: No NF-GARCH results generated\n")
}

cat("\n=== NF-GARCH SIMULATION COMPLETE ===\n")
cat("Engine used:", engine, "\n")
cat("Models attempted:", length(names(model_configs)) * (length(fx_returns) + length(equity_returns)), "\n")
cat("Successful fits:", ifelse(length(nf_results_chrono) > 0, length(nf_results_chrono), 0), "\n")
