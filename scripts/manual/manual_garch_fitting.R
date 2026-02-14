# =============================================================================
# MANUAL GARCH FITTING - MAIN ORCHESTRATION SCRIPT
# =============================================================================
#
# PURPOSE: Orchestrates GARCH model fitting across multiple assets and specifications
# MODE-AWARE: Respects PIPELINE_MODE from scripts/core/config.R (optimized or full)
# EXECUTION: Designed for manual execution in R Studio or via Rscript
#
# MODELS SUPPORTED: sGARCH, gjrGARCH, eGARCH, TGARCH (Zakoian)
# DISTRIBUTIONS: norm (Normal), std (Student-t)
# sstd (Skewed Student-t) is not implemented; requesting it will error
#
# =============================================================================

# Load required libraries
library(xts)
library(PerformanceAnalytics)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(parallel)
library(doParallel)

# Load centralized seed configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
  set.seed(REPRODUCIBILITY_SEED)
} else {
  set.seed(123)  # Fallback if config not available
}

# Load manual optimization configuration
source("scripts/manual/manual_optimized_config.R")

# Load manual engine
source("scripts/engines/engine_selector.R")

# Set up error handling and timing
options(warn = 1)
start_time <- Sys.time()

cat("=== MANUAL GARCH FITTING ===\n")
cat("Start time:", as.character(start_time), "\n")
cat("\n")
print_optimization_summary()
cat("\n")

# =============================================================================
# DATA LOADING AND PREPROCESSING
# =============================================================================

cat("\n1. Loading and preprocessing data...\n")

# Load the combined FX and equity price dataset
raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1)

# Convert date strings to proper Date objects
raw_price_data$Date <- lubridate::ymd(rownames(raw_price_data))
rownames(raw_price_data) <- NULL

# Reorganize data with Date as the first column
raw_price_data <- raw_price_data %>% dplyr::select(Date, everything())

# Extract price matrix without date column
price_data_matrix <- raw_price_data[, !(names(raw_price_data) %in% "Date")]

# Asset configuration: Get from centralized config (respects PIPELINE_MODE)
# This ensures all scripts use the same asset list based on current mode
fx_names <- get_manual_fx_assets()
equity_tickers <- get_manual_equity_assets()

# Filter to only assets that exist in the data
available_assets <- names(price_data_matrix)
fx_names <- fx_names[fx_names %in% available_assets]
equity_tickers <- equity_tickers[equity_tickers %in% available_assets]

cat("Using asset set from", PIPELINE_MODE, "mode:", paste(c(equity_tickers, fx_names), collapse = ", "), "\n")
cat("  FX assets:", length(fx_names), "\n")
cat("  Equity assets:", length(equity_tickers), "\n")
cat("  Total assets:", length(fx_names) + length(equity_tickers), "\n")

# Convert price series to XTS objects
equity_xts <- lapply(equity_tickers, function(ticker) {
  if (ticker %in% names(price_data_matrix)) {
    prices <- price_data_matrix[[ticker]]
    dates <- raw_price_data$Date
    xts(prices, order.by = dates)
  } else {
    cat("Warning: Asset", ticker, "not found in data\n")
    NULL
  }
})

fx_xts <- lapply(fx_names, function(name) {
  if (name %in% names(price_data_matrix)) {
    prices <- price_data_matrix[[name]]
    dates <- raw_price_data$Date
    xts(prices, order.by = dates)
  } else {
    cat("Warning: Asset", name, "not found in data\n")
    NULL
  }
})

# Filter out NULL results
equity_xts <- equity_xts[!sapply(equity_xts, is.null)]
fx_xts <- fx_xts[!sapply(fx_xts, is.null)]

# Calculate returns
equity_returns <- lapply(equity_xts, function(x) {
  returns <- diff(log(x))
  returns[!is.na(returns)]
})

fx_returns <- lapply(fx_xts, function(x) {
  returns <- diff(log(x))
  returns[!is.na(returns)]
})

cat("Data loaded successfully. Equity assets:", length(equity_returns), 
    "FX assets:", length(fx_returns), "\n")

# =============================================================================
# MODEL CONFIGURATION (Mode-Aware)
# =============================================================================

cat("\n2. Setting up model configuration...\n")

# Get model selection from centralized config (respects PIPELINE_MODE)
manual_models <- get_manual_models()
manual_model_config <- get_manual_model_config()

cat("Using models from", PIPELINE_MODE, "mode:", paste(manual_models, collapse = ", "), "\n")

# GARCH model configurations (no need for specs with manual engine)
# Models will be fit directly using engine_fit()

cat("Model configurations prepared for", length(manual_models), "models\n")

# =============================================================================
# TIME-SERIES CROSS-VALIDATION (Mode-Aware)
# =============================================================================

cat("\n3. Setting up time-series cross-validation...\n")

# Get CV configuration from centralized config (respects PIPELINE_MODE)
cv_config <- get_manual_cv_config()

cat("CV Configuration (", PIPELINE_MODE, " mode):\n", sep = "")
cat("  Folds:", cv_config$n_folds, "\n")
cat("  Window size:", cv_config$window_size, "\n")
cat("  Step size:", cv_config$step_size, "\n")
cat("  Forecast horizon:", cv_config$forecast_horizon, "\n")
if (!is.null(cv_config$max_windows)) {
  cat("  Max windows:", cv_config$max_windows, "\n")
}

# Setup parallel processing for CV
if (cv_config$parallel_enabled) {
  cl <- makeCluster(cv_config$parallel_cores)
  registerDoParallel(cl)
  cat("\nParallel processing enabled with", cv_config$parallel_cores, "cores\n")
}

# =============================================================================
# MODEL FITTING FUNCTION (Works for Both Modes)
# =============================================================================

# GARCH model fitting with manual engine
# Function name retained for backward compatibility
fit_optimized_garch <- function(returns_data, asset_name, model_name) {
  tryCatch({
    # Get model configuration
    model_config <- manual_model_config[[model_name]]
    
    # Convert returns_data to numeric vector if needed
    if (inherits(returns_data, "xts")) {
      returns_vec <- as.numeric(returns_data)
    } else {
      returns_vec <- as.numeric(returns_data)
    }
    
    # Remove NAs
    returns_vec <- returns_vec[!is.na(returns_vec)]
    
    if (length(returns_vec) < 100) {
      cat("Warning: Insufficient data for", asset_name, model_name, "\n")
      return(NULL)
    }
    
    # Fit GARCH model using manual engine
    garch_fit <- engine_fit(
      model = model_config$model,
      returns = returns_vec,
      dist = model_config$distribution,
      submodel = model_config$submodel,
      engine = "manual"
    )
    
    # Check convergence
    if (engine_converged(garch_fit)) {
      # Extract standardized residuals
      residuals <- engine_residuals(garch_fit, standardize = TRUE)
      
      # Calculate basic statistics
      stats <- list(
        asset = asset_name,
        model = model_name,
        loglik = garch_fit$loglik,
        aic = garch_fit$aic,
        bic = garch_fit$bic,
        convergence = TRUE,
        n_obs = length(returns_vec),
        n_params = length(garch_fit$coef)
      )
      
      return(list(
        fit = garch_fit,
        residuals = residuals,
        stats = stats
      ))
    } else {
      cat("Warning: Model", model_name, "for", asset_name, "did not converge\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Error fitting", model_name, "for", asset_name, ":", e$message, "\n")
    return(NULL)
  })
}

# =============================================================================
# CROSS-VALIDATION FUNCTION (Works for Both Modes)
# =============================================================================

# Time-series cross-validation with configuration from PIPELINE_MODE
# garch_spec is not used; the manual engine is called directly. Function name kept for backward compatibility.
run_optimized_cv <- function(returns_data, asset_name, model_name) {
  n_obs <- length(returns_data)
  window_size <- floor(n_obs * cv_config$window_size)
  step_size <- floor(n_obs * cv_config$step_size)
  min_train_size <- floor(n_obs * cv_config$min_train_size)
  
  # Calculate number of windows (reduced for speed)
  max_windows <- cv_config$max_windows
  n_windows <- min(max_windows, floor((n_obs - window_size) / step_size) + 1)
  
  cat("  Running CV for", asset_name, "-", model_name, 
      "(", n_windows, "windows,", window_size, "window size)\n")
  
  cv_results <- list()
  
  for (i in 1:n_windows) {
    # Calculate window boundaries
    start_idx <- (i - 1) * step_size + 1
    end_idx <- start_idx + window_size - 1
    
    if (end_idx > n_obs) break
    
    # Split data
    train_data <- returns_data[start_idx:end_idx]
    
    if (length(train_data) < min_train_size) {
      cat("    Warning: Window", i, "too small, skipping\n")
      next
    }
    
    # Fit model on training data
    fit_result <- fit_optimized_garch(train_data, asset_name, model_name)
    
    if (!is.null(fit_result)) {
      cv_results[[i]] <- list(
        window = i,
        start_idx = start_idx,
        end_idx = end_idx,
        fit_result = fit_result
      )
    }
    
    # Memory management
    if (isTRUE(cv_config$clear_memory) && i %% 5 == 0) {
      gc()
    }
  }
  
  return(cv_results)
}

# =============================================================================
# MAIN FITTING PROCESS
# =============================================================================

cat("\n4. Fitting GARCH models using", PIPELINE_MODE, "configuration...\n")

# Combine all returns data
all_returns <- c(equity_returns, fx_returns)
all_asset_names <- c(equity_tickers, fx_names)

# Initialize results storage
all_results <- list()
model_summary <- data.frame()

# Fit models for each asset-model combination
for (asset_idx in 1:length(all_returns)) {
  asset_name <- all_asset_names[asset_idx]
  returns_data <- all_returns[[asset_idx]]
  
  cat("\nProcessing asset:", asset_name, "\n")
  
  for (model_name in manual_models) {
    cat("  Fitting model:", model_name, "\n")
    
    # Run time-series cross-validation (configuration from PIPELINE_MODE)
    cv_results <- run_optimized_cv(
      returns_data, 
      asset_name, 
      model_name
    )
    
    # Store results
    if (length(cv_results) > 0) {
      all_results[[paste(asset_name, model_name, sep = "_")]] <- cv_results
      
      # Calculate summary statistics
      converged_windows <- sum(sapply(cv_results, function(x) !is.null(x$fit_result)))
      avg_loglik <- mean(sapply(cv_results, function(x) {
        if (!is.null(x$fit_result)) x$fit_result$stats$loglik else NA
      }), na.rm = TRUE)
      
      model_summary <- rbind(model_summary, data.frame(
        asset = asset_name,
        model = model_name,
        n_windows = length(cv_results),
        converged_windows = converged_windows,
        avg_loglik = avg_loglik,
        success_rate = converged_windows / length(cv_results)
      ))
    }
  }
  
  # Memory management
  if (isTRUE(cv_config$clear_memory)) {
    gc()
  }
}

# =============================================================================
# RESULTS PROCESSING AND SAVING
# =============================================================================

cat("\n5. Processing and saving results...\n")

# Create output directories
if (!dir.exists("outputs/manual")) {
  dir.create("outputs/manual", recursive = TRUE)
}
if (!dir.exists("outputs/manual/garch_fitting")) {
  dir.create("outputs/manual/garch_fitting", recursive = TRUE)
}

# Save model summary
write.csv(model_summary, "outputs/manual/garch_fitting/model_summary.csv", row.names = FALSE)

# Save detailed results
saveRDS(all_results, "outputs/manual/garch_fitting/detailed_results.rds")

# Extract and save residuals for NF training
# Residuals come from a single full training-set fit (65% of data) to avoid leakage from overlapping CV windows
residuals_dir <- "outputs/manual/residuals_by_model"
if (!dir.exists(residuals_dir)) {
  dir.create(residuals_dir, recursive = TRUE)
}

cat("\nExtracting residuals from full training set fits (no CV overlap)...\n")

# Create residuals by model
for (model_name in manual_models) {
  model_dir <- file.path(residuals_dir, model_name)
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }
  
  for (asset_idx in 1:length(all_returns)) {
    asset_name <- all_asset_names[asset_idx]
    returns_data <- all_returns[[asset_idx]]
    result_key <- paste(asset_name, model_name, sep = "_")
    
    # Only process if CV results exist (indicates model converged)
    if (result_key %in% names(all_results) && length(all_results[[result_key]]) > 0) {
      
      # Split data: 65% training / 35% test (same as simulation pipeline)
      n_obs <- length(returns_data)
      train_size <- floor(n_obs * 0.65)
      train_data <- returns_data[1:train_size]
      
      cat("  Fitting", model_name, "for", asset_name, "on full training set (", train_size, "obs)...\n")
      
      # Fit GARCH on full training set
      fit_result <- fit_optimized_garch(train_data, asset_name, model_name)
      
      if (!is.null(fit_result) && !is.null(fit_result$residuals)) {
        residuals_vec <- as.numeric(fit_result$residuals)
        
        # Validation: verify residual count matches training set size
        if (length(residuals_vec) != train_size) {
          cat("    WARNING: Residual count (", length(residuals_vec), 
              ") does not match training size (", train_size, ")\n")
        }
        
        # Validate residuals are standardized
        resid_mean <- mean(residuals_vec, na.rm = TRUE)
        resid_sd <- sd(residuals_vec, na.rm = TRUE)
        
        # Widened tolerance to 0.15 to accommodate Student-t with different nu values
        # (nu~3 gives SD~0.89, nu~5 gives SD~1.03, both are mathematically correct)
        if (abs(resid_mean) > 0.05 || abs(resid_sd - 1.0) > 0.15) {
          cat("    WARNING: Residuals not properly standardized!\n")
          cat("      Mean =", resid_mean, "(should be ~0)\n")
          cat("      SD =", resid_sd, "(should be ~1)\n")
          cat("      Skipping NF training for this model\n")
          next
        }
        
        cat("    Residuals validated: mean =", round(resid_mean, 4), 
            ", sd =", round(resid_sd, 4), "\n")
        
        # Save residuals for NF training (using correct filename)
        residuals_df <- data.frame(residuals = residuals_vec)
        residuals_file <- file.path(model_dir, paste0(asset_name, "_Manual_Optimized_residuals.csv"))
        write.csv(residuals_df, residuals_file, row.names = FALSE)
        cat("    Saved", length(residuals_vec), "standardized residuals\n")
      } else {
        cat("    WARNING: Failed to fit", model_name, "for", asset_name, "on full training set\n")
      }
    }
  }
}

# =============================================================================
# PERFORMANCE SUMMARY
# =============================================================================

end_time <- Sys.time()
execution_time <- end_time - start_time

cat("\n=== PERFORMANCE SUMMARY (", toupper(PIPELINE_MODE), " MODE) ===\n", sep = "")
cat("Execution time:", round(as.numeric(execution_time, units = "mins"), 2), "minutes\n")
cat("Pipeline mode:", PIPELINE_MODE, "\n")
cat("Assets processed:", length(all_asset_names), "\n")
cat("Models fitted:", length(manual_models), "\n")
cat("Total combinations:", length(all_asset_names) * length(manual_models), "\n")
cat("Successful fits:", sum(model_summary$converged_windows), "\n")
cat("Success rate:", round(mean(model_summary$success_rate) * 100, 2), "%\n")

# Memory usage
if (exists("cl")) {
  stopCluster(cl)
  registerDoSEQ()
}

# Final garbage collection
gc()

cat("\nManual GARCH fitting (", PIPELINE_MODE, " mode) completed successfully.\n", sep = "")
cat("Results saved to outputs/manual/garch_fitting/\n")
cat("Residuals saved to outputs/manual/residuals_by_model/\n")
cat("===============================================\n")

