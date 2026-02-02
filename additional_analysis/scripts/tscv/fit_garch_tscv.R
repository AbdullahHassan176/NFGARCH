# Time-Series Cross-Validation GARCH Fitting
# Rolling window TS CV for all assets and models
# Designed to ensure consistent TS CV across the entire pipeline

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

# Load TS CV configuration
source("scripts/config/tscv_split_config.R")

# Load model configuration
source("scripts/manual/manual_optimized_config.R")

# Load manual engine
source("scripts/engines/engine_selector.R")

# Set up error handling and timing
options(warn = 1)
start_time <- Sys.time()

cat("=== TIME-SERIES CROSS-VALIDATION GARCH FITTING ===\n")
cat("Start time:", as.character(start_time), "\n")
print_tscv_config_summary()

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
fx_names <- get_fx_assets()
equity_tickers <- get_equity_assets()

# Filter to only assets that exist in the data
available_assets <- names(price_data_matrix)
fx_names <- fx_names[fx_names %in% available_assets]
equity_tickers <- equity_tickers[equity_tickers %in% available_assets]

cat("Using asset set:", paste(c(equity_tickers, fx_names), collapse = ", "), "\n")
cat("  FX assets:", length(fx_names), "\n")
cat("  Equity assets:", length(equity_tickers), "\n")

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
# MODEL CONFIGURATION
# =============================================================================

cat("\n2. Setting up model configuration...\n")

# Use model configuration from manual config
manual_models <- get_manual_models()
manual_model_config <- get_manual_model_config()

cat("Using models:", paste(manual_models, collapse = ", "), "\n")

# Setup parallel processing
if (TSCV_CONFIG$parallel_enabled) {
  cl <- makeCluster(TSCV_CONFIG$parallel_cores)
  registerDoParallel(cl)
  cat("Parallel processing enabled with", TSCV_CONFIG$parallel_cores, "cores\n")
}

# =============================================================================
# MODEL FITTING FUNCTION
# =============================================================================

# Fit GARCH model for a single window
fit_garch_window <- function(returns_data, asset_name, model_name, window_info) {
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
      cat("    Warning: Insufficient data for window", window_info$window_id, "\n")
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
        window_id = window_info$window_id,
        train_start = window_info$train_start,
        train_end = window_info$train_end,
        train_size = window_info$train_size,
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
      cat("    Warning: Model did not converge for window", window_info$window_id, "\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("    Error in window", window_info$window_id, ":", e$message, "\n")
    return(NULL)
  })
}

# =============================================================================
# MAIN FITTING PROCESS (TS CV)
# =============================================================================

cat("\n3. Fitting GARCH models with Time-Series Cross-Validation...\n")

# Combine all returns data
all_returns <- c(equity_returns, fx_returns)
all_asset_names <- c(equity_tickers, fx_names)

# Initialize results storage
all_results <- list()
model_summary <- data.frame()

# Process each asset
for (asset_idx in 1:length(all_returns)) {
  asset_name <- all_asset_names[asset_idx]
  returns_data <- all_returns[[asset_idx]]
  
  cat("\n=== Processing asset:", asset_name, "===\n")
  
  # Calculate TS CV windows for this asset
  n_obs <- length(returns_data)
  windows <- calculate_tscv_windows(n_obs, TSCV_CONFIG)
  
  cat("Total observations:", n_obs, "\n")
  cat("Number of CV windows:", length(windows), "\n")
  
  # Initialize output directories for this asset (all windows)
  if (asset_idx == 1) {
    initialize_tscv_directories(length(windows))
  }
  
  # Process each model
  for (model_name in manual_models) {
    cat("\n  Model:", model_name, "\n")
    
    # Process each window
    for (window_info in windows) {
      window_id <- window_info$window_id
      
      cat("    Window", window_id, ":", 
          "indices [", window_info$train_start, "-", window_info$train_end, "], ",
          "size =", window_info$train_size, "\n")
      
      # Extract window data
      window_data <- returns_data[window_info$train_start:window_info$train_end]
      
      # Fit GARCH on this window
      fit_result <- fit_garch_window(window_data, asset_name, model_name, window_info)
      
      # Store results
      if (!is.null(fit_result)) {
        result_key <- paste(asset_name, model_name, window_id, sep = "_")
        all_results[[result_key]] <- fit_result
        
        # Add to summary
        model_summary <- rbind(model_summary, data.frame(
          asset = asset_name,
          model = model_name,
          split_type = "tscv",
          window_id = window_id,
          train_start = window_info$train_start,
          train_end = window_info$train_end,
          train_size = window_info$train_size,
          test_start = window_info$test_start,
          test_end = window_info$test_end,
          test_size = window_info$test_size,
          loglik = fit_result$stats$loglik,
          aic = fit_result$stats$aic,
          bic = fit_result$stats$bic,
          n_params = fit_result$stats$n_params,
          convergence = TRUE
        ))
        
        cat("      Fit successful: LogLik =", round(fit_result$stats$loglik, 2), 
            ", AIC =", round(fit_result$stats$aic, 2), "\n")
      }
      
      # Memory management
      if (TSCV_CONFIG$clear_memory && window_id %% 5 == 0) {
        gc()
      }
    }
  }
}

# =============================================================================
# EXTRACT AND SAVE RESIDUALS FOR NF TRAINING
# =============================================================================

cat("\n4. Extracting residuals from TS CV windows...\n")

# Get unique windows
unique_windows <- unique(model_summary$window_id)

for (window_id in unique_windows) {
  cat("\n  Processing Window", window_id, ":\n")
  
  # Create window-specific residuals directory
  window_resid_dir <- get_window_path(OUTPUT_PATHS$residuals, window_id)
  
  for (model_name in manual_models) {
    model_dir <- file.path(window_resid_dir, model_name)
    if (!dir.exists(model_dir)) {
      dir.create(model_dir, recursive = TRUE)
    }
    
    for (asset_idx in 1:length(all_returns)) {
      asset_name <- all_asset_names[asset_idx]
      result_key <- paste(asset_name, model_name, window_id, sep = "_")
      
      # Check if model converged for this window
      if (result_key %in% names(all_results)) {
        fit_result <- all_results[[result_key]]
        
        if (!is.null(fit_result$residuals)) {
          residuals_vec <- as.numeric(fit_result$residuals)
          
          # Save residuals from this window
          residuals_df <- data.frame(residuals = residuals_vec)
          residuals_file <- file.path(model_dir, paste0(asset_name, "_TSCV_window", window_id, "_residuals.csv"))
          write.csv(residuals_df, residuals_file, row.names = FALSE)
          
          cat("    Saved", length(residuals_vec), "residuals for", asset_name, "-", model_name, "\n")
        }
      }
    }
  }
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n5. Saving results...\n")

# Save model summary
summary_file <- file.path(OUTPUT_PATHS$garch_fitting, "model_summary.csv")
write.csv(model_summary, summary_file, row.names = FALSE)
cat("Model summary saved to:", summary_file, "\n")

# Save window-specific summaries
for (window_id in unique_windows) {
  window_summary <- model_summary[model_summary$window_id == window_id, ]
  window_garch_dir <- get_window_path(OUTPUT_PATHS$garch_fitting, window_id)
  if (!dir.exists(window_garch_dir)) {
    dir.create(window_garch_dir, recursive = TRUE)
  }
  window_summary_file <- file.path(window_garch_dir, "window_summary.csv")
  write.csv(window_summary, window_summary_file, row.names = FALSE)
}

# Save detailed results
detailed_file <- file.path(OUTPUT_PATHS$garch_fitting, "detailed_results.rds")
saveRDS(all_results, detailed_file)
cat("Detailed results saved to:", detailed_file, "\n")

# =============================================================================
# PERFORMANCE SUMMARY
# =============================================================================

# Stop parallel processing
if (TSCV_CONFIG$parallel_enabled && exists("cl")) {
  stopCluster(cl)
  registerDoSEQ()
}

end_time <- Sys.time()
execution_time <- end_time - start_time

cat("\n=== TS CV GARCH FITTING SUMMARY ===\n")
cat("Execution time:", round(as.numeric(execution_time, units = "mins"), 2), "minutes\n")
cat("Assets processed:", length(all_asset_names), "\n")
cat("Models fitted:", length(manual_models), "\n")
cat("CV Windows:", length(unique_windows), "\n")
cat("Total combinations:", length(all_asset_names) * length(manual_models) * length(unique_windows), "\n")
cat("Successful fits:", nrow(model_summary), "\n")
cat("Success rate:", round(nrow(model_summary) / (length(all_asset_names) * length(manual_models) * length(unique_windows)) * 100, 2), "%\n")
cat("\nTS CV Configuration:\n")
cat("  Window size:", TSCV_CONFIG$window_size, "(65%)\n")
cat("  Step size:", TSCV_CONFIG$step_size, "(10%)\n")
cat("  Number of windows:", length(unique_windows), "\n")
cat("\nResults saved to:", OUTPUT_BASE, "\n")
cat("Residuals saved to:", OUTPUT_PATHS$residuals, "\n")
cat("===================================\n")

# Final garbage collection
gc()

cat("\nTS CV GARCH fitting completed successfully.\n")
