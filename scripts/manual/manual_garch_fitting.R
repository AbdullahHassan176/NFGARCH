# Manual Optimized GARCH Fitting
# Implements asset reduction (50%), model reduction (40%), and CV optimization (60%)
# Designed for manual execution in R Studio

# Load required libraries
library(xts)
library(PerformanceAnalytics)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(parallel)
library(doParallel)

# Load manual optimization configuration
source("scripts/manual/manual_optimized_config.R")

# Load manual engine
source("scripts/engines/engine_selector.R")

# Set up error handling and timing
options(warn = 1)
start_time <- Sys.time()

cat("=== MANUAL OPTIMIZED GARCH FITTING ===\n")
cat("Start time:", as.character(start_time), "\n")
print_optimization_summary()

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

# Use optimized asset selection (50% reduction)
equity_tickers <- c("NVDA", "MSFT", "AMZN")  # 3 most representative equity assets
fx_names <- c("EURUSD", "GBPUSD", "USDZAR")  # 3 most representative FX assets

cat("Using optimized assets:", paste(c(equity_tickers, fx_names), collapse = ", "), "\n")

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
# OPTIMIZED MODEL CONFIGURATION
# =============================================================================

cat("\n2. Setting up optimized model configuration...\n")

# Use optimized model selection (40% reduction)
manual_models <- get_manual_models()
manual_model_config <- get_manual_model_config()

cat("Using optimized models:", paste(manual_models, collapse = ", "), "\n")

# GARCH model configurations (no need for specs with manual engine)
# Models will be fit directly using engine_fit()

cat("Model configurations prepared for", length(manual_models), "models\n")

# =============================================================================
# OPTIMIZED TIME-SERIES CROSS-VALIDATION
# =============================================================================

cat("\n3. Setting up optimized time-series cross-validation...\n")

# Get optimized CV configuration
cv_config <- get_manual_cv_config()

cat("CV Configuration:")
cat("  Folds:", cv_config$n_folds, "(reduced from 5)")
cat("  Window size:", cv_config$window_size, "(reduced from 0.65)")
cat("  Step size:", cv_config$step_size, "(increased from 0.1)")
cat("  Forecast horizon:", cv_config$forecast_horizon, "(reduced from 20)")

# Setup parallel processing for CV
if (cv_config$parallel_enabled) {
  cl <- makeCluster(cv_config$parallel_cores)
  registerDoParallel(cl)
  cat("\nParallel processing enabled with", cv_config$parallel_cores, "cores\n")
}

# =============================================================================
# OPTIMIZED MODEL FITTING FUNCTION
# =============================================================================

# Optimized model fitting with manual engine
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
# OPTIMIZED CROSS-VALIDATION FUNCTION
# =============================================================================

# Optimized time-series cross-validation with reduced complexity
# Note: garch_spec not needed - using manual engine directly
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
    if (cv_config$clear_memory && i %% 5 == 0) {
      gc()
    }
  }
  
  return(cv_results)
}

# =============================================================================
# MAIN FITTING PROCESS
# =============================================================================

cat("\n4. Fitting GARCH models with optimized parameters...\n")

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
    
    # Run optimized cross-validation (no spec needed for manual engine)
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
  if (cv_config$clear_memory) {
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
residuals_dir <- "outputs/manual/residuals_by_model"
if (!dir.exists(residuals_dir)) {
  dir.create(residuals_dir, recursive = TRUE)
}

# Create residuals by model
for (model_name in manual_models) {
  model_dir <- file.path(residuals_dir, model_name)
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }
  
  for (asset_name in all_asset_names) {
    result_key <- paste(asset_name, model_name, sep = "_")
    if (result_key %in% names(all_results)) {
      # Extract residuals from all windows
      all_residuals <- c()
      for (window_result in all_results[[result_key]]) {
        if (!is.null(window_result$fit_result)) {
          all_residuals <- c(all_residuals, as.numeric(window_result$fit_result$residuals))
        }
      }
      
      if (length(all_residuals) > 0) {
        # Save residuals
        residuals_df <- data.frame(residuals = all_residuals)
        residuals_file <- file.path(model_dir, paste0(asset_name, "_Manual_Optimized_residuals.csv"))
        write.csv(residuals_df, residuals_file, row.names = FALSE)
      }
    }
  }
}

# =============================================================================
# PERFORMANCE SUMMARY
# =============================================================================

end_time <- Sys.time()
execution_time <- end_time - start_time

cat("\n=== OPTIMIZATION PERFORMANCE SUMMARY ===\n")
cat("Execution time:", round(as.numeric(execution_time, units = "mins"), 2), "minutes\n")
cat("Assets processed:", length(all_asset_names), "(50% reduction from 12)\n")
cat("Models fitted:", length(manual_models), "(40% reduction from 5)\n")
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

cat("\nOptimized GARCH fitting completed successfully!\n")
cat("Results saved to outputs/manual/garch_fitting/\n")
cat("Residuals saved to outputs/manual/residuals_by_model/\n")
cat("===============================================\n")

