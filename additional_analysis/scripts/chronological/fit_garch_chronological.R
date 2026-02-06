# Chronological Split GARCH Fitting
# Pure 65/35 chronological split - NO cross-validation
# Designed to ensure strict data separation for NF-GARCH validation

# Load required libraries
library(xts)
library(PerformanceAnalytics)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Load centralized seed configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
  set.seed(REPRODUCIBILITY_SEED)
} else {
  set.seed(123)  # Fallback if config not available
}

# Load chronological split configuration
source("scripts/config/chrono_split_config.R")

# Load model configuration
source("scripts/manual/manual_optimized_config.R")

# Load manual engine
source("scripts/engines/engine_selector.R")

# Set up error handling and timing
options(warn = 1)
start_time <- Sys.time()

cat("=== CHRONOLOGICAL SPLIT GARCH FITTING ===\n")
cat("Start time:", as.character(start_time), "\n")
print_chrono_config_summary()

# Initialize output directories
initialize_chrono_directories()

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
cat("Model configurations prepared for", length(manual_models), "models\n")

# =============================================================================
# MODEL FITTING FUNCTION
# =============================================================================

# Fit GARCH model with manual engine
fit_garch_chronological <- function(returns_data, asset_name, model_name) {
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
      
      # DEBUG: Print extracted residuals
      if (model_name == "eGARCH") {
        cat("DEBUG fit_garch_chronological: After engine_residuals for", asset_name, model_name, "\n")
        cat("  residuals mean=", mean(residuals), "std=", sd(residuals), "min=", min(residuals), "max=", max(residuals), "\n")
        cat("  garch_fit$residuals mean=", mean(garch_fit$residuals), "\n")
        cat("  garch_fit$std_residuals mean=", mean(garch_fit$std_residuals), "\n")
      }
      
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
# MAIN FITTING PROCESS (CHRONOLOGICAL SPLIT ONLY)
# =============================================================================

cat("\n3. Fitting GARCH models with chronological 65/35 split...\n")

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
  
  # Get chronological split indices
  n_obs <- length(returns_data)
  split_info <- get_chrono_split_indices(n_obs, TRAIN_RATIO)
  
  cat("  Total observations:", n_obs, "\n")
  cat("  Training set:", split_info$train_start, "-", split_info$train_end, 
      "(", split_info$train_size, "obs)\n")
  cat("  Test set:", split_info$test_start, "-", split_info$test_end, 
      "(", split_info$test_size, "obs)\n")
  
  # Extract training data only
  train_data <- returns_data[split_info$train_start:split_info$train_end]
  
  for (model_name in manual_models) {
    cat("  Fitting model:", model_name, "(training set only)\n")
    
    # Fit GARCH on training set
    fit_result <- fit_garch_chronological(train_data, asset_name, model_name)
    
    # Store results
    if (!is.null(fit_result)) {
      result_key <- paste(asset_name, model_name, sep = "_")
      all_results[[result_key]] <- fit_result
      
      # Add to summary
      model_summary <- rbind(model_summary, data.frame(
        asset = asset_name,
        model = model_name,
        split_type = "chronological",
        train_size = split_info$train_size,
        test_size = split_info$test_size,
        loglik = fit_result$stats$loglik,
        aic = fit_result$stats$aic,
        bic = fit_result$stats$bic,
        n_params = fit_result$stats$n_params,
        convergence = TRUE
      ))
      
      cat("    Fit successful: LogLik =", round(fit_result$stats$loglik, 2), 
          ", AIC =", round(fit_result$stats$aic, 2), "\n")
    }
  }
  
  # Memory management
  gc()
}

# =============================================================================
# EXTRACT AND SAVE RESIDUALS FOR NF TRAINING
# =============================================================================

cat("\n4. Extracting residuals from training set fits...\n")

# Create residuals directory structure - construct directly
if (exists("OUTPUT_BASE") && !is.null(OUTPUT_BASE) && OUTPUT_BASE != "") {
  residuals_dir <- paste0(OUTPUT_BASE, "/residuals_by_model")
} else {
  residuals_dir <- "outputs/chronological/residuals_by_model"
}

cat("Residuals directory:", residuals_dir, "\n")
cat("Models to extract:", paste(manual_models, collapse=", "), "\n")

# Create directory if it doesn't exist
if (!dir.exists(residuals_dir)) {
  dir.create(residuals_dir, recursive = TRUE)
  cat("Created residuals directory\n")
}

for (model_name in manual_models) {
  # Skip if model_name is empty or NULL
  if (is.null(model_name) || is.na(model_name) || model_name == "") {
    cat("Skipping empty model name\n")
    next
  }
  
  model_dir <- file.path(residuals_dir, model_name)
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }
  
  for (asset_idx in 1:length(all_returns)) {
    asset_name <- all_asset_names[asset_idx]
    result_key <- paste(asset_name, model_name, sep = "_")
    
    # Check if model converged
    if (result_key %in% names(all_results)) {
      fit_result <- all_results[[result_key]]
      
      # Use standardized residuals for NF training (CRITICAL FIX)
      if (!is.null(fit_result$std_residuals)) {
        residuals_vec <- as.numeric(fit_result$std_residuals)
        
        # Verify standardization
        res_mean <- mean(residuals_vec)
        res_std <- sd(residuals_vec)
        cat("  [", asset_name, "-", model_name, "] Residuals: mean=", sprintf("%.4f", res_mean), 
            " std=", sprintf("%.4f", res_std), "\n", sep="")
        
        # Save STANDARDIZED residuals from training set for NF
        residuals_df <- data.frame(residuals = residuals_vec)
        residuals_file <- file.path(model_dir, paste0(asset_name, "_Chronological_residuals.csv"))
        write.csv(residuals_df, residuals_file, row.names = FALSE)
        
        cat("  Saved", length(residuals_vec), "residuals for", asset_name, "-", model_name, "\n")
      }
    }
  }
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n5. Saving results...\n")

# Construct paths directly
if (exists("OUTPUT_BASE") && !is.null(OUTPUT_BASE) && OUTPUT_BASE != "") {
  garch_fitting_dir <- paste0(OUTPUT_BASE, "/garch_fitting")
} else {
  garch_fitting_dir <- "outputs/chronological/garch_fitting"
}

# Create directory if needed
if (!dir.exists(garch_fitting_dir)) {
  dir.create(garch_fitting_dir, recursive = TRUE)
}

# Save model summary
summary_file <- paste0(garch_fitting_dir, "/model_summary.csv")
write.csv(model_summary, summary_file, row.names = FALSE)
cat("Model summary saved to:", summary_file, "\n")

# Save detailed results
detailed_file <- paste0(garch_fitting_dir, "/detailed_results.rds")
saveRDS(all_results, detailed_file)
cat("Detailed results saved to:", detailed_file, "\n")

# =============================================================================
# PERFORMANCE SUMMARY
# =============================================================================

end_time <- Sys.time()
execution_time <- end_time - start_time

cat("\n=== CHRONOLOGICAL SPLIT GARCH FITTING SUMMARY ===\n")
cat("Execution time:", round(as.numeric(execution_time, units = "mins"), 2), "minutes\n")
cat("Assets processed:", length(all_asset_names), "\n")
cat("Models fitted:", length(manual_models), "\n")
cat("Total combinations:", length(all_asset_names) * length(manual_models), "\n")
cat("Successful fits:", nrow(model_summary), "\n")
cat("Success rate:", round(nrow(model_summary) / (length(all_asset_names) * length(manual_models)) * 100, 2), "%\n")
cat("\nSplit Configuration:\n")
cat("  Training ratio:", TRAIN_RATIO, "(65%)\n")
cat("  Test ratio:", TEST_RATIO, "(35%)\n")
cat("  Cross-validation:", ifelse(USE_TSCV_FOR_MODEL_SELECTION, "Enabled", "Disabled"), "\n")
cat("\nResults saved to:", OUTPUT_BASE, "\n")
cat("Residuals saved to:", OUTPUT_PATHS$residuals, "\n")
cat("================================================\n")

# Final garbage collection
gc()

cat("\nChronological GARCH fitting completed successfully.\n")
