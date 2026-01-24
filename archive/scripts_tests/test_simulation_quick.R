# Quick Test Run - Reduced Assets and Simulation Paths
# This script tests the pipeline with minimal data to verify everything works

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
  source("scripts/utils/standardize_residuals.R")
  source("scripts/utils/return_forecast_evaluation.R")
}, error = function(e) {
  cat("ERROR: Failed to load utility scripts:", e$message, "\n")
  quit(status = 1)
})

# Display current configuration and engine selection
print_config()
engine <- get_engine()
cat("Using engine:", engine, "\n\n")

cat("=== QUICK TEST RUN ===\n")
cat("Reduced assets and simulation paths for fast verification\n\n")

# Load centralized seed configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
  set.seed(REPRODUCIBILITY_SEED)
  cat("Using reproducibility seed:", REPRODUCIBILITY_SEED, "\n")
} else {
  set.seed(123)
  cat("Using fallback seed: 123\n")
}

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

# TEST: Use only 1 equity and 1 FX asset
equity_tickers <- c("NVDA")  # Just one equity
fx_names <- c("EURUSD")      # Just one FX

cat("TEST MODE: Using only", length(equity_tickers), "equity and", length(fx_names), "FX assets\n")

# Convert price series to XTS objects
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

# Calculate log returns
CalculateReturns <- function(x) {
  if (inherits(x, "xts")) {
    diff(log(x))
  } else {
    diff(log(as.numeric(x)))
  }
}

equity_returns <- lapply(equity_xts, function(x) CalculateReturns(x)[-1, ])
fx_returns     <- lapply(fx_xts,     function(x) diff(log(x))[-1, ])

# Model Configuration - TEST: Use only 2 models
cat("TEST MODE: Using only 2 model configurations\n")
model_configs <- list(
  sGARCH_norm  = list(model = "sGARCH", distribution = "norm", submodel = NULL),
  sGARCH_sstd  = list(model = "sGARCH", distribution = "sstd", submodel = NULL)
)

# Data Splitting
get_split_index <- function(x, split_ratio = 0.65) {
  return(floor(nrow(x) * split_ratio))
}

fx_train_returns <- lapply(fx_returns, function(x) x[1:get_split_index(x)])
fx_test_returns  <- lapply(fx_returns, function(x) x[(get_split_index(x) + 1):nrow(x)])

equity_train_returns <- lapply(equity_returns, function(x) x[1:get_split_index(x)])
equity_test_returns  <- lapply(equity_returns, function(x) x[(get_split_index(x) + 1):nrow(x)])

# Load NF Residuals
cat("Loading NF residuals...\n")

tryCatch({
  nf_dirs <- c("outputs/manual/nf_models", "nf_generated_residuals")
  nf_files <- c()
  for (dir in nf_dirs) {
    if (dir.exists(dir)) {
      nf_files <- c(nf_files, list.files(dir, pattern = "*_synthetic_residuals.csv", full.names = TRUE))
    }
  }
  
  if (length(nf_files) == 0) {
    cat("WARNING: No NF residual files found\n")
    nf_residuals_map <- list()
  } else {
    nf_residuals_map <- list()
    
    for (f in nf_files) {
      fname <- basename(f)
      # Parse model and asset from filename (format: MODEL_ASSET_synthetic_residuals.csv)
      fname_clean <- gsub("_synthetic_residuals\\.csv$", "", fname)
      parts <- strsplit(fname_clean, "_")[[1]]
      
      if (length(parts) >= 2) {
        model_part <- parts[1]
        asset_part <- paste(parts[-1], collapse = "_")
        
        # Create multiple possible keys for flexible lookup
        possible_keys <- c(
          paste0(model_part, "_", asset_part),
          paste0(model_part, "_fx_", asset_part),
          paste0(model_part, "_equity_", asset_part),
          paste0(model_part, "_", asset_part, "_residuals_synthetic"),
          fname_clean
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
          
          residual_values <- as.numeric(residual_values)
          residual_values <- residual_values[!is.na(residual_values)]
          if (length(residual_values) > 0) {
            tryCatch({
              residual_values <- standardize_residuals(residual_values, verify = TRUE)
            }, error = function(e) {
              cat("WARNING: Failed to standardize:", e$message, "\n")
              next
            })
          }
          
          # Store under all possible keys
          for (key in possible_keys) {
            nf_residuals_map[[key]] <- residual_values
          }
        }, error = function(e) {
          cat("WARNING: Failed to load:", fname, ":", e$message, "\n")
        })
      }
    }
  }
  
  cat("OK: Loaded", length(nf_residuals_map), "NF residual files\n")
  
}, error = function(e) {
  cat("ERROR: Failed to load NF residuals:", e$message, "\n")
  quit(status = 1)
})

# Modified fit_nf_garch with REDUCED PATHS for testing
fit_nf_garch_test <- function(asset_name, train_returns, test_returns, model_config, nf_resid) {
  cat("  [TEST] Starting fit_nf_garch for", asset_name, model_config[["model"]], "\n")
  
  tryCatch({
    fit <- engine_fit(
      model = model_config[["model"]], 
      returns = train_returns, 
      dist = model_config[["distribution"]], 
      submodel = model_config[["submodel"]], 
      engine = engine
    )
    
    if (!engine_converged(fit)) {
      cat("ERROR: Fit failed for", asset_name, model_config[["model"]], "\n")
      return(NULL)
    }
    
    n_sim <- length(test_returns)
    if (length(nf_resid) < n_sim) {
      cat("WARNING: NF residuals too short\n")
      return(NULL)
    }
    
    nf_resid_vec <- as.numeric(head(nf_resid, n_sim))
    nf_resid_vec <- nf_resid_vec[!is.na(nf_resid_vec)]
    if (length(nf_resid_vec) < n_sim) {
      cat("WARNING: NF residuals contain NAs\n")
      return(NULL)
    }
    
    if (!is_standardized(nf_resid_vec)) {
      nf_resid_vec <- standardize_residuals(nf_resid_vec, verify = TRUE)
    }
    
    # Evaluate return forecasts using multiple paths on TEST data
    cat("  [DEBUG] Starting evaluate_return_forecasts (this may take a while for 10 paths)...\n")
    eval_result <- tryCatch({
      result <- evaluate_return_forecasts(
        fit = fit,
        nf_residuals = nf_resid_vec,
        actual_returns = test_returns,
        horizon = length(test_returns),
        model_type = model_config[["model"]],
        submodel = model_config[["submodel"]],
        engine = engine,
        n_paths = 10  # TEST: Reduced from 1000 to 10
      )
      cat("  [DEBUG] evaluate_return_forecasts completed\n")
      
      # Log if result is invalid
      if (is.null(result) || is.na(result$mse) || result$n_valid_paths == 0) {
        cat("WARNING: Evaluation returned invalid result for", asset_name, model_config[["model"]], 
            " - MSE=", ifelse(is.null(result), "NULL", result$mse),
            ", NPaths=", ifelse(is.null(result), "NULL", result$n_valid_paths), "\n")
      }
      
      return(result)
    }, error = function(e) {
      cat("ERROR: Evaluation failed for", asset_name, model_config[["model"]], ": ", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(eval_result) || is.na(eval_result$mse)) {
      cat("  [DEBUG] Evaluation result is NULL or invalid\n")
      return(NULL)
    }
    cat("  [DEBUG] Evaluation result valid, MSE:", eval_result$mse, "\n")
    
    # Get model information
    cat("  [DEBUG] Getting information criteria...\n")
    ic <- engine_infocriteria(fit)
    cat("  [DEBUG] Information criteria retrieved\n")
    
    cat("  [DEBUG] Creating result data.frame...\n")
    result_df <- data.frame(
      Model = model_config[["model"]],
      Distribution = model_config[["distribution"]],
      Asset = asset_name,
      AIC = ic["AIC"],
      BIC = ic["BIC"],
      LogLikelihood = ic["LogLikelihood"],
      MSE = eval_result$mse,
      MAE = eval_result$mae,
      PredictiveLogLik = eval_result$loglik,
      NPaths = eval_result$n_valid_paths,
      SplitType = "Chrono"
    )
    cat("  [DEBUG] fit_nf_garch completed successfully for", asset_name, model_config[["model"]], "\n")
    return(result_df)
  }, error = function(e) {
    cat("ERROR in fit_nf_garch_test:", conditionMessage(e), "\n")
    cat("  Traceback:\n")
    print(traceback())
    return(NULL)
  })
  
  # This should never be reached, but just in case
  cat("  [WARNING] Function reached end without explicit return\n")
  return(NULL)
}

# Run test analysis
cat("=== CHRONOLOGICAL SPLIT NF-GARCH ANALYSIS (TEST MODE) ===\n")
nf_results_chrono <- list()

for (config_name in names(model_configs)) {
  cfg <- model_configs[[config_name]]
  cat("Processing", config_name, "(Chrono Split)...\n")
  
  # FX
  for (asset in names(fx_returns)) {
    base_model <- cfg[["model"]]
    possible_keys <- c(
      paste0(config_name, "_fx_", asset, "_residuals_synthetic"),
      paste0(base_model, "_", asset)
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
    r <- fit_nf_garch_test(asset, fx_train_returns[[asset]], fx_test_returns[[asset]], cfg, nf_residuals_map[[key]])
    if (!is.null(r)) {
      cat("  [DEBUG] Result type before adding:", class(r), ", is.data.frame:", is.data.frame(r), "\n")
      nf_results_chrono[[length(nf_results_chrono) + 1]] <- r
      cat("  [DEBUG] After adding, type:", class(nf_results_chrono[[length(nf_results_chrono)]]), "\n")
      cat("  [OK] Result added\n")
    }
  }
  
  # Equity
  for (asset in names(equity_returns)) {
    base_model <- cfg[["model"]]
    possible_keys <- c(
      paste0(config_name, "_equity_", asset, "_residuals_synthetic"),
      paste0(base_model, "_", asset)
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
    r <- fit_nf_garch_test(asset, equity_train_returns[[asset]], equity_test_returns[[asset]], cfg, nf_residuals_map[[key]])
    if (!is.null(r)) {
      nf_results_chrono[[length(nf_results_chrono) + 1]] <- r
      cat("  [OK] Result added\n")
    }
  }
}

# Create results
cat("\n=== CREATING RESULTS ===\n")
cat("[DEBUG] nf_results_chrono length:", length(nf_results_chrono), "\n")

if (length(nf_results_chrono) > 0) {
  cat("[DEBUG] Checking each result:\n")
  for (i in 1:length(nf_results_chrono)) {
    x <- nf_results_chrono[[i]]
    cat("  Result", i, ":\n")
    cat("    is.null:", is.null(x), "\n")
    cat("    is.data.frame:", is.data.frame(x), "\n")
    cat("    inherits data.frame:", inherits(x, "data.frame"), "\n")
    if (!is.null(x)) {
      cat("    class:", paste(class(x), collapse=", "), "\n")
      cat("    nrow:", ifelse(is.data.frame(x) || inherits(x, "data.frame"), nrow(x), "N/A"), "\n")
      if (is.data.frame(x) || inherits(x, "data.frame")) {
        cat("    columns:", paste(names(x), collapse=", "), "\n")
      }
    }
  }
  
  valid_indices <- sapply(1:length(nf_results_chrono), function(i) {
    x <- nf_results_chrono[[i]]
    !is.null(x) && (is.data.frame(x) || inherits(x, "data.frame")) && nrow(x) > 0
  })
  cat("[DEBUG] Valid indices:", sum(valid_indices), "out of", length(nf_results_chrono), "\n")
  nf_results_chrono <- nf_results_chrono[valid_indices]
  
  if (length(nf_results_chrono) > 0) {
    nf_results_df <- bind_rows(nf_results_chrono)
    cat("[DEBUG] Combined data.frame rows:", nrow(nf_results_df), "\n")
    
    if (nrow(nf_results_df) > 0) {
      # Save test results
      if (!dir.exists("results/consolidated")) {
        dir.create("results/consolidated", recursive = TRUE, showWarnings = FALSE)
      }
      output_file <- "results/consolidated/NF_GARCH_Results_TEST.xlsx"
      
      wb <- createWorkbook()
      addWorksheet(wb, "Test_Results")
      writeData(wb, "Test_Results", nf_results_df)
      saveWorkbook(wb, output_file, overwrite = TRUE)
      
      cat("\n=== TEST COMPLETE ===\n")
      cat("✓ Results saved to:", output_file, "\n")
      cat("✓ Total results:", nrow(nf_results_df), "\n")
      cat("✓ Columns:", paste(names(nf_results_df), collapse=", "), "\n")
      print(nf_results_df)
    } else {
      cat("ERROR: Combined data.frame is empty\n")
    }
  } else {
    cat("ERROR: No valid results after filtering\n")
  }
} else {
  cat("ERROR: No results generated\n")
}

cat("\n=== TEST RUN COMPLETE ===\n")
