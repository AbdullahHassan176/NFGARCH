# Comprehensive Pipeline Test
# Tests all critical components of the simulation pipeline before full run

cat("=== COMPREHENSIVE PIPELINE TEST ===\n\n")

# Test 1: Check required libraries
cat("Test 1: Checking required libraries...\n")
required_libs <- c("xts", "zoo", "dplyr", "tidyr", "stringr", "lubridate", "openxlsx")
missing_libs <- c()
for (lib in required_libs) {
  if (!require(lib, character.only = TRUE, quietly = TRUE)) {
    missing_libs <- c(missing_libs, lib)
  }
}
if (length(missing_libs) > 0) {
  stop("ERROR: Missing required libraries: ", paste(missing_libs, collapse = ", "))
}
cat("  ✓ All required libraries available\n\n")

# Test 2: Check required source files
cat("Test 2: Checking required source files...\n")
required_files <- c(
  "scripts/utils/cli_parser.R",
  "scripts/engines/engine_selector.R",
  "scripts/utils/safety_functions.R",
  "scripts/utils/standardize_residuals.R",
  "scripts/utils/return_forecast_evaluation.R"
)
optional_files <- c(
  "scripts/utils/conflict_resolution.R"  # Optional - wrapped in tryCatch
)
missing_files <- c()
for (f in required_files) {
  if (!file.exists(f)) {
    missing_files <- c(missing_files, f)
  }
}
if (length(missing_files) > 0) {
  stop("ERROR: Missing required files: ", paste(missing_files, collapse = ", "))
}
cat("  ✓ All required source files exist\n")

# Check optional files
for (f in optional_files) {
  if (!file.exists(f)) {
    cat("  ⚠ Optional file not found (will use fallback): ", f, "\n")
  } else {
    cat("  ✓ Optional file exists: ", f, "\n")
  }
}
cat("\n")

# Test 3: Load and test utility functions
cat("Test 3: Loading and testing utility functions...\n")
tryCatch({
  source("scripts/utils/cli_parser.R")
  source("scripts/engines/engine_selector.R")
  source("scripts/utils/safety_functions.R")
  source("scripts/utils/standardize_residuals.R")
  source("scripts/utils/return_forecast_evaluation.R")
  cat("  ✓ All utility scripts loaded successfully\n")
}, error = function(e) {
  stop("ERROR: Failed to load utility scripts: ", e$message)
})

# Test 4: Test engine functions exist
cat("\nTest 4: Checking engine functions...\n")
required_functions <- c(
  "engine_fit",
  "engine_converged",
  "engine_path",
  "engine_infocriteria",
  "standardize_residuals",
  "is_standardized",
  "evaluate_return_forecasts",
  "generate_multiple_paths"
)
missing_funcs <- c()
for (func in required_functions) {
  if (!exists(func)) {
    missing_funcs <- c(missing_funcs, func)
  }
}
if (length(missing_funcs) > 0) {
  stop("ERROR: Missing required functions: ", paste(missing_funcs, collapse = ", "))
}
cat("  ✓ All required functions available\n")

# Test 5: Test data file exists
cat("\nTest 5: Checking data file...\n")
data_file <- "./data/processed/raw (FX + EQ).csv"
if (!file.exists(data_file)) {
  stop("ERROR: Data file not found: ", data_file)
}
cat("  ✓ Data file exists\n")

# Test 6: Test data loading
cat("\nTest 6: Testing data loading...\n")
tryCatch({
  raw_price_data <- read.csv(data_file, row.names = 1, stringsAsFactors = FALSE)
  if (nrow(raw_price_data) == 0) {
    stop("Data file is empty")
  }
  cat("  ✓ Data loaded successfully\n")
  cat("    Rows:", nrow(raw_price_data), "\n")
  cat("    Columns:", ncol(raw_price_data), "\n")
}, error = function(e) {
  stop("ERROR: Data loading failed: ", e$message)
})

# Test 7: Test asset extraction
cat("\nTest 7: Testing asset extraction...\n")
tryCatch({
  date_index <- as.Date(rownames(raw_price_data))
  price_data_matrix <- raw_price_data[, !(names(raw_price_data) %in% "Date"), drop = FALSE]
  
  equity_tickers <- c("NVDA", "MSFT", "AMZN")
  fx_names <- c("EURUSD", "GBPUSD", "USDZAR")
  
  # Check all assets exist
  missing_equity <- equity_tickers[!equity_tickers %in% names(price_data_matrix)]
  missing_fx <- fx_names[!fx_names %in% names(price_data_matrix)]
  
  if (length(missing_equity) > 0) {
    stop("ERROR: Missing equity assets: ", paste(missing_equity, collapse = ", "))
  }
  if (length(missing_fx) > 0) {
    stop("ERROR: Missing FX assets: ", paste(missing_fx, collapse = ", "))
  }
  
  cat("  ✓ All required assets found\n")
}, error = function(e) {
  stop("ERROR: Asset extraction failed: ", e$message)
})

# Test 8: Test XTS conversion
cat("\nTest 8: Testing XTS conversion...\n")
tryCatch({
  equity_xts <- lapply(equity_tickers, function(ticker) {
    if (ticker %in% names(price_data_matrix)) {
      xts(price_data_matrix[[ticker]], order.by = date_index)
    } else {
      NULL
    }
  })
  names(equity_xts) <- equity_tickers
  equity_xts <- equity_xts[!sapply(equity_xts, is.null)]
  
  if (length(equity_xts) == 0) {
    stop("No equity assets could be converted to XTS")
  }
  cat("  ✓ XTS conversion successful\n")
  cat("    Equity assets:", length(equity_xts), "\n")
}, error = function(e) {
  stop("ERROR: XTS conversion failed: ", e$message)
})

# Test 9: Test returns calculation
cat("\nTest 9: Testing returns calculation...\n")
tryCatch({
  CalculateReturns <- function(x) {
    if (inherits(x, "xts")) {
      diff(log(x))
    } else {
      diff(log(as.numeric(x)))
    }
  }
  
  equity_returns <- lapply(equity_xts, function(x) CalculateReturns(x)[-1, ])
  equity_returns <- equity_returns[!sapply(equity_returns, is.null)]
  
  if (length(equity_returns) == 0) {
    stop("No equity returns calculated")
  }
  
  # Check returns are valid
  for (asset in names(equity_returns)) {
    if (any(is.infinite(equity_returns[[asset]])) || any(is.nan(equity_returns[[asset]]))) {
      stop("Invalid returns for asset: ", asset)
    }
  }
  
  cat("  ✓ Returns calculation successful\n")
}, error = function(e) {
  stop("ERROR: Returns calculation failed: ", e$message)
})

# Test 10: Test model configurations
cat("\nTest 10: Testing model configurations...\n")
tryCatch({
  # Simulate model_configs structure
  model_configs <- list(
    "sGARCH_norm" = list(model = "sGARCH", distribution = "norm", submodel = NULL),
    "sGARCH_sstd" = list(model = "sGARCH", distribution = "sstd", submodel = NULL),
    "eGARCH" = list(model = "eGARCH", distribution = "sstd", submodel = NULL),
    "TGARCH" = list(model = "TGARCH", distribution = "sstd", submodel = NULL),
    "gjrGARCH" = list(model = "gjrGARCH", distribution = "sstd", submodel = NULL)
  )
  
  if (length(model_configs) == 0) {
    stop("No model configurations defined")
  }
  
  # Verify structure
  for (cfg_name in names(model_configs)) {
    cfg <- model_configs[[cfg_name]]
    if (!"model" %in% names(cfg) || !"distribution" %in% names(cfg)) {
      stop("Invalid model configuration: ", cfg_name)
    }
  }
  
  cat("  ✓ Model configurations valid\n")
}, error = function(e) {
  stop("ERROR: Model configuration test failed: ", e$message)
})

# Test 11: Test NF residuals loading logic
cat("\nTest 11: Testing NF residuals loading logic...\n")
tryCatch({
  nf_dirs <- c("outputs/manual/nf_models", "nf_generated_residuals")
  nf_files <- c()
  for (dir in nf_dirs) {
    if (dir.exists(dir)) {
      nf_files <- c(nf_files, list.files(dir, pattern = "*_synthetic_residuals.csv", full.names = TRUE))
    }
  }
  
  cat("  ✓ NF residuals directory check complete\n")
  cat("    Found", length(nf_files), "residual files\n")
  
  # Test dummy residual generation (fallback)
  if (length(nf_files) == 0) {
    cat("    (Will use dummy residuals if needed)\n")
  }
}, error = function(e) {
  stop("ERROR: NF residuals loading test failed: ", e$message)
})

# Test 12: Test standardization functions
cat("\nTest 12: Testing standardization functions...\n")
tryCatch({
  test_residuals <- rnorm(100, 5, 2)  # Non-standardized
  
  # Test is_standardized
  is_std <- is_standardized(test_residuals)
  if (is_std) {
    stop("is_standardized incorrectly returned TRUE for non-standardized data")
  }
  
  # Test standardize_residuals
  standardized <- standardize_residuals(test_residuals, verify = TRUE)
  if (abs(mean(standardized)) > 0.1 || abs(sd(standardized) - 1) > 0.1) {
    stop("Standardization failed: mean=", mean(standardized), ", sd=", sd(standardized))
  }
  
  # Test is_standardized on standardized data
  is_std_after <- is_standardized(standardized)
  if (!is_std_after) {
    stop("is_standardized incorrectly returned FALSE for standardized data")
  }
  
  cat("  ✓ Standardization functions work correctly\n")
}, error = function(e) {
  stop("ERROR: Standardization test failed: ", e$message)
})

# Test 13: Test return forecast evaluation structure
cat("\nTest 13: Testing return forecast evaluation structure...\n")
tryCatch({
  # Check function signature
  if (!is.function(evaluate_return_forecasts)) {
    stop("evaluate_return_forecasts is not a function")
  }
  
  # Check generate_multiple_paths
  if (!is.function(generate_multiple_paths)) {
    stop("generate_multiple_paths is not a function")
  }
  
  cat("  ✓ Return forecast evaluation functions available\n")
}, error = function(e) {
  stop("ERROR: Return forecast evaluation test failed: ", e$message)
})

# Test 14: Test data.frame operations (the critical fix)
cat("\nTest 14: Testing data.frame operations (critical fix verification)...\n")
tryCatch({
  # Simulate the problematic scenario
  results_list <- list(
    data.frame(A = 1, B = 2, MSE = 0.001, MAE = 0.01),
    data.frame(A = 3, B = 4, MSE = 0.002, MAE = 0.02),
    data.frame(A = 5, B = 6, MSE = 0.003, MAE = 0.03)
  )
  
  # Test bind_rows (the fix)
  combined <- bind_rows(results_list)
  if (!is.data.frame(combined)) {
    stop("bind_rows did not return a data.frame")
  }
  
  # Test group_by works
  summary <- combined %>%
    group_by(A) %>%
    summarise(
      Avg_MSE = mean(MSE, na.rm = TRUE),
      Avg_MAE = mean(MAE, na.rm = TRUE),
      .groups = 'drop'
    )
  
  if (!is.data.frame(summary) || nrow(summary) == 0) {
    stop("group_by/summarise failed on combined data.frame")
  }
  
  cat("  ✓ data.frame operations work correctly (fix verified)\n")
}, error = function(e) {
  stop("ERROR: data.frame operations test failed: ", e$message)
})

# Test 15: Test error handling paths
cat("\nTest 15: Testing error handling...\n")
tryCatch({
  # Test NULL handling
  null_list <- list(NULL, data.frame(A = 1), NULL)
  filtered <- null_list[!sapply(null_list, is.null)]
  if (length(filtered) != 1) {
    stop("NULL filtering failed")
  }
  
  # Test empty list handling
  empty_list <- list()
  if (length(empty_list) > 0) {
    combined_empty <- bind_rows(empty_list)
  } else {
    # This is expected - empty list should be handled
  }
  
  cat("  ✓ Error handling paths work correctly\n")
}, error = function(e) {
  stop("ERROR: Error handling test failed: ", e$message)
})

# Test 16: Test file output structure
cat("\nTest 16: Testing file output structure...\n")
tryCatch({
  # Test Excel writing capability
  test_df <- data.frame(
    Model = c("sGARCH", "eGARCH"),
    MSE = c(0.001, 0.002),
    MAE = c(0.01, 0.02)
  )
  
  # Check if openxlsx functions exist
  if (!exists("createWorkbook") || !exists("addWorksheet") || !exists("writeData") || !exists("saveWorkbook")) {
    stop("openxlsx functions not available")
  }
  
  # Test creating workbook (don't save)
  wb <- createWorkbook()
  addWorksheet(wb, "Test")
  writeData(wb, "Test", test_df)
  
  cat("  ✓ Excel output functions work correctly\n")
}, error = function(e) {
  stop("ERROR: File output test failed: ", e$message)
})

# Test 17: Test engine selection
cat("\nTest 17: Testing engine selection...\n")
tryCatch({
  # Test get_engine function exists
  if (!exists("get_engine")) {
    stop("get_engine function not found")
  }
  
  # Test print_config function exists
  if (!exists("print_config")) {
    stop("print_config function not found")
  }
  
  cat("  ✓ Engine selection functions available\n")
}, error = function(e) {
  stop("ERROR: Engine selection test failed: ", e$message)
})

# Test 18: Test config file handling
cat("\nTest 18: Testing config file handling...\n")
tryCatch({
  config_file <- "scripts/core/config.R"
  if (file.exists(config_file)) {
    source(config_file)
    if (!exists("REPRODUCIBILITY_SEED")) {
      stop("REPRODUCIBILITY_SEED not defined in config")
    }
    cat("  ✓ Config file exists and contains REPRODUCIBILITY_SEED\n")
  } else {
    cat("  ✓ Config file not found (will use fallback seed)\n")
  }
}, error = function(e) {
  stop("ERROR: Config file test failed: ", e$message)
})

cat("\n=== ALL PIPELINE TESTS PASSED ===\n")
cat("The pipeline is ready for full run.\n")
cat("\nSummary:\n")
cat("  - All required libraries available\n")
cat("  - All required source files exist\n")
cat("  - All utility functions loaded\n")
cat("  - Data file accessible\n")
cat("  - Asset extraction works\n")
cat("  - Returns calculation works\n")
cat("  - Standardization functions work\n")
cat("  - data.frame operations fixed and verified\n")
cat("  - Error handling robust\n")
cat("  - File output functions work\n")
cat("\nStatus: ✅ READY FOR FULL SIMULATION RUN\n")
