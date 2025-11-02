# Safety Functions for Financial-SDG-GARCH Pipeline
# Provides error handling and safety checks for the pipeline

# Safe file operations
safe_read_csv <- function(file_path, ...) {
  tryCatch({
    if (!file.exists(file_path)) {
      warning("File does not exist: ", file_path)
      return(NULL)
    }
    read.csv(file_path, ...)
  }, error = function(e) {
    warning("Error reading file ", file_path, ": ", conditionMessage(e))
    return(NULL)
  })
}

safe_write_csv <- function(data, file_path, ...) {
  tryCatch({
    # Create directory if it doesn't exist
    dir_path <- dirname(file_path)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    write.csv(data, file_path, ...)
    return(TRUE)
  }, error = function(e) {
    warning("Error writing file ", file_path, ": ", conditionMessage(e))
    return(FALSE)
  })
}

# Safe model fitting (DEPRECATED - rugarch not used)
# Kept for backwards compatibility but will error if called
safe_ugarchfit <- function(spec, data, ...) {
  stop("rugarch engine has been removed. Use manual engine with engine_fit() instead.")
}

# Safe forecasting (DEPRECATED - rugarch not used)
# Kept for backwards compatibility but will error if called
safe_ugarchforecast <- function(fit, n.ahead = 1, ...) {
  stop("rugarch engine has been removed. Use manual engine with engine_forecast() instead.")
}

# =============================================================================
# DATA SPLITTING UTILITIES
# =============================================================================

# Chronological split function (65/35 split)
get_chronological_split <- function(data, split_ratio = 0.65) {
  n <- length(data)
  split_idx <- floor(n * split_ratio)
  
  return(list(
    train = data[1:split_idx],
    test = data[(split_idx + 1):n],
    split_index = split_idx,
    split_ratio = split_ratio
  ))
}

# Time Series Cross-Validation function - OPTIMIZED for speed
get_tscv_splits <- function(data, window_size = 500, step_size = 150, forecast_horizon = 20) {
  n <- length(data)
  splits <- list()
  
  # Calculate optimal number of non-overlapping windows (3-4 windows max)
  max_windows <- 4
  total_available <- n - window_size - forecast_horizon
  optimal_step <- max(step_size, floor(total_available / max_windows))
  
  for (start_idx in seq(1, n - window_size - forecast_horizon, by = optimal_step)) {
    train_end <- start_idx + window_size - 1
    test_start <- start_idx + window_size
    test_end <- start_idx + window_size + forecast_horizon - 1
    
    if (test_end <= n) {
      splits[[length(splits) + 1]] <- list(
        train = data[start_idx:train_end],
        test = data[test_start:test_end],
        window_start = start_idx,
        window_end = train_end,
        test_start = test_start,
        test_end = test_end
      )
    }
    
    # Limit to maximum number of windows for speed
    if (length(splits) >= max_windows) break
  }
  
  return(splits)
}

# Apply analysis to both splits - OPTIMIZED for speed
apply_dual_split_analysis <- function(data, analysis_function, split_name = "analysis", 
                                     window_size = 500, step_size = 150, forecast_horizon = 20) {
  results <- list()
  
  # Chronological split analysis
  cat("Running chronological split analysis for", split_name, "...\n")
  chrono_split <- get_chronological_split(data)
  results$chronological <- analysis_function(chrono_split$train, chrono_split$test, "chronological")
  
  # Time Series Cross-Validation analysis
  cat("Running time series cross-validation analysis for", split_name, "...\n")
  tscv_splits <- get_tscv_splits(data, window_size, step_size, forecast_horizon)
  
  if (length(tscv_splits) > 0) {
    tscv_results <- lapply(tscv_splits, function(split) {
      analysis_function(split$train, split$test, "tscv")
    })
    results$tscv <- tscv_results
    cat("Completed", length(tscv_results), "TS CV windows for", split_name, "\n")
  } else {
    cat("Warning: No valid TS CV splits found for", split_name, "\n")
    results$tscv <- list()
  }
  
  return(results)
}

# Safe directory creation
safe_create_dir <- function(dir_path) {
  tryCatch({
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      return(TRUE)
    }
    return(TRUE)
  }, error = function(e) {
    warning("Error creating directory ", dir_path, ": ", conditionMessage(e))
    return(FALSE)
  })
}

# Safe package loading
safe_library <- function(package_name) {
  tryCatch({
    library(package_name, character.only = TRUE)
    return(TRUE)
  }, error = function(e) {
    warning("Error loading package ", package_name, ": ", conditionMessage(e))
    return(FALSE)
  })
}

# Safe require
safe_require <- function(package_name) {
  tryCatch({
    require(package_name, character.only = TRUE, quietly = TRUE)
  }, error = function(e) {
    warning("Error requiring package ", package_name, ": ", conditionMessage(e))
    return(FALSE)
  })
}

# Data validation functions
validate_data <- function(data, name = "data") {
  if (is.null(data)) {
    stop("Data is NULL: ", name)
  }
  
  if (nrow(data) == 0) {
    warning("Data has 0 rows: ", name)
    return(FALSE)
  }
  
  if (ncol(data) == 0) {
    warning("Data has 0 columns: ", name)
    return(FALSE)
  }
  
  # Check for all NA columns
  na_cols <- sapply(data, function(x) all(is.na(x)))
  if (any(na_cols)) {
    warning("Columns with all NA values: ", paste(names(data)[na_cols], collapse = ", "))
  }
  
  cat("[OK] Data validation passed for:", name, "(", nrow(data), "rows,", ncol(data), "cols)\n")
  return(TRUE)
}

# Model convergence safety
check_model_convergence <- function(fit, model_name = "model") {
  if (is.null(fit)) {
    warning("Model fit is NULL:", model_name)
    return(FALSE)
  }
  
  # Check for convergence
  if (exists("convergence", where = fit)) {
    if (fit$convergence != 0) {
      warning("Model did not converge:", model_name, "convergence code:", fit$convergence)
      return(FALSE)
    }
  }
  
  return(TRUE)
}

# Safe numeric operations
safe_mean <- function(x, na.rm = TRUE) {
  tryCatch({
    if (length(x) == 0) return(NA)
    mean(x, na.rm = na.rm)
  }, error = function(e) {
    warning("Error calculating mean: ", conditionMessage(e))
    return(NA)
  })
}

safe_sd <- function(x, na.rm = TRUE) {
  tryCatch({
    if (length(x) == 0) return(NA)
    sd(x, na.rm = na.rm)
  }, error = function(e) {
    warning("Error calculating standard deviation: ", conditionMessage(e))
    return(NA)
  })
}

# Pipeline status tracking
update_pipeline_status <- function(step, status, message = "") {
  status_file <- "checkpoints/pipeline_status.json"
  
  tryCatch({
    # Create checkpoints directory if it doesn't exist
    if (!dir.exists("checkpoints")) {
      dir.create("checkpoints", recursive = TRUE)
    }
    
    # Read existing status or create new
    if (file.exists(status_file)) {
      status_data <- jsonlite::fromJSON(status_file)
    } else {
      status_data <- list()
    }
    
    # Update status
    status_data[[step]] <- list(
      status = status,
      message = message,
      timestamp = Sys.time()
    )
    
    # Write back
    jsonlite::write_json(status_data, status_file, pretty = TRUE)
    
  }, error = function(e) {
    warning("Error updating pipeline status: ", conditionMessage(e))
  })
}

# Safe row binding function
add_row_safe <- function(df1, df2) {
  tryCatch({
    if (is.null(df1) || nrow(df1) == 0) {
      return(df2)
    }
    if (is.null(df2) || nrow(df2) == 0) {
      return(df1)
    }
    
    # Ensure both data frames have the same column names
    common_cols <- intersect(colnames(df1), colnames(df2))
    if (length(common_cols) == 0) {
      warning("No common columns found between data frames")
      return(df1)
    }
    
    # Select only common columns and bind
    df1_subset <- df1[, common_cols, drop = FALSE]
    df2_subset <- df2[, common_cols, drop = FALSE]
    
    rbind(df1_subset, df2_subset)
  }, error = function(e) {
    warning("Error in add_row_safe: ", conditionMessage(e))
    return(df1)
  })
}

# Error handling wrapper
with_error_handling <- function(expr, error_message = "Operation failed") {
  tryCatch({
    expr
  }, error = function(e) {
    warning(error_message, ": ", conditionMessage(e))
    return(NULL)
  })
}

cat("[OK] Safety functions loaded\n")