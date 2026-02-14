# Time-Series Cross-Validation Split Configuration
# This configuration ensures consistent TS CV across all pipeline stages
# Designed to work with scripts/tscv/ pipeline scripts

# =============================================================================
# SPLIT CONFIGURATION
# =============================================================================

# Split mode identifier
SPLIT_MODE <- "tscv"

# Enable TS CV in GARCH fitting
USE_TSCV_FOR_MODEL_SELECTION <- TRUE

# =============================================================================
# TS CV PARAMETERS
# =============================================================================

# Time-Series Cross-Validation Configuration
# Inherited from manual_optimized_config.R but applied consistently
TSCV_CONFIG <- list(
  # CV folds
  n_folds = 5,
  
  # Window parameters
  window_size = 0.65,           # 65% of data per window
  step_size = 0.1,              # 10% step between windows
  min_train_size = 0.4,         # Minimum 40% for training
  
  # Forecast parameters
  forecast_horizon = 20,
  max_windows = NULL,           # Use all possible windows
  
  # Parallel processing
  parallel_cores = 4,
  parallel_enabled = TRUE,
  
  # Early stopping (disabled for full CV)
  early_stopping = FALSE,
  patience = NULL,
  
  # Memory optimization
  clear_memory = TRUE,
  batch_size = 1000
)

# =============================================================================
# NF TRAINING CONFIGURATION
# =============================================================================

# NF Training: Train on each CV window's training set
# No additional validation split to avoid further data fragmentation
NF_VALIDATION_SPLIT <- 0.0  # No additional validation

# NF training parameters (per window)
NF_TRAINING_CONFIG <- list(
  epochs = 75,
  batch_size = 512,
  learning_rate = 0.001,
  
  # Disable validation (already using CV)
  validation_split = 0.0,
  early_stopping = FALSE,
  
  # Model architecture
  num_layers = 4,
  hidden_features = 64,
  
  # Memory optimization
  clear_cache = TRUE
)

# =============================================================================
# OUTPUT DIRECTORIES
# =============================================================================

# Base output directories for TS CV pipeline
OUTPUT_BASE <- "outputs/tscv"
RESULTS_BASE <- "results/tscv"

# Specific output paths (window-based structure)
# Using paste() to avoid empty path bug with file.path()
OUTPUT_PATHS <- list(
  garch_fitting = paste(OUTPUT_BASE, "garch_fitting", sep="/"),
  residuals = paste(OUTPUT_BASE, "residuals_by_model", sep="/"),
  nf_models = paste(OUTPUT_BASE, "nf_models", sep="/"),
  evaluation = paste(OUTPUT_BASE, "evaluation", sep="/")
)

# DEBUG: Print OUTPUT_PATHS to verify
cat("DEBUG: OUTPUT_BASE =", OUTPUT_BASE, "\n")
cat("DEBUG: OUTPUT_PATHS$garch_fitting =", OUTPUT_PATHS$garch_fitting, "\n")
cat("DEBUG: OUTPUT_PATHS$residuals =", OUTPUT_PATHS$residuals, "\n")

# Specific result paths
RESULTS_PATHS <- list(
  consolidated = paste(RESULTS_BASE, "consolidated", sep="/"),
  tables = paste(RESULTS_BASE, "dissertation_tables", sep="/"),
  figures = paste(RESULTS_BASE, "figures", sep="/"),
  diagnostics = paste(RESULTS_BASE, "diagnostics", sep="/")
)

# =============================================================================
# TS CV HELPER FUNCTIONS
# =============================================================================

# Calculate TS CV windows
calculate_tscv_windows <- function(n_obs, config = TSCV_CONFIG) {
  window_size <- floor(n_obs * config$window_size)
  step_size <- floor(n_obs * config$step_size)
  min_train_size <- floor(n_obs * config$min_train_size)
  
  # Calculate all possible windows
  all_start_indices <- seq(1, n_obs - window_size, by = step_size)
  
  # Limit to max_windows if specified
  if (!is.null(config$max_windows)) {
    all_start_indices <- head(all_start_indices, config$max_windows)
  }
  
  # Create window specifications
  windows <- lapply(seq_along(all_start_indices), function(i) {
    start_idx <- all_start_indices[i]
    end_idx <- start_idx + window_size - 1
    
    list(
      window_id = i,
      start_idx = start_idx,
      end_idx = end_idx,
      train_start = start_idx,
      train_end = end_idx,
      test_start = end_idx + 1,
      test_end = min(end_idx + config$forecast_horizon, n_obs),
      train_size = end_idx - start_idx + 1,
      test_size = min(config$forecast_horizon, n_obs - end_idx)
    )
  })
  
  return(windows)
}

# Get window directory path
get_window_path <- function(base_path, window_id) {
  # Use paste instead of file.path to avoid empty path bug
  paste(base_path, paste0("window_", window_id), sep="/")
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

# Initialize TS CV pipeline directories
initialize_tscv_directories <- function(n_windows = NULL) {
  # DEBUG: Check if OUTPUT_PATHS exists in function scope
  cat("DEBUG [FUNC START]: exists('OUTPUT_PATHS') =", exists("OUTPUT_PATHS"), "\n")
  cat("DEBUG [FUNC START]: OUTPUT_BASE =", get("OUTPUT_BASE", envir = parent.frame()), "\n")
  cat("DEBUG [FUNC START]: length(OUTPUT_PATHS) =", length(get("OUTPUT_PATHS", envir = parent.frame())), "\n")
  
  # Get OUTPUT_PATHS from parent frame to ensure we have the right one
  OUTPUT_PATHS_local <- get("OUTPUT_PATHS", envir = parent.frame())
  RESULTS_PATHS_local <- get("RESULTS_PATHS", envir = parent.frame())
  
  # Create base output directories
  for (path in OUTPUT_PATHS_local) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
      cat("Created:", path, "\n")
    }
  }
  
  # Create base results directories
  for (path in RESULTS_PATHS_local) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
      cat("Created:", path, "\n")
    }
  }
  
  # Create window-specific directories if n_windows specified
  if (!is.null(n_windows)) {
    cat("DEBUG: Creating", n_windows, "window directories\n")
    cat("DEBUG: OUTPUT_PATHS_local$garch_fitting =", OUTPUT_PATHS_local$garch_fitting, "\n")
    for (i in 1:n_windows) {
      # GARCH fitting window directories
      window_garch_dir <- get_window_path(OUTPUT_PATHS_local$garch_fitting, i)
      cat("DEBUG: window_garch_dir for window", i, "=", window_garch_dir, "\n")
      if (!dir.exists(window_garch_dir)) {
        dir.create(window_garch_dir, recursive = TRUE)
      }
      
      # Residuals window directories
      window_resid_dir <- get_window_path(OUTPUT_PATHS_local$residuals, i)
      if (!dir.exists(window_resid_dir)) {
        dir.create(window_resid_dir, recursive = TRUE)
      }
      
      # NF models window directories
      window_nf_dir <- get_window_path(OUTPUT_PATHS_local$nf_models, i)
      if (!dir.exists(window_nf_dir)) {
        dir.create(window_nf_dir, recursive = TRUE)
      }
    }
    cat("Created", n_windows, "window directories\n")
  }
  
  cat("TS CV pipeline directories initialized.\n")
}

# Print configuration summary
print_tscv_config_summary <- function() {
  cat("=== TIME-SERIES CROSS-VALIDATION CONFIGURATION ===\n")
  cat("Split Mode:", SPLIT_MODE, "\n")
  cat("TS CV Enabled:", USE_TSCV_FOR_MODEL_SELECTION, "\n")
  cat("Number of Folds:", TSCV_CONFIG$n_folds, "\n")
  cat("Window Size:", TSCV_CONFIG$window_size, "(", round(100 * TSCV_CONFIG$window_size), "%)\n")
  cat("Step Size:", TSCV_CONFIG$step_size, "(", round(100 * TSCV_CONFIG$step_size), "%)\n")
  cat("Min Train Size:", TSCV_CONFIG$min_train_size, "(", round(100 * TSCV_CONFIG$min_train_size), "%)\n")
  cat("Forecast Horizon:", TSCV_CONFIG$forecast_horizon, "\n")
  cat("Max Windows:", ifelse(is.null(TSCV_CONFIG$max_windows), "All", TSCV_CONFIG$max_windows), "\n")
  cat("Parallel Cores:", TSCV_CONFIG$parallel_cores, "\n")
  cat("NF Validation Split:", NF_VALIDATION_SPLIT, "(No additional validation)\n")
  cat("Output Base:", OUTPUT_BASE, "\n")
  cat("Results Base:", RESULTS_BASE, "\n")
  cat("=================================================\n")
}

# Export configuration
get_tscv_config <- function() {
  list(
    split_mode = SPLIT_MODE,
    use_tscv = USE_TSCV_FOR_MODEL_SELECTION,
    tscv_config = TSCV_CONFIG,
    nf_validation_split = NF_VALIDATION_SPLIT,
    nf_training_config = NF_TRAINING_CONFIG,
    output_base = OUTPUT_BASE,
    results_base = RESULTS_BASE,
    output_paths = OUTPUT_PATHS,
    results_paths = RESULTS_PATHS
  )
}

# Auto-print summary when sourced
if (interactive() || !exists("SUPPRESS_CONFIG_MESSAGES")) {
  print_tscv_config_summary()
}
