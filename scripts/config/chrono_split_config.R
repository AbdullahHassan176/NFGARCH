# Chronological Split Configuration
# This configuration ensures strict 65/35 chronological split across all pipeline stages
# Designed to work with scripts/chronological/ pipeline scripts

# =============================================================================
# SPLIT CONFIGURATION
# =============================================================================

# Split mode identifier
SPLIT_MODE <- "chronological"

# Chronological split ratios
TRAIN_RATIO <- 0.65
TEST_RATIO <- 0.35

# Disable TS CV in GARCH fitting
USE_TSCV_FOR_MODEL_SELECTION <- FALSE

# =============================================================================
# NF TRAINING CONFIGURATION
# =============================================================================

# NF Training: No internal validation split
# Train on 100% of training set residuals (which come from 65% of original data)
NF_VALIDATION_SPLIT <- 0.0  # Train on full training set

# NF training parameters (inherited from manual config but no validation)
NF_TRAINING_CONFIG <- list(
  epochs = 75,
  batch_size = 512,
  learning_rate = 0.001,
  
  # Disable validation
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

# Base output directories for chronological pipeline
OUTPUT_BASE <- "outputs/chronological"
RESULTS_BASE <- "results/chronological"

# Specific output paths
OUTPUT_PATHS <- list(
  garch_fitting = file.path(OUTPUT_BASE, "garch_fitting"),
  residuals = file.path(OUTPUT_BASE, "residuals_by_model"),
  nf_models = file.path(OUTPUT_BASE, "nf_models"),
  evaluation = file.path(OUTPUT_BASE, "evaluation")
)

# Specific result paths
RESULTS_PATHS <- list(
  consolidated = file.path(RESULTS_BASE, "consolidated"),
  tables = file.path(RESULTS_BASE, "dissertation_tables"),
  figures = file.path(RESULTS_BASE, "figures"),
  diagnostics = file.path(RESULTS_BASE, "diagnostics")
)

# =============================================================================
# DATA SPLIT FUNCTION
# =============================================================================

# Get chronological split indices
get_chrono_split_indices <- function(n_obs, train_ratio = 0.65) {
  train_size <- floor(n_obs * train_ratio)
  list(
    train_start = 1,
    train_end = train_size,
    test_start = train_size + 1,
    test_end = n_obs,
    train_size = train_size,
    test_size = n_obs - train_size
  )
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

# Initialize chronological pipeline directories
initialize_chrono_directories <- function() {
  # Create all output directories
  for (path in OUTPUT_PATHS) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
      cat("Created:", path, "\n")
    }
  }
  
  # Create all results directories
  for (path in RESULTS_PATHS) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
      cat("Created:", path, "\n")
    }
  }
  
  cat("Chronological pipeline directories initialized.\n")
}

# Print configuration summary
print_chrono_config_summary <- function() {
  cat("=== CHRONOLOGICAL SPLIT CONFIGURATION ===\n")
  cat("Split Mode:", SPLIT_MODE, "\n")
  cat("Train Ratio:", TRAIN_RATIO, "(65%)\n")
  cat("Test Ratio:", TEST_RATIO, "(35%)\n")
  cat("TS CV Enabled:", USE_TSCV_FOR_MODEL_SELECTION, "\n")
  cat("NF Validation Split:", NF_VALIDATION_SPLIT, "(No validation)\n")
  cat("Output Base:", OUTPUT_BASE, "\n")
  cat("Results Base:", RESULTS_BASE, "\n")
  cat("========================================\n")
}

# Export configuration
get_chrono_config <- function() {
  list(
    split_mode = SPLIT_MODE,
    train_ratio = TRAIN_RATIO,
    test_ratio = TEST_RATIO,
    use_tscv = USE_TSCV_FOR_MODEL_SELECTION,
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
  print_chrono_config_summary()
}
