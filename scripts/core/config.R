#!/usr/bin/env Rscript
# =============================================================================
# MASTER CONFIGURATION FOR NF-GARCH DISSERTATION PIPELINE
# =============================================================================
# 
# This is the SINGLE SOURCE OF TRUTH for all pipeline configurations.
# All scripts should source this file to get consistent settings.
#
# ┌─────────────────────────────────────────────────────────────────────────┐
# │ HOW TO SWITCH BETWEEN OPTIMIZED AND FULL RUNS                          │
# └─────────────────────────────────────────────────────────────────────────┘
#
# 1. CHANGE LINE 30: Set PIPELINE_MODE to either "optimized" or "full"
# 2. RE-RUN: Execute run_all.bat or run_full_dissertation.bat
# 3. That's it! All scripts automatically adapt to the new mode
#
# ┌─────────────────────────────────────────────────────────────────────────┐
# │ OPTIMIZED MODE (DEFAULT) - For Dissertation Main Results               │
# └─────────────────────────────────────────────────────────────────────────┘
#   Assets:      6 (3 FX + 3 Equity) - Representative sample
#   CV Windows:  3 windows - Fast validation
#   NF Layers:   4 layers, 64 hidden - Efficient architecture
#   Epochs:      75 - Quick convergence
#   Runtime:     60-120 minutes
#   RAM:         8GB recommended
#   GPU:         Optional (speeds up NF training)
#   
#   Use for: Development, testing, dissertation main results
#
# ┌─────────────────────────────────────────────────────────────────────────┐
# │ FULL MODE - For Robustness Checks & Appendix                           │
# └─────────────────────────────────────────────────────────────────────────┘
#   Assets:      13 (6 FX + 7 Equity) - Complete cross-section
#   CV Windows:  8-10 windows - Comprehensive validation
#   NF Layers:   8 layers, 256 hidden - Deep architecture
#   Epochs:      150 - Extended training
#   Runtime:     4-8 hours
#   RAM:         16GB recommended
#   GPU:         Highly recommended (10x faster for NF)
#   
#   Use for: Robustness tests, sensitivity analysis, appendix results
#
# =============================================================================

# =============================================================================
# PIPELINE MODE SELECTION
# =============================================================================
# 
# CHANGE THIS TO SWITCH BETWEEN OPTIMIZED AND FULL RUNS
# 
# "optimized" = Fast execution for dissertation (6 assets, 3 CV windows, 75 epochs)
#               Runtime: 60-120 minutes
#               Use this for: Normal development, testing, dissertation results
#
# "full"      = Comprehensive execution (12 assets, all CV windows, 150 epochs)
#               Runtime: 4-8 hours
#               Use this for: Robustness checks, sensitivity analysis, appendix
#
PIPELINE_MODE <- "optimized"  # Change to "full" for comprehensive runs

# =============================================================================
# MODEL CONFIGURATION
# =============================================================================

# =============================================================================
# GARCH MODEL SPECIFICATIONS
# =============================================================================
#
# SUPPORTED DISTRIBUTIONS: 
#   - "norm": Normal distribution (Gaussian)
#   - "std": Student-t distribution (standard parameterization, Var(z)=ν/(ν-2))
#   - "sstd": Skewed Student-t - NOT IMPLEMENTED (will error if requested)
#
# IMPLEMENTATION NOTES:
#   - sGARCH: Standard GARCH(1,1) with stationarity constraint α+β<1
#   - gjrGARCH: GJR-GARCH with leverage effects (Glosten et al. 1993)
#   - eGARCH: Exponential GARCH with log-variance (Nelson 1991)
#   - TGARCH: Zakoian (1994) specification with absolute residuals
#
# =============================================================================

GARCH_MODELS <- list(
  sGARCH_norm = list(
    model = "sGARCH",
    distribution = "norm",
    description = "Standard GARCH with Normal Distribution"
  ),
  # sGARCH_sstd: REMOVED 2026-02-02 - Skewed Student-t not implemented
  # Previous results labeled "sstd" actually used symmetric Student-t "std"
  # For Student-t distribution, add sGARCH_std manually or use NF-GARCH
  eGARCH = list(
    model = "eGARCH",
    distribution = "norm", 
    description = "Exponential GARCH with Normal Distribution (Nelson 1991)"
  ),
  gjrGARCH = list(
    model = "gjrGARCH",
    distribution = "norm",
    description = "GJR-GARCH with Leverage Effects (Glosten et al. 1993)"
  ),
  TGARCH = list(
    model = "TGARCH", 
    distribution = "norm",
    description = "Threshold GARCH (Zakoian 1994 specification)"
  )
)

# NF-GARCH Model Specifications (using NF residuals)
# NOTE: distribution parameter here is metadata only - the NF learns the actual
# innovation distribution from data. The TGARCH component uses 'norm' or 'std'
# during fitting, then innovations are replaced with NF-generated samples.
NF_GARCH_MODELS <- list(
  "NF_tGarch" = list(
    model = "NF_tGarch",
    distribution = "sstd",  # Metadata: NF learns skewed/heavy-tailed distribution
    submodel = "TGARCH",
    description = "Normalizing Flow with Threshold GARCH (Zakoian)"
  )
)

# Engine Configuration - Manual engine only
ENGINE_CONFIG <- list(
  standard_garch_engine = "manual",
  nf_garch_engine = "manual",
  results_dir = "results/manual_results"
)

# =============================================================================
# ASSET CONFIGURATION
# =============================================================================
# 
# OPTIMIZED MODE: 6 assets (3 FX + 3 Equity) - 50% reduction
#   - Representative sample across asset classes
#   - Sufficient for main dissertation findings
#   - Fast execution for iterative development
#
# FULL MODE: 13 assets (6 FX + 7 Equity) - Complete dataset
#   - Comprehensive cross-sectional coverage
#   - Robustness checks across all assets
#   - Appendix/sensitivity analysis
#

# OPTIMIZED ASSETS (50% reduction - dissertation main results)
OPTIMIZED_ASSETS <- list(
  fx = c("EURUSD", "GBPUSD", "USDZAR"),           # 3 FX pairs
  equity = c("NVDA", "MSFT", "AMZN")              # 3 major equities
)

# FULL ASSETS (complete dataset - robustness & appendix)
FULL_ASSETS <- list(
  fx = c("EURUSD", "GBPUSD", "GBPCNY", "USDZAR", "GBPZAR", "EURZAR"),  # 6 FX pairs
  equity = c("X", "NVDA", "MSFT", "PG", "CAT", "WMT", "AMZN")          # 7 equities
)

# ACTIVE ASSETS (based on PIPELINE_MODE)
ASSETS <- if (PIPELINE_MODE == "full") FULL_ASSETS else OPTIMIZED_ASSETS

# All assets combined (flattened list)
ALL_ASSETS <- c(ASSETS$fx, ASSETS$equity)

# Asset metadata
ASSET_METADATA <- list(
  EURUSD = list(type = "fx", description = "Euro/US Dollar"),
  GBPUSD = list(type = "fx", description = "British Pound/US Dollar"),
  GBPCNY = list(type = "fx", description = "British Pound/Chinese Yuan"),
  USDZAR = list(type = "fx", description = "US Dollar/South African Rand"),
  GBPZAR = list(type = "fx", description = "British Pound/South African Rand"),
  EURZAR = list(type = "fx", description = "Euro/South African Rand"),
  X = list(type = "equity", description = "United States Steel Corporation"),
  NVDA = list(type = "equity", description = "NVIDIA Corporation"),
  MSFT = list(type = "equity", description = "Microsoft Corporation"),
  PG = list(type = "equity", description = "Procter & Gamble Company"),
  CAT = list(type = "equity", description = "Caterpillar Inc."),
  WMT = list(type = "equity", description = "Walmart Inc."),
  AMZN = list(type = "equity", description = "Amazon.com Inc.")
)

# =============================================================================
# OUTPUT SCHEMAS
# =============================================================================

# Excel sheet schemas for validation
OUTPUT_SCHEMAS <- list(
  Model_Performance_Summary = c(
    "Model", "Model_Family", "Engine", "Split_Type", "Source", 
    "Avg_AIC", "Avg_BIC", "Avg_LogLik", "Avg_MSE", "Avg_MAE"
  ),
  VaR_Performance_Summary = c(
    "Model", "Asset", "Confidence_Level", "Total_Obs", "Expected_Rate", 
    "Violations", "Violation_Rate", "Kupiec_PValue", "Christoffersen_PValue", "DQ_PValue"
  ),
  Stress_Test_Summary = c(
    "Model", "Asset", "Scenario_Type", "Scenario_Name", "Convergence_Rate",
    "Pass_LB_Test", "Pass_ARCH_Test", "Total_Tests", "Robustness_Score"
  ),
  NF_Winners_By_Asset = c(
    "Asset", "Winning_Model", "Split", "Metric", "Value"
  ),
  Distributional_Fit_Summary = c(
    "Model", "Asset", "Test_Type", "Statistic", "P_Value", "Decision"
  )
)

# =============================================================================
# CURRENT PIPELINE STRUCTURE
# =============================================================================

# Current pipeline scripts (as used by run_all.bat and run_modular.bat)
CURRENT_PIPELINE_SCRIPTS <- list(
  pipeline_diagnostic = "scripts/utils/pipeline_diagnostic.R",
  eda = "scripts/eda/eda_summary_stats.R",
  garch_fitting = "scripts/model_fitting/fit_garch_models.R",
  residual_extraction = "scripts/model_fitting/extract_residuals.R",
  nf_training = "scripts/model_fitting/train_nf_models.py",
  nf_evaluation = "scripts/model_fitting/evaluate_nf_fit.py",
  nf_garch_manual = "scripts/simulation_forecasting/simulate_nf_garch_engine.R",
  forecasting = "scripts/simulation_forecasting/forecast_garch_variants.R",
  forecast_evaluation = "scripts/evaluation/wilcoxon_winrate_analysis.R",
  stylized_facts = "scripts/evaluation/stylized_fact_tests.R",
  var_backtesting = "scripts/evaluation/var_backtesting.R",
  nfgarch_var_backtesting = "scripts/evaluation/nfgarch_var_backtesting.R",
  stress_testing = "scripts/stress_tests/evaluate_under_stress.R",
  nfgarch_stress_testing = "scripts/evaluation/nfgarch_stress_testing.R",
  consolidation = "scripts/core/consolidation.R",
  validation = "scripts/utils/validate_pipeline.py",
  appendix_log = "scripts/utils/generate_appendix_log.py"
)

# =============================================================================
# FILE PATHS
# =============================================================================

# Input data paths
DATA_PATHS <- list(
  raw_data = "data/processed/combined_data.csv",
  nf_residuals_dir = "nf_generated_residuals",
  checkpoints_dir = "checkpoints"
)

# Output paths
OUTPUT_PATHS <- list(
  base_dir = "outputs",
  eda = "outputs/eda",
  model_eval = "outputs/model_eval", 
  var_backtest = "outputs/var_backtest",
  stress_tests = "outputs/stress_tests",
  consolidated_results = "outputs/Consolidated_NF_GARCH_Results.xlsx",
  dissertation_results = "outputs/Dissertation_Consolidated_Results.xlsx",
  # Results paths
  manual_results = "results/manual_results"
)

# =============================================================================
# REPRODUCIBILITY CONFIGURATION
# =============================================================================

# Centralized seed for reproducibility across all scripts
# This seed should be used consistently across R and Python scripts
REPRODUCIBILITY_SEED <- 123

# Get R and Python executables from environment or use defaults
get_r_executable <- function() {
  r_exe <- Sys.getenv("RSCRIPT", unset = NA)
  if (is.na(r_exe)) {
    # Try common locations
    if (.Platform$OS.type == "windows") {
      r_exe <- "Rscript.exe"
    } else {
      r_exe <- "Rscript"
    }
  }
  return(r_exe)
}

get_python_executable <- function() {
  py_exe <- Sys.getenv("PYTHON", unset = NA)
  if (is.na(py_exe)) {
    py_exe <- "python"
  }
  return(py_exe)
}

# =============================================================================
# SIMULATION PARAMETERS
# =============================================================================

# NF-GARCH simulation parameters
SIMULATION_PARAMS <- list(
  n_simulations = 1000,
  forecast_horizon = 10,
  confidence_levels = c(0.95, 0.99),
  seed = REPRODUCIBILITY_SEED  # Use centralized seed
)

# =============================================================================
# TIME-SERIES CROSS-VALIDATION CONFIGURATION
# =============================================================================
#
# OPTIMIZED MODE: 3-4 windows, larger steps - Fast iteration
# FULL MODE: 8-10 windows, smaller steps - Comprehensive validation
#

# OPTIMIZED CV PARAMETERS (for fast execution)
TSCV_OPTIMIZED <- list(
  window_size = 0.65,              # 65% of data per window
  step_size = 0.15,                # 15% step (larger = fewer windows)
  forecast_horizon = 20,           # 20 steps ahead
  max_windows = 3,                 # Limit to 3 windows
  min_train_size = 0.4,            # Minimum 40% for training
  parallel_cores = 4,
  parallel_enabled = TRUE
)

# FULL CV PARAMETERS (for comprehensive analysis)
# CHANGE TO THIS FOR ROBUSTNESS: More windows, smaller steps, longer forecasts
TSCV_FULL <- list(
  window_size = 0.65,              # 65% of data per window
  step_size = 0.05,                # 5% step (smaller = more windows, ~10 total)
  forecast_horizon = 40,           # 40 steps ahead (longer horizon)
  max_windows = NULL,              # No limit - use all possible windows
  min_train_size = 0.3,            # Minimum 30% for training
  parallel_cores = 8,              # Use more cores if available
  parallel_enabled = TRUE
)

# ACTIVE CV CONFIGURATION (based on PIPELINE_MODE)
TSCV_CONFIG <- if (PIPELINE_MODE == "full") TSCV_FULL else TSCV_OPTIMIZED

# =============================================================================
# NORMALIZING FLOW TRAINING CONFIGURATION
# =============================================================================
#
# OPTIMIZED MODE: 75 epochs, 4 layers, 64 hidden - Fast training
# FULL MODE: 150 epochs, 8 layers, 256 hidden - Deep architecture
#

# OPTIMIZED NF PARAMETERS (for fast iteration)
NF_OPTIMIZED <- list(
  epochs = 75,                     # Reduced for speed
  batch_size = 512,                # Large batches for GPU efficiency
  learning_rate = 0.001,
  
  # Early stopping
  early_stopping = TRUE,
  patience = 15,
  min_delta = 1e-4,
  
  # Validation
  validation_split = 0.2,
  validation_frequency = 5,
  
  # Model architecture (SHALLOW for speed)
  num_layers = 4,                  # Shallow network
  hidden_features = 64,            # Small hidden dimension
  
  # Optimization
  gradient_checkpointing = TRUE,
  mixed_precision = TRUE,
  clear_cache = TRUE
)

# FULL NF PARAMETERS (for research-quality results)
# CHANGE TO THIS FOR PUBLICATION: Deeper networks, more training, better capacity
NF_FULL <- list(
  epochs = 150,                    # More training iterations
  batch_size = 256,                # Smaller batches for better gradients
  learning_rate = 0.0005,          # Lower learning rate for stability
  
  # Early stopping (more patience for convergence)
  early_stopping = TRUE,
  patience = 25,                   # More patience
  min_delta = 5e-5,                # Stricter convergence criterion
  
  # Validation
  validation_split = 0.2,
  validation_frequency = 3,        # More frequent validation
  
  # Model architecture (DEEP for better capacity)
  num_layers = 8,                  # Deep network (doubled!)
  hidden_features = 256,           # Large hidden dimension (4x increase!)
  
  # Advanced features
  dropout = 0.1,                   # Regularization
  batch_norm = TRUE,               # Batch normalization for stability
  residual_connections = TRUE,     # Residual connections for deep networks
  
  # Optimization
  gradient_checkpointing = TRUE,
  mixed_precision = TRUE,
  gradient_clipping = 1.0,         # Clip gradients for stability
  weight_decay = 1e-5,             # L2 regularization
  clear_cache = TRUE,
  
  # Learning rate schedule
  lr_scheduler = "cosine",         # Cosine annealing
  warmup_epochs = 10               # Warmup for stability
)

# ACTIVE NF CONFIGURATION (based on PIPELINE_MODE)
NF_CONFIG <- if (PIPELINE_MODE == "full") NF_FULL else NF_OPTIMIZED

# =============================================================================
# VALIDATION PARAMETERS
# =============================================================================

# Validation thresholds
VALIDATION_THRESHOLDS <- list(
  min_rows_per_sheet = 1,
  max_missing_pct = 0.0,  # No missing values allowed
  min_assets_covered = 12,
  required_confidence_levels = c(0.95, 0.99)
)

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================
# 
# These functions automatically return the correct configuration based on
# PIPELINE_MODE. Scripts should use these instead of accessing variables directly.
#

# Get all assets (respects PIPELINE_MODE)
get_pipeline_assets <- function() {
  return(ALL_ASSETS)
}

# Get FX assets only (respects PIPELINE_MODE)
get_fx_assets <- function() {
  return(ASSETS$fx)
}

# Get equity assets only (respects PIPELINE_MODE)
get_equity_assets <- function() {
  return(ASSETS$equity)
}

# BACKWARD COMPATIBILITY: Alias for manual scripts
get_manual_assets <- function() {
  return(ALL_ASSETS)
}

get_manual_fx_assets <- function() {
  return(ASSETS$fx)
}

get_manual_equity_assets <- function() {
  return(ASSETS$equity)
}

# Get NF configuration (respects PIPELINE_MODE)
get_nf_config <- function() {
  return(NF_CONFIG)
}

# Get CV configuration (respects PIPELINE_MODE)
get_cv_config <- function() {
  return(TSCV_CONFIG)
}

# Get all model names (standard GARCH + NF-GARCH)
get_all_models <- function() {
  c(names(GARCH_MODELS), names(NF_GARCH_MODELS))
}

# Get standard GARCH models only
get_standard_garch_models <- function() {
  names(GARCH_MODELS)
}

# Get NF-GARCH models only  
get_nf_garch_models <- function() {
  names(NF_GARCH_MODELS)
}

# Get current pipeline script paths
get_pipeline_script <- function(step_name) {
  if (step_name %in% names(CURRENT_PIPELINE_SCRIPTS)) {
    return(CURRENT_PIPELINE_SCRIPTS[[step_name]])
  } else {
    stop("Unknown pipeline step: ", step_name)
  }
}

# Get assets by type
get_assets_by_type <- function(type) {
  if (type %in% names(ASSETS)) {
    return(ASSETS[[type]])
  } else {
    stop("Invalid asset type. Use 'fx' or 'equity'")
  }
}

# Validate schema compliance
validate_schema <- function(data, schema_name) {
  if (!schema_name %in% names(OUTPUT_SCHEMAS)) {
    stop("Unknown schema: ", schema_name)
  }
  
  required_cols <- OUTPUT_SCHEMAS[[schema_name]]
  actual_cols <- colnames(data)
  
  missing_cols <- setdiff(required_cols, actual_cols)
  if (length(missing_cols) > 0) {
    stop("Missing required columns for schema '", schema_name, "': ", 
         paste(missing_cols, collapse = ", "))
  }
  
  return(TRUE)
}

# Print configuration summary
print_config_summary <- function() {
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║  NF-GARCH PIPELINE CONFIGURATION SUMMARY                   ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("PIPELINE MODE:", toupper(PIPELINE_MODE), "\n")
  if (PIPELINE_MODE == "optimized") {
    cat("  → Fast execution for dissertation (60-120 min)\n")
    cat("  → To switch to FULL mode: Edit scripts/core/config.R line 23\n")
  } else {
    cat("  → Comprehensive execution for robustness (4-8 hours)\n")
    cat("  → Using all assets, windows, and deep NF architecture\n")
  }
  cat("\n")
  cat("ASSETS:\n")
  cat("  - FX pairs:", length(ASSETS$fx), 
      if(PIPELINE_MODE == "full") "(FULL: 6)" else "(OPTIMIZED: 3)", "\n")
  cat("    ", paste(ASSETS$fx, collapse = ", "), "\n")
  cat("  - Equities:", length(ASSETS$equity),
      if(PIPELINE_MODE == "full") "(FULL: 7)" else "(OPTIMIZED: 3)", "\n")
  cat("    ", paste(ASSETS$equity, collapse = ", "), "\n")
  cat("\n")
  cat("MODELS:\n")
  cat("  - Standard GARCH:", length(GARCH_MODELS), "\n")
  cat("  - NF-GARCH:", length(NF_GARCH_MODELS), "\n")
  cat("\n")
  cat("NF ARCHITECTURE:\n")
  cat("  - Layers:", NF_CONFIG$num_layers, "\n")
  cat("  - Hidden features:", NF_CONFIG$hidden_features, "\n")
  cat("  - Epochs:", NF_CONFIG$epochs, "\n")
  cat("  - Batch size:", NF_CONFIG$batch_size, "\n")
  cat("\n")
  cat("TIME-SERIES CV:\n")
  cat("  - Window size:", TSCV_CONFIG$window_size, "\n")
  cat("  - Step size:", TSCV_CONFIG$step_size, "\n")
  cat("  - Max windows:", 
      if(is.null(TSCV_CONFIG$max_windows)) "Unlimited" else TSCV_CONFIG$max_windows, "\n")
  cat("  - Forecast horizon:", TSCV_CONFIG$forecast_horizon, "\n")
  cat("\n")
  cat("ENGINE:\n")
  cat("  - Standard GARCH:", ENGINE_CONFIG$standard_garch_engine, "\n")
  cat("  - NF-GARCH:", ENGINE_CONFIG$nf_garch_engine, "\n")
  cat("\n")
  cat("REPRODUCIBILITY SEED:", REPRODUCIBILITY_SEED, "\n")
  cat("════════════════════════════════════════════════════════════\n")
}

# Load configuration
load_config <- function() {
  # This function can be called to ensure config is loaded
  invisible(NULL)
}

# =============================================================================
# JSON EXPORT FOR PYTHON INTEGRATION
# =============================================================================

export_config_to_json <- function(output_file = "scripts/core/nf_config.json") {
  #' Export NF Configuration to JSON for Python Scripts
  #'
  #' @description
  #' Exports the active NF_CONFIG and related settings to a JSON file that
  #' Python scripts can read. This ensures Python and R use the same configuration
  #' based on PIPELINE_MODE.
  #'
  #' @param output_file Path to JSON output file
  #'
  #' @return Invisible NULL (side effect: writes JSON file)
  #'
  #' @examples
  #' export_config_to_json()
  #' # Python can now read: scripts/core/nf_config.json
  
  # Create config object for export
  config_export <- list(
    # Pipeline metadata
    pipeline_mode = PIPELINE_MODE,
    reproducibility_seed = REPRODUCIBILITY_SEED,
    
    # Asset configuration
    assets = list(
      fx = ASSETS$fx,
      equity = ASSETS$equity,
      all_assets = ALL_ASSETS
    ),
    
    # NF training parameters
    nf_config = list(
      # Training
      epochs = as.integer(NF_CONFIG$epochs),
      batch_size = as.integer(NF_CONFIG$batch_size),
      learning_rate = NF_CONFIG$learning_rate,
      
      # Early stopping
      early_stopping = NF_CONFIG$early_stopping,
      patience = as.integer(NF_CONFIG$patience),
      min_delta = NF_CONFIG$min_delta,
      
      # Validation
      validation_split = NF_CONFIG$validation_split,
      validation_frequency = as.integer(NF_CONFIG$validation_frequency),
      
      # Model architecture
      num_layers = as.integer(NF_CONFIG$num_layers),
      hidden_features = as.integer(NF_CONFIG$hidden_features),
      
      # Advanced features (if present in full mode)
      dropout = if(!is.null(NF_CONFIG$dropout)) NF_CONFIG$dropout else NULL,
      batch_norm = if(!is.null(NF_CONFIG$batch_norm)) NF_CONFIG$batch_norm else NULL,
      residual_connections = if(!is.null(NF_CONFIG$residual_connections)) NF_CONFIG$residual_connections else NULL,
      
      # Optimization
      gradient_checkpointing = NF_CONFIG$gradient_checkpointing,
      mixed_precision = NF_CONFIG$mixed_precision,
      gradient_clipping = if(!is.null(NF_CONFIG$gradient_clipping)) NF_CONFIG$gradient_clipping else NULL,
      weight_decay = if(!is.null(NF_CONFIG$weight_decay)) NF_CONFIG$weight_decay else NULL,
      clear_cache = NF_CONFIG$clear_cache,
      
      # Learning rate schedule (if present)
      lr_scheduler = if(!is.null(NF_CONFIG$lr_scheduler)) NF_CONFIG$lr_scheduler else NULL,
      warmup_epochs = if(!is.null(NF_CONFIG$warmup_epochs)) as.integer(NF_CONFIG$warmup_epochs) else NULL
    ),
    
    # Output directories
    output_paths = list(
      nf_models = "outputs/manual/nf_models",
      residuals = "outputs/manual/residuals_by_model",
      garch_fitting = "outputs/manual/garch_fitting"
    )
  )
  
  # Try to use jsonlite if available, otherwise use base R
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    # Use jsonlite for pretty JSON
    json_string <- jsonlite::toJSON(config_export, 
                                     pretty = TRUE, 
                                     auto_unbox = TRUE,
                                     null = "null")
    writeLines(json_string, output_file)
  } else {
    # Fallback: manual JSON construction
    # This is a simplified version that handles the essential types
    json_lines <- c(
      "{",
      sprintf('  "pipeline_mode": "%s",', PIPELINE_MODE),
      sprintf('  "reproducibility_seed": %d,', REPRODUCIBILITY_SEED),
      '  "nf_config": {',
      sprintf('    "epochs": %d,', NF_CONFIG$epochs),
      sprintf('    "batch_size": %d,', NF_CONFIG$batch_size),
      sprintf('    "learning_rate": %g,', NF_CONFIG$learning_rate),
      sprintf('    "num_layers": %d,', NF_CONFIG$num_layers),
      sprintf('    "hidden_features": %d', NF_CONFIG$hidden_features),
      '  }',
      '}'
    )
    writeLines(json_lines, output_file)
    warning("jsonlite package not available. Using simplified JSON export.")
  }
  
  cat("✓ Config exported to:", output_file, "\n")
  cat("  Mode:", PIPELINE_MODE, "\n")
  cat("  NF Layers:", NF_CONFIG$num_layers, "\n")
  cat("  NF Hidden Features:", NF_CONFIG$hidden_features, "\n")
  cat("  NF Epochs:", NF_CONFIG$epochs, "\n")
  
  invisible(NULL)
}

# Auto-export config when this file is sourced
# This ensures Python scripts always have access to current config
if (!exists(".config_exported") || !.config_exported) {
  export_config_to_json()
  .config_exported <- TRUE
}

