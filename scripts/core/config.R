#!/usr/bin/env Rscript
# Pipeline configuration. Set PIPELINE_MODE to "optimized" or "full".

PIPELINE_MODE <- "full"

# GARCH: norm, std (Student-t). sstd not implemented.

GARCH_MODELS <- list(
  sGARCH_std = list(
    model = "sGARCH",
    distribution = "std",
    description = "Standard GARCH with Student-t Distribution"
  ),
  eGARCH_std = list(
    model = "eGARCH",
    distribution = "std", 
    description = "Exponential GARCH with Student-t Distribution (Nelson 1991)"
  ),
  gjrGARCH_std = list(
    model = "gjrGARCH",
    distribution = "std",
    description = "GJR-GARCH with Student-t (Leverage Effects - Glosten et al. 1993)"
  ),
  TGARCH_std = list(
    model = "TGARCH", 
    distribution = "std",
    description = "Threshold GARCH with Student-t (Zakoian 1994 specification)"
  )
)

NF_GARCH_MODELS <- list(
  "NF_tGarch" = list(
    model = "NF_tGarch",
    distribution = "sstd",
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

OPTIMIZED_ASSETS <- list(
  fx = c("EURUSD", "GBPUSD", "USDZAR"),           # 3 FX pairs
  equity = c("NVDA", "MSFT", "AMZN")
)

FULL_ASSETS <- list(
  fx = c("EURUSD", "GBPUSD", "GBPCNY", "USDZAR", "GBPZAR", "EURZAR"),
  equity = c("X", "NVDA", "MSFT", "PG", "CAT", "WMT", "AMZN")
)

ASSETS <- if (PIPELINE_MODE == "full") FULL_ASSETS else OPTIMIZED_ASSETS

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

REPRODUCIBILITY_SEED <- 123

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

SIMULATION_PARAMS <- list(
  n_simulations = 1000,
  forecast_horizon = 10,
  confidence_levels = c(0.95, 0.99),
  seed = REPRODUCIBILITY_SEED  # Use centralized seed
)

TSCV_OPTIMIZED <- list(
  window_size = 0.65,              # 65% of data per window
  step_size = 0.15,                # 15% step (larger = fewer windows)
  forecast_horizon = 20,           # 20 steps ahead
  max_windows = 3,                 # Limit to 3 windows
  min_train_size = 0.4,            # Minimum 40% for training
  parallel_cores = 4,
  parallel_enabled = TRUE,
  clear_memory = FALSE             # Disable aggressive memory clearing (can cause issues)
)

TSCV_FULL <- list(
  window_size = 0.65,              # 65% of data per window
  step_size = 0.05,                # 5% step (smaller = more windows, ~10 total)
  forecast_horizon = 40,           # 40 steps ahead (longer horizon)
  max_windows = NULL,              # No limit - use all possible windows
  min_train_size = 0.3,            # Minimum 30% for training
  parallel_cores = 8,              # Use more cores if available
  parallel_enabled = TRUE
)

TSCV_CONFIG <- if (PIPELINE_MODE == "full") TSCV_FULL else TSCV_OPTIMIZED

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

NF_CONFIG <- if (PIPELINE_MODE == "full") NF_FULL else NF_OPTIMIZED

VALIDATION_THRESHOLDS <- list(
  min_rows_per_sheet = 1,
  max_missing_pct = 0.0,  # No missing values allowed
  min_assets_covered = 12,
  required_confidence_levels = c(0.95, 0.99)
)

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

print_config_summary <- function() {
  cat("PIPELINE MODE:", toupper(PIPELINE_MODE), "\n")
  cat("ASSETS FX:", paste(ASSETS$fx, collapse = ", "), "\n")
  cat("ASSETS EQUITY:", paste(ASSETS$equity, collapse = ", "), "\n")
  cat("SEED:", REPRODUCIBILITY_SEED, "\n")
}

load_config <- function() { invisible(NULL) }

