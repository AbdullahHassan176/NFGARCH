#!/usr/bin/env Rscript
# Centralized Configuration for Financial-SDG-GARCH Pipeline
# This file consolidates all configuration settings in one place

# =============================================================================
# MODEL CONFIGURATION
# =============================================================================

# Standard GARCH Model Specifications
GARCH_MODELS <- list(
  sGARCH_norm = list(
    model = "sGARCH",
    distribution = "norm",
    description = "Standard GARCH with Normal Distribution"
  ),
  sGARCH_sstd = list(
    model = "sGARCH", 
    distribution = "sstd",
    description = "Standard GARCH with Skewed Student-t Distribution"
  ),
  eGARCH = list(
    model = "eGARCH",
    distribution = "norm", 
    description = "Exponential GARCH with Normal Distribution"
  ),
  gjrGARCH = list(
    model = "gjrGARCH",
    distribution = "norm",
    description = "Glosten-Jagannathan-Runkle GARCH"
  ),
  TGARCH = list(
    model = "TGARCH", 
    distribution = "norm",
    description = "Threshold GARCH"
  )
)

# NF-GARCH Model Specifications (using NF residuals)
NF_GARCH_MODELS <- list(
  "NF_tGarch" = list(
    model = "NF_tGarch",
    distribution = "sstd",
    submodel = "TGARCH",
    description = "Normalizing Flow with Threshold GARCH"
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

# Asset Categories
ASSETS <- list(
  fx = c("EURUSD", "GBPUSD", "GBPCNY", "USDZAR", "GBPZAR", "EURZAR"),
  equity = c("X", "NVDA", "MSFT", "PG", "CAT", "WMT", "AMZN")
)

# All assets combined
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
# SIMULATION PARAMETERS
# =============================================================================

# NF-GARCH simulation parameters
SIMULATION_PARAMS <- list(
  n_simulations = 1000,
  forecast_horizon = 10,
  confidence_levels = c(0.95, 0.99),
  seed = 12345
)

# OPTIMIZED TSCV PARAMETERS for speed
TSCV_OPTIMIZATION <- list(
  window_size = 500,
  step_size = 150,  # Increased from 50 for speed
  forecast_horizon = 20,  # Reduced from 40 for speed
  max_windows = 4,  # Limit to 3-4 non-overlapping windows
  parallel_cores = 4  # Use up to 4 cores for parallel processing
)

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
  cat("=== FINANCIAL-SDG-GARCH CONFIGURATION SUMMARY ===\n")
  cat("Standard GARCH Models:", length(GARCH_MODELS), "\n")
  cat("NF-GARCH Models:", length(NF_GARCH_MODELS), "\n")
  cat("FX Assets:", length(ASSETS$fx), "\n")
  cat("Equity Assets:", length(ASSETS$equity), "\n")
  cat("Current Pipeline Scripts:", length(CURRENT_PIPELINE_SCRIPTS), "\n")
  cat("Output Schemas:", length(OUTPUT_SCHEMAS), "\n")
  cat("Engine Configuration:\n")
  cat("  - Standard GARCH Engine:", ENGINE_CONFIG$standard_garch_engine, "\n")
  cat("  - NF-GARCH Engine:", ENGINE_CONFIG$nf_garch_engine, "\n")
  cat("================================================\n")
}

# Load configuration
load_config <- function() {
  # This function can be called to ensure config is loaded
  invisible(NULL)
}

