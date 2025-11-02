# Optimized Configuration for 1-Hour Pipeline Execution
# Maintains statistical rigor while reducing computational load

# Asset selection (6 most representative)
OPTIMIZED_ASSETS <- c(
  "EURUSD", "GBPUSD", "USDZAR",  # FX (3)
  "NVDA", "MSFT", "AMZN"         # Equity (3)
)

# Model selection (3 most effective variants)
OPTIMIZED_MODELS <- c("sGARCH", "eGARCH", "TGARCH")

# Reduced time-series cross-validation
OPTIMIZED_CV_CONFIG <- list(
  n_folds = 3,           # Reduced from 5
  window_size = 0.6,     # Reduced from 0.65
  step_size = 0.1,       # Increased step size
  min_train_size = 0.4   # Minimum training size
)

# Core evaluation metrics only
CORE_METRICS <- c(
  "RMSE", "MAE", "LogLik",           # Forecasting
  "KS_distance", "Wasserstein",       # Distributional
  "VaR_95", "VaR_99"                 # Risk
)

# NF training optimization
NF_OPTIMIZATION <- list(
  epochs = 50,           # Reduced from 100
  batch_size = 256,     # Increased batch size
  learning_rate = 0.001,
  early_stopping = 10,  # Early stopping
  validation_split = 0.2
)

# Parallel processing configuration
PARALLEL_CONFIG <- list(
  n_cores = 4,          # Use 4 cores
  parallel_assets = TRUE,
  parallel_models = TRUE,
  parallel_cv = TRUE
)

# Quick mode flags
QUICK_MODE <- list(
  skip_detailed_eda = TRUE,
  skip_stress_testing = TRUE,
  skip_stylized_facts = TRUE,
  skip_detailed_plots = TRUE,
  use_simple_consolidation = TRUE
)

# Export configuration
export_optimized_config <- function() {
  config <- list(
    assets = OPTIMIZED_ASSETS,
    models = OPTIMIZED_MODELS,
    cv = OPTIMIZED_CV_CONFIG,
    metrics = CORE_METRICS,
    nf = NF_OPTIMIZATION,
    parallel = PARALLEL_CONFIG,
    quick = QUICK_MODE
  )
  
  saveRDS(config, "config/optimized_config.rds")
  cat("Optimized configuration saved to config/optimized_config.rds\n")
}

# Load optimized configuration
load_optimized_config <- function() {
  if (file.exists("config/optimized_config.rds")) {
    return(readRDS("config/optimized_config.rds"))
  } else {
    cat("Optimized config not found, using defaults\n")
    return(list())
  }
}


