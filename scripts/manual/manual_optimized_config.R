#!/usr/bin/env Rscript
# =============================================================================
# BACKWARD COMPATIBILITY WRAPPER
# =============================================================================
#
# This file now simply sources the master config at scripts/core/config.R
# 
# All configuration is centralized in scripts/core/config.R
# To change between optimized/full runs, edit that file instead.
#
# This wrapper exists to maintain backward compatibility with scripts that
# source "scripts/manual/manual_optimized_config.R"
#
# =============================================================================

# Source the master configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
} else {
  stop("Master config not found: scripts/core/config.R")
}

# Print a reminder that this is now a wrapper
if (interactive()) {
  cat("════════════════════════════════════════════════════════════\n")
  cat("Configuration loaded from scripts/core/config.R\n")
  cat("      Current mode:", PIPELINE_MODE, "\n")
  cat("      To change modes, edit scripts/core/config.R\n")
  cat("════════════════════════════════════════════════════════════\n\n")
}

# Provide backward-compatible aliases
# (All functions now point to the master config functions)

# These are already defined in core/config.R and available here:
# - get_manual_assets()
# - get_manual_fx_assets()
# - get_manual_equity_assets()
# - get_nf_config()
# - get_cv_config()

# Legacy function names for full backward compatibility
get_manual_models <- function() {
  get_standard_garch_models()
}

get_manual_model_config <- function() {
  GARCH_MODELS
}

get_manual_cv_config <- function() {
  get_cv_config()
}

get_manual_nf_config <- function() {
  get_nf_config()
}

print_optimization_summary <- function() {
  print_config_summary()
}

# Export all key variables for backward compatibility
MANUAL_ASSETS <- ALL_ASSETS
MANUAL_MODELS <- names(GARCH_MODELS)
MANUAL_MODEL_CONFIG <- GARCH_MODELS
MANUAL_CV_CONFIG <- TSCV_CONFIG
MANUAL_NF_CONFIG <- NF_CONFIG

ASSET_METADATA <- list(
  FX = ASSETS$fx,
  EQUITY = ASSETS$equity,
  total_count = length(ALL_ASSETS),
  original_count = length(c(FULL_ASSETS$fx, FULL_ASSETS$equity)),
  reduction_pct = if (PIPELINE_MODE == "optimized") 50 else 0
)

MODEL_METADATA <- list(
  total_count = length(GARCH_MODELS),
  original_count = length(GARCH_MODELS),
  reduction_pct = 0
)

CV_METADATA <- list(
  optimized_windows = if (!is.null(TSCV_CONFIG$max_windows)) TSCV_CONFIG$max_windows else "Unlimited",
  time_savings_pct = if (PIPELINE_MODE == "optimized") 60 else 0
)
