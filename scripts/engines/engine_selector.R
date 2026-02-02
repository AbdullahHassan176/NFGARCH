#!/usr/bin/env Rscript
# =============================================================================
# ENGINE SELECTOR - GARCH Model Abstraction Layer
# =============================================================================
#
# PURPOSE:
# Provides a standardized interface for GARCH model operations, abstracting
# the underlying implementation details. This allows pipeline scripts to work
# with GARCH models through a consistent API regardless of the backend engine.
#
# CURRENT STATUS:
# Currently supports ONLY the "manual" engine (custom R implementations).
# The engine parameter is retained for API consistency and future extensibility.
#
# REVIEWED: ✅ 2026-02-02
# Academic code review verified the manual GARCH implementation is mathematically
# correct and statistically valid. All models tested for correctness across:
#   ✅ Parameter estimation (MLE with proper constraints)
#   ✅ Variance recursion (sGARCH, gjrGARCH, eGARCH, TGARCH-Zakoian)
#   ✅ Residual extraction (raw and standardized)
#   ✅ Forecasting (1-step and multi-step simulation-based)
#   ✅ Path simulation (for NF-GARCH framework)
#   ✅ Model diagnostics (AIC, BIC, convergence)
#
# ENGINE TYPES:
#   - "manual" (CURRENT): Custom R implementation of GARCH models
#     * Provides fine-grained control over estimation
#     * Optimized for NF-GARCH pipeline's specific requirements
#     * Direct access to residuals, sigma, and parameters
#     * Verified mathematically correct (2026-02-02 review)
#
#   - "rugarch" (NOT IMPLEMENTED): Not used anywhere in pipeline
#   - "fGarch" (NOT IMPLEMENTED): Not used anywhere in pipeline
#
# CONFIGURATION:
# This script does NOT directly source configuration files. Instead, it receives
# model types, distributions, and other parameters from calling scripts (e.g.,
# manual_garch_fitting.R, simulate_nf_garch_engine.R) which DO source the
# centralized config at scripts/core/config.R.
#
# This design keeps the engine abstraction clean and configuration-independent,
# allowing it to work with any parameters passed by the caller.
#
# PUBLIC FUNCTIONS:
#   engine_fit()          : Fit GARCH model and return standardized object
#   engine_forecast()     : Generate h-step ahead volatility forecasts
#   engine_path()         : Simulate return paths for NF-GARCH (using custom innovations)
#   engine_residuals()    : Extract standardized residuals for NF training
#   engine_infocriteria() : Get model fit statistics (AIC, BIC, LogLikelihood)
#   engine_converged()    : Check if model fitting converged successfully
#
# USAGE EXAMPLE:
#   source("scripts/engines/engine_selector.R")
#   fit <- engine_fit(model = "sGARCH", returns = my_returns, dist = "std")
#   resids <- engine_residuals(fit, standardize = TRUE)
#   forecast <- engine_forecast(fit, h = 20)
#
# DEPENDENCIES:
# Sources manual GARCH implementations from scripts/manual_garch/:
#   - fit_sgarch_manual.R  : Standard GARCH(1,1)
#   - fit_gjr_manual.R     : GJR-GARCH (threshold GARCH)
#   - fit_egarch_manual.R  : Exponential GARCH
#   - fit_tgarch_manual.R  : Threshold GARCH
#   - forecast_manual.R    : Forecasting and path simulation utilities
#
# =============================================================================

# Load manual GARCH model implementations
source("scripts/manual_garch/fit_sgarch_manual.R")
source("scripts/manual_garch/fit_gjr_manual.R")
source("scripts/manual_garch/fit_egarch_manual.R")
source("scripts/manual_garch/fit_tgarch_manual.R")
source("scripts/manual_garch/forecast_manual.R")

# =============================================================================
# CORE ENGINE FUNCTIONS
# =============================================================================

engine_fit <- function(model, returns, dist, submodel = NULL, engine = "manual") {
  #' Fit GARCH Model Using Manual Engine
  #'
  #' @description
  #' Fits the specified GARCH model to return data and returns a standardized
  #' fit object with consistent fields across all model types.
  #'
  #' @param model Character string specifying GARCH variant:
  #'   "sGARCH", "gjrGARCH", "eGARCH", "TGARCH", "NF_tGARCH", "fGARCH"
  #' @param returns Numeric vector of returns to fit
  #' @param dist Distribution for innovations: "norm", "std", "sstd"
  #' @param submodel Optional submodel specification (e.g., "TGARCH" for fGARCH)
  #' @param engine Engine to use (only "manual" supported, parameter retained for API consistency)
  #'
  #' @return List containing standardized fit object with fields:
  #'   - engine: "manual"
  #'   - model_type: GARCH variant name
  #'   - distribution: Innovation distribution used
  #'   - convergence: Logical indicating successful convergence
  #'   - loglik: Log-likelihood
  #'   - aic: Akaike Information Criterion
  #'   - bic: Bayesian Information Criterion
  #'   - coef: Named vector of estimated parameters
  #'   - sigma: Fitted conditional volatility (sigma_t)
  #'   - residuals: Raw residuals
  #'   - std_residuals: Standardized residuals (for NF training)
  #'   - fitted: Fitted values (conditional mean)
  #'   - manual_fit: Original fit object from manual engine
  #'
  #' @examples
  #' fit <- engine_fit(model = "sGARCH", returns = rnorm(1000), dist = "std")
  #' sigma <- fit$sigma
  #' resids <- fit$std_residuals
  
  # Validate engine parameter
  if (!is.null(engine) && engine != "manual") {
    warning("Only 'manual' engine is supported. Ignoring engine parameter: ", engine)
  }
  
  # Handle distribution compatibility
  # REVIEWED 2026-02-02: Manual GARCH implementation supports norm, std
  # Skewed Student-t (sstd) is NOT implemented - error if requested
  if (dist == "sstd") {
    stop("Skewed Student-t distribution (sstd) is not implemented in manual engine.\n",
         "Supported distributions: 'norm' (Normal), 'std' (symmetric Student-t)\n",
         "Note: For NF-GARCH, the NF learns the actual innovation distribution,\n",
         "      so using 'std' with NF is appropriate for skewed/heavy-tailed data.")
  }
  manual_dist <- dist
  
  # Dispatch to appropriate model fitting function
  if (model == "sGARCH") {
    fit <- fit_sgarch_manual(returns, dist = manual_dist)
    
  } else if (model == "gjrGARCH") {
    fit <- fit_gjr_manual(returns, dist = manual_dist)
    
  } else if (model == "eGARCH") {
    fit <- fit_egarch_manual(returns, dist = manual_dist)
    
  } else if (model == "TGARCH") {
    fit <- fit_tgarch_manual(returns, dist = manual_dist)
    
  } else if (model == "NF_tGARCH" && submodel == "TGARCH") {
    # NF-GARCH models: use underlying GARCH for fitting
    fit <- fit_tgarch_manual(returns, dist = manual_dist)
    
  } else if (model == "fGARCH" && submodel == "TGARCH") {
    # fGARCH with TGARCH submodel
    fit <- fit_tgarch_manual(returns, dist = manual_dist)
    
  } else {
    stop("Manual engine does not support model: ", model, 
         if (!is.null(submodel)) paste0(" with submodel: ", submodel) else "")
  }
  
  # Construct standardized result object
  # All fields are guaranteed to be present regardless of model type
  result <- list(
    engine = "manual",
    model_type = fit$model_type,
    distribution = fit$distribution,
    submodel = submodel,
    convergence = fit$convergence,
    loglik = fit$loglik,
    aic = fit$aic,
    bic = fit$bic,
    coef = fit$coef,
    sigma = fit$sigma,              # Conditional volatility time series
    residuals = fit$residuals,       # Raw residuals (r_t)
    std_residuals = fit$std_residuals, # Standardized residuals (z_t = r_t / sigma_t)
    fitted = fit$fitted,             # Fitted conditional mean
    manual_fit = fit                 # Original fit object (for advanced operations)
  )
  
  return(result)
}

engine_forecast <- function(fit, h, engine = NULL) {
  #' Generate h-Step Ahead Volatility Forecasts
  #'
  #' @description
  #' Generates multi-step ahead forecasts for conditional volatility (sigma)
  #' and conditional mean (mu) using the fitted GARCH model.
  #'
  #' @param fit Standardized fit object from engine_fit()
  #' @param h Integer, number of steps ahead to forecast
  #' @param engine Ignored (retained for API consistency)
  #'
  #' @return List with:
  #'   - sigma: Vector of length h with forecasted volatilities
  #'   - mean: Vector of length h with forecasted conditional means
  #'
  #' @examples
  #' fit <- engine_fit("sGARCH", returns, "std")
  #' forecast <- engine_forecast(fit, h = 20)
  #' future_sigma <- forecast$sigma
  
  # Try to use the manual fit's predict method if available
  if (!is.null(fit$manual_fit) && !is.null(fit$manual_fit$predict)) {
    return(fit$manual_fit$predict(h))
  } else {
    # Fallback: simple persistence forecast
    # Uses last observed volatility and mean parameter
    # This is a naive forecast but ensures the function always returns valid output
    return(list(
      sigma = rep(fit$sigma[length(fit$sigma)], h),
      mean = rep(fit$coef["mu"], h)
    ))
  }
}

engine_path <- function(fit, z, h, model, submodel = NULL, engine = NULL) {
  #' Simulate Return Paths Using Custom Innovations
  #'
  #' @description
  #' Simulates return paths for NF-GARCH by using custom innovations (z)
  #' instead of parametric draws. This is the core of the NF-GARCH approach:
  #' GARCH dynamics are preserved, but innovations come from the learned NF.
  #'
  #' @param fit Standardized fit object from engine_fit()
  #' @param z Numeric vector of custom innovations (e.g., from Normalizing Flow)
  #' @param h Integer, number of steps to simulate
  #' @param model GARCH model type
  #' @param submodel Optional submodel specification
  #' @param engine Ignored (retained for API consistency)
  #'
  #' @return List with:
  #'   - returns: Simulated returns (length h)
  #'   - sigma: Simulated volatilities (length h)
  #'
  #' @details
  #' The function recursively applies the GARCH equation:
  #'   sigma_t^2 = omega + alpha * epsilon_{t-1}^2 + beta * sigma_{t-1}^2
  #'   r_t = mu + sigma_t * z_t
  #' where z_t comes from the NF instead of a parametric distribution.
  #'
  #' @examples
  #' fit <- engine_fit("sGARCH", returns, "std")
  #' nf_innovations <- rnorm(20)  # In practice, from NF
  #' paths <- engine_path(fit, z = nf_innovations, h = 20, model = "sGARCH")
  
  # Call manual path simulation function
  result <- manual_path(fit$manual_fit, z, h, model, submodel)
  
  # Validate output length (sanity check)
  if (length(result$returns) != h) {
    warning(
      "Manual path returned incorrect length. ",
      "Expected: ", h, ", Got: ", length(result$returns),
      "\nThis may indicate an issue with the path simulation function."
    )
  }
  
  return(result)
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

engine_residuals <- function(fit, standardize = TRUE) {
  #' Extract Residuals from Fitted GARCH Model
  #'
  #' @description
  #' Extracts residuals in the format needed for Normalizing Flow training.
  #' Standardized residuals (z_t = r_t / sigma_t) are typically used for NF.
  #'
  #' @param fit Standardized fit object from engine_fit()
  #' @param standardize Logical, return standardized residuals (TRUE) or raw residuals (FALSE)
  #'
  #' @return Numeric vector of residuals
  #'
  #' @examples
  #' fit <- engine_fit("sGARCH", returns, "std")
  #' z <- engine_residuals(fit, standardize = TRUE)  # For NF training
  #' eps <- engine_residuals(fit, standardize = FALSE)  # Raw residuals
  
  if (standardize) {
    return(fit$std_residuals)
  } else {
    return(fit$residuals)
  }
}

engine_infocriteria <- function(fit) {
  #' Get Model Information Criteria
  #'
  #' @description
  #' Extracts information criteria for model comparison and selection.
  #'
  #' @param fit Standardized fit object from engine_fit()
  #'
  #' @return Named numeric vector with:
  #'   - AIC: Akaike Information Criterion
  #'   - BIC: Bayesian Information Criterion
  #'   - LogLikelihood: Log-likelihood value
  #'
  #' @examples
  #' fit <- engine_fit("sGARCH", returns, "std")
  #' ic <- engine_infocriteria(fit)
  #' best_aic <- which.min(ic["AIC"])
  
  return(c(
    AIC = fit$aic, 
    BIC = fit$bic, 
    LogLikelihood = fit$loglik
  ))
}

engine_converged <- function(fit) {
  #' Check Model Convergence Status
  #'
  #' @description
  #' Checks whether the GARCH model fitting converged successfully.
  #' Non-convergence may indicate numerical issues, poor starting values,
  #' or model misspecification.
  #'
  #' @param fit Standardized fit object from engine_fit()
  #'
  #' @return Logical, TRUE if converged, FALSE otherwise
  #'
  #' @examples
  #' fit <- engine_fit("sGARCH", returns, "std")
  #' if (!engine_converged(fit)) {
  #'   warning("Model did not converge!")
  #' }
  
  return(fit$convergence)
}

# =============================================================================
# END OF ENGINE SELECTOR
# =============================================================================
