# =============================================================================
# MANUAL GARCH FORECASTING AND PATH SIMULATION
# =============================================================================
#
# MULTI-STEP FORECAST METHODOLOGY (h > 1):
# Uses simulation-based approach where E[ε_{t+h}] = 0 for h > 1, reflecting
# the conditional expectation given information at time t. This is appropriate
# for the NF-GARCH framework where future innovations are drawn from the fitted
# normalizing flow.
#
# For standard GARCH models:
#   - 1-step: Uses actual last residual ε_t
#   - h-step (h>1): Sets ε_{t+h-1} = 0 in variance recursion
#   - Convergence: Forecasts converge to ω/(1-β) as h→∞ for eGARCH/TGARCH,
#                  and ω/(1-α-β) for sGARCH/gjrGARCH
#
# This approach is consistent with the NF-GARCH simulation framework and provides
# valid volatility forecasts. Alternative analytical approaches exist but this
# simulation-based method integrates naturally with NF path generation.
#
# =============================================================================

source("scripts/manual_garch/manual_garch_core.R")

manual_forecast <- function(fit, h) {
  # Multi-step ahead volatility forecast using simulation-based approach
  # 
  # For h=1: Uses actual last residual
  # For h>1: Sets E[ε_{t+h}]=0 (conditional expectation)
  #
  if (h <= 0) stop("h must be positive")
  
  last_sigma <- tail(fit$sigma, 1)
  last_residual <- tail(fit$residuals, 1)
  
  sigma_forecast <- numeric(h)
  sigma_forecast[1] <- forecast_one_step(fit, last_sigma, last_residual, fit$model_type)
  
  # Multi-step: set residual=0 (conditional expectation)
  for (i in 2:h) {
    sigma_forecast[i] <- forecast_one_step(fit, sigma_forecast[i-1], 0, fit$model_type)
  }
  
  # Find mu parameter (it has a prefix)
  mu_idx <- grep("mu", names(fit$coef))
  mu <- if (length(mu_idx) > 0) fit$coef[mu_idx[1]] else 0
  
  return(list(
    sigma = sigma_forecast,
    mean = rep(mu, h)
  ))
}

manual_path <- function(fit, z, h, model, submodel = NULL) {
  # Manual path simulation for NF-GARCH
  # fit: fitted manual GARCH object
  # z: innovations (NF residuals)
  # h: forecast horizon
  # model: model type (for compatibility)
  # submodel: submodel type (for compatibility)
  
  if (h <= 0) stop("h must be positive")
  if (length(z) < h) stop("Not enough innovations provided")
  
  # Use only the first h innovations
  z <- z[1:h]
  
  # Get last state
  last_sigma <- tail(fit$sigma, 1)
  last_residual <- tail(fit$residuals, 1)
  
  # Find mu parameter (it has a prefix)
  mu_idx <- grep("mu", names(fit$coef))
  mu <- if (length(mu_idx) > 0) fit$coef[mu_idx[1]] else 0
  
  # Initialize arrays
  sigma_path <- numeric(h)
  returns_path <- numeric(h)
  
  # First step
  sigma_path[1] <- forecast_one_step(fit, last_sigma, last_residual, fit$model_type)
  returns_path[1] <- mu + sigma_path[1] * z[1]
  
  # Subsequent steps
  for (i in 2:h) {
    # Pass raw residual for consistency with model fitting
    # Different models handle residuals differently:
    #   - sGARCH, gjrGARCH, TGARCH: use raw residuals (r_t = y_t - mu)
    #   - eGARCH: uses standardized residuals internally (z_t = r_t / sigma_t)
    raw_residual <- returns_path[i-1] - mu
    sigma_path[i] <- forecast_one_step(fit, sigma_path[i-1], raw_residual, fit$model_type)
    returns_path[i] <- mu + sigma_path[i] * z[i]
  }
  
  return(list(
    returns = returns_path,
    sigma = sigma_path,
    innovations = z
  ))
}

# Compatibility function to match rugarch interface
manual_simulate_nf_garch <- function(fit, z_nf, horizon = 40, model = NULL, submodel = NULL) {
  # Wrapper function to match the existing simulate_nf_garch interface
  # This allows seamless integration with the existing pipeline
  
  if (is.null(model)) {
    model <- fit$model_type
  }
  
  result <- manual_path(fit, z_nf, horizon, model, submodel)
  
  # Return in the same format as the original simulate_nf_garch
  return(list(
    returns = result$returns,
    sigma = result$sigma,
    innovations = result$innovations
  ))
}
