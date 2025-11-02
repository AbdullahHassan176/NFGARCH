# Engine Selector - Manual Engine Only
# Simplified API for manual GARCH engine

# Load manual GARCH functions
source("scripts/manual_garch/fit_sgarch_manual.R")
source("scripts/manual_garch/fit_gjr_manual.R")
source("scripts/manual_garch/fit_egarch_manual.R")
source("scripts/manual_garch/fit_tgarch_manual.R")
source("scripts/manual_garch/forecast_manual.R")

engine_fit <- function(model, returns, dist, submodel = NULL, engine = "manual") {
  # Manual GARCH fitting function
  # Returns a standardized fit object with consistent fields
  
  if (!is.null(engine) && engine != "manual") {
    warning("Only 'manual' engine is supported. Ignoring engine parameter: ", engine)
  }
  
  # Map sstd to std for manual engine (skewed-t not implemented yet)
  manual_dist <- if (dist == "sstd") "std" else dist
  
  if (model == "sGARCH") {
    fit <- fit_sgarch_manual(returns, dist = manual_dist)
  } else if (model == "gjrGARCH") {
    fit <- fit_gjr_manual(returns, dist = manual_dist)
  } else if (model == "eGARCH") {
    fit <- fit_egarch_manual(returns, dist = manual_dist)
  } else if (model == "TGARCH") {
    fit <- fit_tgarch_manual(returns, dist = manual_dist)
  } else if (model == "NF_tGARCH" && submodel == "TGARCH") {
    fit <- fit_tgarch_manual(returns, dist = manual_dist)
  } else if (model == "fGARCH" && submodel == "TGARCH") {
    fit <- fit_tgarch_manual(returns, dist = manual_dist)
  } else {
    stop("Manual engine does not support model: ", model, " with submodel: ", submodel)
  }
  
  # Return standardized fit object
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
    sigma = fit$sigma,
    residuals = fit$residuals,
    std_residuals = fit$std_residuals,
    fitted = fit$fitted,
    manual_fit = fit
  )
  
  return(result)
}

engine_forecast <- function(fit, h, engine = NULL) {
  # Manual forecasting function
  # Use manual forecast - call the predict method
  if (!is.null(fit$manual_fit) && !is.null(fit$manual_fit$predict)) {
    return(fit$manual_fit$predict(h))
  } else {
    # Fallback: create simple forecast
    return(list(
      sigma = rep(fit$sigma[length(fit$sigma)], h),
      mean = rep(fit$coef["mu"], h)
    ))
  }
}

engine_path <- function(fit, z, h, model, submodel = NULL, engine = NULL) {
  # Manual path simulation function for NF-GARCH
  # Use manual path - this should work without errors
  result <- manual_path(fit$manual_fit, z, h, model, submodel)
  
  # Ensure we return the correct length
  if (length(result$returns) != h) {
    warning("Manual path returned incorrect length. Expected: ", h, ", Got: ", length(result$returns))
  }
  
  return(result)
}

# Helper function to get standardized residuals for NF training
engine_residuals <- function(fit, standardize = TRUE) {
  # Get residuals in standardized format for NF training
  if (standardize) {
    return(fit$std_residuals)
  } else {
    return(fit$residuals)
  }
}

# Helper function to get model information criteria
engine_infocriteria <- function(fit) {
  # Get information criteria in standardized format
  return(c(AIC = fit$aic, BIC = fit$bic, LogLikelihood = fit$loglik))
}

# Helper function to check if fit was successful
engine_converged <- function(fit) {
  return(fit$convergence)
}
