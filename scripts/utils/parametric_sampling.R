# Parametric Sampling for Standard GARCH
# Functions for generating innovations from parametric distributions

#' Sample residuals from parametric distribution
#' 
#' For Standard GARCH models, we should sample from the assumed parametric 
#' distribution (Normal or Student-t) rather than bootstrap from training residuals.
#' This gives Standard GARCH its full advantage from the parametric assumptions.
#' 
#' @param n Number of samples to generate
#' @param distribution Distribution type ("norm", "std", "sstd", etc.)
#' @param shape Shape parameter (degrees of freedom for Student-t)
#' @param skew Skewness parameter (for skewed distributions)
#' @return Vector of standardized innovations (mean=0, var=1)
#' @export
sample_parametric_residuals <- function(n, distribution, shape = NULL, skew = NULL) {
  if (n < 1) stop("n must be >= 1")
  
  distribution <- tolower(distribution)
  
  if (distribution == "norm") {
    # Standard normal
    return(rnorm(n, mean = 0, sd = 1))
    
  } else if (distribution %in% c("std", "t")) {
    # Student-t distribution
    if (is.null(shape)) {
      warning("Student-t shape parameter not provided, using df=5")
      shape <- 5
    }
    
    # Sample from Student-t and standardize to unit variance
    df <- shape
    if (df <= 2) {
      warning("Student-t df <= 2 has infinite variance, using df=2.5")
      df <- 2.5
    }
    
    z <- rt(n, df = df)
    # Standardize to have unit variance: Var[t_df] = df/(df-2)
    z <- z / sqrt(df / (df - 2))
    return(z)
    
  } else if (distribution == "sstd" || distribution == "skewt") {
    # Skewed Student-t (simplified implementation)
    # For full implementation, would need skewed.GeneralizedT or similar
    if (is.null(shape)) shape <- 5
    if (is.null(skew)) skew <- 1  # Symmetric
    
    df <- shape
    if (df <= 2) df <- 2.5
    
    # Simple skewness by adjusting signs
    z <- rt(n, df = df)
    z <- z / sqrt(df / (df - 2))
    
    # Apply skewness (simplified)
    if (skew != 1) {
      # Flip signs with probability based on skewness
      flip_prob <- 1 / (1 + skew)
      signs <- ifelse(runif(n) < flip_prob, -1, 1)
      z <- z * signs
    }
    
    return(z)
    
  } else if (distribution == "ged") {
    # Generalized Error Distribution
    if (is.null(shape)) shape <- 2  # shape=2 is normal
    
    # For simplicity, fallback to normal
    # Full GED requires gamma functions
    warning("GED distribution not fully implemented, using normal")
    return(rnorm(n, mean = 0, sd = 1))
    
  } else {
    # Unknown distribution, fallback to normal
    warning("Unknown distribution '", distribution, "', using normal")
    return(rnorm(n, mean = 0, sd = 1))
  }
}


#' Extract distribution parameters from a fitted model
#' 
#' @param fit Fitted GARCH model object
#' @param distribution Distribution name
#' @return List with shape and skew parameters (or NULL if not applicable)
#' @export
extract_distribution_params <- function(fit, distribution) {
  params <- list(shape = NULL, skew = NULL)
  
  if (is.null(fit) || is.null(fit$manual_fit)) {
    return(params)
  }
  
  distribution <- tolower(distribution)
  
  # Extract shape parameter (degrees of freedom for Student-t)
  if (distribution %in% c("std", "t", "sstd", "skewt")) {
    if (!is.null(fit$manual_fit$shape)) {
      params$shape <- fit$manual_fit$shape
    } else if (!is.null(fit$manual_fit$df)) {
      params$shape <- fit$manual_fit$df
    }
  }
  
  # Extract skewness parameter
  if (distribution %in% c("sstd", "skewt")) {
    if (!is.null(fit$manual_fit$skew)) {
      params$skew <- fit$manual_fit$skew
    }
  }
  
  return(params)
}


#' Generate parametric residuals for a fitted Standard GARCH model
#' 
#' Wrapper function that extracts parameters and generates samples
#' 
#' @param fit Fitted GARCH model
#' @param n Number of samples
#' @param distribution Distribution name
#' @return Vector of standardized parametric innovations
#' @export
generate_standard_garch_innovations <- function(fit, n, distribution) {
  params <- extract_distribution_params(fit, distribution)
  
  innovations <- sample_parametric_residuals(
    n = n,
    distribution = distribution,
    shape = params$shape,
    skew = params$skew
  )
  
  return(innovations)
}
