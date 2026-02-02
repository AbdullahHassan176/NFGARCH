#!/usr/bin/env Rscript
# Synthetic Data Generating Process (DGP) for Distribution Recovery Experiment
# Simulates GARCH(1,1) returns with known innovation distribution

# Load centralized seed configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
  set.seed(REPRODUCIBILITY_SEED)
} else {
  set.seed(123)
}

library(xts)

# =============================================================================
# INNOVATION SAMPLERS
# =============================================================================

#' Sample from Student-t distribution
#' @param n Number of samples
#' @param nu Degrees of freedom (must be > 2)
#' @return Vector of standardized Student-t samples
sample_student_t <- function(n, nu = 5) {
  if (nu <= 2) stop("nu must be > 2 for finite variance")
  # Sample from Student-t and standardize
  z <- rt(n, df = nu)
  # Standardize to have mean 0 and variance 1
  z <- (z - mean(z)) / sd(z)
  return(z)
}

#' Sample from skewed Student-t distribution (Fernandez & Steel, 1998)
#' @param n Number of samples
#' @param nu Degrees of freedom
#' @param xi Skewness parameter (xi > 0, xi=1 is symmetric)
#' @return Vector of standardized skewed-t samples
sample_skewed_t <- function(n, nu = 5, xi = 1.5) {
  if (nu <= 2) stop("nu must be > 2 for finite variance")
  if (xi <= 0) stop("xi must be > 0")
  
  # Generate symmetric Student-t
  z_sym <- rt(n, df = nu)
  
  # Apply skewness transformation
  # If z < 0, scale by 1/xi; if z >= 0, scale by xi
  z_skew <- ifelse(z_sym < 0, z_sym / xi, z_sym * xi)
  
  # Standardize to have mean 0 and variance 1
  z_skew <- (z_skew - mean(z_skew)) / sd(z_skew)
  
  return(z_skew)
}

#' Sample from mixture of two Gaussians
#' @param n Number of samples
#' @param p Mixing probability (0 < p < 1)
#' @param mu1 Mean of first component
#' @param mu2 Mean of second component
#' @param sigma1 SD of first component
#' @param sigma2 SD of second component
#' @return Vector of standardized mixture samples
sample_mixture_gaussian <- function(n, p = 0.3, mu1 = -0.5, mu2 = 0.5, 
                                     sigma1 = 1.0, sigma2 = 1.5) {
  # Sample component indicators
  component <- rbinom(n, size = 1, prob = p)
  
  # Sample from each component
  z <- ifelse(component == 1, 
              rnorm(n, mean = mu1, sd = sigma1),
              rnorm(n, mean = mu2, sd = sigma2))
  
  # Standardize
  z <- (z - mean(z)) / sd(z)
  
  return(z)
}

# =============================================================================
# GARCH(1,1) SIMULATION
# =============================================================================

#' Simulate GARCH(1,1) process
#' @param T Sample size
#' @param omega Intercept parameter (must be > 0)
#' @param alpha ARCH parameter (must be >= 0)
#' @param beta GARCH parameter (must be >= 0)
#' @param innovation_sampler Function that takes n and returns n innovations
#' @param mu Mean return (default 0)
#' @param var_floor Minimum variance floor (default 1e-12)
#' @return List with:
#'   - returns: r_t (observed returns)
#'   - sigma: σ_t (volatility series)
#'   - eps: ε_t (error terms)
#'   - z: z_t (standardized innovations, GROUND TRUTH)
simulate_garch11 <- function(T, omega, alpha, beta, innovation_sampler, 
                              mu = 0, var_floor = 1e-12) {
  
  # Check stationarity condition
  if (alpha + beta >= 1) {
    warning("Non-stationary: alpha + beta = ", alpha + beta, 
            ". Results may be unreliable.")
  }
  
  # Initialize
  n <- T + 100  # Burn-in period
  sigma2 <- numeric(n)
  eps <- numeric(n)
  r <- numeric(n)
  
  # Initial variance (unconditional variance)
  sigma2[1] <- omega / (1 - alpha - beta)
  if (!is.finite(sigma2[1]) || sigma2[1] <= 0) {
    sigma2[1] <- omega
  }
  
  # Generate innovations
  z <- innovation_sampler(n)
  
  # Simulate GARCH recursion
  for (t in 2:n) {
    # Variance recursion: σ²_t = ω + α ε²_{t-1} + β σ²_{t-1}
    sigma2[t] <- omega + alpha * eps[t-1]^2 + beta * sigma2[t-1]
    sigma2[t] <- pmax(sigma2[t], var_floor)
    
    # Error term: ε_t = σ_t * z_t
    eps[t] <- sqrt(sigma2[t]) * z[t]
    
    # Return: r_t = μ + ε_t
    r[t] <- mu + eps[t]
  }
  
  # Remove burn-in
  keep_idx <- 101:n
  return(list(
    returns = r[keep_idx],
    sigma = sqrt(sigma2[keep_idx]),
    eps = eps[keep_idx],
    z = z[keep_idx]  # TRUE innovations (ground truth)
  ))
}

# =============================================================================
# DEFAULT DGP CONFIGURATION
# =============================================================================

#' Get default DGP configuration
#' @return List with DGP parameters
get_default_dgp_config <- function() {
  list(
    T = 2000,              # Sample size
    omega = 0.0001,        # GARCH intercept
    alpha = 0.1,           # ARCH parameter
    beta = 0.85,           # GARCH parameter (alpha + beta = 0.95 < 1, stationary)
    mu = 0,               # Mean return
    innovation_type = "skewed_t",  # "student_t", "skewed_t", or "mixture_gaussian"
    innovation_params = list(
      nu = 5,             # Degrees of freedom for t-distributions
      xi = 1.5            # Skewness for skewed-t
    ),
    seed = if (exists("REPRODUCIBILITY_SEED")) REPRODUCIBILITY_SEED else 123
  )
}

#' Create innovation sampler from config
#' @param innovation_type Type of innovation distribution
#' @param innovation_params Parameters for the distribution
#' @return Function that samples innovations
create_innovation_sampler <- function(innovation_type, innovation_params) {
  if (innovation_type == "student_t") {
    function(n) sample_student_t(n, nu = innovation_params$nu)
  } else if (innovation_type == "skewed_t") {
    function(n) sample_skewed_t(n, nu = innovation_params$nu, 
                                xi = innovation_params$xi)
  } else if (innovation_type == "mixture_gaussian") {
    function(n) sample_mixture_gaussian(n, 
                                       p = innovation_params$p %||% 0.3,
                                       mu1 = innovation_params$mu1 %||% -0.5,
                                       mu2 = innovation_params$mu2 %||% 0.5,
                                       sigma1 = innovation_params$sigma1 %||% 1.0,
                                       sigma2 = innovation_params$sigma2 %||% 1.5)
  } else {
    stop("Unknown innovation_type: ", innovation_type)
  }
}

# Helper for default values
`%||%` <- function(x, y) if (is.null(x)) y else x

