# =============================================================================
# MANUAL TGARCH(1,1) IMPLEMENTATION - ZAKOIAN (1994) SPECIFICATION
# =============================================================================
#
# SPECIFICATION: Zakoian (1994) Threshold GARCH with conditional standard deviation
#   Mean equation:     r_t = μ + ε_t
#   Innovation:        ε_t = σ_t z_t, where z_t ~ D(0,1)
#   Volatility equation: σ_t = ω + α |ε_{t-1}| + η I(ε_{t-1}<0)|ε_{t-1}| + β σ_{t-1}
#   Threshold indicator: I(ε_{t-1}<0) = 1 if ε_{t-1}<0, else 0
#
# DISTRIBUTIONS SUPPORTED: norm (Normal), std (Student-t)
#
# IMPLEMENTATION NOTE:
# This implements Zakoian's TGARCH specification using CONDITIONAL STANDARD DEVIATION
# with absolute residuals. This is distinct from variance-based TGARCH formulations
# (e.g., fGARCH submodels) but is a widely-used and valid alternative specification.
#
# Reference: Zakoian, J-M. (1994). "Threshold Heteroskedastic Models."
#            Journal of Economic Dynamics and Control, 18(5), 931-955.
#
# ASYMMETRY INTERPRETATION:
#   - η > 0: Negative shocks increase volatility more than positive shocks
#   - η < 0: Positive shocks increase volatility more (unusual)
#   - η = 0: Symmetric response (reduces to absolute-value GARCH)
#
# =============================================================================

source("scripts/manual_garch/manual_garch_core.R")

fit_tgarch_manual <- function(returns, dist = c("norm", "std"), init = NULL) {
  dist <- match.arg(dist)
  n <- length(returns)
  
  # Initialize parameters with better starting values
  sample_var <- var(returns, na.rm = TRUE)
  sample_mean <- mean(returns, na.rm = TRUE)
  sample_sd <- sqrt(sample_var)
  
  if (is.null(init)) {
    if (dist == "norm") {
      # 5 parameters: μ, ω, α, η, β
      # Better initial values based on GARCH literature
      init <- c(
        mu = sample_mean,
        omega = log(sample_sd * 0.05),  # Small constant
        alpha = qlogis(0.1),            # Transform to unconstrained: α ≈ 0.1
        eta = 0,                         # Start with no asymmetry
        beta = qlogis(0.85)             # Transform to unconstrained: β ≈ 0.85
      )
    } else if (dist == "std") {
      # 6 parameters: μ, ω, α, η, β, ν
      init <- c(
        mu = sample_mean,
        omega = log(sample_sd * 0.05),
        alpha = qlogis(0.1),
        eta = 0,
        beta = qlogis(0.85),
        nu = log(8)  # Start with df=10 (2 + exp(log(8)))
      )
    }
  }
  
  # Negative log-likelihood function
  neg_ll <- function(theta) {
    tryCatch({
      # Transform parameters
      if (dist == "norm") {
        params <- transform_params(theta, "TGARCH")
        mu <- params$mu
        omega <- params$omega
        alpha <- params$alpha
        eta <- params$eta
        beta <- params$beta
        
        # Initialize variance recursion
        sigma <- rep(sqrt(sample_var), n)
        residuals <- returns - mu
        
        # Variance recursion with burn-in
        for (t in 2:n) {
          indicator <- ifelse(residuals[t-1] < 0, 1, 0)
          sigma[t] <- omega + alpha * abs(residuals[t-1]) + eta * indicator * abs(residuals[t-1]) + beta * sigma[t-1]
          sigma[t] <- pmax(sigma[t], safe_sqrt(var_floor))
        }
        
        # Compute log-likelihood
        ll <- compute_ll_normal(returns, sigma, mu)
        return(-ll)
        
      } else if (dist == "std") {
        # Extract parameters
        mu <- theta[1]
        omega <- exp(theta[2])
        alpha <- 1 / (1 + exp(-theta[3]))
        eta <- theta[4]  # asymmetry parameter
        beta <- 1 / (1 + exp(-theta[5]))
        nu <- 2 + exp(theta[6])
        
        # Initialize variance recursion
        sigma <- rep(sqrt(sample_var), n)
        residuals <- returns - mu
        
        # Variance recursion with burn-in
        for (t in 2:n) {
          indicator <- ifelse(residuals[t-1] < 0, 1, 0)
          sigma[t] <- omega + alpha * abs(residuals[t-1]) + eta * indicator * abs(residuals[t-1]) + beta * sigma[t-1]
          sigma[t] <- pmax(sigma[t], safe_sqrt(var_floor))
        }
        
        # Compute log-likelihood
        ll <- compute_ll_student_t(returns, sigma, mu, nu)
        return(-ll)
      }
    }, error = function(e) {
      return(1e10)
    })
  }
  
  # Set up box constraints for L-BFGS-B
  # Tightly constrain mu around sample mean (±0.005 for daily returns)
  mu_bound <- max(abs(sample_mean) * 2, 0.005)
  
  if (dist == "norm") {
    lower <- c(sample_mean - mu_bound, log(1e-10), -10, -5, -10)
    upper <- c(sample_mean + mu_bound, log(sample_var), 10, 5, 10)
  } else {
    lower <- c(sample_mean - mu_bound, log(1e-10), -10, -5, -10, log(1))
    upper <- c(sample_mean + mu_bound, log(sample_var), 10, 5, 10, log(20))
  }
  
  # Optimize with L-BFGS-B (box-constrained)
  opt_result <- optim(
    par = init,
    fn = neg_ll,
    method = "L-BFGS-B",  # Box-constrained optimizer
    lower = lower,
    upper = upper,
    control = list(
      maxit = 300,        # More iterations with constraints
      factr = 1e7         # Moderate tolerance (1e7 * machine precision)
    )
  )
  
  # If convergence failed, try from different starting point
  if (opt_result$convergence != 0 || opt_result$value > 1e8) {
    if (dist == "norm") {
      init2 <- c(sample_mean, log(sample_sd * 0.01), qlogis(0.05), 0, qlogis(0.9))
    } else {
      init2 <- c(sample_mean, log(sample_sd * 0.01), qlogis(0.05), 0, qlogis(0.9), log(5))
    }
    
    opt_result2 <- optim(
      par = init2,
      fn = neg_ll,
      method = "L-BFGS-B",
      lower = lower,
      upper = upper,
      control = list(maxit = 300, factr = 1e7)
    )
    
    # Use better result
    if (opt_result2$value < opt_result$value) {
      opt_result <- opt_result2
    }
  }
  
  # Check convergence
  if (opt_result$convergence != 0) {
    warning("Optimization may not have converged. Convergence code: ", opt_result$convergence)
  }
  
  # Extract final parameters and compute fitted values
  if (dist == "norm") {
    params <- transform_params(opt_result$par, "TGARCH")
    mu <- params$mu
    omega <- params$omega
    alpha <- params$alpha
    eta <- params$eta
    beta <- params$beta
    
    # Compute final sigma and residuals
    sigma <- rep(sqrt(sample_var), n)
    residuals <- returns - mu
    
    for (t in 2:n) {
      indicator <- ifelse(residuals[t-1] < 0, 1, 0)
      sigma[t] <- omega + alpha * abs(residuals[t-1]) + eta * indicator * abs(residuals[t-1]) + beta * sigma[t-1]
      sigma[t] <- pmax(sigma[t], safe_sqrt(var_floor))
    }
    
    std_residuals <- residuals / sigma
    
    # Compute log-likelihood and information criteria
    ll <- -opt_result$value
    ic <- aic_bic_from_ll(ll, 5, n)  # 5 parameters
    
    # Create return object
    result <- list(
      par_constrained = params$par_constrained,
      par_unconstrained = opt_result$par,
      loglik = ll,
      aic = ic$aic,
      bic = ic$bic,
      convergence = opt_result$convergence == 0,
      sigma = sigma,
      residuals = residuals,
      std_residuals = std_residuals,
      fitted = rep(mu, n),
      coef = params$par_constrained,
      model_type = "TGARCH",
      distribution = dist
    )
    
  } else if (dist == "std") {
    # Extract parameters
    mu <- opt_result$par[1]
    omega <- exp(opt_result$par[2])
    alpha <- 1 / (1 + exp(-opt_result$par[3]))
    eta <- opt_result$par[4]
    beta <- 1 / (1 + exp(-opt_result$par[5]))
    nu <- 2 + exp(opt_result$par[6])
    
    # Compute final sigma and residuals
    sigma <- rep(sqrt(sample_var), n)
    residuals <- returns - mu
    
    for (t in 2:n) {
      indicator <- ifelse(residuals[t-1] < 0, 1, 0)
      sigma[t] <- omega + alpha * abs(residuals[t-1]) + eta * indicator * abs(residuals[t-1]) + beta * sigma[t-1]
      sigma[t] <- pmax(sigma[t], safe_sqrt(var_floor))
    }
    
    # Standardize residuals - Student-t requires additional scaling
    # Student-t(nu) has Var(z) = nu/(nu-2), so scale by sqrt((nu-2)/nu) to get Var=1
    std_residuals <- residuals / sigma
    if (nu > 2) {
      std_residuals <- std_residuals * sqrt((nu - 2) / nu)
    }
    
    # Compute log-likelihood and information criteria
    ll <- -opt_result$value
    ic <- aic_bic_from_ll(ll, 6, n)  # 6 parameters
    
    # Create return object
    result <- list(
      par_constrained = c(mu = mu, omega = omega, alpha = alpha, eta = eta, beta = beta, nu = nu),
      par_unconstrained = opt_result$par,
      loglik = ll,
      aic = ic$aic,
      bic = ic$bic,
      convergence = opt_result$convergence == 0,
      sigma = sigma,
      residuals = residuals,
      std_residuals = std_residuals,
      fitted = rep(mu, n),
      coef = c(mu = mu, omega = omega, alpha = alpha, eta = eta, beta = beta, nu = nu),
      model_type = "TGARCH",
      distribution = dist
    )
  }
  
  # Add predict function
  result$predict <- function(h) {
    # h-step ahead forecast
    if (h <= 0) stop("h must be positive")
    
    last_sigma <- tail(result$sigma, 1)
    last_residual <- tail(result$residuals, 1)
    
    sigma_forecast <- numeric(h)
    sigma_forecast[1] <- forecast_one_step(result, last_sigma, last_residual, "TGARCH")
    
    for (i in 2:h) {
      sigma_forecast[i] <- forecast_one_step(result, sigma_forecast[i-1], 0, "TGARCH")
    }
    
    return(list(
      sigma = sigma_forecast,
      mean = rep(result$coef["mu"], h)
    ))
  }
  
  return(result)
}
