# =============================================================================
# MANUAL sGARCH(1,1) IMPLEMENTATION
# =============================================================================
#
# SPECIFICATION:
#   Mean equation:     r_t = μ + ε_t
#   Innovation:        ε_t = σ_t z_t, where z_t ~ D(0,1)
#   Variance equation: σ²_t = ω + α ε²_{t-1} + β σ²_{t-1}
#
# DISTRIBUTIONS SUPPORTED: norm (Normal), std (Student-t)
#
# ESTIMATION METHOD: Maximum Likelihood Estimation (MLE)
# OPTIMIZER: BFGS with parameter transformation for constraint enforcement
#
# CONSTRAINT ENFORCEMENT:
#   - ω > 0         : Via exp(θ₂)
#   - α, β ∈ (0,1)  : Via logistic transformation
#   - α + β < 1     : Via product constraint β = (1-ε)(1-α)β_raw, ε=1e-4
#
# STATIONARITY: Automatically satisfied by constraint enforcement above.
# Unconditional variance = ω/(1-α-β) is guaranteed to be finite and positive.
#
# =============================================================================

source("scripts/manual_garch/manual_garch_core.R")

fit_sgarch_manual <- function(returns, dist = c("norm", "std"), init = NULL) {
  dist <- match.arg(dist)
  n <- length(returns)
  
  # Initialize parameters if not provided
  if (is.null(init)) {
    sample_var <- var(returns, na.rm = TRUE)
    sample_mean <- mean(returns, na.rm = TRUE)
    
    if (dist == "norm") {
      # 4 parameters: μ, ω, α, β (UNCONSTRAINED theta values for optim)
      # Use inverse logit to convert constrained initial guesses to unconstrained theta
      alpha_init <- 0.1
      beta_raw_init <- 0.8  # This will be constrained further by transform_params
      
      init <- c(
        mu = sample_mean,
        omega = log(sample_var * 0.05),  # log transform
        alpha = log(alpha_init / (1 - alpha_init)),  # inverse logit
        beta = log(beta_raw_init / (1 - beta_raw_init))  # inverse logit for beta_raw
      )
    } else if (dist == "std") {
      # 5 parameters: μ, ω, α, β, ν (UNCONSTRAINED theta values for optim)
      alpha_init <- 0.1
      beta_raw_init <- 0.8
      
      init <- c(
        mu = sample_mean,
        omega = log(sample_var * 0.05),
        alpha = log(alpha_init / (1 - alpha_init)),  # inverse logit
        beta = log(beta_raw_init / (1 - beta_raw_init)),  # inverse logit
        nu = log(5)  # ν = 2 + exp(log(5)) = 7
      )
    }
  }
  
  # Negative log-likelihood function
  neg_ll <- function(theta) {
    tryCatch({
      # Transform parameters
      if (dist == "norm") {
        params <- transform_params(theta, "sGARCH")
        mu <- params$mu
        omega <- params$omega
        alpha <- params$alpha
        beta <- params$beta
        
        # Initialize variance recursion
        sigma2 <- rep(sample_var, n)
        residuals <- returns - mu
        
        # Variance recursion with burn-in
        for (t in 2:n) {
          sigma2[t] <- omega + alpha * residuals[t-1]^2 + beta * sigma2[t-1]
          sigma2[t] <- pmax(sigma2[t], var_floor)
        }
        
        sigma <- sqrt(sigma2)
        
        # Compute log-likelihood
        ll <- compute_ll_normal(returns, sigma, mu)
        return(-ll)
        
      } else if (dist == "std") {
        # Extract parameters
        mu <- theta[1]
        omega <- exp(theta[2])
        alpha <- 1 / (1 + exp(-theta[3]))
        beta_raw <- 1 / (1 + exp(-theta[4]))
        beta <- (1 - 1e-4) * (1 - alpha) * beta_raw
        nu <- 2 + exp(theta[5])  # ν > 2
        
        # Initialize variance recursion
        sigma2 <- rep(sample_var, n)
        residuals <- returns - mu
        
        # Variance recursion with burn-in
        for (t in 2:n) {
          sigma2[t] <- omega + alpha * residuals[t-1]^2 + beta * sigma2[t-1]
          sigma2[t] <- pmax(sigma2[t], var_floor)
        }
        
        sigma <- sqrt(sigma2)
        
        # Compute log-likelihood
        ll <- compute_ll_student_t(returns, sigma, mu, nu)
        return(-ll)
      }
    }, error = function(e) {
      return(1e10)  # Return large value for failed optimization
    })
  }
  
  # DEBUG: Check initial neg_ll
  init_ll <- neg_ll(init)
  cat("DEBUG sGARCH optim: init_ll=", sprintf("%.4f", init_ll), "\n", sep="")
  
  # Set parameter bounds for L-BFGS-B
  mu_bound <- max(abs(sample_mean) * 2, 0.005)
  lower <- c(-mu_bound, log(sample_var * 0.001), log(0.01/(1-0.01)), log(0.01/(1-0.01)))
  upper <- c(mu_bound, log(sample_var * 0.5), log(0.3/(1-0.3)), log(0.95/(1-0.95)))
  
  # Optimize with L-BFGS-B (box-constrained)
  opt_result <- optim(
    par = init,
    fn = neg_ll,
    method = "L-BFGS-B",  # Box-constrained optimizer
    lower = lower,
    upper = upper,
    control = list(
      maxit = 500,         # More iterations
      factr = 1e7,         # Moderate precision
      trace = 1            # Print optimization progress
    )
  )
  
  # Check convergence
  cat("DEBUG sGARCH optim: final_ll=", sprintf("%.4f", opt_result$value), 
      " convergence=", opt_result$convergence, " counts=", opt_result$counts[1], "\n", sep="")
  if (opt_result$convergence != 0) {
    warning("Optimization may not have converged. Convergence code: ", opt_result$convergence)
  }
  
  # Extract final parameters and compute fitted values
  if (dist == "norm") {
    params <- transform_params(opt_result$par, "sGARCH")
    mu <- params$mu
    omega <- params$omega
    alpha <- params$alpha
    beta <- params$beta
    
    # Compute final sigma and residuals
    sigma2 <- rep(sample_var, n)
    residuals <- returns - mu
    
    for (t in 2:n) {
      sigma2[t] <- omega + alpha * residuals[t-1]^2 + beta * sigma2[t-1]
      sigma2[t] <- pmax(sigma2[t], var_floor)
    }
    
    sigma <- sqrt(sigma2)
    std_residuals <- residuals / sigma
    
    # DEBUG: Check standardization
    cat("DEBUG sGARCH: mu=", sprintf("%.6f", mu), " omega=", sprintf("%.6f", omega), 
        " alpha=", sprintf("%.4f", alpha), " beta=", sprintf("%.4f", beta), "\n", sep="")
    cat("  residuals: mean=", sprintf("%.6f", mean(residuals)), " std=", sprintf("%.6f", sd(residuals)), "\n", sep="")
    cat("  sigma: mean=", sprintf("%.6f", mean(sigma)), " std=", sprintf("%.6f", sd(sigma)), "\n", sep="")
    cat("  std_residuals: mean=", sprintf("%.6f", mean(std_residuals)), " std=", sprintf("%.6f", sd(std_residuals)), "\n", sep="")
    
    # Compute log-likelihood and information criteria
    ll <- -opt_result$value
    ic <- aic_bic_from_ll(ll, 4, n)  # 4 parameters
    
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
      model_type = "sGARCH",
      distribution = dist
    )
    
  } else if (dist == "std") {
    # Extract parameters
    mu <- opt_result$par[1]
    omega <- exp(opt_result$par[2])
    alpha <- 1 / (1 + exp(-opt_result$par[3]))
    beta_raw <- 1 / (1 + exp(-opt_result$par[4]))
    beta <- (1 - 1e-4) * (1 - alpha) * beta_raw
    nu <- 2 + exp(opt_result$par[5])
    
    # Compute final sigma and residuals
    sigma2 <- rep(sample_var, n)
    residuals <- returns - mu
    
    for (t in 2:n) {
      sigma2[t] <- omega + alpha * residuals[t-1]^2 + beta * sigma2[t-1]
      sigma2[t] <- pmax(sigma2[t], var_floor)
    }
    
    sigma <- sqrt(sigma2)
    
    # Standardize residuals - Student-t requires additional scaling
    # Student-t(nu) has Var(z) = nu/(nu-2), so scale by sqrt((nu-2)/nu) to get Var=1
    std_residuals <- residuals / sigma
    if (nu > 2) {
      std_residuals <- std_residuals * sqrt((nu - 2) / nu)
    }
    
    # Compute log-likelihood and information criteria
    ll <- -opt_result$value
    ic <- aic_bic_from_ll(ll, 5, n)  # 5 parameters
    
    # Create return object
    result <- list(
      par_constrained = c(mu = mu, omega = omega, alpha = alpha, beta = beta, nu = nu),
      par_unconstrained = opt_result$par,
      loglik = ll,
      aic = ic$aic,
      bic = ic$bic,
      convergence = opt_result$convergence == 0,
      sigma = sigma,
      residuals = residuals,
      std_residuals = std_residuals,
      fitted = rep(mu, n),
      coef = c(mu = mu, omega = omega, alpha = alpha, beta = beta, nu = nu),
      model_type = "sGARCH",
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
    sigma_forecast[1] <- forecast_one_step(result, last_sigma, last_residual, "sGARCH")
    
    for (i in 2:h) {
      sigma_forecast[i] <- forecast_one_step(result, sigma_forecast[i-1], 0, "sGARCH")
    }
    
    return(list(
      sigma = sigma_forecast,
      mean = rep(result$coef["mu"], h)
    ))
  }
  
  return(result)
}
