# Essential Statistical Tests for Optimized Pipeline
# Maintains rigor while reducing computational load

# Core forecasting metrics
evaluate_forecasting <- function(predictions, actuals) {
  metrics <- list(
    RMSE = sqrt(mean((predictions - actuals)^2)),
    MAE = mean(abs(predictions - actuals)),
    LogLik = sum(dnorm(actuals, predictions, sd(predictions - actuals), log = TRUE))
  )
  return(metrics)
}

# Core distributional metrics
evaluate_distributional <- function(real_data, synthetic_data) {
  metrics <- list(
    KS_distance = ks.test(real_data, synthetic_data)$statistic,
    Wasserstein = wasserstein_distance(real_data, synthetic_data),
    KL_divergence = kl_divergence(real_data, synthetic_data)
  )
  return(metrics)
}

# Essential VaR backtesting
evaluate_var <- function(returns, var_forecasts, confidence_level = 0.95) {
  exceedances <- returns < var_forecasts
  exceedance_rate <- mean(exceedances)
  
  # Kupiec test
  n <- length(returns)
  x <- sum(exceedances)
  p <- 1 - confidence_level
  
  # Likelihood ratio test
  lr_stat <- 2 * (x * log(x/(n*p)) + (n-x) * log((n-x)/(n*(1-p))))
  p_value <- 1 - pchisq(lr_stat, 1)
  
  return(list(
    exceedance_rate = exceedance_rate,
    kupiec_p_value = p_value,
    passes_kupiec = p_value > 0.05
  ))
}

# Essential stylized facts (reduced set)
evaluate_stylized_facts <- function(returns) {
  facts <- list(
    # Heavy tails
    kurtosis = moments::kurtosis(returns),
    excess_kurtosis = moments::kurtosis(returns) - 3,
    
    # Skewness
    skewness = moments::skewness(returns),
    
    # Volatility clustering (simplified)
    arch_lm_p = arch_lm_test(returns)$p.value,
    
    # Autocorrelation in squared returns
    acf_squared = acf(returns^2, plot = FALSE)$acf[2]
  )
  
  return(facts)
}

# ARCH-LM test (simplified)
arch_lm_test <- function(returns, lags = 5) {
  n <- length(returns)
  squared_returns <- returns^2
  
  # Simple ARCH test
  lm_result <- lm(squared_returns[2:n] ~ squared_returns[1:(n-1)])
  f_stat <- summary(lm_result)$fstatistic[1]
  p_value <- pf(f_stat, 1, n-2, lower.tail = FALSE)
  
  return(list(p.value = p_value))
}

# Wasserstein distance (simplified)
wasserstein_distance <- function(x, y) {
  # Simplified 1D Wasserstein distance
  x_sorted <- sort(x)
  y_sorted <- sort(y)
  n <- min(length(x), length(y))
  
  return(mean(abs(x_sorted[1:n] - y_sorted[1:n])))
}

# KL divergence (simplified)
kl_divergence <- function(p, q) {
  # Simplified KL divergence using histograms
  breaks <- seq(min(c(p, q)), max(c(p, q)), length.out = 50)
  p_hist <- hist(p, breaks = breaks, plot = FALSE)$density
  q_hist <- hist(q, breaks = breaks, plot = FALSE)$density
  
  # Add small epsilon to avoid log(0)
  epsilon <- 1e-10
  p_hist <- p_hist + epsilon
  q_hist <- q_hist + epsilon
  
  return(sum(p_hist * log(p_hist / q_hist)))
}

# Main evaluation function
evaluate_optimized <- function(model_results, config) {
  cat("Running essential statistical tests...\n")
  
  results <- list()
  
  for (i in seq_along(model_results)) {
    result <- model_results[[i]]
    
    # Forecasting metrics
    if (!is.null(result$predictions) && !is.null(result$actuals)) {
      results[[i]]$forecasting <- evaluate_forecasting(
        result$predictions, 
        result$actuals
      )
    }
    
    # Distributional metrics
    if (!is.null(result$real_data) && !is.null(result$synthetic_data)) {
      results[[i]]$distributional <- evaluate_distributional(
        result$real_data,
        result$synthetic_data
      )
    }
    
    # VaR backtesting
    if (!is.null(result$returns) && !is.null(result$var_forecasts)) {
      results[[i]]$var <- evaluate_var(
        result$returns,
        result$var_forecasts
      )
    }
    
    # Stylized facts
    if (!is.null(result$returns)) {
      results[[i]]$stylized_facts <- evaluate_stylized_facts(result$returns)
    }
  }
  
  return(results)
}


