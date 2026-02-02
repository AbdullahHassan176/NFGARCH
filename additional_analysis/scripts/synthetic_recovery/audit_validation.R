#!/usr/bin/env Rscript
# Audit Validation Functions
# Sanity checks and invariants for the synthetic recovery experiment

library(moments)

# =============================================================================
# DGP VALIDATION
# =============================================================================

#' Validate true innovation distribution properties
validate_z_true <- function(z_true, innovation_type, innovation_params, tolerance_mean = 0.01, tolerance_sd = 0.01) {
  results <- list()
  
  # Mean check
  mean_z <- mean(z_true, na.rm = TRUE)
  results$mean_ok <- abs(mean_z) < tolerance_mean
  results$mean_value <- mean_z
  results$mean_tolerance <- tolerance_mean
  
  # SD check
  sd_z <- sd(z_true, na.rm = TRUE)
  results$sd_ok <- abs(sd_z - 1) < tolerance_sd
  results$sd_value <- sd_z
  results$sd_tolerance <- tolerance_sd
  
  # Skewness check (for skewed-t)
  if (innovation_type == "skewed_t") {
    skew_z <- moments::skewness(z_true, na.rm = TRUE)
    results$skewness_positive <- skew_z > 0
    results$skewness_value <- skew_z
    results$skewness_expected_range <- c(0.5, 2.0)  # Approximate for xi=1.5
    results$skewness_in_range <- skew_z >= results$skewness_expected_range[1] && 
                                  skew_z <= results$skewness_expected_range[2]
  }
  
  # Kurtosis check (for t-distributions)
  if (innovation_type %in% c("student_t", "skewed_t")) {
    kurt_z <- moments::kurtosis(z_true, na.rm = TRUE)
    results$kurtosis_heavy_tailed <- kurt_z > 3
    results$kurtosis_value <- kurt_z
    # For nu=5, expected kurtosis is around 6 (excess kurtosis = 6/(nu-4) for nu>4)
    results$kurtosis_expected_range <- c(3, 15)
    results$kurtosis_in_range <- kurt_z >= results$kurtosis_expected_range[1] && 
                                  kurt_z <= results$kurtosis_expected_range[2]
  }
  
  results$all_checks_passed <- all(c(results$mean_ok, results$sd_ok))
  if (innovation_type == "skewed_t") {
    results$all_checks_passed <- results$all_checks_passed && results$skewness_positive
  }
  
  return(results)
}

# =============================================================================
# STANDARDIZED RESIDUAL VALIDATION
# =============================================================================

#' Validate standardized residuals properties
validate_z_hat <- function(z_hat, method_name, tolerance_mean = 0.1, tolerance_sd = 0.1) {
  if (is.null(z_hat) || length(z_hat) < 10) {
    return(list(
      method = method_name,
      valid = FALSE,
      reason = "Insufficient data"
    ))
  }
  
  results <- list()
  results$method <- method_name
  results$n <- length(z_hat)
  
  # Mean check
  mean_z <- mean(z_hat, na.rm = TRUE)
  results$mean_ok <- abs(mean_z) < tolerance_mean
  results$mean_value <- mean_z
  results$mean_tolerance <- tolerance_mean
  
  # SD check
  sd_z <- sd(z_hat, na.rm = TRUE)
  results$sd_ok <- abs(sd_z - 1) < tolerance_sd
  results$sd_value <- sd_z
  results$sd_tolerance <- tolerance_sd
  
  # Additional checks
  results$has_nan <- any(is.nan(z_hat))
  results$has_inf <- any(is.infinite(z_hat))
  results$finite_count <- sum(is.finite(z_hat))
  
  results$valid <- results$mean_ok && results$sd_ok && !results$has_nan && !results$has_inf
  
  return(results)
}

# =============================================================================
# METRIC COMPUTATION VERIFICATION
# =============================================================================

#' Verify KS statistic computation
verify_ks_computation <- function(z_true, z_hat) {
  if (length(z_true) != length(z_hat)) {
    return(list(
      error = "Sample sizes must match for KS test",
      n_true = length(z_true),
      n_hat = length(z_hat)
    ))
  }
  
  ks_result <- ks.test(z_true, z_hat)
  
  return(list(
    statistic = as.numeric(ks_result$statistic),
    p_value = ks_result$p.value,
    method = ks_result$method,
    n = length(z_true),
    valid = TRUE
  ))
}

#' Verify skewness computation consistency
verify_skewness_computation <- function(data) {
  # Check if moments::skewness uses Fisher's definition (bias-corrected)
  skew_moments <- moments::skewness(data, na.rm = TRUE)
  
  # Manual computation (Fisher's skewness, bias-corrected)
  n <- length(data[!is.na(data)])
  if (n < 3) return(list(error = "Insufficient data"))
  
  mean_data <- mean(data, na.rm = TRUE)
  sd_data <- sd(data, na.rm = TRUE)
  
  # Fisher's skewness: E[(X-μ)³] / σ³, with bias correction
  skew_manual <- (n / ((n-1)*(n-2))) * sum(((data - mean_data) / sd_data)^3, na.rm = TRUE)
  
  return(list(
    moments_skewness = skew_moments,
    manual_skewness = skew_manual,
    difference = abs(skew_moments - skew_manual),
    consistent = abs(skew_moments - skew_manual) < 1e-6
  ))
}

#' Verify kurtosis computation (Fisher vs Pearson)
verify_kurtosis_computation <- function(data) {
  # moments::kurtosis returns excess kurtosis (Fisher's definition)
  kurt_moments <- moments::kurtosis(data, na.rm = TRUE)
  
  # Manual computation of excess kurtosis
  n <- length(data[!is.na(data)])
  if (n < 4) return(list(error = "Insufficient data"))
  
  mean_data <- mean(data, na.rm = TRUE)
  sd_data <- sd(data, na.rm = TRUE)
  
  # Excess kurtosis = E[(X-μ)⁴] / σ⁴ - 3
  # With bias correction
  m4 <- mean(((data - mean_data) / sd_data)^4, na.rm = TRUE)
  kurt_manual <- (n*(n+1) / ((n-1)*(n-2)*(n-3))) * sum(((data - mean_data) / sd_data)^4, na.rm = TRUE) - 
                 3 * (n-1)^2 / ((n-2)*(n-3))
  
  return(list(
    moments_kurtosis = kurt_moments,
    manual_kurtosis = kurt_manual,
    difference = abs(kurt_moments - kurt_manual),
    consistent = abs(kurt_moments - kurt_manual) < 1e-4,
    note = "moments::kurtosis returns excess kurtosis (Fisher's definition)"
  ))
}

# =============================================================================
# SCALE CONSISTENCY CHECK
# =============================================================================

#' Check if all distributions are on the same scale
check_scale_consistency <- function(z_true, z_hat_gaussian, z_hat_student_t, z_nf) {
  results <- list()
  
  # Collect all non-null distributions
  dists <- list(
    "True" = z_true,
    "Gaussian_GARCH" = z_hat_gaussian,
    "Student_t_GARCH" = z_hat_student_t,
    "NF_GARCH" = z_nf
  )
  
  dists <- dists[!sapply(dists, is.null)]
  
  # Compute mean and SD for each
  stats <- lapply(dists, function(z) {
    if (is.null(z) || length(z) < 10) return(NULL)
    list(
      mean = mean(z, na.rm = TRUE),
      sd = sd(z, na.rm = TRUE),
      n = length(z)
    )
  })
  
  stats <- stats[!sapply(stats, is.null)]
  
  results$statistics <- stats
  
  # Check if all means are close to 0
  means <- sapply(stats, function(s) s$mean)
  results$mean_consistency <- all(abs(means) < 0.2)
  results$mean_range <- range(means)
  
  # Check if all SDs are close to 1
  sds <- sapply(stats, function(s) s$sd)
  results$sd_consistency <- all(abs(sds - 1) < 0.2)
  results$sd_range <- range(sds)
  
  results$all_consistent <- results$mean_consistency && results$sd_consistency
  
  return(results)
}

