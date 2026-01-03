# Standardization Utilities for NF-GARCH Pipeline
# Centralized standardization functions to ensure consistency

#' Standardize residuals to mean ≈ 0, SD ≈ 1
#' 
#' This function standardizes residuals using the standard formula:
#' z_std = (z - mean(z)) / sd(z)
#' 
#' @param z Numeric vector of residuals to standardize
#' @param verify Logical, if TRUE, verifies standardization after applying
#' @return Standardized residuals with mean ≈ 0, SD ≈ 1
#' @export
standardize_residuals <- function(z, verify = FALSE) {
  # Convert to numeric and remove NAs
  z <- as.numeric(z)
  z <- z[!is.na(z)]
  
  if (length(z) == 0) {
    stop("Cannot standardize: input is empty or all NA")
  }
  
  # Calculate mean and standard deviation
  z_mean <- mean(z, na.rm = TRUE)
  z_sd <- sd(z, na.rm = TRUE)
  
  # Check for invalid variance
  if (!is.finite(z_sd) || z_sd == 0) {
    stop("Cannot standardize: variance is zero or invalid")
  }
  
  # Standardize
  z_std <- (z - z_mean) / z_sd
  
  # Optional verification
  if (verify) {
    mean_check <- mean(z_std, na.rm = TRUE)
    sd_check <- sd(z_std, na.rm = TRUE)
    if (abs(mean_check) > 0.01 || abs(sd_check - 1) > 0.01) {
      warning("Standardization verification failed: mean = ", round(mean_check, 6), 
              ", SD = ", round(sd_check, 6))
    }
  }
  
  return(z_std)
}

#' Check if residuals are already standardized
#' 
#' @param z Numeric vector of residuals to check
#' @param mean_tolerance Tolerance for mean check (default 0.1)
#' @param sd_tolerance Tolerance for SD check (default 0.1)
#' @return Logical, TRUE if standardized, FALSE otherwise
#' @export
is_standardized <- function(z, mean_tolerance = 0.1, sd_tolerance = 0.1) {
  z <- as.numeric(z)
  z <- z[!is.na(z)]
  
  if (length(z) == 0) return(FALSE)
  
  z_mean <- mean(z, na.rm = TRUE)
  z_sd <- sd(z, na.rm = TRUE)
  
  return(abs(z_mean) <= mean_tolerance && abs(z_sd - 1) <= sd_tolerance)
}




