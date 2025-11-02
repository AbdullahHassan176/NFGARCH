# Manual Engine Mathematical Verification Script
# Verifies that manual GARCH implementations use correct mathematical equations

cat("=== MANUAL ENGINE MATH VERIFICATION ===\n\n")

# Load manual GARCH implementations
source("scripts/manual_garch/manual_garch_core.R")
source("scripts/manual_garch/fit_sgarch_manual.R")
source("scripts/manual_garch/fit_egarch_manual.R")
source("scripts/manual_garch/fit_gjr_manual.R")
source("scripts/manual_garch/fit_tgarch_manual.R")

# Test data - simple returns series
set.seed(123)
n <- 1000
test_returns <- rnorm(n, mean = 0, sd = 0.02)

# Track verification results
verification_results <- list()

# =============================================================================
# 1. sGARCH Variance Recursion Verification
# =============================================================================
cat("1. Testing sGARCH variance recursion...\n")

fit_sgarch <- fit_sgarch_manual(test_returns, dist = "norm")

# Extract parameters
omega <- fit_sgarch$coef["omega"]
alpha <- fit_sgarch$coef["alpha"]
beta <- fit_sgarch$coef["beta"]
mu <- fit_sgarch$coef["mu"]

# Manual variance calculation
sigma2_manual <- rep(0, n)
residuals <- test_returns - mu
sigma2_manual[1] <- var(test_returns, na.rm = TRUE)

for (t in 2:n) {
  sigma2_manual[t] <- omega + alpha * residuals[t-1]^2 + beta * sigma2_manual[t-1]
}

# Compare with fitted sigma
sigma2_fitted <- fit_sgarch$sigma^2

# Check difference (allow small numerical error)
max_diff <- max(abs(sigma2_manual - sigma2_fitted), na.rm = TRUE)

if (max_diff < 1e-6) {
  cat("   [OK] sGARCH variance recursion: PASSED (max diff =", max_diff, ")\n")
  verification_results$sgarch_variance <- TRUE
} else {
  cat("   ✗ sGARCH variance recursion: FAILED (max diff =", max_diff, ")\n")
  verification_results$sgarch_variance <- FALSE
}

# =============================================================================
# 2. eGARCH Log-Variance Recursion Verification
# =============================================================================
cat("2. Testing eGARCH log-variance recursion...\n")

fit_egarch <- fit_egarch_manual(test_returns, dist = "norm")

# Extract parameters
omega_e <- fit_egarch$coef["omega"]
alpha_e <- fit_egarch$coef["alpha"]
beta_e <- fit_egarch$coef["beta"]
gamma_e <- fit_egarch$coef["gamma"]
mu_e <- fit_egarch$coef["mu"]

# Manual log-variance calculation
log_sigma2_manual <- rep(0, n)
residuals_e <- test_returns - mu_e
sigma2_e <- rep(var(test_returns, na.rm = TRUE), n)
std_residuals_e <- residuals_e / sqrt(sigma2_e)

log_sigma2_manual[1] <- log(sigma2_e[1])
E_abs_z <- sqrt(2/pi)  # E|z| for standard normal

for (t in 2:n) {
  log_sigma2_manual[t] <- omega_e + 
    alpha_e * (abs(std_residuals_e[t-1]) - E_abs_z) + 
    gamma_e * std_residuals_e[t-1] + 
    beta_e * log_sigma2_manual[t-1]
  sigma2_e[t] <- exp(log_sigma2_manual[t])
  std_residuals_e[t] <- residuals_e[t] / sqrt(sigma2_e[t])
}

# Compare with fitted sigma (convert to log scale)
log_sigma2_fitted <- log(fit_egarch$sigma^2)

max_diff_e <- max(abs(log_sigma2_manual - log_sigma2_fitted), na.rm = TRUE)

if (max_diff_e < 1e-5) {
  cat("   [OK] eGARCH log-variance recursion: PASSED (max diff =", max_diff_e, ")\n")
  verification_results$egarch_variance <- TRUE
} else {
  cat("   ✗ eGARCH log-variance recursion: FAILED (max diff =", max_diff_e, ")\n")
  verification_results$egarch_variance <- FALSE
}

# =============================================================================
# 3. gjrGARCH Variance Recursion Verification
# =============================================================================
cat("3. Testing gjrGARCH variance recursion...\n")

fit_gjr <- fit_gjr_manual(test_returns, dist = "norm")

# Extract parameters
omega_g <- fit_gjr$coef["omega"]
alpha_g <- fit_gjr$coef["alpha"]
beta_g <- fit_gjr$coef["beta"]
gamma_g <- fit_gjr$coef["gamma"]
mu_g <- fit_gjr$coef["mu"]

# Manual variance calculation
sigma2_gjr_manual <- rep(0, n)
residuals_g <- test_returns - mu_g
sigma2_gjr_manual[1] <- var(test_returns, na.rm = TRUE)

for (t in 2:n) {
  indicator <- ifelse(residuals_g[t-1] < 0, 1, 0)
  sigma2_gjr_manual[t] <- omega_g + 
    alpha_g * residuals_g[t-1]^2 + 
    gamma_g * residuals_g[t-1]^2 * indicator + 
    beta_g * sigma2_gjr_manual[t-1]
}

# Compare with fitted sigma
sigma2_gjr_fitted <- fit_gjr$sigma^2
max_diff_gjr <- max(abs(sigma2_gjr_manual - sigma2_gjr_fitted), na.rm = TRUE)

if (max_diff_gjr < 1e-6) {
  cat("   [OK] gjrGARCH variance recursion: PASSED (max diff =", max_diff_gjr, ")\n")
  verification_results$gjr_variance <- TRUE
} else {
  cat("   ✗ gjrGARCH variance recursion: FAILED (max diff =", max_diff_gjr, ")\n")
  verification_results$gjr_variance <- FALSE
}

# =============================================================================
# 4. TGARCH Variance Recursion Verification
# =============================================================================
cat("4. Testing TGARCH variance recursion...\n")

fit_tgarch <- fit_tgarch_manual(test_returns, dist = "norm")

# Extract parameters
omega_t <- fit_tgarch$coef["omega"]
alpha_t <- fit_tgarch$coef["alpha"]
beta_t <- fit_tgarch$coef["beta"]
eta_t <- fit_tgarch$coef["eta"]
mu_t <- fit_tgarch$coef["mu"]

# Manual variance calculation
sigma2_tgarch_manual <- rep(0, n)
residuals_t <- test_returns - mu_t
sigma2_tgarch_manual[1] <- var(test_returns, na.rm = TRUE)

for (t in 2:n) {
  indicator_t <- ifelse(residuals_t[t-1] < 0, 1, 0)
  sigma2_tgarch_manual[t] <- omega_t + 
    (alpha_t + eta_t * indicator_t) * residuals_t[t-1]^2 + 
    beta_t * sigma2_tgarch_manual[t-1]
}

# Compare with fitted sigma
sigma2_tgarch_fitted <- fit_tgarch$sigma^2
max_diff_tgarch <- max(abs(sigma2_tgarch_manual - sigma2_tgarch_fitted), na.rm = TRUE)

if (max_diff_tgarch < 1e-6) {
  cat("   [OK] TGARCH variance recursion: PASSED (max diff =", max_diff_tgarch, ")\n")
  verification_results$tgarch_variance <- TRUE
} else {
  cat("   ✗ TGARCH variance recursion: FAILED (max diff =", max_diff_tgarch, ")\n")
  verification_results$tgarch_variance <- FALSE
}

# =============================================================================
# 5. Log-Likelihood Calculation Verification
# =============================================================================
cat("5. Testing log-likelihood calculation...\n")

# For normal distribution, log-likelihood should be:
# LL = -0.5 * sum[log(2*pi) + log(sigma^2) + (residual/sigma)^2]

residuals_ll <- test_returns - fit_sgarch$coef["mu"]
sigma2_ll <- fit_sgarch$sigma^2

# Manual log-likelihood
ll_manual <- -0.5 * sum(log(2*pi) + log(sigma2_ll) + (residuals_ll^2 / sigma2_ll), na.rm = TRUE)

# Compare with fitted log-likelihood
ll_fitted <- fit_sgarch$loglik
ll_diff <- abs(ll_manual - ll_fitted)

if (ll_diff < 1e-6) {
  cat("   [OK] Log-likelihood calculation: PASSED (diff =", ll_diff, ")\n")
  verification_results$loglikelihood <- TRUE
} else {
  cat("   ✗ Log-likelihood calculation: FAILED (diff =", ll_diff, ")\n")
  cat("      Manual LL:", ll_manual, "\n")
  cat("      Fitted LL:", ll_fitted, "\n")
  verification_results$loglikelihood <- FALSE
}

# =============================================================================
# 6. Parameter Constraints Verification
# =============================================================================
cat("6. Testing parameter constraints...\n")

constraints_ok <- TRUE

# sGARCH constraints
if (fit_sgarch$coef["omega"] <= 0) {
  cat("   ✗ sGARCH: omega must be > 0\n")
  constraints_ok <- FALSE
}
if (fit_sgarch$coef["alpha"] < 0) {
  cat("   ✗ sGARCH: alpha must be >= 0\n")
  constraints_ok <- FALSE
}
if (fit_sgarch$coef["beta"] < 0) {
  cat("   ✗ sGARCH: beta must be >= 0\n")
  constraints_ok <- FALSE
}
if (fit_sgarch$coef["alpha"] + fit_sgarch$coef["beta"] >= 1) {
  cat("   ✗ sGARCH: alpha + beta must be < 1 (stationarity)\n")
  constraints_ok <- FALSE
}

# eGARCH constraints
if (fit_egarch$coef["beta"] >= 1) {
  cat("   ✗ eGARCH: beta must be < 1 (stationarity)\n")
  constraints_ok <- FALSE
}

# gjrGARCH constraints
if (fit_gjr$coef["omega"] <= 0) {
  cat("   ✗ gjrGARCH: omega must be > 0\n")
  constraints_ok <- FALSE
}
if (fit_gjr$coef["alpha"] < 0 || fit_gjr$coef["beta"] < 0 || fit_gjr$coef["gamma"] < 0) {
  cat("   ✗ gjrGARCH: alpha, beta, gamma must be >= 0\n")
  constraints_ok <- FALSE
}

if (constraints_ok) {
  cat("   [OK] Parameter constraints: PASSED\n")
  verification_results$constraints <- TRUE
} else {
  verification_results$constraints <- FALSE
}

# =============================================================================
# 7. Stationarity Conditions Verification
# =============================================================================
cat("7. Testing stationarity conditions...\n")

stationarity_ok <- TRUE

# sGARCH: α + β < 1
sgarch_stationary <- (fit_sgarch$coef["alpha"] + fit_sgarch$coef["beta"]) < 1
if (!sgarch_stationary) {
  cat("   ✗ sGARCH: α + β =", fit_sgarch$coef["alpha"] + fit_sgarch$coef["beta"], 
      ">= 1 (non-stationary)\n")
  stationarity_ok <- FALSE
}

# eGARCH: |β| < 1
egarch_stationary <- abs(fit_egarch$coef["beta"]) < 1
if (!egarch_stationary) {
  cat("   ✗ eGARCH: |β| =", abs(fit_egarch$coef["beta"]), ">= 1 (non-stationary)\n")
  stationarity_ok <- FALSE
}

# gjrGARCH: α + γ/2 + β < 1
gjr_stationary <- (fit_gjr$coef["alpha"] + fit_gjr$coef["gamma"]/2 + fit_gjr$coef["beta"]) < 1
if (!gjr_stationary) {
  cat("   ✗ gjrGARCH: α + γ/2 + β =", 
      fit_gjr$coef["alpha"] + fit_gjr$coef["gamma"]/2 + fit_gjr$coef["beta"],
      ">= 1 (non-stationary)\n")
  stationarity_ok <- FALSE
}

if (stationarity_ok) {
  cat("   [OK] Stationarity conditions: PASSED\n")
  verification_results$stationarity <- TRUE
} else {
  verification_results$stationarity <- FALSE
}

# =============================================================================
# Final Summary
# =============================================================================
cat("\n=== VERIFICATION SUMMARY ===\n")

all_passed <- all(unlist(verification_results))

if (all_passed) {
  cat("[OK] All mathematical verifications PASSED\n")
  cat("\nThe manual GARCH engine correctly implements:\n")
  cat("  - sGARCH variance recursion\n")
  cat("  - eGARCH log-variance recursion\n")
  cat("  - gjrGARCH variance recursion\n")
  cat("  - TGARCH variance recursion\n")
  cat("  - Log-likelihood calculations\n")
  cat("  - Parameter constraints\n")
  cat("  - Stationarity conditions\n")
  cat("\nYou can proceed with confidence!\n")
} else {
  cat("✗ Some verifications FAILED\n")
  cat("Please review the errors above.\n")
  failed_tests <- names(verification_results)[!unlist(verification_results)]
  cat("Failed tests:", paste(failed_tests, collapse = ", "), "\n")
}

cat("\n=== END VERIFICATION ===\n")

# Return results for programmatic checking
invisible(verification_results)

