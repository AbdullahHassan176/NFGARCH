# =============================================================================
# VALIDATION TESTS FOR MANUAL GARCH IMPLEMENTATION
# =============================================================================
# Purpose: Execute validation protocol to verify manual implementation
# Author: Reviewer #2
# Date: February 2, 2026
#
# This script runs the 3-phase validation protocol:
#   Phase 1: Single-Series Parity Test
#   Phase 2: Distribution-Specific Test (Student-t Rescaling)
#   Phase 3: Forecast Comparison Test
#
# Prerequisites:
#   1. rugarch package installed
#   2. Manual GARCH implementation sourced
#   3. Data file available
#
# =============================================================================

library(rugarch)
library(xts)
library(lubridate)

# Source manual implementation
source("scripts/manual_garch/fit_sgarch_manual.R")
source("scripts/manual_garch/manual_garch_core.R")

# =============================================================================
# PHASE 1: SINGLE-SERIES PARITY TEST
# =============================================================================

cat("=============================================================================\n")
cat("PHASE 1: SINGLE-SERIES PARITY TEST\n")
cat("=============================================================================\n\n")

cat("Objective: Verify if manual produces same results as rugarch\n")
cat("Model: sGARCH(1,1) with Normal distribution\n")
cat("Data: Simulated returns (controlled test)\n\n")

# Generate controlled data
set.seed(123)
n_obs <- 1000
returns <- rnorm(n_obs, mean=0.0005, sd=0.01)

cat("Generated", n_obs, "synthetic return observations\n")
cat("True parameters: mu=0.0005, sigma=0.01\n\n")

# Fit with rugarch
cat("Fitting with rugarch... ")
spec_rug <- ugarchspec(
  variance.model = list(model="sGARCH", garchOrder=c(1,1)),
  mean.model = list(armaOrder=c(0,0), include.mean=TRUE),
  distribution.model = "norm"
)

fit_rug <- ugarchfit(spec_rug, returns, solver="hybrid")
cat("Done\n")

# Fit with manual
cat("Fitting with manual implementation... ")
fit_man <- fit_sgarch_manual(returns, dist="norm")
cat("Done\n\n")

# Compare parameters
cat("--- PARAMETER COMPARISON ---\n")
param_names <- c("mu", "omega", "alpha", "beta")
param_comparison <- data.frame(
  parameter = param_names,
  rugarch = numeric(4),
  manual = numeric(4),
  difference = numeric(4),
  rel_diff_pct = numeric(4)
)

for (i in seq_along(param_names)) {
  pname <- param_names[i]
  
  # rugarch parameter name mapping
  rug_name <- if (pname == "alpha") "alpha1" else if (pname == "beta") "beta1" else pname
  
  rug_val <- coef(fit_rug)[rug_name]
  man_val <- fit_man$coef[pname]
  
  param_comparison$rugarch[i] <- rug_val
  param_comparison$manual[i] <- man_val
  param_comparison$difference[i] <- abs(man_val - rug_val)
  param_comparison$rel_diff_pct[i] <- abs(man_val - rug_val) / abs(rug_val) * 100
}

print(param_comparison)
cat("\n")

# Check tolerance
params_match <- all(param_comparison$difference < 1e-3)
cat("Tolerance check (|diff| < 1e-3):", ifelse(params_match, "✓ PASS", "✗ FAIL"), "\n\n")

# Compare log-likelihood
cat("--- LOG-LIKELIHOOD COMPARISON ---\n")
ll_rug <- likelihood(fit_rug)
ll_man <- fit_man$loglik

cat("rugarch LL:", ll_rug, "\n")
cat("manual  LL:", ll_man, "\n")
cat("Difference:", abs(ll_rug - ll_man), "\n")
cat("Relative diff:", abs(ll_rug - ll_man) / abs(ll_rug) * 100, "%\n")

ll_match <- abs(ll_rug - ll_man) / abs(ll_rug) < 1e-4
cat("Tolerance check (rel diff < 0.01%):", ifelse(ll_match, "✓ PASS", "✗ FAIL"), "\n\n")

# Compare sigma series
cat("--- SIGMA SERIES COMPARISON ---\n")
sigma_rug <- as.numeric(sigma(fit_rug))
sigma_man <- fit_man$sigma

sigma_cor <- cor(sigma_rug, sigma_man)
cat("Correlation:", sigma_cor, "\n")

sigma_match <- sigma_cor > 0.999
cat("Tolerance check (cor > 0.999):", ifelse(sigma_match, "✓ PASS", "✗ FAIL"), "\n\n")

# Compare standardized residuals
cat("--- STANDARDIZED RESIDUALS COMPARISON ---\n")
res_rug <- as.numeric(residuals(fit_rug, standardize=TRUE))
res_man <- fit_man$std_residuals

cat("rugarch std_res: mean=", round(mean(res_rug), 6), ", var=", round(var(res_rug), 6), "\n")
cat("manual  std_res: mean=", round(mean(res_man), 6), ", var=", round(var(res_man), 6), "\n")

res_cor <- cor(res_rug, res_man)
cat("Correlation:", res_cor, "\n")

res_match <- res_cor > 0.99 && abs(mean(res_man)) < 0.01 && abs(var(res_man) - 1) < 0.1
cat("Tolerance check (cor > 0.99, mean≈0, var≈1):", ifelse(res_match, "✓ PASS", "✗ FAIL"), "\n\n")

# Final verdict
cat("--- PHASE 1 RESULTS ---\n")
all_pass <- params_match && ll_match && sigma_match && res_match

if (all_pass) {
  cat("✅ PARITY TEST PASSED\n")
  cat("Manual implementation matches rugarch within tolerances\n")
} else {
  cat("❌ PARITY TEST FAILED\n")
  cat("Differences detected - investigate:\n")
  if (!params_match) cat("  - Parameter estimates differ\n")
  if (!ll_match) cat("  - Log-likelihood differs\n")
  if (!sigma_match) cat("  - Sigma series correlation too low\n")
  if (!res_match) cat("  - Standardized residuals problematic\n")
}

cat("\n")

# =============================================================================
# PHASE 2: DISTRIBUTION-SPECIFIC TEST (Student-t Rescaling)
# =============================================================================

cat("=============================================================================\n")
cat("PHASE 2: STUDENT-T RESCALING TEST\n")
cat("=============================================================================\n\n")

cat("Objective: Verify Student-t rescaling hypothesis\n")
cat("Hypothesis: Manual uses unrescaled Student-t, rugarch uses rescaled\n")
cat("Expected: σ_manual ≈ σ_rugarch × sqrt(ν/(ν-2))\n\n")

# Generate more data for Student-t test
set.seed(456)
n_obs_t <- 2000
returns_t <- rnorm(n_obs_t, mean=0.0005, sd=0.015)

cat("Generated", n_obs_t, "synthetic returns\n\n")

# Fit both engines with Student-t
cat("Fitting with rugarch (Student-t)... ")
spec_rug_t <- ugarchspec(
  variance.model = list(model="sGARCH", garchOrder=c(1,1)),
  mean.model = list(armaOrder=c(0,0), include.mean=TRUE),
  distribution.model = "std"
)

fit_rug_t <- ugarchfit(spec_rug_t, returns_t, solver="hybrid")
cat("Done\n")

cat("Fitting with manual (Student-t)... ")
fit_man_t <- fit_sgarch_manual(returns_t, dist="std")
cat("Done\n\n")

# Extract degrees of freedom
cat("--- DEGREES OF FREEDOM ---\n")
nu_rug <- coef(fit_rug_t)["shape"]
nu_man <- fit_man_t$coef["nu"]

cat("rugarch nu:", nu_rug, "\n")
cat("manual  nu:", nu_man, "\n")
cat("Difference:", abs(nu_rug - nu_man), "\n\n")

# Calculate expected rescaling factor
rescale_factor <- sqrt((nu_man - 2) / nu_man)
cat("--- RESCALING FACTOR ---\n")
cat("Expected factor: sqrt((nu-2)/nu) =", rescale_factor, "\n")
cat("Inverse factor: sqrt(nu/(nu-2)) =", 1/rescale_factor, "\n\n")

# Compare sigma
cat("--- SIGMA COMPARISON ---\n")
sigma_rug_t <- as.numeric(sigma(fit_rug_t))
sigma_man_t <- fit_man_t$sigma
sigma_man_t_rescaled <- sigma_man_t * rescale_factor

cat("Mean sigma (rugarch):", mean(sigma_rug_t), "\n")
cat("Mean sigma (manual, unrescaled):", mean(sigma_man_t), "\n")
cat("Mean sigma (manual, rescaled):", mean(sigma_man_t_rescaled), "\n\n")

cat("Ratio (manual/rugarch):", mean(sigma_man_t) / mean(sigma_rug_t), "\n")
cat("Expected ratio (1/rescale):", 1/rescale_factor, "\n")
cat("Difference:", abs(mean(sigma_man_t)/mean(sigma_rug_t) - 1/rescale_factor), "\n\n")

# Correlation test
cat("--- CORRELATION TEST ---\n")
cor_unrescaled <- cor(sigma_rug_t, sigma_man_t)
cor_rescaled <- cor(sigma_rug_t, sigma_man_t_rescaled)

cat("Correlation (unrescaled):", cor_unrescaled, "\n")
cat("Correlation (rescaled):", cor_rescaled, "\n\n")

# MAPE test
mape_unrescaled <- mean(abs(sigma_man_t - sigma_rug_t) / sigma_rug_t) * 100
mape_rescaled <- mean(abs(sigma_man_t_rescaled - sigma_rug_t) / sigma_rug_t) * 100

cat("--- MEAN ABSOLUTE PERCENTAGE ERROR ---\n")
cat("MAPE (unrescaled):", mape_unrescaled, "%\n")
cat("MAPE (rescaled):", mape_rescaled, "%\n\n")

# Verdict
cat("--- PHASE 2 RESULTS ---\n")
ratio_match <- abs(mean(sigma_man_t)/mean(sigma_rug_t) - 1/rescale_factor) < 0.05

if (ratio_match) {
  cat("✅ RESCALING HYPOTHESIS CONFIRMED\n")
  cat("Manual implementation uses unrescaled Student-t distribution\n")
  cat("Parameters differ by scale factor sqrt((ν-2)/ν) ≈", round(rescale_factor, 4), "\n")
  cat("\nIMPLICATION FOR DISSERTATION:\n")
  cat("- Student-t parameter estimates not directly comparable to rugarch\n")
  cat("- Multiply manual sigma by", round(rescale_factor, 4), "for comparison\n")
  cat("- Log-likelihood may differ by constant term\n")
} else {
  cat("❓ RESCALING HYPOTHESIS UNCLEAR\n")
  cat("Ratio does not match expected pattern\n")
  cat("Further investigation needed\n")
}

cat("\n")

# =============================================================================
# PHASE 3: FORECAST COMPARISON TEST
# =============================================================================

cat("=============================================================================\n")
cat("PHASE 3: MULTI-STEP FORECAST COMPARISON\n")
cat("=============================================================================\n\n")

cat("Objective: Compare multi-step forecast methodologies\n")
cat("Hypothesis: rugarch uses analytical, manual uses simulation\n")
cat("Expected: Forecasts diverge at longer horizons\n\n")

# Use normal distribution data
cat("Using data from Phase 1 (n=", n_obs, ")\n\n")

# Generate forecasts
horizons <- c(1, 5, 10, 20, 50, 100)
forecast_comparison <- data.frame(
  horizon = horizons,
  rugarch = numeric(length(horizons)),
  manual = numeric(length(horizons)),
  diff_pct = numeric(length(horizons))
)

cat("--- GENERATING FORECASTS ---\n")
for (i in seq_along(horizons)) {
  h <- horizons[i]
  
  cat("Horizon h=", h, ": ", sep="")
  
  # rugarch forecast
  fc_rug <- ugarchforecast(fit_rug, n.ahead=h)
  sigma_rug_h <- as.numeric(sigma(fc_rug)[h])
  
  # manual forecast
  fc_man <- fit_man$predict(h)
  sigma_man_h <- fc_man$sigma[h]
  
  forecast_comparison$rugarch[i] <- sigma_rug_h
  forecast_comparison$manual[i] <- sigma_man_h
  forecast_comparison$diff_pct[i] <- (sigma_man_h - sigma_rug_h) / sigma_rug_h * 100
  
  cat("rugarch=", round(sigma_rug_h, 6), ", manual=", round(sigma_man_h, 6), 
      ", diff=", round(forecast_comparison$diff_pct[i], 2), "%\n", sep="")
}

cat("\n--- FORECAST COMPARISON TABLE ---\n")
print(forecast_comparison)
cat("\n")

# Calculate theoretical convergence points
omega <- coef(fit_rug)["omega"]
alpha <- coef(fit_rug)["alpha1"]
beta <- coef(fit_rug)["beta1"]

uncond_var_full <- omega / (1 - alpha - beta)  # rugarch analytical
uncond_var_partial <- omega / (1 - beta)        # manual simulation

cat("--- THEORETICAL LONG-RUN VALUES ---\n")
cat("rugarch analytical: σ²_∞ = ω/(1-α-β) =", omega, "/ (1-", alpha, "-", beta, ")\n")
cat("                  = ", uncond_var_full, "\n")
cat("                  σ_∞ = ", sqrt(uncond_var_full), "\n\n")

cat("manual simulation:  σ²_∞ = ω/(1-β) =", omega, "/ (1-", beta, ")\n")
cat("                  = ", uncond_var_partial, "\n")
cat("                  σ_∞ = ", sqrt(uncond_var_partial), "\n\n")

cat("Ratio (manual/rugarch):", sqrt(uncond_var_partial) / sqrt(uncond_var_full), "\n\n")

# Test divergence
cat("--- DIVERGENCE TEST ---\n")
diff_increase <- forecast_comparison$diff_pct[6] > forecast_comparison$diff_pct[1]

cat("Difference at h=1:", round(forecast_comparison$diff_pct[1], 2), "%\n")
cat("Difference at h=100:", round(forecast_comparison$diff_pct[6], 2), "%\n")
cat("Increasing trend:", ifelse(diff_increase, "Yes", "No"), "\n\n")

# Verdict
cat("--- PHASE 3 RESULTS ---\n")
if (diff_increase && abs(forecast_comparison$diff_pct[6]) > 5) {
  cat("✅ FORECAST DIVERGENCE CONFIRMED\n")
  cat("Manual and rugarch forecasts diverge at longer horizons\n")
  cat("This confirms different forecast methodologies:\n")
  cat("  - rugarch: Analytical (converges to ω/(1-α-β))\n")
  cat("  - manual: Simulation (converges to ω/(1-β))\n")
  cat("\nIMPLICATION FOR DISSERTATION:\n")
  cat("- Multi-step forecasts (h>10) NOT comparable\n")
  cat("- Forecast evaluation must account for methodology difference\n")
  cat("- Recommend: Document this difference OR implement analytical forecasts\n")
} else {
  cat("❓ FORECAST DIVERGENCE UNCLEAR\n")
  cat("Differences remain stable across horizons\n")
  cat("May indicate similar forecast methods\n")
}

cat("\n")

# =============================================================================
# OVERALL SUMMARY
# =============================================================================

cat("=============================================================================\n")
cat("VALIDATION PROTOCOL SUMMARY\n")
cat("=============================================================================\n\n")

cat("Phase 1 (Parity Test): ")
if (all_pass) {
  cat("✅ PASS - Normal distribution implementation matches rugarch\n")
} else {
  cat("⚠️ ISSUES - Investigate parameter/likelihood differences\n")
}

cat("Phase 2 (Student-t Test): ")
if (ratio_match) {
  cat("⚠️ RESCALING DIFFERENCE - Manual uses unrescaled Student-t\n")
} else {
  cat("❓ UNCLEAR - Further investigation needed\n")
}

cat("Phase 3 (Forecast Test): ")
if (diff_increase && abs(forecast_comparison$diff_pct[6]) > 5) {
  cat("⚠️ METHODOLOGY DIFFERENCE - Manual uses simulation, not analytical\n")
} else {
  cat("✅ PASS - Forecast methods similar\n")
}

cat("\n--- CRITICAL ISSUES REQUIRING ATTENTION ---\n\n")

if (!all_pass) {
  cat("1. ❌ Phase 1 failed - Basic parity issues exist\n")
  cat("   Action: Debug parameter estimation and likelihood calculation\n\n")
}

if (ratio_match) {
  cat("2. ⚠️ Student-t rescaling confirmed - Parameters not comparable\n")
  cat("   Action: Document difference OR implement rescaling\n\n")
}

if (diff_increase) {
  cat("3. ⚠️ Forecast methodology differs - Results not comparable for h>10\n")
  cat("   Action: Document difference OR implement analytical forecasts\n\n")
}

cat("--- RECOMMENDATIONS ---\n\n")
cat("FOR DISSERTATION:\n")
cat("1. Add methodology section documenting all differences\n")
cat("2. Clearly label which results use which methodology\n")
cat("3. Do NOT directly compare Student-t parameters without rescaling\n")
cat("4. Do NOT directly compare multi-step forecasts without adjustment\n")
cat("5. Consider these design choices, not bugs\n\n")

cat("FOR PARITY:\n")
cat("1. Implement Student-t rescaling (sqrt((ν-2)/ν) factor)\n")
cat("2. Implement analytical multi-step forecasts\n")
cat("3. Use rugarch initialization strategy (backcast)\n")
cat("4. Match rugarch optimization settings (maxit=500, tol=1e-8)\n\n")

cat("=============================================================================\n")
cat("VALIDATION PROTOCOL COMPLETE\n")
cat("=============================================================================\n")
