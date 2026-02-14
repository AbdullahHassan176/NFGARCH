# =============================================================================
# RUGARCH REFERENCE IMPLEMENTATION FOR PARITY TESTING
# =============================================================================
# Purpose: Generate ground-truth outputs for validating manual GARCH
# Author: Reviewer #2
# Date: February 2, 2026
#
# This script fits standard GARCH models using rugarch (gold standard) and
# saves results in a format comparable to manual implementation outputs.
#
# Usage:
#   1. Ensure rugarch is installed: install.packages("rugarch")
#   2. Set DATA_FILE to your returns data
#   3. Run script to generate: outputs/rugarch_reference/
#   4. Compare outputs to manual implementation results
#
# =============================================================================

library(rugarch)
library(xts)
library(lubridate)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Data source
DATA_FILE <- "./data/processed/raw (FX + EQ).csv"
OUTPUT_DIR <- "./outputs/rugarch_reference"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Asset to use for testing
TEST_ASSET <- "EURUSD"  # Change as needed: EURUSD, GBPUSD, USDZAR, NVDA, MSFT, AMZN

# Reproducibility
set.seed(123)

# =============================================================================
# DATA LOADING
# =============================================================================

cat("=============================================================================\n")
cat("RUGARCH REFERENCE IMPLEMENTATION FOR PARITY TESTING\n")
cat("=============================================================================\n\n")

cat("Loading data from:", DATA_FILE, "\n")

# Load price data
raw_data <- read.csv(DATA_FILE, row.names = 1)
raw_data$Date <- lubridate::ymd(rownames(raw_data))
rownames(raw_data) <- NULL

# Extract asset prices
if (!(TEST_ASSET %in% names(raw_data))) {
  stop("Asset ", TEST_ASSET, " not found in data. Available assets: ", 
       paste(names(raw_data)[names(raw_data) != "Date"], collapse = ", "))
}

prices <- xts(raw_data[[TEST_ASSET]], order.by = raw_data$Date)
returns <- diff(log(prices))[-1]  # Log returns, remove NA

cat("Loaded", length(returns), "return observations for", TEST_ASSET, "\n")
cat("Date range:", as.character(index(returns)[1]), "to", as.character(index(returns)[length(returns)]), "\n")
cat("Mean return:", round(mean(returns, na.rm=TRUE), 6), "\n")
cat("Std dev:", round(sd(returns, na.rm=TRUE), 4), "\n\n")

# =============================================================================
# MODEL SPECIFICATIONS
# =============================================================================

specs <- list(
  sGARCH_norm = ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "norm"
  ),
  
  sGARCH_std = ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "std"
  ),
  
  sGARCH_sstd = ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "sstd"
  ),
  
  gjrGARCH_norm = ugarchspec(
    variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "norm"
  ),
  
  eGARCH_norm = ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "norm"
  ),
  
  TGARCH_norm = ugarchspec(
    variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "TGARCH"),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "norm"
  )
)

# =============================================================================
# MODEL FITTING
# =============================================================================

cat("=== FITTING MODELS ===\n\n")

fits <- list()
for (model_name in names(specs)) {
  cat("Fitting:", model_name, "... ")
  
  tryCatch({
    fit <- ugarchfit(spec = specs[[model_name]], data = returns, solver = "hybrid")
    
    if (convergence(fit) == 0) {
      fits[[model_name]] <- fit
      ll <- likelihood(fit)
      cat("Converged (LL =", round(ll, 2), ")\n")
    } else {
      cat("Did not converge (code:", convergence(fit), ")\n")
    }
  }, error = function(e) {
    cat("Error:", e$message, "\n")
  })
}

cat("\nSuccessfully fit", length(fits), "out of", length(specs), "models\n\n")

# =============================================================================
# EXTRACT AND SAVE RESULTS
# =============================================================================

cat("=== EXTRACTING AND SAVING RESULTS ===\n\n")

summary_list <- list()

for (model_name in names(fits)) {
  fit <- fits[[model_name]]
  
  cat("Processing:", model_name, "\n")
  
  # Extract components
  coef_vec <- coef(fit)
  sigma_vec <- as.numeric(sigma(fit))
  residuals_raw <- as.numeric(residuals(fit, standardize = FALSE))
  residuals_std <- as.numeric(residuals(fit, standardize = TRUE))
  fitted_mean <- as.numeric(fitted(fit))
  
  loglik <- likelihood(fit)
  info_crit <- infocriteria(fit)
  
  # Save parameters
  params_df <- data.frame(
    parameter = names(coef_vec),
    value = as.numeric(coef_vec)
  )
  write.csv(params_df, 
            file.path(OUTPUT_DIR, paste0(model_name, "_parameters.csv")), 
            row.names = FALSE)
  
  # Save sigma (conditional volatility)
  sigma_df <- data.frame(
    index = 1:length(sigma_vec),
    date = as.character(index(returns)),
    sigma = sigma_vec
  )
  write.csv(sigma_df, 
            file.path(OUTPUT_DIR, paste0(model_name, "_sigma.csv")), 
            row.names = FALSE)
  
  # Save standardized residuals
  std_res_df <- data.frame(
    index = 1:length(residuals_std),
    date = as.character(index(returns)),
    std_residuals = residuals_std
  )
  write.csv(std_res_df, 
            file.path(OUTPUT_DIR, paste0(model_name, "_std_residuals.csv")), 
            row.names = FALSE)
  
  # Save raw residuals
  raw_res_df <- data.frame(
    index = 1:length(residuals_raw),
    date = as.character(index(returns)),
    raw_residuals = residuals_raw
  )
  write.csv(raw_res_df, 
            file.path(OUTPUT_DIR, paste0(model_name, "_raw_residuals.csv")), 
            row.names = FALSE)
  
  # Save information criteria
  ic_df <- data.frame(
    metric = c("LogLikelihood", "AIC", "BIC", "Shibata", "Hannan-Quinn"),
    value = c(loglik, info_crit)
  )
  write.csv(ic_df, 
            file.path(OUTPUT_DIR, paste0(model_name, "_info_criteria.csv")), 
            row.names = FALSE)
  
  # =======================================================================
  # FORECASTING
  # =======================================================================
  
  cat("  Generating forecasts... ")
  
  # Multi-step ahead forecasts
  horizons <- c(1, 5, 10, 20, 50, 100)
  forecast_df <- data.frame(
    horizon = horizons, 
    sigma_forecast = numeric(length(horizons)),
    mean_forecast = numeric(length(horizons))
  )
  
  for (i in seq_along(horizons)) {
    fc <- ugarchforecast(fit, n.ahead = horizons[i])
    forecast_df$sigma_forecast[i] <- as.numeric(sigma(fc)[horizons[i]])
    forecast_df$mean_forecast[i] <- as.numeric(fitted(fc)[horizons[i]])
  }
  
  write.csv(forecast_df, 
            file.path(OUTPUT_DIR, paste0(model_name, "_forecasts.csv")), 
            row.names = FALSE)
  
  cat("OK\n")
  
  # =======================================================================
  # PATH SIMULATION
  # =======================================================================
  
  cat("  Simulating paths... ")
  
  # Simulate 100-step path
  set.seed(123)
  sim <- ugarchpath(spec = specs[[model_name]], n.sim = 100, m.sim = 1, rseed = 123)
  
  sim_returns <- as.numeric(fitted(sim))
  sim_sigma <- as.numeric(sigma(sim))
  
  sim_df <- data.frame(
    step = 1:100,
    returns = sim_returns,
    sigma = sim_sigma
  )
  write.csv(sim_df, 
            file.path(OUTPUT_DIR, paste0(model_name, "_simulation.csv")), 
            row.names = FALSE)
  
  cat("OK\n")
  
  # =======================================================================
  # DIAGNOSTICS
  # =======================================================================
  
  # Persistence (for GARCH-type models)
  persistence <- NA
  uncond_var <- NA
  half_life <- NA
  
  if ("alpha1" %in% names(coef_vec) && "beta1" %in% names(coef_vec)) {
    alpha <- coef_vec["alpha1"]
    beta <- coef_vec["beta1"]
    persistence <- alpha + beta
    
    # Add gamma for gjrGARCH
    if ("gamma1" %in% names(coef_vec)) {
      gamma <- coef_vec["gamma1"]
      # For symmetric distributions E[I_{t-1}] = 0.5
      # For skewed distributions, need to calculate properly
      persistence <- alpha + beta + gamma * 0.5
    }
    
    if (persistence < 1 && persistence > 0) {
      omega <- coef_vec["omega"]
      uncond_var <- omega / (1 - persistence)
      half_life <- -log(2) / log(persistence)
    }
  }
  
  diag_df <- data.frame(
    metric = c("persistence", "unconditional_variance", "half_life", "n_obs", "n_params"),
    value = c(persistence, uncond_var, half_life, length(returns), length(coef_vec))
  )
  write.csv(diag_df, 
            file.path(OUTPUT_DIR, paste0(model_name, "_diagnostics.csv")), 
            row.names = FALSE)
  
  # Add to summary
  summary_list[[model_name]] <- list(
    loglik = loglik,
    aic = info_crit["Akaike"],
    bic = info_crit["Bayes"],
    persistence = persistence,
    uncond_var = uncond_var,
    half_life = half_life,
    n_params = length(coef_vec),
    convergence = convergence(fit)
  )
  
  cat("\n")
}

# =============================================================================
# SUMMARY REPORT
# =============================================================================

cat("=== SUMMARY REPORT ===\n\n")

summary_df <- do.call(rbind, lapply(names(summary_list), function(nm) {
  s <- summary_list[[nm]]
  data.frame(
    model = nm,
    converged = (s$convergence == 0),
    loglik = round(s$loglik, 2),
    aic = round(s$aic, 2),
    bic = round(s$bic, 2),
    persistence = round(s$persistence, 4),
    uncond_var = round(s$uncond_var, 8),
    half_life = round(s$half_life, 2),
    n_params = s$n_params,
    stringsAsFactors = FALSE
  )
}))

print(summary_df)
cat("\n")

write.csv(summary_df, file.path(OUTPUT_DIR, "summary.csv"), row.names = FALSE)

# =============================================================================
# METADATA
# =============================================================================

metadata <- list(
  asset = TEST_ASSET,
  n_obs = length(returns),
  date_range = paste(as.character(index(returns)[1]), "to", as.character(index(returns)[length(returns)])),
  mean_return = mean(returns, na.rm=TRUE),
  sd_return = sd(returns, na.rm=TRUE),
  rugarch_version = as.character(packageVersion("rugarch")),
  R_version = R.version.string,
  generated_date = as.character(Sys.time()),
  seed = 123
)

metadata_df <- data.frame(
  key = names(metadata),
  value = sapply(metadata, as.character),
  stringsAsFactors = FALSE
)

write.csv(metadata_df, file.path(OUTPUT_DIR, "metadata.csv"), row.names = FALSE)

cat("All reference outputs saved to:", OUTPUT_DIR, "\n\n")

# =============================================================================
# COMPARISON CHECKLIST
# =============================================================================

cat("=== COMPARISON CHECKLIST FOR MANUAL IMPLEMENTATION ===\n\n")

cat("1. PARAMETERS\n")
cat("   Compare: ", file.path(OUTPUT_DIR, "*_parameters.csv"), "\n")
cat("   With: Manual implementation parameter estimates\n")
cat("   Expected: Correlation > 0.95, |diff| < 1e-3\n")
cat("   Note: Student-t uses rescaling factor sqrt((nu-2)/nu)\n\n")

cat("2. SIGMA SERIES\n")
cat("   Compare: ", file.path(OUTPUT_DIR, "*_sigma.csv"), "\n")
cat("   With: Manual sigma_t estimates\n")
cat("   Expected: Correlation > 0.999\n")
cat("   Note: Student-t manual sigma may be ~1.3x rugarch due to rescaling\n\n")

cat("3. LOG-LIKELIHOOD\n")
cat("   Compare: ", file.path(OUTPUT_DIR, "*_info_criteria.csv"), "\n")
cat("   With: Manual log-likelihood\n")
cat("   Expected: Relative difference < 1e-4\n")
cat("   Note: May differ by constant term (especially Student-t)\n\n")

cat("4. STANDARDIZED RESIDUALS\n")
cat("   Compare: ", file.path(OUTPUT_DIR, "*_std_residuals.csv"), "\n")
cat("   With: Manual standardized residuals\n")
cat("   Check: mean ≈ 0, variance ≈ 1\n")
cat("   Expected: Correlation > 0.99\n\n")

cat("5. FORECASTS\n")
cat("   Compare: ", file.path(OUTPUT_DIR, "*_forecasts.csv"), "\n")
cat("   With: Manual multi-step forecasts\n")
cat("   1-step: Should match closely\n")
cat("   Multi-step (h>1): WILL DIFFER if manual uses simulation\n")
cat("   rugarch uses analytical forecasts (converges to omega/(1-alpha-beta))\n")
cat("   Manual simulation converges to omega/(1-beta) [DIFFERENT]\n\n")

cat("6. SIMULATIONS\n")
cat("   Compare: ", file.path(OUTPUT_DIR, "*_simulation.csv"), "\n")
cat("   With: Manual path simulations\n")
cat("   Note: Will differ due to random seed unless coordinated\n\n")

cat("=== KNOWN ISSUES FROM CODE REVIEW ===\n\n")

cat("ISSUE 2: Student-t Rescaling (CRITICAL)\n")
cat("  Problem: Manual uses unrescaled Student-t, rugarch uses rescaled\n")
cat("  Impact: σ_manual ≈ σ_rugarch × sqrt(ν/(ν-2))\n")
cat("  Example: For ν=5, σ_manual ≈ 1.29 × σ_rugarch\n")
cat("  Fix: Multiply manual sigma by sqrt((ν-2)/ν) before comparing\n\n")

cat("ISSUE 4: TGARCH Specification (BLOCKING)\n")
cat("  Problem: Manual uses Zakoian (abs residuals), rugarch uses fGARCH\n")
cat("  Impact: Different models, parameters NOT comparable\n")
cat("  Fix: Acknowledge these are different TGARCH variants\n\n")

cat("ISSUE 6: Multi-Step Forecasts (CRITICAL)\n")
cat("  Problem: Manual uses simulation, rugarch uses analytical\n")
cat("  Impact: Forecasts diverge for h>10\n")
cat("  Manual: σ²_{t+h} → ω/(1-β) as h → ∞\n")
cat("  rugarch: σ²_{t+h} → ω/(1-α-β) as h → ∞\n")
cat("  Fix: Document difference OR implement analytical forecasts\n\n")

cat("ISSUE 8: Skewed Student-t (DATA INTEGRITY)\n")
cat("  Problem: Manual silently downgrades sstd → std\n")
cat("  Impact: Results labeled 'sstd' actually use 'std'\n")
cat("  Fix: Remove sstd from configuration\n\n")

cat("=============================================================================\n")
cat("REFERENCE GENERATION COMPLETE\n")
cat("=============================================================================\n\n")

cat("Next steps:\n")
cat("1. Run manual implementation on same data (", TEST_ASSET, ")\n")
cat("2. Compare outputs using checklist above\n")
cat("3. Document all differences found\n")
cat("4. Apply corrections from code review\n\n")
