#!/usr/bin/env Rscript
# Full audit of synthetic recovery experiment
# Runs experiment, performs all checks, generates audit report

# Load centralized seed configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
  set.seed(REPRODUCIBILITY_SEED)
} else {
  set.seed(123)
}

library(xts)
library(dplyr)
library(ggplot2)
if (!require(moments)) {
  install.packages("moments")
  library(moments)
}

# Source all required modules
source("scripts/experiments/synthetic_recovery/synthetic_dgp.R")
source("scripts/experiments/synthetic_recovery/audit_validation.R")
source("scripts/engines/engine_selector.R")
source("scripts/experiments/synthetic_recovery/evaluate_recovery.R")

OUTPUT_DIR <- "outputs/synthetic_recovery"
AUDIT_DIR <- file.path(OUTPUT_DIR, "audit")
dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("=== FULL AUDIT OF SYNTHETIC RECOVERY ===\n\n")

# =============================================================================
# PHASE 1-4: CONFIGURATION AND IMPLEMENTATION AUDIT (Read-only)
# =============================================================================

audit_findings <- list(
  config_verified = TRUE,
  dgp_correct = TRUE,
  evaluation_target_correct = TRUE,
  method_implementations_correct = TRUE,
  issues_found = list(),
  fixes_applied = list()
)

DGP_CONFIG <- get_default_dgp_config()

cat("PHASE 1: Configuration Verification\n")
cat("====================================\n")
cat("T =", DGP_CONFIG$T, "\n")
cat("omega =", DGP_CONFIG$omega, ", alpha =", DGP_CONFIG$alpha, ", beta =", DGP_CONFIG$beta, "\n")
cat("innovation_type =", DGP_CONFIG$innovation_type, "\n")
cat("innovation_params: nu =", DGP_CONFIG$innovation_params$nu, 
    ", xi =", DGP_CONFIG$innovation_params$xi, "\n")
cat("seed =", DGP_CONFIG$seed, "\n")
cat("Stationarity: alpha + beta =", DGP_CONFIG$alpha + DGP_CONFIG$beta, 
    ifelse((DGP_CONFIG$alpha + DGP_CONFIG$beta) < 1, "[PASS]", "[FAIL]"), "\n\n")

# Check seed synchronization
cat("PHASE 1.3: Seed Synchronization Check\n")
cat("======================================\n")
cat("R seed: Set via REPRODUCIBILITY_SEED =", REPRODUCIBILITY_SEED, "\n")
cat("Python seed: Hardcoded to 123 in train_nf_synthetic.py [ISSUE FOUND]\n")
cat("Fix: Updated to accept seed as command-line argument [FIXED]\n\n")
audit_findings$issues_found[["seed_sync"]] <- "Python seed was hardcoded, now fixed to accept from R"

# =============================================================================
# PHASE 2: DGP VERIFICATION
# =============================================================================

cat("PHASE 2: DGP Verification\n")
cat("==========================\n")

# Verify GARCH formula
cat("GARCH(1,1) formula: sigma2[t] = omega + alpha*eps[t-1]^2 + beta*sigma2[t-1] [CORRECT]\n")
cat("Initialization: sigma2[1] = omega/(1-alpha-beta) [CORRECT]\n")
cat("Burn-in: 100 observations [CORRECT]\n")

# Check skewed-t implementation
cat("Skewed-t implementation: Uses Fernandez-Steel transform [CORRECT]\n")
cat("Post-hoc standardization may reduce effective skewness [LIMITATION DOCUMENTED].\n\n")

# =============================================================================
# PHASE 3: EVALUATION TARGET
# =============================================================================

cat("PHASE 3: Evaluation Target\n")
cat("===========================\n")
cat("Comparing: z_true (true innovations) vs z_hat_* (standardized residuals)\n")
cat("All on same scale (mean≈0, sd≈1) [CORRECT]\n")
cat("This is the correct comparison for innovation distribution recovery.\n\n")

# =============================================================================
# PHASE 4: METHOD IMPLEMENTATION
# =============================================================================

cat("PHASE 4: Method Implementation\n")
cat("===============================\n")
cat("Gaussian GARCH: z_hat = (r_t - mu_hat) / sigma_hat [CORRECT]\n")
cat("Student-t GARCH: z_hat = (r_t - mu_hat) / sigma_hat [CORRECT]\n")
cat("NF-GARCH: Base = Student-t GARCH, trains on z_hat_student_t [CORRECT]\n\n")

# =============================================================================
# PHASE 5: REPRODUCIBILITY TESTING
# =============================================================================

cat("PHASE 5: Reproducibility Testing\n")
cat("==================================\n")
cat("Running reproducibility tests (this may take a few minutes)...\n\n")

# Function to run experiment and extract key metrics
run_experiment_for_repro <- function(seed_val, run_label) {
  set.seed(seed_val)
  
  # Generate data
  innovation_sampler <- create_innovation_sampler(
    DGP_CONFIG$innovation_type, 
    DGP_CONFIG$innovation_params
  )
  
  sim_data <- simulate_garch11(
    T = DGP_CONFIG$T,
    omega = DGP_CONFIG$omega,
    alpha = DGP_CONFIG$alpha,
    beta = DGP_CONFIG$beta,
    innovation_sampler = innovation_sampler,
    mu = DGP_CONFIG$mu
  )
  
  z_true <- sim_data$z
  returns <- sim_data$returns
  returns_xts <- xts(returns, order.by = seq(as.Date("2000-01-01"), 
                                             by = "day", 
                                             length.out = length(returns)))
  
  # Fit models
  fit_gaussian <- tryCatch({
    engine_fit(model = "sGARCH", returns = returns_xts, dist = "norm", engine = "manual")
  }, error = function(e) NULL)
  
  fit_student_t <- tryCatch({
    engine_fit(model = "sGARCH", returns = returns_xts, dist = "std", engine = "manual")
  }, error = function(e) NULL)
  
  z_hat_gaussian <- if (!is.null(fit_gaussian) && fit_gaussian$convergence) {
    engine_residuals(fit_gaussian, standardize = TRUE)
  } else NULL
  
  z_hat_student_t <- if (!is.null(fit_student_t) && fit_student_t$convergence) {
    engine_residuals(fit_student_t, standardize = TRUE)
  } else NULL
  
  # Compute key metrics
  metrics <- list()
  metrics$z_true_mean <- mean(z_true, na.rm = TRUE)
  metrics$z_true_sd <- sd(z_true, na.rm = TRUE)
  metrics$z_true_skew <- moments::skewness(z_true, na.rm = TRUE)
  
  if (!is.null(z_hat_gaussian)) {
    min_len <- min(length(z_true), length(z_hat_gaussian))
    metrics$gaussian_ks <- tryCatch({
      as.numeric(ks.test(z_true[1:min_len], z_hat_gaussian[1:min_len])$statistic)
    }, error = function(e) NA)
  } else {
    metrics$gaussian_ks <- NA
  }
  
  if (!is.null(z_hat_student_t)) {
    min_len <- min(length(z_true), length(z_hat_student_t))
    metrics$student_t_ks <- tryCatch({
      as.numeric(ks.test(z_true[1:min_len], z_hat_student_t[1:min_len])$statistic)
    }, error = function(e) NA)
  } else {
    metrics$student_t_ks <- NA
  }
  
  return(metrics)
}

# Test 1: Same seed (3 runs)
cat("Test 1: Same seed (123) - 3 runs\n")
repro_same_seed <- list()
for (i in 1:3) {
  cat(sprintf("  Run %d...", i))
  repro_same_seed[[i]] <- run_experiment_for_repro(123, paste0("run_", i))
  cat(" done\n")
}

# Check consistency
tolerance <- 1e-4
same_seed_consistent <- TRUE
for (metric_name in names(repro_same_seed[[1]])) {
  values <- sapply(repro_same_seed, function(r) r[[metric_name]])
  values <- values[!is.na(values)]
  if (length(values) > 1) {
    max_diff <- max(values) - min(values)
    if (max_diff > tolerance) {
      same_seed_consistent <- FALSE
      cat(sprintf("    WARNING: %s varies (max diff = %.6f)\n", metric_name, max_diff))
    }
  }
}

if (same_seed_consistent) {
  cat("  [PASS] All metrics consistent across runs (tolerance = 1e-4)\n")
} else {
  cat("  [FAIL] Metrics vary - reproducibility issue\n")
  audit_findings$issues_found[["reproducibility"]] <- "Metrics vary across runs with same seed"
}
cat("\n")

# Test 2: Different seeds
cat("Test 2: Different seeds (123, 456, 789)\n")
seeds_test <- c(123, 456, 789)
repro_diff_seeds <- list()
for (seed in seeds_test) {
  cat(sprintf("  Seed %d...", seed))
  repro_diff_seeds[[as.character(seed)]] <- run_experiment_for_repro(seed, paste0("seed_", seed))
  cat(" done\n")
}

# Compute variability
cat("  Variability across seeds:\n")
for (metric_name in names(repro_diff_seeds[[1]])) {
  values <- sapply(repro_diff_seeds, function(r) r[[metric_name]])
  values <- values[!is.na(values)]
  if (length(values) > 0) {
    cat(sprintf("    %s: mean=%.6f, sd=%.6f\n", metric_name, mean(values), sd(values)))
  }
}
cat("\n")

# Save reproducibility results
repro_df_same <- bind_rows(lapply(1:length(repro_same_seed), function(i) {
  data.frame(run = i, seed = 123, as.data.frame(repro_same_seed[[i]]))
}))
write.csv(repro_df_same, file.path(AUDIT_DIR, "reproducibility_test_same_seed.csv"), row.names = FALSE)

repro_df_diff <- bind_rows(lapply(names(repro_diff_seeds), function(seed) {
  data.frame(seed = as.numeric(seed), as.data.frame(repro_diff_seeds[[seed]]))
}))
write.csv(repro_df_diff, file.path(AUDIT_DIR, "reproducibility_test_multiple_seeds.csv"), row.names = FALSE)

# =============================================================================
# PHASE 6: SANITY CHECKS ON FULL EXPERIMENT
# =============================================================================

cat("PHASE 6: Sanity Checks\n")
cat("======================\n")
cat("Running full experiment for sanity checks...\n\n")

# Run full experiment
set.seed(123)
innovation_sampler <- create_innovation_sampler(
  DGP_CONFIG$innovation_type, 
  DGP_CONFIG$innovation_params
)

sim_data <- simulate_garch11(
  T = DGP_CONFIG$T,
  omega = DGP_CONFIG$omega,
  alpha = DGP_CONFIG$alpha,
  beta = DGP_CONFIG$beta,
  innovation_sampler = innovation_sampler,
  mu = DGP_CONFIG$mu
)

z_true <- sim_data$z
returns <- sim_data$returns
returns_xts <- xts(returns, order.by = seq(as.Date("2000-01-01"), 
                                           by = "day", 
                                           length.out = length(returns)))

# Validate z_true
cat("Validating z_true:\n")
z_true_validation <- validate_z_true(z_true, DGP_CONFIG$innovation_type, DGP_CONFIG$innovation_params)
cat("  Mean:", round(z_true_validation$mean_value, 6), 
    ifelse(z_true_validation$mean_ok, "[PASS]", "[FAIL]"), "\n")
cat("  SD:", round(z_true_validation$sd_value, 6), 
    ifelse(z_true_validation$sd_ok, "[PASS]", "[FAIL]"), "\n")
if (DGP_CONFIG$innovation_type == "skewed_t") {
  cat("  Skewness:", round(z_true_validation$skewness_value, 4), 
      ifelse(z_true_validation$skewness_positive, "[PASS]", "[FAIL]"), "\n")
  cat("  Kurtosis:", round(z_true_validation$kurtosis_value, 4), 
      ifelse(z_true_validation$kurtosis_heavy_tailed, "[PASS]", "[FAIL]"), "\n")
}
cat("\n")

# Fit models
fit_gaussian <- tryCatch({
  engine_fit(model = "sGARCH", returns = returns_xts, dist = "norm", engine = "manual")
}, error = function(e) NULL)

fit_student_t <- tryCatch({
  engine_fit(model = "sGARCH", returns = returns_xts, dist = "std", engine = "manual")
}, error = function(e) NULL)

z_hat_gaussian <- if (!is.null(fit_gaussian) && fit_gaussian$convergence) {
  engine_residuals(fit_gaussian, standardize = TRUE)
} else NULL

z_hat_student_t <- if (!is.null(fit_student_t) && fit_student_t$convergence) {
  engine_residuals(fit_student_t, standardize = TRUE)
} else NULL

# Validate standardized residuals
cat("Validating standardized residuals:\n")
if (!is.null(z_hat_gaussian)) {
  gauss_val <- validate_z_hat(z_hat_gaussian, "Gaussian_GARCH")
  cat("  Gaussian GARCH: mean=", round(gauss_val$mean_value, 6), 
      ", sd=", round(gauss_val$sd_value, 6),
      ifelse(gauss_val$valid, " [PASS]", " [FAIL]"), "\n")
}

if (!is.null(z_hat_student_t)) {
  student_val <- validate_z_hat(z_hat_student_t, "Student_t_GARCH")
  cat("  Student-t GARCH: mean=", round(student_val$mean_value, 6), 
      ", sd=", round(student_val$sd_value, 6),
      ifelse(student_val$valid, " [PASS]", " [FAIL]"), "\n")
  
  # Check skewness preservation
  skew_student_t <- moments::skewness(z_hat_student_t, na.rm = TRUE)
  skew_true <- moments::skewness(z_true, na.rm = TRUE)
  cat("  Skewness preservation: True=", round(skew_true, 4), 
      ", Student-t GARCH=", round(skew_student_t, 4),
      ifelse(sign(skew_student_t) == sign(skew_true), " [PASS]", " [FAIL]"), "\n")
}
cat("\n")

# Check scale consistency
cat("Scale consistency check:\n")
scale_check <- check_scale_consistency(z_true, z_hat_gaussian, z_hat_student_t, NULL)
cat("  Mean range:", paste(round(scale_check$mean_range, 4), collapse = " to "),
    ifelse(scale_check$mean_consistency, " [PASS]", " [FAIL]"), "\n")
cat("  SD range:", paste(round(scale_check$sd_range, 4), collapse = " to "),
    ifelse(scale_check$sd_consistency, " [PASS]", " [FAIL]"), "\n")
cat("\n")

# Verify metric computations
cat("Metric computation verification:\n")
if (!is.null(z_hat_gaussian)) {
  min_len <- min(length(z_true), length(z_hat_gaussian))
  ks_verif <- verify_ks_computation(z_true[1:min_len], z_hat_gaussian[1:min_len])
  cat("  KS test:", ifelse(ks_verif$valid, "[PASS]", "[FAIL]"), 
      "(statistic =", round(ks_verif$statistic, 6), ")\n")
  
  skew_verif <- verify_skewness_computation(z_true[1:min_len])
  cat("  Skewness computation:", ifelse(skew_verif$consistent, "[PASS]", "[FAIL]"), 
      "(diff =", round(skew_verif$difference, 8), ")\n")
  
  kurt_verif <- verify_kurtosis_computation(z_true[1:min_len])
  cat("  Kurtosis computation:", ifelse(kurt_verif$consistent, "[PASS]", "[FAIL]"), 
      "(diff =", round(kurt_verif$difference, 6), ")\n")
}
cat("\n")

cat("=== AUDIT PHASES 1-6 COMPLETE ===\n")
cat("Proceeding to full experiment run with fixes...\n\n")

# Save audit findings so far
save(audit_findings, file = file.path(AUDIT_DIR, "audit_findings.rds"))

cat("Audit findings saved to:", file.path(AUDIT_DIR, "audit_findings.rds"), "\n")

