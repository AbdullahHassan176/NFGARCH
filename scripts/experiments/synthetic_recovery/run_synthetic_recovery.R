#!/usr/bin/env Rscript
# Synthetic Distribution Recovery Experiment
# Main entrypoint for the experiment

# Load centralized seed configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
  set.seed(REPRODUCIBILITY_SEED)
} else {
  set.seed(123)
}

# Load required libraries
library(xts)
library(dplyr)
library(ggplot2)
if (!require(moments)) {
  install.packages("moments")
  library(moments)
}

# Source experiment modules
source("scripts/experiments/synthetic_recovery/synthetic_dgp.R")
source("scripts/engines/engine_selector.R")

# =============================================================================
# CONFIGURATION
# =============================================================================

OUTPUT_DIR <- "outputs/synthetic_recovery"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR, "plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR, "residuals"), recursive = TRUE, showWarnings = FALSE)

# DGP Configuration
DGP_CONFIG <- get_default_dgp_config()
# Override if needed:
# DGP_CONFIG$T <- 2000
# DGP_CONFIG$innovation_type <- "skewed_t"

cat("=== SYNTHETIC DISTRIBUTION RECOVERY EXPERIMENT ===\n")
cat("DGP Configuration:\n")
cat("  Sample size (T):", DGP_CONFIG$T, "\n")
cat("  GARCH params: omega =", DGP_CONFIG$omega, 
    ", alpha =", DGP_CONFIG$alpha, ", beta =", DGP_CONFIG$beta, "\n")
cat("  Innovation type:", DGP_CONFIG$innovation_type, "\n")
cat("  Innovation params:", paste(names(DGP_CONFIG$innovation_params), 
                                   DGP_CONFIG$innovation_params, sep = "=", collapse = ", "), "\n")
cat("  Seed:", DGP_CONFIG$seed, "\n\n")

# =============================================================================
# STEP 1: GENERATE SYNTHETIC DATA
# =============================================================================

cat("Step 1: Generating synthetic GARCH(1,1) data...\n")

# Create innovation sampler
innovation_sampler <- create_innovation_sampler(
  DGP_CONFIG$innovation_type, 
  DGP_CONFIG$innovation_params
)

# Simulate data
sim_data <- simulate_garch11(
  T = DGP_CONFIG$T,
  omega = DGP_CONFIG$omega,
  alpha = DGP_CONFIG$alpha,
  beta = DGP_CONFIG$beta,
  innovation_sampler = innovation_sampler,
  mu = DGP_CONFIG$mu
)

# Extract components
returns <- sim_data$returns
z_true <- sim_data$z  # GROUND TRUTH innovations

cat("  Generated", length(returns), "observations\n")
cat("  Return stats: mean =", round(mean(returns), 6), 
    ", sd =", round(sd(returns), 6), "\n")
cat("  True innovation stats: mean =", round(mean(z_true), 6), 
    ", sd =", round(sd(z_true), 6), "\n")
cat("  True innovation skewness =", round(moments::skewness(z_true), 4), "\n")
cat("  True innovation kurtosis =", round(moments::kurtosis(z_true), 4), "\n\n")

# Save true innovations
write.csv(data.frame(z_true = z_true), 
          file.path(OUTPUT_DIR, "residuals", "z_true.csv"), 
          row.names = FALSE)

# =============================================================================
# STEP 2: FIT COMPETING MODELS
# =============================================================================

cat("Step 2: Fitting competing GARCH models...\n")

# Convert returns to xts for compatibility
returns_xts <- xts(returns, order.by = seq(as.Date("2000-01-01"), 
                                             by = "day", 
                                             length.out = length(returns)))

# 2a) Fit Gaussian GARCH(1,1)
cat("  2a) Fitting Gaussian GARCH(1,1)...\n")
fit_gaussian <- tryCatch({
  engine_fit(model = "sGARCH", returns = returns_xts, dist = "norm", engine = "manual")
}, error = function(e) {
  cat("    ERROR:", e$message, "\n")
  NULL
})

if (!is.null(fit_gaussian) && fit_gaussian$convergence) {
  z_hat_gaussian <- engine_residuals(fit_gaussian, standardize = TRUE)
  z_hat_gaussian <- z_hat_gaussian[!is.na(z_hat_gaussian)]
  write.csv(data.frame(residual = z_hat_gaussian), 
            file.path(OUTPUT_DIR, "residuals", "z_hat_gaussian.csv"), 
            row.names = FALSE)
  cat("    ✓ Converged. Extracted", length(z_hat_gaussian), "standardized residuals\n")
} else {
  cat("    ✗ Failed to converge\n")
  z_hat_gaussian <- NULL
}

# 2b) Fit Student-t GARCH(1,1)
cat("  2b) Fitting Student-t GARCH(1,1)...\n")
fit_student_t <- tryCatch({
  engine_fit(model = "sGARCH", returns = returns_xts, dist = "std", engine = "manual")
}, error = function(e) {
  cat("    ERROR:", e$message, "\n")
  NULL
})

if (!is.null(fit_student_t) && fit_student_t$convergence) {
  z_hat_student_t <- engine_residuals(fit_student_t, standardize = TRUE)
  z_hat_student_t <- z_hat_student_t[!is.na(z_hat_student_t)]
  write.csv(data.frame(residual = z_hat_student_t), 
            file.path(OUTPUT_DIR, "residuals", "z_hat_student_t.csv"), 
            row.names = FALSE)
  cat("    ✓ Converged. Extracted", length(z_hat_student_t), "standardized residuals\n")
} else {
  cat("    ✗ Failed to converge\n")
  z_hat_student_t <- NULL
}

# 2c) Fit NF-GARCH (two-stage: GARCH + NF on residuals)
cat("  2c) Fitting NF-GARCH (two-stage)...\n")

# Use Student-t GARCH as base for standardization (better for skewed distributions)
# Fallback to Gaussian if Student-t failed
if (!is.null(fit_student_t) && fit_student_t$convergence) {
  z_hat_base <- z_hat_student_t
  base_model_name <- "Student_t"
  cat("    Using Student-t GARCH as base for NF training\n")
} else if (!is.null(fit_gaussian) && fit_gaussian$convergence) {
  z_hat_base <- z_hat_gaussian
  base_model_name <- "Gaussian"
  cat("    Using Gaussian GARCH as base for NF training (Student-t failed)\n")
} else {
  z_hat_base <- NULL
  base_model_name <- NULL
}

if (!is.null(z_hat_base)) {
  
  # Save residuals for Python NF training
  nf_residuals_file <- file.path(OUTPUT_DIR, "residuals", "z_hat_for_nf.csv")
  write.csv(data.frame(residual = z_hat_base), nf_residuals_file, row.names = FALSE)
  
  cat("    Saved residuals for NF training:", nf_residuals_file, "\n")
  cat("    Running Python NF training...\n")
  
  # Call Python script to train NF
  python_script <- file.path(getwd(), "scripts", "experiments", "synthetic_recovery", "train_nf_synthetic.py")
  nf_model_path <- file.path(OUTPUT_DIR, "nf_model.pth")
  
  # Use normalized paths
  python_cmd <- sprintf('python "%s" "%s" "%s"', 
                         normalizePath(python_script, mustWork = FALSE), 
                         normalizePath(nf_residuals_file, mustWork = FALSE),
                         normalizePath(nf_model_path, mustWork = FALSE))
  
  cat("    Running:", python_cmd, "\n")
  system_result <- tryCatch({
    system(python_cmd, intern = TRUE, show.output.on.console = FALSE)
  }, error = function(e) {
    cat("    ERROR running Python:", e$message, "\n")
    NULL
  })
  
  if (!is.null(system_result)) {
    cat("    Python output:\n")
    cat(paste("     ", system_result, collapse = "\n"), "\n")
  }
  
  # Check if NF model was created
  nf_model_file <- file.path(OUTPUT_DIR, "nf_model.pth")
  if (file.exists(nf_model_file)) {
    cat("    ✓ NF model trained and saved\n")
    
    # Sample from NF (will be done in evaluation script)
    cat("    NF samples will be generated in evaluation step\n")
  } else {
    cat("    ✗ NF model training failed\n")
  }
} else {
  cat("    ✗ Cannot fit NF-GARCH: base GARCH fit failed\n")
}

cat("\n")

# =============================================================================
# STEP 3: EVALUATE DISTRIBUTION RECOVERY
# =============================================================================

cat("Step 3: Evaluating distribution recovery...\n")

# Source evaluation script
source("scripts/experiments/synthetic_recovery/evaluate_recovery.R")

# Run evaluation
evaluation_results <- evaluate_distribution_recovery(
  z_true = z_true,
  z_hat_gaussian = z_hat_gaussian,
  z_hat_student_t = z_hat_student_t,
  nf_model_path = file.path(OUTPUT_DIR, "nf_model.pth"),
  nf_residuals_path = file.path(OUTPUT_DIR, "residuals", "z_hat_for_nf.csv"),
  output_dir = OUTPUT_DIR,
  dgp_config = DGP_CONFIG
)

cat("  Evaluation complete. Results saved to:", OUTPUT_DIR, "\n\n")

# =============================================================================
# STEP 4: GENERATE SUMMARY REPORT
# =============================================================================

cat("Step 4: Generating summary report...\n")

# Create markdown report
report_file <- file.path(OUTPUT_DIR, "README.md")
if (exists("generate_experiment_report")) {
  generate_experiment_report(
    dgp_config = DGP_CONFIG,
    evaluation_results = evaluation_results,
    output_file = report_file
  )
  cat("  Report written to:", report_file, "\n\n")
} else {
  cat("  WARNING: generate_experiment_report function not found\n")
}

cat("=== EXPERIMENT COMPLETE ===\n")
cat("All outputs saved to:", OUTPUT_DIR, "\n")

