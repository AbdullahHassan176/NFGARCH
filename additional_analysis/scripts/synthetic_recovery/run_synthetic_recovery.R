#!/usr/bin/env Rscript
# Synthetic Distribution Recovery Experiment
# Main entrypoint for the experiment - Multi-Seed Version

# Load centralized seed configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
} else {
  REPRODUCIBILITY_SEED <- 123
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
source("scripts/experiments/synthetic_recovery/evaluate_recovery.R")

# =============================================================================
# HELPER FUNCTION: GENERATE MULTISEED REPORT
# =============================================================================

generate_multiseed_report <- function(output_dir, metrics_raw_agg, metrics_shape_agg,
                                     summary_raw_agg, summary_shape_agg,
                                     skewness_sign_match_rate, n_seeds) {
  
  report <- c(
    "# Multi-Seed Synthetic Distribution Recovery Experiment - Results",
    "",
    paste("**Date**:", Sys.Date()),
    paste("**Number of seeds**:", n_seeds),
    "",
    "## Overview",
    "",
    "This report summarizes results from running the synthetic distribution recovery experiment across multiple random seeds to assess stability and robustness.",
    "",
    "## Evaluation Modes",
    "",
    "### RAW Mode",
    "",
    "Metrics computed on distributions as produced by each pipeline (including scale drift).",
    "This answers: **\"What distribution does each full pipeline output?\"**",
    "",
    "### SHAPE Mode",
    "",
    "Metrics computed on standardized distributions (mean=0, SD=1).",
    "This answers: **\"How well does each method recover the innovation SHAPE ignoring scale/mean drift?\"**",
    "",
    "## Aggregated Recovery Metrics (RAW Mode)",
    "",
    "| Method | KS Stat (mean±sd) | Wasserstein (mean±sd) | Skew Diff (mean±sd) | Kurt Diff (mean±sd) |",
    "|--------|-------------------|----------------------|---------------------|---------------------|"
  )
  
  if (!is.null(metrics_raw_agg) && nrow(metrics_raw_agg) > 0) {
    for (i in 1:nrow(metrics_raw_agg)) {
      row <- metrics_raw_agg[i, ]
      report <- c(report,
        sprintf("| %s | %.4f±%.4f | %.4f±%.4f | %.4f±%.4f | %.4f±%.4f |",
                row$method,
                row$ks_stat_mean, row$ks_stat_sd,
                row$wasserstein_mean, row$wasserstein_sd,
                row$skewness_diff_mean, row$skewness_diff_sd,
                row$kurtosis_diff_mean, row$kurtosis_diff_sd)
      )
    }
  }
  
  report <- c(report,
    "",
    "## Aggregated Recovery Metrics (SHAPE Mode)",
    "",
    "| Method | KS Stat (mean±sd) | Wasserstein (mean±sd) | Skew Diff (mean±sd) | Kurt Diff (mean±sd) |",
    "|--------|-------------------|----------------------|---------------------|---------------------|"
  )
  
  if (!is.null(metrics_shape_agg) && nrow(metrics_shape_agg) > 0) {
    for (i in 1:nrow(metrics_shape_agg)) {
      row <- metrics_shape_agg[i, ]
      report <- c(report,
        sprintf("| %s | %.4f±%.4f | %.4f±%.4f | %.4f±%.4f | %.4f±%.4f |",
                row$method,
                row$ks_stat_mean, row$ks_stat_sd,
                row$wasserstein_mean, row$wasserstein_sd,
                row$skewness_diff_mean, row$skewness_diff_sd,
                row$kurtosis_diff_mean, row$kurtosis_diff_sd)
      )
    }
  }
  
  report <- c(report,
    "",
    "## Scale Drift Analysis (RAW Mode)",
    "",
    "| Method | Mean (mean±sd) | SD (mean±sd) |",
    "|--------|---------------|--------------|"
  )
  
  if (!is.null(summary_raw_agg) && nrow(summary_raw_agg) > 0) {
    for (i in 1:nrow(summary_raw_agg)) {
      row <- summary_raw_agg[i, ]
      report <- c(report,
        sprintf("| %s | %.4f±%.4f | %.4f±%.4f |",
                row$Method, row$Mean_mean, row$Mean_sd, row$SD_mean, row$SD_sd)
      )
    }
  }
  
  report <- c(report,
    "",
    "## NF-GARCH Skewness Sign Match Rate",
    "",
    paste("**Rate**:", if (!is.na(skewness_sign_match_rate)) {
      paste0(round(skewness_sign_match_rate * 100, 1), "%")
    } else "N/A"),
    "",
    "This is the proportion of seeds where NF-GARCH recovered the correct sign of skewness.",
    "",
    "## Winner Summary",
    "",
    "### RAW Mode Winners",
    "",
    "| Metric | Winner |",
    "|--------|--------|"
  )
  
  if (!is.null(metrics_raw_agg) && nrow(metrics_raw_agg) > 0) {
    # Find winners
    ks_winner <- metrics_raw_agg[which.min(metrics_raw_agg$ks_stat_mean), "method"]
    wass_winner <- metrics_raw_agg[which.min(metrics_raw_agg$wasserstein_mean), "method"]
    skew_winner <- metrics_raw_agg[which.min(metrics_raw_agg$skewness_diff_mean), "method"]
    kurt_winner <- metrics_raw_agg[which.min(metrics_raw_agg$kurtosis_diff_mean), "method"]
    
    report <- c(report,
      paste("| KS Statistic |", ks_winner, "|"),
      paste("| Wasserstein Distance |", wass_winner, "|"),
      paste("| Skewness Difference |", skew_winner, "|"),
      paste("| Kurtosis Difference |", kurt_winner, "|")
    )
  }
  
  report <- c(report,
    "",
    "### SHAPE Mode Winners",
    "",
    "| Metric | Winner |",
    "|--------|--------|"
  )
  
  if (!is.null(metrics_shape_agg) && nrow(metrics_shape_agg) > 0) {
    # Find winners
    ks_winner <- metrics_shape_agg[which.min(metrics_shape_agg$ks_stat_mean), "method"]
    wass_winner <- metrics_shape_agg[which.min(metrics_shape_agg$wasserstein_mean), "method"]
    skew_winner <- metrics_shape_agg[which.min(metrics_shape_agg$skewness_diff_mean), "method"]
    kurt_winner <- metrics_shape_agg[which.min(metrics_shape_agg$kurtosis_diff_mean), "method"]
    
    report <- c(report,
      paste("| KS Statistic |", ks_winner, "|"),
      paste("| Wasserstein Distance |", wass_winner, "|"),
      paste("| Skewness Difference |", skew_winner, "|"),
      paste("| Kurtosis Difference |", kurt_winner, "|")
    )
  }
  
  report <- c(report,
    "",
    "## Files Generated",
    "",
    "- `recovery_metrics_raw_aggregate.csv`: Aggregated RAW metrics",
    "- `recovery_metrics_shape_aggregate.csv`: Aggregated SHAPE metrics",
    "- `summary_statistics_raw_aggregate.csv`: Aggregated RAW summary statistics",
    "- `summary_statistics_shape_aggregate.csv`: Aggregated SHAPE summary statistics",
    "- `seed_*/`: Per-seed results directories",
    "",
    "## Notes",
    "",
    "- RAW metrics include scale drift effects",
    "- SHAPE metrics isolate distributional shape recovery",
    "- Lower values indicate better recovery",
    "- Standard deviations indicate stability across seeds",
    ""
  )
  
  writeLines(report, file.path(output_dir, "MULTISEED_REPORT.md"))
  cat("Generated MULTISEED_REPORT.md\n")
}

# =============================================================================
# CONFIGURATION
# =============================================================================

OUTPUT_DIR <- "outputs/synthetic_recovery"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Multi-seed configuration
SEEDS <- c(11, 22, 33, 44, 55, 66, 77, 88, 99, 123)

# DGP Configuration
DGP_CONFIG <- get_default_dgp_config()

cat("=== SYNTHETIC DISTRIBUTION RECOVERY EXPERIMENT (MULTI-SEED) ===\n")
cat("DGP Configuration:\n")
cat("  Sample size (T):", DGP_CONFIG$T, "\n")
cat("  GARCH params: omega =", DGP_CONFIG$omega, 
    ", alpha =", DGP_CONFIG$alpha, ", beta =", DGP_CONFIG$beta, "\n")
cat("  Innovation type:", DGP_CONFIG$innovation_type, "\n")
cat("  Innovation params:", paste(names(DGP_CONFIG$innovation_params), 
                                   DGP_CONFIG$innovation_params, sep = "=", collapse = ", "), "\n")
cat("  Seeds:", paste(SEEDS, collapse = ", "), "\n")
cat("  Total runs:", length(SEEDS), "\n\n")

# =============================================================================
# SINGLE SEED EXPERIMENT FUNCTION
# =============================================================================

run_single_seed_experiment <- function(seed, dgp_config, base_output_dir) {
  cat("\n")
  cat("=", rep("=", 60), "\n", sep = "")
  cat("Running experiment with seed =", seed, "\n")
  cat("=", rep("=", 60), "\n", sep = "")
  
  # Set seed
  set.seed(seed)
  
  # Create per-seed output directory
  seed_output_dir <- file.path(base_output_dir, paste0("seed_", seed))
  dir.create(seed_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(seed_output_dir, "plots"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(seed_output_dir, "residuals"), recursive = TRUE, showWarnings = FALSE)
  
  # Update DGP config with current seed
  dgp_config$seed <- seed
  
  # ===========================================================================
  # STEP 1: GENERATE SYNTHETIC DATA
  # ===========================================================================
  
  cat("Step 1: Generating synthetic GARCH(1,1) data...\n")
  
  # Create innovation sampler
  innovation_sampler <- create_innovation_sampler(
    dgp_config$innovation_type, 
    dgp_config$innovation_params
  )
  
  # Simulate data
  sim_data <- simulate_garch11(
    T = dgp_config$T,
    omega = dgp_config$omega,
    alpha = dgp_config$alpha,
    beta = dgp_config$beta,
    innovation_sampler = innovation_sampler,
    mu = dgp_config$mu
  )
  
  # Extract components
  returns <- sim_data$returns
  z_true <- sim_data$z  # GROUND TRUTH innovations
  
  cat("  Generated", length(returns), "observations\n")
  cat("  True innovation stats: mean =", round(mean(z_true), 6), 
      ", sd =", round(sd(z_true), 6), "\n")
  cat("  True innovation skewness =", round(moments::skewness(z_true), 4), "\n\n")
  
  # Save true innovations
  write.csv(data.frame(z_true = z_true), 
            file.path(seed_output_dir, "residuals", "z_true.csv"), 
            row.names = FALSE)
  
  # ===========================================================================
  # STEP 2: FIT COMPETING MODELS
  # ===========================================================================
  
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
    # A) SIGMA vs SIGMA2 VERIFICATION
    sigma_gaussian <- fit_gaussian$sigma
    sigma2_gaussian <- sigma_gaussian^2
    max_sigma_diff <- max(abs(sigma2_gaussian - sigma_gaussian^2), na.rm = TRUE)
    cat("    [VERIFY] Sigma consistency check:\n")
    cat("      mean(sigma) =", round(mean(sigma_gaussian, na.rm = TRUE), 6), "\n")
    cat("      sd(sigma) =", round(sd(sigma_gaussian, na.rm = TRUE), 6), "\n")
    cat("      max|sigma^2 - sigma2| =", round(max_sigma_diff, 10), "\n")
    if (max_sigma_diff > 1e-6) {
      warning("Gaussian GARCH: sigma^2 != sigma2, max diff = ", max_sigma_diff)
    }
    
    z_hat_gaussian <- engine_residuals(fit_gaussian, standardize = TRUE)
    z_hat_gaussian <- z_hat_gaussian[!is.na(z_hat_gaussian)]
    cat("      sd(z_hat_gaussian) =", round(sd(z_hat_gaussian, na.rm = TRUE), 6), "\n")
    
    write.csv(data.frame(residual = z_hat_gaussian), 
              file.path(seed_output_dir, "residuals", "z_hat_gaussian.csv"), 
              row.names = FALSE)
    cat("    [OK] Converged. Extracted", length(z_hat_gaussian), "standardized residuals\n")
  } else {
    cat("    [FAILED] Failed to converge\n")
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
    # A) SIGMA vs SIGMA2 VERIFICATION
    sigma_student_t <- fit_student_t$sigma
    sigma2_student_t <- sigma_student_t^2
    max_sigma_diff <- max(abs(sigma2_student_t - sigma_student_t^2), na.rm = TRUE)
    cat("    [VERIFY] Sigma consistency check:\n")
    cat("      mean(sigma) =", round(mean(sigma_student_t, na.rm = TRUE), 6), "\n")
    cat("      sd(sigma) =", round(sd(sigma_student_t, na.rm = TRUE), 6), "\n")
    cat("      max|sigma^2 - sigma2| =", round(max_sigma_diff, 10), "\n")
    if (max_sigma_diff > 1e-6) {
      warning("Student-t GARCH: sigma^2 != sigma2, max diff = ", max_sigma_diff)
    }
    
    z_hat_student_t <- engine_residuals(fit_student_t, standardize = TRUE)
    z_hat_student_t <- z_hat_student_t[!is.na(z_hat_student_t)]
    cat("      sd(z_hat_student_t) [original] =", round(sd(z_hat_student_t, na.rm = TRUE), 6), "\n")
    
    # B) STUDENT-T VARIANCE NORMALIZATION
    # Extract nu from coef (handle different naming)
    nu_hat <- tryCatch({
      if (!is.null(fit_student_t$coef) && "nu" %in% names(fit_student_t$coef)) {
        as.numeric(fit_student_t$coef["nu"])
      } else if (!is.null(fit_student_t$manual_fit) && !is.null(fit_student_t$manual_fit$coef) && "nu" %in% names(fit_student_t$manual_fit$coef)) {
        as.numeric(fit_student_t$manual_fit$coef["nu"])
      } else {
        NA
      }
    }, error = function(e) {
      cat("      Warning: Could not extract nu_hat:", e$message, "\n")
      NA
    })
    
    if (!is.na(nu_hat) && is.numeric(nu_hat) && nu_hat > 2) {
      var_correction <- sqrt((nu_hat - 2) / nu_hat)
      z_hat_student_t_corrected <- z_hat_student_t * var_correction
      cat("      nu_hat =", round(nu_hat, 4), "\n")
      cat("      var_correction = sqrt((nu-2)/nu) =", round(var_correction, 6), "\n")
      cat("      sd(z_hat_student_t) [corrected] =", round(sd(z_hat_student_t_corrected, na.rm = TRUE), 6), "\n")
      
      # Save both versions
      write.csv(data.frame(residual = z_hat_student_t), 
                file.path(seed_output_dir, "residuals", "z_hat_student_t.csv"), 
                row.names = FALSE)
      write.csv(data.frame(residual = z_hat_student_t_corrected), 
                file.path(seed_output_dir, "residuals", "z_hat_student_t_corrected.csv"), 
                row.names = FALSE)
      
      # Use corrected version for evaluation
      z_hat_student_t <- z_hat_student_t_corrected
      
      # Save nu_hat for later aggregation
      nu_estimate <- data.frame(seed = seed, nu_hat = nu_hat, var_correction = var_correction)
      write.csv(nu_estimate, 
                file.path(seed_output_dir, "student_t_nu_estimate.csv"), 
                row.names = FALSE)
    } else {
      warning("Student-t GARCH: nu_hat invalid or <= 2, skipping variance correction")
      write.csv(data.frame(residual = z_hat_student_t), 
                file.path(seed_output_dir, "residuals", "z_hat_student_t.csv"), 
                row.names = FALSE)
    }
    
    cat("    [OK] Converged. Extracted", length(z_hat_student_t), "standardized residuals\n")
  } else {
    cat("    [FAILED] Failed to converge\n")
    z_hat_student_t <- NULL
  }
  
  # 2c) Fit NF-GARCH (two-stage: GARCH + NF on residuals)
  cat("  2c) Fitting NF-GARCH (two-stage)...\n")
  
  # Use Student-t GARCH as base for standardization
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
    nf_residuals_file <- file.path(seed_output_dir, "residuals", "z_hat_for_nf.csv")
    write.csv(data.frame(residual = z_hat_base), nf_residuals_file, row.names = FALSE)
    
    cat("    Running Python NF training (baseline config: layers=4, hidden=64)...\n")
    
    # Call Python script to train NF
    python_script <- file.path(getwd(), "scripts", "experiments", "synthetic_recovery", "train_nf_synthetic.py")
    nf_model_path <- file.path(seed_output_dir, "nf_model.pth")
    
    # Pass seed to Python
    python_cmd <- sprintf('python "%s" "%s" "%s" %d', 
                         normalizePath(python_script, mustWork = FALSE), 
                         normalizePath(nf_residuals_file, mustWork = FALSE),
                         normalizePath(nf_model_path, mustWork = FALSE),
                         seed)
    
    system_result <- tryCatch({
      system(python_cmd, intern = TRUE, show.output.on.console = FALSE)
    }, error = function(e) {
      cat("    ERROR running Python:", e$message, "\n")
      NULL
    })
    
    # Check if NF model was created
    if (file.exists(nf_model_path)) {
      cat("    [OK] NF model trained and saved\n")
    } else {
      cat("    [FAILED] NF model training failed\n")
    }
    
    # D) NF ARCHITECTURE STABILITY TEST: Run alternative config on subset of seeds
    test_seeds <- c(11, 22, 33)  # Test on first 3 seeds
    if (seed %in% test_seeds) {
      cat("    Running alternative NF config (layers=8, hidden=128) for architecture test...\n")
      nf_model_path_alt <- file.path(seed_output_dir, "nf_model_alt.pth")
      
      # Create alternative config by modifying Python call
      # We'll pass a flag to use alternative config
      python_cmd_alt <- sprintf('python "%s" "%s" "%s" %d --alt-config', 
                               normalizePath(python_script, mustWork = FALSE), 
                               normalizePath(nf_residuals_file, mustWork = FALSE),
                               normalizePath(nf_model_path_alt, mustWork = FALSE),
                               seed)
      
      system_result_alt <- tryCatch({
        system(python_cmd_alt, intern = TRUE, show.output.on.console = FALSE)
      }, error = function(e) {
        cat("    ERROR running Python (alt config):", e$message, "\n")
        NULL
      })
      
      if (file.exists(nf_model_path_alt)) {
        cat("    [OK] Alternative NF model trained and saved\n")
      }
    }
  } else {
    cat("    [FAILED] Cannot fit NF-GARCH: base GARCH fit failed\n")
  }
  
  # ===========================================================================
  # STEP 3: EVALUATE DISTRIBUTION RECOVERY
  # ===========================================================================
  
  cat("Step 3: Evaluating distribution recovery...\n")
  
  # Run evaluation
  evaluation_results <- evaluate_distribution_recovery(
    z_true = z_true,
    z_hat_gaussian = z_hat_gaussian,
    z_hat_student_t = z_hat_student_t,
    nf_model_path = file.path(seed_output_dir, "nf_model.pth"),
    nf_residuals_path = file.path(seed_output_dir, "residuals", "z_hat_for_nf.csv"),
    output_dir = seed_output_dir,
    dgp_config = dgp_config
  )
  
  cat("  Evaluation complete for seed", seed, "\n")
  
  # Return results for aggregation
  return(list(
    seed = seed,
    evaluation_results = evaluation_results,
    z_true_skewness = moments::skewness(z_true, na.rm = TRUE),
    z_nf_skewness = if (!is.null(evaluation_results$summary_stats_raw) && 
                        "NF-GARCH" %in% evaluation_results$summary_stats_raw$Method) {
      evaluation_results$summary_stats_raw$Skewness[evaluation_results$summary_stats_raw$Method == "NF-GARCH"]
    } else NA
  ))
}

# =============================================================================
# RUN MULTI-SEED EXPERIMENT
# =============================================================================

cat("Starting multi-seed experiment...\n\n")

all_results <- list()

for (i in seq_along(SEEDS)) {
  seed <- SEEDS[i]
  cat("\n[", i, "/", length(SEEDS), "] Processing seed =", seed, "\n")
  
  result <- tryCatch({
    run_single_seed_experiment(seed, DGP_CONFIG, OUTPUT_DIR)
  }, error = function(e) {
    cat("ERROR in seed", seed, ":", e$message, "\n")
    NULL
  })
  
  if (!is.null(result)) {
    all_results[[as.character(seed)]] <- result
  }
}

cat("\n")
cat("=", rep("=", 60), "\n", sep = "")
cat("AGGREGATING RESULTS ACROSS SEEDS\n")
cat("=", rep("=", 60), "\n", sep = "")

# =============================================================================
# AGGREGATE RESULTS
# =============================================================================

# Aggregate recovery metrics
if (length(all_results) > 0) {
  # Collect all metrics
  metrics_raw_list <- list()
  metrics_shape_list <- list()
  summary_raw_list <- list()
  summary_shape_list <- list()
  skewness_sign_matches <- numeric()
  
  for (seed_str in names(all_results)) {
    result <- all_results[[seed_str]]
    eval_res <- result$evaluation_results
    
    if (!is.null(eval_res$metrics_raw)) {
      eval_res$metrics_raw$seed <- as.numeric(seed_str)
      metrics_raw_list[[seed_str]] <- eval_res$metrics_raw
    }
    
    if (!is.null(eval_res$metrics_shape)) {
      eval_res$metrics_shape$seed <- as.numeric(seed_str)
      metrics_shape_list[[seed_str]] <- eval_res$metrics_shape
    }
    
    if (!is.null(eval_res$summary_stats_raw)) {
      eval_res$summary_stats_raw$seed <- as.numeric(seed_str)
      summary_raw_list[[seed_str]] <- eval_res$summary_stats_raw
    }
    
    if (!is.null(eval_res$summary_stats_shape)) {
      eval_res$summary_stats_shape$seed <- as.numeric(seed_str)
      summary_shape_list[[seed_str]] <- eval_res$summary_stats_shape
    }
    
    # Check skewness sign match for NF
    if (!is.na(result$z_true_skewness) && !is.na(result$z_nf_skewness)) {
      sign_match <- sign(result$z_true_skewness) == sign(result$z_nf_skewness)
      skewness_sign_matches <- c(skewness_sign_matches, as.numeric(sign_match))
    }
  }
  
  # Aggregate metrics (mean, sd, median)
  if (length(metrics_raw_list) > 0) {
    metrics_raw_all <- bind_rows(metrics_raw_list)
    
    metrics_raw_agg <- metrics_raw_all %>%
      group_by(method) %>%
      summarise(
        ks_stat_mean = mean(ks_stat, na.rm = TRUE),
        ks_stat_sd = sd(ks_stat, na.rm = TRUE),
        ks_stat_median = median(ks_stat, na.rm = TRUE),
        wasserstein_mean = mean(wasserstein, na.rm = TRUE),
        wasserstein_sd = sd(wasserstein, na.rm = TRUE),
        wasserstein_median = median(wasserstein, na.rm = TRUE),
        skewness_diff_mean = mean(skewness_diff, na.rm = TRUE),
        skewness_diff_sd = sd(skewness_diff, na.rm = TRUE),
        skewness_diff_median = median(skewness_diff, na.rm = TRUE),
        kurtosis_diff_mean = mean(kurtosis_diff, na.rm = TRUE),
        kurtosis_diff_sd = sd(kurtosis_diff, na.rm = TRUE),
        kurtosis_diff_median = median(kurtosis_diff, na.rm = TRUE),
        .groups = "drop"
      )
    
    write.csv(metrics_raw_agg, 
              file.path(OUTPUT_DIR, "recovery_metrics_raw_aggregate.csv"), 
              row.names = FALSE)
    cat("Saved aggregated RAW metrics\n")
  }
  
  if (length(metrics_shape_list) > 0) {
    metrics_shape_all <- bind_rows(metrics_shape_list)
    
    metrics_shape_agg <- metrics_shape_all %>%
      group_by(method) %>%
      summarise(
        ks_stat_mean = mean(ks_stat, na.rm = TRUE),
        ks_stat_sd = sd(ks_stat, na.rm = TRUE),
        ks_stat_median = median(ks_stat, na.rm = TRUE),
        wasserstein_mean = mean(wasserstein, na.rm = TRUE),
        wasserstein_sd = sd(wasserstein, na.rm = TRUE),
        wasserstein_median = median(wasserstein, na.rm = TRUE),
        skewness_diff_mean = mean(skewness_diff, na.rm = TRUE),
        skewness_diff_sd = sd(skewness_diff, na.rm = TRUE),
        skewness_diff_median = median(skewness_diff, na.rm = TRUE),
        kurtosis_diff_mean = mean(kurtosis_diff, na.rm = TRUE),
        kurtosis_diff_sd = sd(kurtosis_diff, na.rm = TRUE),
        kurtosis_diff_median = median(kurtosis_diff, na.rm = TRUE),
        .groups = "drop"
      )
    
    write.csv(metrics_shape_agg, 
              file.path(OUTPUT_DIR, "recovery_metrics_shape_aggregate.csv"), 
              row.names = FALSE)
    cat("Saved aggregated SHAPE metrics\n")
  }
  
  # Aggregate summary statistics
  if (length(summary_raw_list) > 0) {
    summary_raw_all <- bind_rows(summary_raw_list)
    
    summary_raw_agg <- summary_raw_all %>%
      group_by(Method) %>%
      summarise(
        Mean_mean = mean(Mean, na.rm = TRUE),
        Mean_sd = sd(Mean, na.rm = TRUE),
        SD_mean = mean(SD, na.rm = TRUE),
        SD_sd = sd(SD, na.rm = TRUE),
        Skewness_mean = mean(Skewness, na.rm = TRUE),
        Skewness_sd = sd(Skewness, na.rm = TRUE),
        Kurtosis_mean = mean(Kurtosis, na.rm = TRUE),
        Kurtosis_sd = sd(Kurtosis, na.rm = TRUE),
        Q01_mean = mean(Q01, na.rm = TRUE),
        Q99_mean = mean(Q99, na.rm = TRUE),
        .groups = "drop"
      )
    
    write.csv(summary_raw_agg, 
              file.path(OUTPUT_DIR, "summary_statistics_raw_aggregate.csv"), 
              row.names = FALSE)
    cat("Saved aggregated RAW summary statistics\n")
  }
  
  if (length(summary_shape_list) > 0) {
    summary_shape_all <- bind_rows(summary_shape_list)
    
    summary_shape_agg <- summary_shape_all %>%
      group_by(Method) %>%
      summarise(
        Mean_mean = mean(Mean, na.rm = TRUE),
        Mean_sd = sd(Mean, na.rm = TRUE),
        SD_mean = mean(SD, na.rm = TRUE),
        SD_sd = sd(SD, na.rm = TRUE),
        Skewness_mean = mean(Skewness, na.rm = TRUE),
        Skewness_sd = sd(Skewness, na.rm = TRUE),
        Kurtosis_mean = mean(Kurtosis, na.rm = TRUE),
        Kurtosis_sd = sd(Kurtosis, na.rm = TRUE),
        Q01_mean = mean(Q01, na.rm = TRUE),
        Q99_mean = mean(Q99, na.rm = TRUE),
        .groups = "drop"
      )
    
    write.csv(summary_shape_agg, 
              file.path(OUTPUT_DIR, "summary_statistics_shape_aggregate.csv"), 
              row.names = FALSE)
    cat("Saved aggregated SHAPE summary statistics\n")
  }
  
  # Compute skewness sign match rate
  skewness_sign_match_rate <- if (length(skewness_sign_matches) > 0) {
    mean(skewness_sign_matches)
  } else {
    NA
  }
  
  cat("\nSkewness sign match rate for NF-GARCH:", 
      if (!is.na(skewness_sign_match_rate)) {
        paste0(round(skewness_sign_match_rate * 100, 1), "%")
      } else "N/A", "\n")
  
  # Aggregate Student-t nu estimates
  nu_estimates_list <- list()
  for (seed_str in names(all_results)) {
    nu_file <- file.path(OUTPUT_DIR, paste0("seed_", seed_str), "student_t_nu_estimate.csv")
    if (file.exists(nu_file)) {
      tryCatch({
        nu_df <- read.csv(nu_file)
        nu_estimates_list[[seed_str]] <- nu_df
      }, error = function(e) {
        cat("  Warning: Could not read nu estimate for seed", seed_str, ":", e$message, "\n")
      })
    }
  }
  if (length(nu_estimates_list) > 0) {
    nu_estimates_all <- bind_rows(nu_estimates_list)
    write.csv(nu_estimates_all, 
              file.path(OUTPUT_DIR, "student_t_nu_estimates.csv"), 
              row.names = FALSE)
    cat("Saved Student-t nu estimates\n")
  }
  
  # Aggregate NF training LL comparison
  ll_comparison_list <- list()
  for (seed_str in names(all_results)) {
    ll_file <- file.path(OUTPUT_DIR, paste0("seed_", seed_str), "nf_model_ll_comparison.csv")
    if (file.exists(ll_file)) {
      tryCatch({
        ll_df <- read.csv(ll_file)
        ll_comparison_list[[seed_str]] <- ll_df
      }, error = function(e) {
        cat("  Warning: Could not read LL comparison for seed", seed_str, ":", e$message, "\n")
      })
    }
  }
  if (length(ll_comparison_list) > 0) {
    ll_comparison_all <- bind_rows(ll_comparison_list)
    write.csv(ll_comparison_all, 
              file.path(OUTPUT_DIR, "nf_training_ll_comparison_aggregate.csv"), 
              row.names = FALSE)
    cat("Saved NF training LL comparison aggregate\n")
    cat("  Pass rate:", round(mean(ll_comparison_all$passes_check, na.rm = TRUE) * 100, 1), "%\n")
  }
  
  # Aggregate NF sanity check metrics
  nf_sanity_raw_list <- list()
  nf_sanity_shape_list <- list()
  for (seed_str in names(all_results)) {
    sanity_raw_file <- file.path(OUTPUT_DIR, paste0("seed_", seed_str), "nf_fit_sanity_raw.csv")
    sanity_shape_file <- file.path(OUTPUT_DIR, paste0("seed_", seed_str), "nf_fit_sanity_shape.csv")
    if (file.exists(sanity_raw_file)) {
      tryCatch({
        sanity_df <- read.csv(sanity_raw_file)
        sanity_df$seed <- as.numeric(seed_str)
        nf_sanity_raw_list[[seed_str]] <- sanity_df
      }, error = function(e) {
        cat("  Warning: Could not read NF sanity RAW for seed", seed_str, ":", e$message, "\n")
      })
    }
    if (file.exists(sanity_shape_file)) {
      tryCatch({
        sanity_df <- read.csv(sanity_shape_file)
        sanity_df$seed <- as.numeric(seed_str)
        nf_sanity_shape_list[[seed_str]] <- sanity_df
      }, error = function(e) {
        cat("  Warning: Could not read NF sanity SHAPE for seed", seed_str, ":", e$message, "\n")
      })
    }
  }
  if (length(nf_sanity_raw_list) > 0) {
    nf_sanity_raw_all <- bind_rows(nf_sanity_raw_list)
    nf_sanity_raw_agg <- nf_sanity_raw_all %>%
      group_by(metric) %>%
      summarise(
        value_mean = mean(value, na.rm = TRUE),
        value_sd = sd(value, na.rm = TRUE),
        value_median = median(value, na.rm = TRUE),
        .groups = "drop"
      )
    write.csv(nf_sanity_raw_agg, 
              file.path(OUTPUT_DIR, "nf_fit_sanity_raw_aggregate.csv"), 
              row.names = FALSE)
    cat("Saved NF sanity check RAW aggregate\n")
  }
  if (length(nf_sanity_shape_list) > 0) {
    nf_sanity_shape_all <- bind_rows(nf_sanity_shape_list)
    nf_sanity_shape_agg <- nf_sanity_shape_all %>%
      group_by(metric) %>%
      summarise(
        value_mean = mean(value, na.rm = TRUE),
        value_sd = sd(value, na.rm = TRUE),
        value_median = median(value, na.rm = TRUE),
        .groups = "drop"
      )
    write.csv(nf_sanity_shape_agg, 
              file.path(OUTPUT_DIR, "nf_fit_sanity_shape_aggregate.csv"), 
              row.names = FALSE)
    cat("Saved NF sanity check SHAPE aggregate\n")
  }
  
  # Generate MULTISEED_REPORT.md
  generate_multiseed_report(OUTPUT_DIR, metrics_raw_agg, metrics_shape_agg, 
                           summary_raw_agg, summary_shape_agg, 
                           skewness_sign_match_rate, length(all_results))
}

cat("\n=== MULTI-SEED EXPERIMENT COMPLETE ===\n")
cat("All outputs saved to:", OUTPUT_DIR, "\n")

# =============================================================================
# HELPER FUNCTION: GENERATE MULTISEED REPORT
# =============================================================================

generate_multiseed_report <- function(output_dir, metrics_raw_agg, metrics_shape_agg,
                                     summary_raw_agg, summary_shape_agg,
                                     skewness_sign_match_rate, n_seeds) {
  
  report <- c(
    "# Multi-Seed Synthetic Distribution Recovery Experiment - Results",
    "",
    paste("**Date**:", Sys.Date()),
    paste("**Number of seeds**:", n_seeds),
    "",
    "## Overview",
    "",
    "This report summarizes results from running the synthetic distribution recovery experiment across multiple random seeds to assess stability and robustness.",
    "",
    "## Evaluation Modes",
    "",
    "### RAW Mode",
    "",
    "Metrics computed on distributions as produced by each pipeline (including scale drift).",
    "This answers: **\"What distribution does each full pipeline output?\"**",
    "",
    "### SHAPE Mode",
    "",
    "Metrics computed on standardized distributions (mean=0, SD=1).",
    "This answers: **\"How well does each method recover the innovation SHAPE ignoring scale/mean drift?\"**",
    "",
    "## Aggregated Recovery Metrics (RAW Mode)",
    "",
    "| Method | KS Stat (mean±sd) | Wasserstein (mean±sd) | Skew Diff (mean±sd) | Kurt Diff (mean±sd) |",
    "|--------|-------------------|----------------------|---------------------|---------------------|"
  )
  
  if (!is.null(metrics_raw_agg) && nrow(metrics_raw_agg) > 0) {
    for (i in 1:nrow(metrics_raw_agg)) {
      row <- metrics_raw_agg[i, ]
      report <- c(report,
        sprintf("| %s | %.4f±%.4f | %.4f±%.4f | %.4f±%.4f | %.4f±%.4f |",
                row$method,
                row$ks_stat_mean, row$ks_stat_sd,
                row$wasserstein_mean, row$wasserstein_sd,
                row$skewness_diff_mean, row$skewness_diff_sd,
                row$kurtosis_diff_mean, row$kurtosis_diff_sd)
      )
    }
  }
  
  report <- c(report,
    "",
    "## Aggregated Recovery Metrics (SHAPE Mode)",
    "",
    "| Method | KS Stat (mean±sd) | Wasserstein (mean±sd) | Skew Diff (mean±sd) | Kurt Diff (mean±sd) |",
    "|--------|-------------------|----------------------|---------------------|---------------------|"
  )
  
  if (!is.null(metrics_shape_agg) && nrow(metrics_shape_agg) > 0) {
    for (i in 1:nrow(metrics_shape_agg)) {
      row <- metrics_shape_agg[i, ]
      report <- c(report,
        sprintf("| %s | %.4f±%.4f | %.4f±%.4f | %.4f±%.4f | %.4f±%.4f |",
                row$method,
                row$ks_stat_mean, row$ks_stat_sd,
                row$wasserstein_mean, row$wasserstein_sd,
                row$skewness_diff_mean, row$skewness_diff_sd,
                row$kurtosis_diff_mean, row$kurtosis_diff_sd)
      )
    }
  }
  
  report <- c(report,
    "",
    "## Scale Drift Analysis (RAW Mode)",
    "",
    "| Method | Mean (mean±sd) | SD (mean±sd) |",
    "|--------|---------------|--------------|"
  )
  
  if (!is.null(summary_raw_agg) && nrow(summary_raw_agg) > 0) {
    for (i in 1:nrow(summary_raw_agg)) {
      row <- summary_raw_agg[i, ]
      report <- c(report,
        sprintf("| %s | %.4f±%.4f | %.4f±%.4f |",
                row$Method, row$Mean_mean, row$Mean_sd, row$SD_mean, row$SD_sd)
      )
    }
  }
  
  report <- c(report,
    "",
    "## NF-GARCH Skewness Sign Match Rate",
    "",
    paste("**Rate**:", if (!is.na(skewness_sign_match_rate)) {
      paste0(round(skewness_sign_match_rate * 100, 1), "%")
    } else "N/A"),
    "",
    "This is the proportion of seeds where NF-GARCH recovered the correct sign of skewness.",
    "",
    "## Winner Summary",
    "",
    "### RAW Mode Winners",
    "",
    "| Metric | Winner |",
    "|--------|--------|"
  )
  
  if (!is.null(metrics_raw_agg) && nrow(metrics_raw_agg) > 0) {
    # Find winners
    ks_winner <- metrics_raw_agg[which.min(metrics_raw_agg$ks_stat_mean), "method"]
    wass_winner <- metrics_raw_agg[which.min(metrics_raw_agg$wasserstein_mean), "method"]
    skew_winner <- metrics_raw_agg[which.min(metrics_raw_agg$skewness_diff_mean), "method"]
    kurt_winner <- metrics_raw_agg[which.min(metrics_raw_agg$kurtosis_diff_mean), "method"]
    
    report <- c(report,
      paste("| KS Statistic |", ks_winner, "|"),
      paste("| Wasserstein Distance |", wass_winner, "|"),
      paste("| Skewness Difference |", skew_winner, "|"),
      paste("| Kurtosis Difference |", kurt_winner, "|")
    )
  }
  
  report <- c(report,
    "",
    "### SHAPE Mode Winners",
    "",
    "| Metric | Winner |",
    "|--------|--------|"
  )
  
  if (!is.null(metrics_shape_agg) && nrow(metrics_shape_agg) > 0) {
    # Find winners
    ks_winner <- metrics_shape_agg[which.min(metrics_shape_agg$ks_stat_mean), "method"]
    wass_winner <- metrics_shape_agg[which.min(metrics_shape_agg$wasserstein_mean), "method"]
    skew_winner <- metrics_shape_agg[which.min(metrics_shape_agg$skewness_diff_mean), "method"]
    kurt_winner <- metrics_shape_agg[which.min(metrics_shape_agg$kurtosis_diff_mean), "method"]
    
    report <- c(report,
      paste("| KS Statistic |", ks_winner, "|"),
      paste("| Wasserstein Distance |", wass_winner, "|"),
      paste("| Skewness Difference |", skew_winner, "|"),
      paste("| Kurtosis Difference |", kurt_winner, "|")
    )
  }
  
  report <- c(report,
    "",
    "## Files Generated",
    "",
    "- `recovery_metrics_raw_aggregate.csv`: Aggregated RAW metrics",
    "- `recovery_metrics_shape_aggregate.csv`: Aggregated SHAPE metrics",
    "- `summary_statistics_raw_aggregate.csv`: Aggregated RAW summary statistics",
    "- `summary_statistics_shape_aggregate.csv`: Aggregated SHAPE summary statistics",
    "- `seed_*/`: Per-seed results directories",
    "",
    "## Notes",
    "",
    "- RAW metrics include scale drift effects",
    "- SHAPE metrics isolate distributional shape recovery",
    "- Lower values indicate better recovery",
    "- Standard deviations indicate stability across seeds",
    ""
  )
  
  writeLines(report, file.path(output_dir, "MULTISEED_REPORT.md"))
  cat("Generated MULTISEED_REPORT.md\n")
}
