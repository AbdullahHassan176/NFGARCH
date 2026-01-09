#!/usr/bin/env Rscript
# Distribution Recovery Evaluation
# Computes metrics and generates plots comparing true vs recovered distributions

library(ggplot2)
library(dplyr)
if (!require(moments)) {
  install.packages("moments")
  library(moments)
}

# Load utility functions
if (file.exists("scripts/evaluation/calculate_distributional_metrics.R")) {
  source("scripts/evaluation/calculate_distributional_metrics.R")
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Calculate distributional metrics
calculate_recovery_metrics <- function(z_true, z_hat) {
  if (is.null(z_hat) || length(z_hat) < 10) {
    return(list(
      ks_stat = NA,
      wasserstein = NA,
      skewness_diff = NA,
      kurtosis_diff = NA,
      q01_diff = NA,
      q05_diff = NA,
      q95_diff = NA,
      q99_diff = NA
    ))
  }
  
  # Ensure same length
  min_len <- min(length(z_true), length(z_hat))
  z_true_sub <- z_true[1:min_len]
  z_hat_sub <- z_hat[1:min_len]
  
  # KS statistic
  ks_result <- tryCatch({
    ks.test(z_true_sub, z_hat_sub)
  }, error = function(e) NULL)
  ks_stat <- if (!is.null(ks_result)) ks_result$statistic else NA
  
  # Wasserstein distance
  wass_dist <- tryCatch({
    if (requireNamespace("transport", quietly = TRUE)) {
      transport::wasserstein1d(z_true_sub, z_hat_sub)
    } else {
      # Manual approximation
      sorted_true <- sort(z_true_sub)
      sorted_hat <- sort(z_hat_sub)
      mean(abs(sorted_true - sorted_hat))
    }
  }, error = function(e) NA)
  
  # Tail quantiles
  q_true <- quantile(z_true_sub, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = TRUE)
  q_hat <- quantile(z_hat_sub, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = TRUE)
  
  # Moments
  skew_true <- moments::skewness(z_true_sub, na.rm = TRUE)
  skew_hat <- moments::skewness(z_hat_sub, na.rm = TRUE)
  kurt_true <- moments::kurtosis(z_true_sub, na.rm = TRUE)
  kurt_hat <- moments::kurtosis(z_hat_sub, na.rm = TRUE)
  
  return(list(
    ks_stat = as.numeric(ks_stat),
    wasserstein = wass_dist,
    skewness_diff = abs(skew_hat - skew_true),
    kurtosis_diff = abs(kurt_hat - kurt_true),
    q01_diff = abs(q_hat[1] - q_true[1]),
    q05_diff = abs(q_hat[2] - q_true[2]),
    q95_diff = abs(q_hat[3] - q_true[3]),
    q99_diff = abs(q_hat[4] - q_true[4])
  ))
}

#' Generate KDE overlay plot
plot_kde_overlay <- function(z_true, z_hat_gaussian, z_hat_student_t, z_nf, 
                              output_file) {
  
  # Prepare data
  df_list <- list()
  
  if (!is.null(z_true) && length(z_true) > 0) {
    df_list[["True"]] <- data.frame(
      z = z_true,
      method = "True"
    )
  }
  
  if (!is.null(z_hat_gaussian) && length(z_hat_gaussian) > 0) {
    df_list[["Gaussian GARCH"]] <- data.frame(
      z = z_hat_gaussian,
      method = "Gaussian GARCH"
    )
  }
  
  if (!is.null(z_hat_student_t) && length(z_hat_student_t) > 0) {
    df_list[["Student-t GARCH"]] <- data.frame(
      z = z_hat_student_t,
      method = "Student-t GARCH"
    )
  }
  
  if (!is.null(z_nf) && length(z_nf) > 0) {
    df_list[["NF-GARCH"]] <- data.frame(
      z = z_nf,
      method = "NF-GARCH"
    )
  }
  
  if (length(df_list) == 0) {
    cat("WARNING: No data to plot\n")
    return(NULL)
  }
  
  # Combine
  df <- bind_rows(df_list)
  
  # Sample if too large (for plotting speed)
  max_samples <- 5000
  if (nrow(df) > max_samples) {
    df <- df %>% 
      group_by(method) %>%
      sample_n(min(max_samples, n())) %>%
      ungroup()
  }
  
  # Create plot
  p <- ggplot(df, aes(x = z, color = method, linetype = method)) +
    geom_density(alpha = 0.6, size = 1) +
    scale_color_manual(values = c(
      "True" = "black",
      "Gaussian GARCH" = "red",
      "Student-t GARCH" = "blue",
      "NF-GARCH" = "green"
    )) +
    scale_linetype_manual(values = c(
      "True" = "solid",
      "Gaussian GARCH" = "dashed",
      "Student-t GARCH" = "dotted",
      "NF-GARCH" = "longdash"
    )) +
    labs(
      title = "Distribution Recovery: KDE Overlay",
      subtitle = "Comparison of True vs Recovered Innovation Distributions",
      x = "Standardized Innovation (z)",
      y = "Density",
      color = "Method",
      linetype = "Method"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  
  ggsave(output_file, plot = p, width = 10, height = 6, dpi = 300)
  cat("Saved KDE plot:", output_file, "\n")
  
  return(p)
}

#' Generate QQ plot against true distribution
plot_qq <- function(z_true, z_hat, method_name, output_file) {
  if (is.null(z_hat) || length(z_hat) < 10) {
    cat("WARNING: Insufficient data for QQ plot:", method_name, "\n")
    return(NULL)
  }
  
  # Sample if too large
  min_len <- min(length(z_true), length(z_hat))
  max_samples <- 2000
  if (min_len > max_samples) {
    idx <- sample(1:min_len, max_samples)
    z_true_sub <- z_true[idx]
    z_hat_sub <- z_hat[idx]
  } else {
    z_true_sub <- z_true[1:min_len]
    z_hat_sub <- z_hat[1:min_len]
  }
  
  # Quantiles
  probs <- seq(0.01, 0.99, by = 0.01)
  q_true <- quantile(z_true_sub, probs = probs, na.rm = TRUE)
  q_hat <- quantile(z_hat_sub, probs = probs, na.rm = TRUE)
  
  df <- data.frame(
    q_true = as.numeric(q_true),
    q_hat = as.numeric(q_hat)
  )
  
  # Reference line (y = x)
  max_val <- max(c(df$q_true, df$q_hat), na.rm = TRUE)
  min_val <- min(c(df$q_true, df$q_hat), na.rm = TRUE)
  
  p <- ggplot(df, aes(x = q_true, y = q_hat)) +
    geom_point(alpha = 0.5, size = 0.8) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(
      title = paste("QQ Plot:", method_name, "vs True"),
      x = "True Quantiles",
      y = paste(method_name, "Quantiles"),
      subtitle = "Points on red line indicate perfect recovery"
    ) +
    theme_minimal() +
    coord_fixed(ratio = 1, xlim = c(min_val, max_val), ylim = c(min_val, max_val))
  
  ggsave(output_file, plot = p, width = 8, height = 8, dpi = 300)
  cat("Saved QQ plot:", output_file, "\n")
  
  return(p)
}

#' Load NF samples from Python output
load_nf_samples <- function(nf_model_path, nf_residuals_path) {
  # Try to load pre-generated samples
  samples_file <- gsub("\\.pth$", "_samples.csv", nf_model_path)
  
  if (file.exists(samples_file)) {
    cat("Loading NF samples from:", samples_file, "\n")
    samples_df <- read.csv(samples_file)
    if ("z_nf" %in% names(samples_df)) {
      return(samples_df$z_nf)
    } else {
      return(samples_df[[1]])
    }
  }
  
  # If samples don't exist, try to generate them using Python
  cat("NF samples not found. Attempting to generate...\n")
  python_script <- "scripts/experiments/synthetic_recovery/generate_nf_samples.py"
  
  if (file.exists(python_script)) {
    cmd <- sprintf('python "%s" "%s" "%s"', 
                   python_script, nf_model_path, samples_file)
    system(cmd)
    
    if (file.exists(samples_file)) {
      samples_df <- read.csv(samples_file)
      return(samples_df[[1]])
    }
  }
  
  cat("WARNING: Could not load or generate NF samples\n")
  return(NULL)
}

# =============================================================================
# MAIN EVALUATION FUNCTION
# =============================================================================

evaluate_distribution_recovery <- function(z_true, z_hat_gaussian, z_hat_student_t,
                                           nf_model_path, nf_residuals_path,
                                           output_dir, dgp_config) {
  
  cat("Evaluating distribution recovery...\n")
  
  # Load NF samples
  z_nf <- load_nf_samples(nf_model_path, nf_residuals_path)
  
  # Calculate metrics for each method
  metrics_list <- list()
  
  if (!is.null(z_hat_gaussian)) {
    cat("  Computing metrics for Gaussian GARCH...\n")
    metrics_list[["Gaussian_GARCH"]] <- calculate_recovery_metrics(z_true, z_hat_gaussian)
  }
  
  if (!is.null(z_hat_student_t)) {
    cat("  Computing metrics for Student-t GARCH...\n")
    metrics_list[["Student_t_GARCH"]] <- calculate_recovery_metrics(z_true, z_hat_student_t)
  }
  
  if (!is.null(z_nf)) {
    cat("  Computing metrics for NF-GARCH...\n")
    metrics_list[["NF_GARCH"]] <- calculate_recovery_metrics(z_true, z_nf)
  }
  
  # Create metrics data frame
  metrics_df <- bind_rows(lapply(names(metrics_list), function(name) {
    m <- metrics_list[[name]]
    data.frame(
      method = name,
      ks_stat = m$ks_stat,
      wasserstein = m$wasserstein,
      skewness_diff = m$skewness_diff,
      kurtosis_diff = m$kurtosis_diff,
      q01_diff = m$q01_diff,
      q05_diff = m$q05_diff,
      q95_diff = m$q95_diff,
      q99_diff = m$q99_diff
    )
  }))
  
  # Save metrics
  write.csv(metrics_df, 
            file.path(output_dir, "recovery_metrics.csv"), 
            row.names = FALSE)
  cat("  Saved metrics to:", file.path(output_dir, "recovery_metrics.csv"), "\n")
  
  # Generate plots
  plots_dir <- file.path(output_dir, "plots")
  
  # KDE overlay
  plot_kde_overlay(z_true, z_hat_gaussian, z_hat_student_t, z_nf,
                   file.path(plots_dir, "kde_overlay.png"))
  
  # QQ plots
  if (!is.null(z_hat_gaussian)) {
    plot_qq(z_true, z_hat_gaussian, "Gaussian GARCH",
            file.path(plots_dir, "qq_gaussian.png"))
  }
  
  if (!is.null(z_hat_student_t)) {
    plot_qq(z_true, z_hat_student_t, "Student-t GARCH",
            file.path(plots_dir, "qq_student_t.png"))
  }
  
  if (!is.null(z_nf)) {
    plot_qq(z_true, z_nf, "NF-GARCH",
            file.path(plots_dir, "qq_nf.png"))
  }
  
  # Summary statistics table
  summary_stats <- data.frame(
    Method = c("True", "Gaussian GARCH", "Student-t GARCH", "NF-GARCH"),
    Mean = c(
      mean(z_true, na.rm = TRUE),
      ifelse(!is.null(z_hat_gaussian), mean(z_hat_gaussian, na.rm = TRUE), NA),
      ifelse(!is.null(z_hat_student_t), mean(z_hat_student_t, na.rm = TRUE), NA),
      ifelse(!is.null(z_nf), mean(z_nf, na.rm = TRUE), NA)
    ),
    SD = c(
      sd(z_true, na.rm = TRUE),
      ifelse(!is.null(z_hat_gaussian), sd(z_hat_gaussian, na.rm = TRUE), NA),
      ifelse(!is.null(z_hat_student_t), sd(z_hat_student_t, na.rm = TRUE), NA),
      ifelse(!is.null(z_nf), sd(z_nf, na.rm = TRUE), NA)
    ),
    Skewness = c(
      moments::skewness(z_true, na.rm = TRUE),
      ifelse(!is.null(z_hat_gaussian), moments::skewness(z_hat_gaussian, na.rm = TRUE), NA),
      ifelse(!is.null(z_hat_student_t), moments::skewness(z_hat_student_t, na.rm = TRUE), NA),
      ifelse(!is.null(z_nf), moments::skewness(z_nf, na.rm = TRUE), NA)
    ),
    Kurtosis = c(
      moments::kurtosis(z_true, na.rm = TRUE),
      ifelse(!is.null(z_hat_gaussian), moments::kurtosis(z_hat_gaussian, na.rm = TRUE), NA),
      ifelse(!is.null(z_hat_student_t), moments::kurtosis(z_hat_student_t, na.rm = TRUE), NA),
      ifelse(!is.null(z_nf), moments::kurtosis(z_nf, na.rm = TRUE), NA)
    ),
    Q01 = c(
      quantile(z_true, 0.01, na.rm = TRUE),
      ifelse(!is.null(z_hat_gaussian), quantile(z_hat_gaussian, 0.01, na.rm = TRUE), NA),
      ifelse(!is.null(z_hat_student_t), quantile(z_hat_student_t, 0.01, na.rm = TRUE), NA),
      ifelse(!is.null(z_nf), quantile(z_nf, 0.01, na.rm = TRUE), NA)
    ),
    Q99 = c(
      quantile(z_true, 0.99, na.rm = TRUE),
      ifelse(!is.null(z_hat_gaussian), quantile(z_hat_gaussian, 0.99, na.rm = TRUE), NA),
      ifelse(!is.null(z_hat_student_t), quantile(z_hat_student_t, 0.99, na.rm = TRUE), NA),
      ifelse(!is.null(z_nf), quantile(z_nf, 0.99, na.rm = TRUE), NA)
    )
  )
  
  write.csv(summary_stats, 
            file.path(output_dir, "summary_statistics.csv"), 
            row.names = FALSE)
  cat("  Saved summary statistics\n")
  
  return(list(
    metrics = metrics_df,
    summary_stats = summary_stats
  ))
}

#' Generate experiment report
generate_experiment_report <- function(dgp_config, evaluation_results, output_file) {
  
  metrics_df <- evaluation_results$metrics
  summary_stats <- evaluation_results$summary_stats
  
  report <- c(
    "# Synthetic Distribution Recovery Experiment - Results Summary",
    "",
    "## Experiment Overview",
    "",
    "This experiment evaluates how well different GARCH models recover the **true innovation distribution** from synthetic GARCH(1,1) data.",
    "",
    "## Data Generating Process (DGP)",
    "",
    "### GARCH Parameters",
    paste("- **Sample size (T):**", dgp_config$T),
    paste("- **Omega (ω):**", dgp_config$omega),
    paste("- **Alpha (α):**", dgp_config$alpha),
    paste("- **Beta (β):**", dgp_config$beta),
    paste("- **Stationarity:** α + β =", dgp_config$alpha + dgp_config$beta, 
          ifelse(dgp_config$alpha + dgp_config$beta < 1, "(stationary)", "(non-stationary)")),
    "",
    "### Innovation Distribution",
    paste("- **Type:**", dgp_config$innovation_type),
    paste("- **Parameters:**", paste(names(dgp_config$innovation_params), 
                                     dgp_config$innovation_params, 
                                     sep = "=", collapse = ", ")),
    paste("- **Seed:**", dgp_config$seed),
    "",
    "## Models Fitted",
    "",
    "1. **Gaussian GARCH(1,1)**: Standard GARCH with normal innovations",
    "2. **Student-t GARCH(1,1)**: GARCH with Student-t innovations",
    "3. **NF-GARCH(1,1)**: Two-stage approach",
    "   - Fit Gaussian GARCH(1,1) to extract standardized residuals",
    "   - Train normalizing flow on residuals",
    "   - Sample from fitted flow",
    "",
    "## Distribution Recovery Metrics",
    "",
    "The following metrics compare each method's recovered distribution against the **true innovation distribution**:",
    "",
    "| Method | KS Statistic | Wasserstein Distance | Skewness Diff | Kurtosis Diff |",
    "|--------|--------------|---------------------|---------------|---------------|"
  )
  
  # Add metrics rows
  for (i in 1:nrow(metrics_df)) {
    row <- metrics_df[i, ]
    report <- c(report,
                sprintf("| %s | %.4f | %.4f | %.4f | %.4f |",
                        row$method,
                        row$ks_stat,
                        row$wasserstein,
                        row$skewness_diff,
                        row$kurtosis_diff))
  }
  
  report <- c(report,
              "",
              "### Interpretation",
              "",
              "- **KS Statistic**: Lower is better (0 = perfect match)",
              "- **Wasserstein Distance**: Lower is better (0 = perfect match)",
              "- **Skewness/Kurtosis Diff**: Lower is better (0 = perfect match)",
              "",
              "## Summary Statistics",
              "",
              "| Method | Mean | SD | Skewness | Kurtosis | Q(0.01) | Q(0.99) |",
              "|--------|------|----|----------|----------|---------|---------|"
  )
  
  # Add summary rows
  for (i in 1:nrow(summary_stats)) {
    row <- summary_stats[i, ]
    report <- c(report,
                sprintf("| %s | %.4f | %.4f | %.4f | %.4f | %.4f | %.4f |",
                        row$Method,
                        row$Mean,
                        row$SD,
                        row$Skewness,
                        row$Kurtosis,
                        row$Q01,
                        row$Q99))
  }
  
  report <- c(report,
              "",
              "## Plots",
              "",
              "All plots are saved in the `plots/` directory:",
              "",
              "- `kde_overlay.png`: KDE overlay comparing all methods",
              "- `qq_gaussian.png`: QQ plot for Gaussian GARCH",
              "- `qq_student_t.png`: QQ plot for Student-t GARCH",
              "- `qq_nf.png`: QQ plot for NF-GARCH",
              "",
              "## Files Generated",
              "",
              "- `recovery_metrics.csv`: Detailed recovery metrics",
              "- `summary_statistics.csv`: Summary statistics table",
              "- `residuals/z_true.csv`: True innovations (ground truth)",
              "- `residuals/z_hat_gaussian.csv`: Gaussian GARCH residuals",
              "- `residuals/z_hat_student_t.csv`: Student-t GARCH residuals",
              "- `nf_model.pth`: Trained normalizing flow model",
              "",
              "## Notes",
              "",
              "- This experiment focuses on **distribution recovery**, not forecast accuracy",
              "- The true innovation distribution `z_true` is the ground truth",
              "- Lower metric values indicate better recovery",
              "- NF-GARCH uses a two-stage approach: GARCH fit → NF training → sampling",
              ""
  )
  
  writeLines(report, output_file)
  cat("Report written to:", output_file, "\n")
}

