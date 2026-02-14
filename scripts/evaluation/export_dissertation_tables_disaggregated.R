#!/usr/bin/env Rscript
# Export DISAGGREGATED tables only for the dissertation.
# No aggregation: every row = one asset, or one asset-model, or one asset-model-scenario.
# All outputs are traceable to pipeline scripts (compare_nf_vs_standard_garch.R,
# calculate_stylized_facts.R, var_backtesting_comprehensive.R, stress_testing_comprehensive.R,
# calculate_distributional_metrics.R, create_final_dashboard.R).
# Usage: run after pipeline; reads from results/consolidated/*.xlsx and writes
#        results/dissertation_tables/per_asset_*.csv and .tex.

if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
  set.seed(REPRODUCIBILITY_SEED)
} else {
  set.seed(123)
}

library(openxlsx)
library(dplyr)

output_dir <- "results/dissertation_tables"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Helper: write tabularx block only
write_tabularx_tex <- function(df, path, col_spec, headers = NULL, digits = 4, max_abs = 1e10) {
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
  if (is.null(headers)) headers <- gsub("_", "\\\\_", names(df))
  if (length(digits) == 1) digits <- rep(digits, ncol(df))
  fmt_cell <- function(v, d) {
    if (is.na(v) && !is.numeric(v)) return("--")
    if (is.numeric(v)) {
      if (is.na(v) || is.infinite(v)) return("--")
      if (abs(v) > max_abs || (abs(v) < 1e-10 && v != 0)) return("---")
      if (d == 0) return(as.character(round(v)))
      format(round(v, d), nsmall = d, scientific = FALSE, trim = TRUE)
    } else gsub("_", "\\\\_", as.character(v))
  }
  hrow <- paste(headers, collapse = " & ")
  body <- vector("character", nrow(df))
  for (i in seq_len(nrow(df))) {
    r <- df[i, , drop = FALSE]
    body[i] <- paste(sapply(seq_len(ncol(df)), function(j) fmt_cell(r[[j]], digits[j])), collapse = " & ")
  }
  writeLines(c(
    paste0("\\begin{tabularx}{\\linewidth}{", col_spec, "}"),
    "\\toprule", paste0(hrow, " \\\\"), "\\midrule",
    paste0(body, " \\\\"), "\\bottomrule", "\\end{tabularx}"
  ), path)
}

cat("=== EXPORTING DISAGGREGATED DISSERTATION TABLES ===\n\n")

# Paths (same as extract_dissertation_tables.R)
dashboard_file    <- "results/consolidated/Final_Dashboard.xlsx"
nf_comp_file      <- "results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx"
var_file          <- "results/consolidated/VaR_Backtesting.xlsx"
stress_file       <- "results/consolidated/Stress_Testing.xlsx"
stylized_file     <- "results/consolidated/Stylized_Facts.xlsx"
dist_file         <- "results/consolidated/Distributional_Metrics.xlsx"

# 1) NF vs Standard: Combined_Results = one row per (Asset, Model, Distribution, Source)
if (file.exists(nf_comp_file)) {
  combined <- read.xlsx(nf_comp_file, sheet = "Combined_Results")
  out <- combined %>%
    select(Asset, Model, Distribution, Source, MSE, MAE, AIC, BIC, LogLikelihood) %>%
    arrange(Asset, Model, Distribution, Source)
  write.csv(out, file.path(output_dir, "per_asset_nf_vs_standard.csv"), row.names = FALSE)
  tex_cols <- c("Asset", "Model", "Dist.", "Source", "MSE", "MAE", "AIC")
  if (ncol(out) >= 7) {
    tex_df <- out %>% select(1:min(7, ncol(out)))
    write_tabularx_tex(tex_df, file.path(output_dir, "per_asset_nf_vs_standard.tex"),
                      "l l l l *{3}{>{\\raggedleft\\arraybackslash}X}",
                      headers = tex_cols[1:min(7,ncol(out))],
                      digits = c(0,0,0,0,4,4,0), max_abs = 1e10)
  }
  cat("[OK] per_asset_nf_vs_standard (from Combined_Results)\n")
}

# 2) Stylized facts: one row per asset
if (file.exists(stylized_file)) {
  sf <- read.xlsx(stylized_file, sheet = "Stylized_Facts")
  cols <- c("Asset", "Asset_Class", "Volatility_clustering_persistence", "Leverage_effect",
            "Gain_loss_asymmetry_ratio", "Skewness")
  cols <- intersect(cols, names(sf))
  out <- sf %>% select(all_of(cols)) %>% arrange(Asset)
  write.csv(out, file.path(output_dir, "per_asset_stylized_facts.csv"), row.names = FALSE)
  write_tabularx_tex(out, file.path(output_dir, "per_asset_stylized_facts.tex"),
                     "l l *{4}{>{\\raggedleft\\arraybackslash}X}",
                     headers = gsub("_", " ", cols), digits = 3)
  cat("[OK] per_asset_stylized_facts\n")
}

# 3) VaR: one row per (Asset, Model, Confidence_Level)
if (file.exists(var_file)) {
  vr <- read.xlsx(var_file, sheet = "VaR_Backtesting")
  out <- vr %>% arrange(Asset, Model, Confidence_Level)
  write.csv(out, file.path(output_dir, "per_asset_var_backtesting.csv"), row.names = FALSE)
  cols <- c("Asset", "Model", "Confidence_Level", "Exceedance_Rate", "Expected_Rate", "Kupiec_pvalue", "Christoffersen_pvalue")
  cols <- intersect(cols, names(vr))
  if (length(cols) >= 4)
    write_tabularx_tex(vr %>% select(all_of(cols)), file.path(output_dir, "per_asset_var_backtesting.tex"),
                      paste(rep("l", length(cols)), collapse = " "),
                      digits = c(0,0,2,4,2,2,2), max_abs = 1e10)
  cat("[OK] per_asset_var_backtesting\n")
}

# 4) Stress: one row per (Asset, Scenario, Model) or per (Asset, Scenario)
if (file.exists(stress_file)) {
  sheets <- getSheetNames(stress_file)
  if ("Stress_Test_Results" %in% sheets) {
    st <- read.xlsx(stress_file, sheet = "Stress_Test_Results")
    write.csv(st, file.path(output_dir, "per_asset_stress_test_results.csv"), row.names = FALSE)
    cat("[OK] per_asset_stress_test_results\n")
  }
  if ("Forecast_Under_Stress" %in% sheets) {
    fs <- read.xlsx(stress_file, sheet = "Forecast_Under_Stress")
    write.csv(fs, file.path(output_dir, "per_asset_forecast_under_stress.csv"), row.names = FALSE)
    cat("[OK] per_asset_forecast_under_stress\n")
  }
}

# 5) Distributional: one row per (Asset, Model) or per asset-model
if (file.exists(dist_file)) {
  dm <- read.xlsx(dist_file, sheet = "Distributional_Metrics")
  write.csv(dm, file.path(output_dir, "per_asset_distributional_metrics.csv"), row.names = FALSE)
  cat("[OK] per_asset_distributional_metrics\n")
}

# 6) Baseline / chronological performance: per-asset if available
if (file.exists(dashboard_file)) {
  sheets <- getSheetNames(dashboard_file)
  if ("Detailed_Chrono_Results" %in% sheets) {
    dc <- read.xlsx(dashboard_file, sheet = "Detailed_Chrono_Results")
    write.csv(dc, file.path(output_dir, "per_asset_baseline_chrono.csv"), row.names = FALSE)
    cat("[OK] per_asset_baseline_chrono (Detailed_Chrono_Results)\n")
  }
  if ("Performance_Chrono" %in% sheets) {
    pc <- read.xlsx(dashboard_file, sheet = "Performance_Chrono")
    if ("Asset" %in% names(pc)) {
      write.csv(pc, file.path(output_dir, "per_asset_baseline_chrono.csv"), row.names = FALSE)
      cat("[OK] per_asset_baseline_chrono (Performance_Chrono with Asset)\n")
    }
  }
}

cat("\nDisaggregated tables written to:", output_dir, "\n")
cat("No aggregated summaries are produced; all tables are per-asset or per-asset-model.\n")
