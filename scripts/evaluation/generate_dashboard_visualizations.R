#!/usr/bin/env Rscript
# Generate Visualizations for HTML Dashboard
# Creates plots for all evaluation metrics and saves them as PNG files

# Load centralized seed configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
  set.seed(REPRODUCIBILITY_SEED)
} else {
  set.seed(123)  # Fallback if config not available
}

library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(stringr)

cat("=== GENERATING DASHBOARD VISUALIZATIONS ===\n\n")

plots_dir <- "results/dashboard_plots"
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

tryCatch({
# 1. Model Performance Comparison
cat("1. Creating model performance plots...\n")

# Load NF-GARCH results
results_file <- "results/consolidated/NF_GARCH_Results_manual.xlsx"
if (file.exists(results_file)) {
  chrono_results <- read.xlsx(results_file, sheet = "Chrono_Split_NF_GARCH")
  sheets_nf <- openxlsx::getSheetNames(results_file)
  tscv_results <- if ("TS_CV_NF_GARCH" %in% sheets_nf) {
    read.xlsx(results_file, sheet = "TS_CV_NF_GARCH")
  } else {
    data.frame()
  }

  # Plot 1: MSE by Model (Chrono Split)
  if (nrow(chrono_results) > 0 && "MSE" %in% names(chrono_results)) {
    p1 <- chrono_results %>%
      filter(!is.na(MSE) & is.finite(MSE) & MSE > 0) %>%
      group_by(Model) %>%
      summarise(mean_MSE = mean(MSE, na.rm = TRUE),
                se_MSE = sd(MSE, na.rm = TRUE) / sqrt(n()),
                .groups = "drop") %>%
      ggplot(aes(x = reorder(Model, mean_MSE), y = mean_MSE, fill = Model)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_errorbar(aes(ymin = mean_MSE - se_MSE, ymax = mean_MSE + se_MSE), width = 0.2) +
      scale_y_log10() +
      labs(title = "Mean Squared Error by Model (Chronological Split)",
           x = "Model", y = "Mean MSE (log scale)",
           subtitle = "Lower is better") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(paste(plots_dir, "mse_by_model_chrono.png", sep="/"), p1, width = 10, height = 6, dpi = 300)
    
    # Plot 2: AIC by Model
    if ("AIC" %in% names(chrono_results)) {
      p2 <- chrono_results %>%
        filter(!is.na(AIC) & is.finite(AIC)) %>%
        group_by(Model) %>%
        summarise(mean_AIC = mean(AIC, na.rm = TRUE),
                  se_AIC = sd(AIC, na.rm = TRUE) / sqrt(n()),
                  .groups = "drop") %>%
        ggplot(aes(x = reorder(Model, mean_AIC), y = mean_AIC, fill = Model)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        geom_errorbar(aes(ymin = mean_AIC - se_AIC, ymax = mean_AIC + se_AIC), width = 0.2) +
        labs(title = "Akaike Information Criterion by Model",
             x = "Model", y = "Mean AIC",
             subtitle = "Lower is better") +
        theme_minimal() +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggsave(paste(plots_dir, "aic_by_model.png", sep="/"), p2, width = 10, height = 6, dpi = 300)
    }
  }
}

# =============================================================================
# 2. Distributional Metrics
# =============================================================================

cat("2. Creating distributional metrics plots...\n")

dist_file <- "results/consolidated/Distributional_Metrics.xlsx"
if (file.exists(dist_file)) {
  dist_metrics <- read.xlsx(dist_file, sheet = "Distributional_Metrics")
  dist_summary <- read.xlsx(dist_file, sheet = "Summary_Statistics")
  
  # Plot 3: KS Distance by Model
  if ("KS_distance" %in% names(dist_metrics)) {
    p3 <- dist_metrics %>%
      filter(!is.na(KS_distance)) %>%
      group_by(Model) %>%
      summarise(mean_KS = mean(KS_distance, na.rm = TRUE),
                se_KS = sd(KS_distance, na.rm = TRUE) / sqrt(n()),
                .groups = "drop") %>%
      ggplot(aes(x = reorder(Model, mean_KS), y = mean_KS, fill = Model)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_errorbar(aes(ymin = mean_KS - se_KS, ymax = mean_KS + se_KS), width = 0.2) +
      labs(title = "Kolmogorov-Smirnov Distance by Model",
           x = "Model", y = "Mean KS Distance",
           subtitle = "Lower indicates better distributional fit") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(paste(plots_dir, "ks_distance_by_model.png", sep="/"), p3, width = 10, height = 6, dpi = 300)
  }
  
  # Plot 4: Wasserstein Distance by Model
  if ("Wasserstein_distance" %in% names(dist_metrics)) {
    p4 <- dist_metrics %>%
      filter(!is.na(Wasserstein_distance) & is.finite(Wasserstein_distance) & Wasserstein_distance > 0) %>%
      group_by(Model) %>%
      summarise(mean_Wass = mean(Wasserstein_distance, na.rm = TRUE),
                se_Wass = sd(Wasserstein_distance, na.rm = TRUE) / sqrt(n()),
                .groups = "drop") %>%
      ggplot(aes(x = reorder(Model, mean_Wass), y = mean_Wass, fill = Model)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_errorbar(aes(ymin = mean_Wass - se_Wass, ymax = mean_Wass + se_Wass), width = 0.2) +
      scale_y_log10() +
      labs(title = "Wasserstein Distance by Model",
           x = "Model", y = "Mean Wasserstein Distance (log scale)",
           subtitle = "Lower indicates better distributional fit") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(paste(plots_dir, "wasserstein_distance_by_model.png", sep="/"), p4, width = 10, height = 6, dpi = 300)
  }
  
  # Plot 5: Tail Index by Model
  if ("Tail_index" %in% names(dist_metrics)) {
    p5 <- dist_metrics %>%
      filter(!is.na(Tail_index)) %>%
      group_by(Model) %>%
      summarise(mean_Tail = mean(Tail_index, na.rm = TRUE),
                se_Tail = sd(Tail_index, na.rm = TRUE) / sqrt(n()),
                .groups = "drop") %>%
      ggplot(aes(x = reorder(Model, mean_Tail), y = mean_Tail, fill = Model)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_errorbar(aes(ymin = mean_Tail - se_Tail, ymax = mean_Tail + se_Tail), width = 0.2) +
      labs(title = "Tail Index by Model (Hill Estimator)",
           x = "Model", y = "Mean Tail Index",
           subtitle = "Higher indicates heavier tails") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(paste(plots_dir, "tail_index_by_model.png", sep="/"), p5, width = 10, height = 6, dpi = 300)
  }
}

# =============================================================================
# 3. Stylized Facts
# =============================================================================

cat("3. Creating stylized facts plots...\n")

stylized_file <- "results/consolidated/Stylized_Facts.xlsx"
if (file.exists(stylized_file)) {
  stylized_facts <- read.xlsx(stylized_file, sheet = "Stylized_Facts")
  stylized_summary <- read.xlsx(stylized_file, sheet = "Summary_By_Asset_Class")
  
  # Plot 6: Volatility Clustering by Asset Class
  if ("Volatility_clustering_persistence" %in% names(stylized_facts)) {
    p6 <- stylized_facts %>%
      filter(!is.na(Volatility_clustering_persistence)) %>%
      ggplot(aes(x = Asset_Class, y = Volatility_clustering_persistence, fill = Asset_Class)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.5) +
      labs(title = "Volatility Clustering by Asset Class",
           x = "Asset Class", y = "Volatility Clustering Persistence",
           subtitle = "Higher values indicate stronger clustering") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggsave(paste(plots_dir, "volatility_clustering_by_class.png", sep="/"), p6, width = 8, height = 6, dpi = 300)
  }
  
  # Plot 7: Leverage Effect by Asset Class
  if ("Leverage_effect" %in% names(stylized_facts)) {
    p7 <- stylized_facts %>%
      filter(!is.na(Leverage_effect)) %>%
      ggplot(aes(x = Asset_Class, y = Leverage_effect, fill = Asset_Class)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Leverage Effect by Asset Class",
           x = "Asset Class", y = "Leverage Effect",
           subtitle = "Positive values indicate negative returns increase volatility") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggsave(paste(plots_dir, "leverage_effect_by_class.png", sep="/"), p7, width = 8, height = 6, dpi = 300)
  }
}

# =============================================================================
# 4. VaR Backtesting
# =============================================================================

cat("4. Creating VaR backtesting plots...\n")

var_file <- "results/consolidated/VaR_Backtesting.xlsx"
if (file.exists(var_file)) {
  var_results <- read.xlsx(var_file, sheet = "VaR_Backtesting")
  var_summary <- read.xlsx(var_file, sheet = "Summary_Statistics")
  
  # Plot 8: Exceedance Rates by Model
  if ("Exceedance_Rate" %in% names(var_results)) {
    p8 <- var_results %>%
      filter(Confidence_Level == 0.95) %>%
      group_by(Model) %>%
      summarise(mean_exceedance = mean(Exceedance_Rate, na.rm = TRUE),
                se_exceedance = sd(Exceedance_Rate, na.rm = TRUE) / sqrt(n()),
                expected_rate = mean(Expected_Rate, na.rm = TRUE),
                .groups = "drop") %>%
      ggplot(aes(x = Model, y = mean_exceedance, fill = Model)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_errorbar(aes(ymin = mean_exceedance - se_exceedance, ymax = mean_exceedance + se_exceedance), width = 0.2) +
      geom_hline(aes(yintercept = expected_rate), linetype = "dashed", color = "red", size = 1) +
      labs(title = "VaR Exceedance Rates (95% Confidence)",
           x = "Model", y = "Exceedance Rate",
           subtitle = "Red dashed line = expected rate (5%)") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(paste(plots_dir, "var_exceedance_rates.png", sep="/"), p8, width = 10, height = 6, dpi = 300)
  }
  
  # Plot 9: Kupiec Test P-values
  if ("Kupiec_pvalue" %in% names(var_results)) {
    p9 <- var_results %>%
      filter(Confidence_Level == 0.95) %>%
      ggplot(aes(x = Model, y = Kupiec_pvalue, fill = Model)) +
      geom_boxplot(alpha = 0.7) +
      geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
      labs(title = "Kupiec Test P-values (95% Confidence)",
           x = "Model", y = "P-value",
           subtitle = "Red dashed line = significance threshold (0.05)") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(paste(plots_dir, "kupiec_pvalues.png", sep="/"), p9, width = 10, height = 6, dpi = 300)
  }
}

# =============================================================================
# 5. Stress Testing
# =============================================================================

cat("5. Creating stress testing plots...\n")

stress_file <- "results/consolidated/Stress_Testing.xlsx"
if (file.exists(stress_file)) {
  stress_results <- read.xlsx(stress_file, sheet = "Stress_Test_Results")
  stress_summary <- read.xlsx(stress_file, sheet = "Summary_Statistics")
  
  # Plot 10: Historical Crisis Volatility
  if ("Volatility" %in% names(stress_results) && "Scenario_Type" %in% names(stress_results)) {
    p10 <- stress_results %>%
      filter(Scenario_Type == "Historical_Crisis") %>%
      ggplot(aes(x = Scenario_Name, y = Volatility, fill = Scenario_Name)) +
      geom_boxplot(alpha = 0.7) +
      labs(title = "Volatility During Historical Crises",
           x = "Crisis Period", y = "Volatility",
           subtitle = "GFC = Global Financial Crisis 2008") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(paste(plots_dir, "historical_crisis_volatility.png", sep="/"), p10, width = 8, height = 6, dpi = 300)
  }
  
  # Plot 11: Hypothetical Shock Impact
  if ("Volatility_Increase_Pct" %in% names(stress_results)) {
    p11 <- stress_results %>%
      filter(Scenario_Type == "Hypothetical_Shock" & !is.na(Volatility_Increase_Pct)) %>%
      ggplot(aes(x = Scenario_Name, y = Volatility_Increase_Pct, fill = Scenario_Name)) +
      geom_boxplot(alpha = 0.7) +
      labs(title = "Volatility Increase from Hypothetical Shocks",
           x = "Shock Scenario", y = "Volatility Increase (%)",
           subtitle = "Percentage increase relative to baseline") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(paste(plots_dir, "hypothetical_shock_impact.png", sep="/"), p11, width = 10, height = 6, dpi = 300)
  }
}

# =============================================================================
# 6. NF vs Standard Comparison
# =============================================================================

cat("6. Creating NF vs Standard comparison plots...\n")

comparison_file <- "results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx"
if (file.exists(comparison_file)) {
  # Try to load comparison data
  tryCatch({
    combined_results <- read.xlsx(comparison_file, sheet = "Combined_Results")
    
    # Plot 12: MSE Comparison (NF vs Standard)
    if ("MSE" %in% names(combined_results) && "Source" %in% names(combined_results)) {
      p12 <- combined_results %>%
        filter(!is.na(MSE) & is.finite(MSE) & MSE > 0) %>%
        group_by(Model, Source) %>%
        summarise(mean_MSE = mean(MSE, na.rm = TRUE),
                  .groups = "drop") %>%
        ggplot(aes(x = Model, y = mean_MSE, fill = Source)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
        scale_y_log10() +
        labs(title = "NF-GARCH vs Standard GARCH: MSE Comparison",
             x = "Model", y = "Mean MSE (log scale)",
             subtitle = "Lower is better") +
        theme_minimal() +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggsave(paste(plots_dir, "nf_vs_standard_mse.png", sep="/"), p12, width = 10, height = 6, dpi = 300)
    }
    
    # Plot 13: Asset Class Comparison
    tryCatch({
      asset_class_summary <- read.xlsx(comparison_file, sheet = "Asset_Class_Summary")
      
      if ("mean_MSE" %in% names(asset_class_summary) && "Asset_Class" %in% names(asset_class_summary)) {
        p13 <- asset_class_summary %>%
          filter(!is.na(mean_MSE) & is.finite(mean_MSE) & mean_MSE > 0) %>%
          ggplot(aes(x = Asset_Class, y = mean_MSE, fill = Source)) +
          geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
          scale_y_log10() +
          labs(title = "NF-GARCH vs Standard GARCH by Asset Class",
               x = "Asset Class", y = "Mean MSE (log scale)",
               subtitle = "Lower is better") +
          theme_minimal() +
          theme(legend.position = "bottom")
        
        ggsave(paste(plots_dir, "nf_vs_standard_by_class.png", sep="/"), p13, width = 8, height = 6, dpi = 300)
      }
    }, error = function(e) {
      cat("[WARNING] Asset class comparison sheet not available\n")
    })
  }, error = function(e) {
    cat("[WARNING] Comparison file not fully available:", e$message, "\n")
  })
}
}, error = function(e) {
  cat("[WARNING] Some visualizations failed:", e$message, "\n")
})

cat("\n[OK] Visualizations done. Building dashboard HTML.\n")

# Build professional HTML dashboard with per-asset, per-model tables (no aggregation)
dir.create("results", showWarnings = FALSE)
html_file <- "results/dashboard_visualizations.html"
plots_base <- "dashboard_plots"
escape <- function(x) gsub("&", "&amp;", gsub("<", "&lt;", gsub(">", "&gt;", gsub('"', "&quot;", as.character(x)))))
df_to_table <- function(df, cap = NULL) {
  if (!is.data.frame(df) || nrow(df) == 0) return("")
  nr <- nrow(df)
  nc <- ncol(df)
  hdr <- paste0("<tr>", paste0("<th>", escape(names(df)), "</th>", collapse = ""), "</tr>")
  rows <- vapply(seq_len(nr), function(i) {
    paste0("<tr>", paste0("<td>", escape(df[i, ]), "</td>", collapse = ""), "</tr>")
  }, character(1))
  paste0(
    if (!is.null(cap)) paste0("<caption>", escape(cap), "</caption>") else "",
    "<thead>", hdr, "</thead><tbody>", paste(rows, collapse = ""), "</tbody>"
  )
}
format_num <- function(x) if (is.numeric(x)) round(x, 6) else x

sections <- character(0)

# 1. Forecast accuracy (chronological) — per asset, per model
if (exists("chrono_results") && is.data.frame(chrono_results) && nrow(chrono_results) > 0) {
  cols <- intersect(c("Asset", "Model", "MSE", "MAE", "AIC", "LogLikelihood"), names(chrono_results))
  if (length(cols) >= 3) {
    tbl_df <- chrono_results[, cols]
    for (j in which(sapply(tbl_df, is.numeric))) tbl_df[[j]] <- format_num(tbl_df[[j]])
    sections <- c(sections, paste0(
      '<section class="card"><h2>1. Forecast accuracy (chronological split)</h2>',
      '<p>MSE and MAE by asset and model. Lower MSE/MAE is better.</p>',
      '<div class="table-wrap"><table>',
      df_to_table(tbl_df),
      '</table></div></section>'
    ))
  }
}

# 2. Distributional fit — per model (and asset if present)
if (exists("dist_metrics") && is.data.frame(dist_metrics) && nrow(dist_metrics) > 0) {
  cols <- intersect(c("Model", "Asset", "KS_distance", "Wasserstein_distance", "Tail_index", "Mean_KS", "Mean_Wasserstein", "Mean_Tail_Index_Std", "Mean_Tail_Index_NF"), names(dist_metrics))
  if (length(cols) >= 2) {
    tbl_df <- dist_metrics[, cols]
    for (j in which(sapply(tbl_df, is.numeric))) tbl_df[[j]] <- format_num(tbl_df[[j]])
    sections <- c(sections, paste0(
      '<section class="card"><h2>2. Distributional fit</h2>',
      '<p>Residual distribution metrics by model (and asset where available). Lower KS/Wasserstein = better fit.</p>',
      '<div class="table-wrap"><table>',
      df_to_table(tbl_df),
      '</table></div></section>'
    ))
  }
}

# 3. VaR backtesting — per model, per asset where available
if (exists("var_results") && is.data.frame(var_results) && nrow(var_results) > 0) {
  cols <- intersect(c("Model", "Asset", "Confidence_Level", "Exceedance_Rate", "Expected_Rate", "Kupiec_pvalue"), names(var_results))
  if (length(cols) >= 2) {
    tbl_df <- var_results[, cols]
    for (j in which(sapply(tbl_df, is.numeric))) tbl_df[[j]] <- format_num(tbl_df[[j]])
    sections <- c(sections, paste0(
      '<section class="card"><h2>3. VaR backtesting</h2>',
      '<p>Exceedance rates and Kupiec test p-values by model and asset. Exceedance rate should be close to expected.</p>',
      '<div class="table-wrap"><table>',
      df_to_table(tbl_df),
      '</table></div></section>'
    ))
  }
}

# 4. Stress testing — per scenario, per asset where available
if (exists("stress_results") && is.data.frame(stress_results) && nrow(stress_results) > 0) {
  cols <- intersect(c("Scenario_Type", "Scenario_Name", "Asset", "Volatility", "Max_Drawdown", "Volatility_Increase_Pct"), names(stress_results))
  if (length(cols) >= 2) {
    tbl_df <- stress_results[, cols]
    for (j in which(sapply(tbl_df, is.numeric))) tbl_df[[j]] <- format_num(tbl_df[[j]])
    sections <- c(sections, paste0(
      '<section class="card"><h2>4. Stress testing</h2>',
      '<p>Volatility and drawdown under historical and hypothetical stress scenarios.</p>',
      '<div class="table-wrap"><table>',
      df_to_table(tbl_df),
      '</table></div></section>'
    ))
  }
}

# Fallback: load from dissertation_tables CSVs when Excel not available
dt_dir <- "results/dissertation_tables"
if (length(sections) == 0 && dir.exists(dt_dir)) {
  tryCatch({
    if (file.exists(file.path(dt_dir, "nf_vs_standard_by_model.csv"))) {
      d <- read.csv(file.path(dt_dir, "nf_vs_standard_by_model.csv"), stringsAsFactors = FALSE)
      for (j in which(sapply(d, is.numeric))) d[[j]] <- format_num(d[[j]])
      sections <- c(sections, paste0(
        '<section class="card"><h2>1. NF vs standard GARCH by model</h2>',
        '<p>Mean MSE and MAE by model (no aggregation across models).</p>',
        '<div class="table-wrap"><table>', df_to_table(d), '</table></div></section>'
      ))
    }
    if (file.exists(file.path(dt_dir, "distributional_metrics_by_model.csv"))) {
      d <- read.csv(file.path(dt_dir, "distributional_metrics_by_model.csv"), stringsAsFactors = FALSE)
      for (j in which(sapply(d, is.numeric))) d[[j]] <- format_num(d[[j]])
      sections <- c(sections, paste0(
        '<section class="card"><h2>2. Distributional fit by model</h2>',
        '<p>KS, Wasserstein, tail index by model.</p>',
        '<div class="table-wrap"><table>', df_to_table(d), '</table></div></section>'
      ))
    }
    if (file.exists(file.path(dt_dir, "var_backtesting_by_model.csv"))) {
      d <- read.csv(file.path(dt_dir, "var_backtesting_by_model.csv"), stringsAsFactors = FALSE)
      for (j in which(sapply(d, is.numeric))) d[[j]] <- format_num(d[[j]])
      sections <- c(sections, paste0(
        '<section class="card"><h2>3. VaR backtesting by model</h2>',
        '<p>Exceedance rates and test p-values by model and confidence level.</p>',
        '<div class="table-wrap"><table>', df_to_table(d), '</table></div></section>'
      ))
    }
    if (file.exists(file.path(dt_dir, "stress_testing_summary.csv"))) {
      d <- read.csv(file.path(dt_dir, "stress_testing_summary.csv"), stringsAsFactors = FALSE)
      for (j in which(sapply(d, is.numeric))) d[[j]] <- format_num(d[[j]])
      sections <- c(sections, paste0(
        '<section class="card"><h2>4. Stress testing</h2>',
        '<p>Volatility and drawdown by scenario.</p>',
        '<div class="table-wrap"><table>', df_to_table(d), '</table></div></section>'
      ))
    }
  }, error = function(e) cat("[WARNING] CSV fallback:", e$message, "\n"))
}

# 5. Visualizations
pngs <- list.files(plots_dir, pattern = "\\.png$", full.names = FALSE)
if (length(pngs) > 0) {
  img_lines <- paste0(
    '<figure><img src="', plots_base, "/", pngs, '" alt="', gsub("\\.png$", "", pngs), '" /></figure>',
    collapse = "\n"
  )
  sections <- c(sections, paste0(
    '<section class="card"><h2>Visualizations</h2>',
    '<div class="fig-grid">', img_lines, '</div></section>'
  ))
}

css <- '<style>
* { box-sizing: border-box; }
body { font-family: Georgia, "Times New Roman", serif; margin: 0; padding: 0; background: #1e293b; color: #e2e8f0; line-height: 1.6; }
.container { max-width: 1100px; margin: 0 auto; padding: 2rem; }
h1 { font-size: 1.85rem; font-weight: 700; margin-bottom: 0.25rem; color: #f8fafc; letter-spacing: -0.02em; }
.subtitle { color: #94a3b8; margin-bottom: 2rem; font-size: 0.95rem; }
.card { background: #334155; border-radius: 10px; padding: 1.5rem 1.75rem; margin-bottom: 1.5rem; box-shadow: 0 4px 6px rgba(0,0,0,0.2); border: 1px solid #475569; }
.card h2 { font-size: 1.1rem; font-weight: 600; margin-top: 0; margin-bottom: 0.75rem; color: #f1f5f9; }
.card p { margin: 0 0 1rem 0; color: #cbd5e1; font-size: 0.9rem; }
.table-wrap { overflow-x: auto; }
table { width: 100%; border-collapse: collapse; font-size: 0.85rem; }
th, td { padding: 0.55rem 0.75rem; text-align: left; border-bottom: 1px solid #475569; }
th { background: #475569; font-weight: 600; color: #f8fafc; }
tr:hover { background: #3f4a5c; }
.fig-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 1rem; }
.fig-grid figure { margin: 0; }
.fig-grid img { width: 100%; height: auto; border-radius: 6px; border: 1px solid #475569; }
a { color: #7dd3fc; }
</style>'

intro <- '<div class="container">
<h1>NF-GARCH research dashboard</h1>
<p class="subtitle">Chronological split results: forecast accuracy, distributional fit, VaR backtesting, and stress tests. Tables show per-asset, per-model results (no aggregation).</p>'

body_sections <- if (length(sections) > 0) paste(sections, collapse = "") else '<section class="card"><p>No result data found. Run the pipeline first.</p></section>'
html_content <- paste0(
  '<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1">',
  '<title>NF-GARCH dashboard</title>', css, '</head><body>',
  intro, body_sections, '<p class="subtitle" style="margin-top:2rem;">Excel: <a href="consolidated/Final_Dashboard.xlsx">Final_Dashboard.xlsx</a></p></div></body></html>'
)
writeLines(html_content, html_file)
cat("Dashboard HTML written to:", html_file, "\n")

cat("=== VISUALIZATION GENERATION COMPLETE ===\n")

