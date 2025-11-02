#!/usr/bin/env Rscript
# Generate Visualizations for HTML Dashboard
# Creates plots for all evaluation metrics and saves them as PNG files

library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(stringr)

cat("=== GENERATING DASHBOARD VISUALIZATIONS ===\n\n")

# Create output directory for plots
plots_dir <- "results/dashboard_plots"
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
}

# =============================================================================
# 1. Model Performance Comparison
# =============================================================================

cat("1. Creating model performance plots...\n")

# Load NF-GARCH results
results_file <- "results/consolidated/NF_GARCH_Results_manual.xlsx"
if (file.exists(results_file)) {
  chrono_results <- read.xlsx(results_file, sheet = "Chrono_Split_NF_GARCH")
  tscv_results <- read.xlsx(results_file, sheet = "TS_CV_NF_GARCH")
  
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
    
    ggsave(file.path(plots_dir, "mse_by_model_chrono.png"), p1, width = 10, height = 6, dpi = 300)
    
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
      
      ggsave(file.path(plots_dir, "aic_by_model.png"), p2, width = 10, height = 6, dpi = 300)
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
    
    ggsave(file.path(plots_dir, "ks_distance_by_model.png"), p3, width = 10, height = 6, dpi = 300)
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
    
    ggsave(file.path(plots_dir, "wasserstein_distance_by_model.png"), p4, width = 10, height = 6, dpi = 300)
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
    
    ggsave(file.path(plots_dir, "tail_index_by_model.png"), p5, width = 10, height = 6, dpi = 300)
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
    
    ggsave(file.path(plots_dir, "volatility_clustering_by_class.png"), p6, width = 8, height = 6, dpi = 300)
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
    
    ggsave(file.path(plots_dir, "leverage_effect_by_class.png"), p7, width = 8, height = 6, dpi = 300)
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
    
    ggsave(file.path(plots_dir, "var_exceedance_rates.png"), p8, width = 10, height = 6, dpi = 300)
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
    
    ggsave(file.path(plots_dir, "kupiec_pvalues.png"), p9, width = 10, height = 6, dpi = 300)
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
    
    ggsave(file.path(plots_dir, "historical_crisis_volatility.png"), p10, width = 8, height = 6, dpi = 300)
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
    
    ggsave(file.path(plots_dir, "hypothetical_shock_impact.png"), p11, width = 10, height = 6, dpi = 300)
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
      
      ggsave(file.path(plots_dir, "nf_vs_standard_mse.png"), p12, width = 10, height = 6, dpi = 300)
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
        
        ggsave(file.path(plots_dir, "nf_vs_standard_by_class.png"), p13, width = 8, height = 6, dpi = 300)
      }
    }, error = function(e) {
      cat("[WARNING] Asset class comparison sheet not available\n")
    })
  }, error = function(e) {
    cat("[WARNING] Comparison file not fully available:", e$message, "\n")
  })
}

cat("\n[OK] All visualizations generated and saved to:", plots_dir, "\n")
cat("=== VISUALIZATION GENERATION COMPLETE ===\n")

