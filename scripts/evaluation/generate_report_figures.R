#!/usr/bin/env Rscript
# Generate Report Figures for Dissertation
# Creates: Fig-R1, Fig-R2/R3, Fig-R4/R5, Fig-R7, Fig-R8

library(ggplot2)
library(gridExtra)
library(openxlsx)
library(dplyr)
library(tidyr)
library(xts)

cat("=== GENERATING REPORT FIGURES ===\n\n")

# Create output directory
fig_dir <- "results/figures"
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

# =============================================================================
# Fig-R1: ACF/PACF of Squared Returns (Stylized Facts)
# =============================================================================

cat("1. Generating Fig-R1: ACF/PACF of squared returns...\n")

# Load raw price data
raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1)
raw_price_data$Date <- as.Date(rownames(raw_price_data))

# Calculate returns for a representative asset (EURUSD for FX, NVDA for equity)
fx_asset <- "EURUSD"
equity_asset <- "NVDA"

if (fx_asset %in% names(raw_price_data) && equity_asset %in% names(raw_price_data)) {
  # FX returns
  fx_prices <- raw_price_data[[fx_asset]]
  fx_returns <- diff(log(fx_prices))[!is.na(diff(log(fx_prices)))]
  fx_squared <- fx_returns^2
  
  # Equity returns
  eq_prices <- raw_price_data[[equity_asset]]
  eq_returns <- diff(log(eq_prices))[!is.na(diff(log(eq_prices)))]
  eq_squared <- eq_returns^2
  
  # Calculate ACF and PACF
  fx_acf <- acf(fx_squared, lag.max = 40, plot = FALSE)
  fx_pacf <- pacf(fx_squared, lag.max = 40, plot = FALSE)
  
  eq_acf <- acf(eq_squared, lag.max = 40, plot = FALSE)
  eq_pacf <- pacf(eq_squared, lag.max = 40, plot = FALSE)
  
  # Create plots
  # FX ACF/PACF
  fx_acf_df <- data.frame(Lag = 0:length(fx_acf$acf[-1]), ACF = fx_acf$acf)
  fx_pacf_df <- data.frame(Lag = 1:length(fx_pacf$acf), PACF = fx_pacf$acf)
  
  p1a <- ggplot(fx_acf_df, aes(x = Lag, y = ACF)) +
    geom_col(width = 0.8, fill = "steelblue", alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    geom_hline(yintercept = c(-1.96, 1.96) / sqrt(length(fx_squared)), linetype = "dashed", color = "red", alpha = 0.7) +
    labs(title = paste("ACF of Squared Returns:", fx_asset), 
         x = "Lag", y = "ACF") +
    theme_minimal()
  
  p1b <- ggplot(fx_pacf_df, aes(x = Lag, y = PACF)) +
    geom_col(width = 0.8, fill = "steelblue", alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    geom_hline(yintercept = c(-1.96, 1.96) / sqrt(length(fx_squared)), linetype = "dashed", color = "red", alpha = 0.7) +
    labs(title = paste("PACF of Squared Returns:", fx_asset), 
         x = "Lag", y = "PACF") +
    theme_minimal()
  
  # Equity ACF/PACF
  eq_acf_df <- data.frame(Lag = 0:length(eq_acf$acf[-1]), ACF = eq_acf$acf)
  eq_pacf_df <- data.frame(Lag = 1:length(eq_pacf$acf), PACF = eq_pacf$acf)
  
  p1c <- ggplot(eq_acf_df, aes(x = Lag, y = ACF)) +
    geom_col(width = 0.8, fill = "darkgreen", alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    geom_hline(yintercept = c(-1.96, 1.96) / sqrt(length(eq_squared)), linetype = "dashed", color = "red", alpha = 0.7) +
    labs(title = paste("ACF of Squared Returns:", equity_asset), 
         x = "Lag", y = "ACF") +
    theme_minimal()
  
  p1d <- ggplot(eq_pacf_df, aes(x = Lag, y = PACF)) +
    geom_col(width = 0.8, fill = "darkgreen", alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    geom_hline(yintercept = c(-1.96, 1.96) / sqrt(length(eq_squared)), linetype = "dashed", color = "red", alpha = 0.7) +
    labs(title = paste("PACF of Squared Returns:", equity_asset), 
         x = "Lag", y = "PACF") +
    theme_minimal()
  
  # Combine plots
  p1_combined <- grid.arrange(p1a, p1b, p1c, p1d, nrow = 2, ncol = 2)
  
  ggsave(file.path(fig_dir, "Fig-R1_stylisedfacts_acf_pacf.png"), 
         p1_combined, width = 14, height = 10, dpi = 300)
  
  cat("  [OK] Fig-R1 saved\n")
}

# =============================================================================
# Fig-R2/R3: Histogram + QQ Plot for Residuals (Equity and FX)
# =============================================================================

cat("\n2. Generating Fig-R2/R3: Residual histograms and QQ plots...\n")

# Load residual data
equity_example <- "NVDA"
fx_example <- "EURUSD"
model_example <- "TGARCH"  # Use TGARCH as representative model

# Load equity residuals
# Check multiple possible paths and naming conventions
equity_resid_file <- NULL
fx_resid_file <- NULL

possible_paths <- c(
  file.path("outputs", "manual", "residuals_by_model", model_example, paste0(equity_example, "_Manual_Optimized_residuals.csv")),
  file.path("outputs", "manual", "residuals_by_model", model_example, paste0("equity_", equity_example, "_Chrono_Split_residuals.csv")),
  file.path("residuals_by_model", model_example, paste0("equity_", equity_example, "_Chrono_Split_residuals.csv")),
  file.path("data", "residuals_by_model", model_example, paste0("equity_", equity_example, "_Chrono_Split_residuals.csv"))
)

for (path in possible_paths) {
  if (file.exists(path)) {
    equity_resid_file <- path
    break
  }
}

possible_paths_fx <- c(
  file.path("outputs", "manual", "residuals_by_model", model_example, paste0(fx_example, "_Manual_Optimized_residuals.csv")),
  file.path("outputs", "manual", "residuals_by_model", model_example, paste0("fx_", fx_example, "_Chrono_Split_residuals.csv")),
  file.path("residuals_by_model", model_example, paste0("fx_", fx_example, "_Chrono_Split_residuals.csv")),
  file.path("data", "residuals_by_model", model_example, paste0("fx_", fx_example, "_Chrono_Split_residuals.csv"))
)

for (path in possible_paths_fx) {
  if (file.exists(path)) {
    fx_resid_file <- path
    break
  }
}

if (!is.null(equity_resid_file) && file.exists(equity_resid_file)) {
  equity_resid_data <- read.csv(equity_resid_file)
  equity_resid <- if ("residual" %in% names(equity_resid_data)) {
    equity_resid_data$residual
  } else {
    equity_resid_data[[1]]
  }
  equity_resid <- as.numeric(equity_resid[!is.na(equity_resid)])
  
  # Equity histogram
  p2a <- ggplot(data.frame(residual = equity_resid), aes(x = residual)) +
    geom_histogram(aes(y = ..density..), bins = 50, fill = "steelblue", alpha = 0.7, color = "black") +
    stat_function(fun = dnorm, args = list(mean = mean(equity_resid), sd = sd(equity_resid)), 
                  color = "red", size = 1, linetype = "dashed") +
    labs(title = paste("Residual Histogram:", equity_example, model_example),
         x = "Standardized Residuals", y = "Density") +
    theme_minimal()
  
  # Equity QQ plot
  p2b <- ggplot(data.frame(residual = equity_resid), aes(sample = residual)) +
    stat_qq(alpha = 0.6, color = "steelblue") +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(title = paste("Q-Q Plot:", equity_example, model_example),
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  # Combine equity plots
  p2 <- grid.arrange(p2a, p2b, nrow = 1, ncol = 2)
  ggsave(file.path(fig_dir, "Fig-R2_hist_qq_equity.png"), 
         p2, width = 12, height = 6, dpi = 300)
  cat("  [OK] Fig-R2 saved\n")
}

if (!is.null(fx_resid_file) && file.exists(fx_resid_file)) {
  fx_resid_data <- read.csv(fx_resid_file)
  fx_resid <- if ("residual" %in% names(fx_resid_data)) {
    fx_resid_data$residual
  } else {
    fx_resid_data[[1]]
  }
  fx_resid <- as.numeric(fx_resid[!is.na(fx_resid)])
  
  # FX histogram
  p3a <- ggplot(data.frame(residual = fx_resid), aes(x = residual)) +
    geom_histogram(aes(y = ..density..), bins = 50, fill = "darkgreen", alpha = 0.7, color = "black") +
    stat_function(fun = dnorm, args = list(mean = mean(fx_resid), sd = sd(fx_resid)), 
                  color = "red", size = 1, linetype = "dashed") +
    labs(title = paste("Residual Histogram:", fx_example, model_example),
         x = "Standardized Residuals", y = "Density") +
    theme_minimal()
  
  # FX QQ plot
  p3b <- ggplot(data.frame(residual = fx_resid), aes(sample = residual)) +
    stat_qq(alpha = 0.6, color = "darkgreen") +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(title = paste("Q-Q Plot:", fx_example, model_example),
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  # Combine FX plots
  p3 <- grid.arrange(p3a, p3b, nrow = 1, ncol = 2)
  ggsave(file.path(fig_dir, "Fig-R3_hist_qq_fx.png"), 
         p3, width = 12, height = 6, dpi = 300)
  cat("  [OK] Fig-R3 saved\n")
}

# =============================================================================
# Fig-R4/R5: NF vs GARCH QQ Comparison (Equity and FX)
# =============================================================================

cat("\n3. Generating Fig-R4/R5: NF vs GARCH QQ comparison...\n")

# Load NF residuals
nf_resid_dir <- "outputs/manual/nf_models"
nf_files <- list.files(nf_resid_dir, pattern = "*_synthetic_residuals.csv", full.names = TRUE)

nf_residuals_map <- list()
for (f in nf_files) {
  fname <- basename(f)
  fname_clean <- stringr::str_replace(fname, "_synthetic_residuals\\.csv$", "")
  parts <- strsplit(fname_clean, "_")[[1]]
  
  if (length(parts) >= 2) {
    model_name <- parts[1]
    asset_name <- parts[2]
    key <- paste(model_name, asset_name, sep = "_")
    
    tryCatch({
      nf_data <- read.csv(f)
      nf_resid <- if ("residual" %in% names(nf_data)) {
        nf_data$residual
      } else {
        nf_data[[1]]
      }
      nf_resid <- as.numeric(nf_resid[!is.na(nf_resid)])
      if (length(nf_resid) > 0) {
        # Standardize
        nf_resid <- (nf_resid - mean(nf_resid)) / sd(nf_resid)
        nf_residuals_map[[key]] <- nf_resid
      }
    }, error = function(e) {
      # Skip
    })
  }
}

# Equity comparison (NF vs GARCH)
eq_key_std <- paste(model_example, equity_example, sep = "_")
eq_key_nf <- paste(model_example, equity_example, sep = "_")

if (!is.null(equity_resid_file) && file.exists(equity_resid_file) && eq_key_nf %in% names(nf_residuals_map)) {
  std_resid <- equity_resid
  nf_resid <- nf_residuals_map[[eq_key_nf]]
  
  # Standardize both
  std_resid <- (std_resid - mean(std_resid)) / sd(std_resid)
  nf_resid <- (nf_resid - mean(nf_resid)) / sd(nf_resid)
  
  # Create comparison data
  min_len <- min(length(std_resid), length(nf_resid))
  std_resid <- std_resid[1:min_len]
  nf_resid <- nf_resid[1:min_len]
  
  # Standard GARCH QQ
  p4a <- ggplot(data.frame(residual = std_resid), aes(sample = residual)) +
    stat_qq(alpha = 0.6, color = "steelblue") +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(title = paste("Standard GARCH:", equity_example, model_example),
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  # NF-GARCH QQ
  p4b <- ggplot(data.frame(residual = nf_resid), aes(sample = residual)) +
    stat_qq(alpha = 0.6, color = "darkgreen") +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(title = paste("NF-GARCH:", equity_example, model_example),
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  # Combine
  p4 <- grid.arrange(p4a, p4b, nrow = 1, ncol = 2)
  ggsave(file.path(fig_dir, "Fig-R4_nf_vs_garch_resqq_equity.png"), 
         p4, width = 12, height = 6, dpi = 300)
  cat("  [OK] Fig-R4 saved\n")
}

# FX comparison (NF vs GARCH)
fx_key_std <- paste(model_example, fx_example, sep = "_")
fx_key_nf <- paste(model_example, fx_example, sep = "_")

if (!is.null(fx_resid_file) && file.exists(fx_resid_file) && fx_key_nf %in% names(nf_residuals_map)) {
  std_resid <- fx_resid
  nf_resid <- nf_residuals_map[[fx_key_nf]]
  
  # Standardize both
  std_resid <- (std_resid - mean(std_resid)) / sd(std_resid)
  nf_resid <- (nf_resid - mean(nf_resid)) / sd(nf_resid)
  
  # Create comparison data
  min_len <- min(length(std_resid), length(nf_resid))
  std_resid <- std_resid[1:min_len]
  nf_resid <- nf_resid[1:min_len]
  
  # Standard GARCH QQ
  p5a <- ggplot(data.frame(residual = std_resid), aes(sample = residual)) +
    stat_qq(alpha = 0.6, color = "steelblue") +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(title = paste("Standard GARCH:", fx_example, model_example),
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  # NF-GARCH QQ
  p5b <- ggplot(data.frame(residual = nf_resid), aes(sample = residual)) +
    stat_qq(alpha = 0.6, color = "darkgreen") +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(title = paste("NF-GARCH:", fx_example, model_example),
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  # Combine
  p5 <- grid.arrange(p5a, p5b, nrow = 1, ncol = 2)
  ggsave(file.path(fig_dir, "Fig-R5_nf_vs_garch_resqq_fx.png"), 
         p5, width = 12, height = 6, dpi = 300)
  cat("  [OK] Fig-R5 saved\n")
}

# =============================================================================
# Fig-R7: Stress Test - GFC vs COVID Error Comparison
# =============================================================================

cat("\n4. Generating Fig-R7: Stress test GFC vs COVID error comparison...\n")

stress_file <- "results/consolidated/Stress_Testing.xlsx"
if (file.exists(stress_file)) {
  tryCatch({
    stress_forecast <- read.xlsx(stress_file, sheet = "Forecast_Under_Stress")
    
    # Filter for historical crises
    crisis_data <- stress_forecast %>%
      filter(Scenario_Type == "Historical_Crisis") %>%
      filter(Scenario_Name %in% c("GFC_2008", "COVID_2020"))
    
    if (nrow(crisis_data) > 0) {
      # Aggregate by crisis and model
      crisis_summary <- crisis_data %>%
        group_by(Scenario_Name, Model) %>%
        summarise(
          NF_GARCH_MSE = mean(NF_GARCH_MSE, na.rm = TRUE),
          NF_GARCH_MAE = mean(NF_GARCH_MAE, na.rm = TRUE),
          Standard_GARCH_MSE = mean(Standard_GARCH_MSE, na.rm = TRUE),
          Standard_GARCH_MAE = mean(Standard_GARCH_MAE, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        pivot_longer(cols = c(NF_GARCH_MSE, NF_GARCH_MAE, Standard_GARCH_MSE, Standard_GARCH_MAE),
                    names_to = "Metric_Type", values_to = "Value") %>%
        separate(Metric_Type, into = c("Model_Type", "Metric"), sep = "_", extra = "merge") %>%
        mutate(Crisis = ifelse(Scenario_Name == "GFC_2008", "GFC 2008", "COVID-19 2020")) %>%
        filter(!is.na(Value) & is.finite(Value))
      
      if (nrow(crisis_summary) > 0) {
        # MSE comparison
        mse_data <- crisis_summary %>% filter(Metric == "MSE")
        
        if (nrow(mse_data) > 0) {
          # Filter out infinite values and ensure proper scaling
          mse_data <- mse_data %>% filter(is.finite(Value) & Value > 0)
          
          if (nrow(mse_data) > 0) {
            p7a <- mse_data %>%
              ggplot(aes(x = Crisis, y = Value, fill = Model_Type)) +
              geom_bar(stat = "identity", position = "dodge", alpha = 0.8, color = "black", linewidth = 0.3) +
              scale_y_log10() +
              labs(title = "Forecast MSE: GFC vs COVID-19 Crisis Periods",
                   x = "Crisis Period", y = "Mean Squared Error (log scale)",
                   fill = "Model Type") +
              theme_minimal(base_size = 12) +
              theme(legend.position = "bottom",
                    plot.background = element_rect(fill = "white", color = NA),
                    panel.background = element_rect(fill = "white"),
                    panel.grid.major = element_line(color = "grey90"),
                    panel.grid.minor = element_line(color = "grey95"))
            
            # Facet only if multiple models
            if (length(unique(mse_data$Model)) > 1) {
              p7a <- p7a + facet_wrap(~ Model, scales = "free_y", ncol = 2)
            }
          } else {
            p7a <- ggplot() + 
              annotate("text", x = 0.5, y = 0.5, label = "No MSE data available", size = 6, color = "black") +
              theme_void()
          }
        } else {
          p7a <- ggplot() + 
            annotate("text", x = 0.5, y = 0.5, label = "No MSE data available", size = 6, color = "black") +
            theme_void()
        }
        
        # MAE comparison
        mae_data <- crisis_summary %>% filter(Metric == "MAE")
        
        if (nrow(mae_data) > 0) {
          # Filter out infinite values
          mae_data <- mae_data %>% filter(is.finite(Value) & Value > 0)
          
          if (nrow(mae_data) > 0) {
            p7b <- mae_data %>%
              ggplot(aes(x = Crisis, y = Value, fill = Model_Type)) +
              geom_bar(stat = "identity", position = "dodge", alpha = 0.8, color = "black", linewidth = 0.3) +
              labs(title = "Forecast MAE: GFC vs COVID-19 Crisis Periods",
                   x = "Crisis Period", y = "Mean Absolute Error",
                   fill = "Model Type") +
              theme_minimal(base_size = 12) +
              theme(legend.position = "bottom",
                    plot.background = element_rect(fill = "white", color = NA),
                    panel.background = element_rect(fill = "white"),
                    panel.grid.major = element_line(color = "grey90"),
                    panel.grid.minor = element_line(color = "grey95"))
            
            # Facet only if multiple models
            if (length(unique(mae_data$Model)) > 1) {
              p7b <- p7b + facet_wrap(~ Model, scales = "free_y", ncol = 2)
            }
          } else {
            p7b <- ggplot() + 
              annotate("text", x = 0.5, y = 0.5, label = "No MAE data available", size = 6, color = "black") +
              theme_void()
          }
        } else {
          p7b <- ggplot() + 
            annotate("text", x = 0.5, y = 0.5, label = "No MAE data available", size = 6, color = "black") +
            theme_void()
        }
        
        # Combine plots with explicit background
        p7 <- grid.arrange(p7a, p7b, nrow = 2, ncol = 1)
        
        # Save with explicit background
        ggsave(file.path(fig_dir, "Fig-R7_stress_gfc_vs_covid.png"), 
               p7, width = 14, height = 12, dpi = 300, bg = "white")
        cat("  [OK] Fig-R7 saved\n")
      }
    }
  }, error = function(e) {
    cat("  [WARNING] Could not generate Fig-R7:", e$message, "\n")
  })
}

# =============================================================================
# Fig-R8: NF Win Rate Bar Chart
# =============================================================================

cat("\n5. Generating Fig-R8: NF win rate bar chart...\n")

comparison_file <- "results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx"
if (file.exists(comparison_file)) {
  tryCatch({
    win_rate <- read.xlsx(comparison_file, sheet = "Win_Rate_Analysis")
    
    if (nrow(win_rate) > 0 && "win_rate" %in% names(win_rate)) {
      p8 <- win_rate %>%
        ggplot(aes(x = reorder(Model, win_rate), y = win_rate, fill = Model)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        geom_hline(yintercept = 50, linetype = "dashed", color = "red", size = 1) +
        labs(title = "NF-GARCH Win Rate by Model",
             subtitle = "Percentage of asset-model combinations where NF-GARCH MSE < Standard GARCH MSE",
             x = "Model", y = "Win Rate (%)",
             caption = "Red dashed line indicates 50% (equal performance)") +
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
        theme_minimal() +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
        coord_flip()
      
      ggsave(file.path(fig_dir, "Fig-R8_nf_winrate_bars.png"), 
             p8, width = 10, height = 6, dpi = 300)
      cat("  [OK] Fig-R8 saved\n")
    }
  }, error = function(e) {
    cat("  [WARNING] Could not generate Fig-R8:", e$message, "\n")
  })
}

cat("\n=== FIGURE GENERATION COMPLETE ===\n")
cat("Figures saved to:", fig_dir, "\n")

