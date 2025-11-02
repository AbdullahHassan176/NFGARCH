#!/usr/bin/env Rscript
# Enhanced EDA Summary Statistics Script
# Performs comprehensive exploratory data analysis including stylized facts testing
# for financial time series data (FX and Equity returns)

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(scales)
library(viridis)
library(tseries)
library(forecast)
library(urca)
library(moments)
library(FinTS)
library(rugarch)
library(xts)
library(zoo)

# Source utilities
source("scripts/utils/conflict_resolution.R")
source("scripts/utils/enhanced_plotting.R")
source("scripts/utils/safety_functions.R")

# Initialize pipeline
initialize_pipeline()

cat("=== RUNNING EXPLORATORY DATA ANALYSIS ===\n")

# Load data
cat("Loading data...\n")
data_file <- "data/processed/raw (FX + EQ).csv"

if (!file.exists(data_file)) {
  stop("Data file not found: ", data_file)
}

data <- read.csv(data_file, row.names = 1)
data$Date <- as.Date(rownames(data))

cat("Data loaded successfully\n")
cat("  Rows:", nrow(data), "\n")
cat("  Columns:", ncol(data), "\n")

# Separate FX and Equity data
fx_cols <- c("EURUSD", "GBPUSD", "GBPCNY", "USDZAR", "GBPZAR", "EURZAR")
equity_cols <- c("NVDA", "MSFT", "PG", "CAT", "WMT", "AMZN")

fx_data <- data[, fx_cols, drop = FALSE]
equity_data <- data[, equity_cols, drop = FALSE]

# Calculate returns
fx_returns <- fx_data %>%
  mutate_all(~c(NA, diff(log(.)))) %>%
  filter(!is.na(.[,1]))

equity_returns <- equity_data %>%
  mutate_all(~c(NA, diff(log(.)))) %>%
  filter(!is.na(.[,1]))

# =============================================================================
# STYLIZED FACTS TESTING FUNCTIONS
# =============================================================================

# Function to perform stationarity tests
perform_stationarity_tests <- function(returns, asset_name) {
  cat("  Running stationarity tests for", asset_name, "\n")
  
  # Remove any remaining NA values
  clean_returns <- na.omit(returns)
  
  # ADF Test
  adf_test <- tryCatch({
    adf_result <- adf.test(clean_returns)
    list(
      statistic = adf_result$statistic,
      p_value = adf_result$p.value,
      critical_values = adf_result$critical,
      test_type = "ADF"
    )
  }, error = function(e) {
    list(statistic = NA, p_value = NA, critical_values = NA, test_type = "ADF", error = e$message)
  })
  
  # KPSS Test
  kpss_test <- tryCatch({
    kpss_result <- kpss.test(clean_returns)
    list(
      statistic = kpss_result$statistic,
      p_value = kpss_result$p.value,
      critical_values = kpss_result$critical,
      test_type = "KPSS"
    )
  }, error = function(e) {
    list(statistic = NA, p_value = NA, critical_values = NA, test_type = "KPSS", error = e$message)
  })
  
  return(list(
    asset = asset_name,
    adf = adf_test,
    kpss = kpss_test,
    n_obs = length(clean_returns)
  ))
}

# Function to perform normality and distributional tests
perform_distributional_tests <- function(returns, asset_name) {
  cat("  Running distributional tests for", asset_name, "\n")
  
  clean_returns <- na.omit(returns)
  
  # Jarque-Bera normality test
  jb_test <- tryCatch({
    jb_result <- jarque.bera.test(clean_returns)
    list(
      statistic = jb_result$statistic,
      p_value = jb_result$p.value,
      test_type = "Jarque-Bera"
    )
  }, error = function(e) {
    list(statistic = NA, p_value = NA, test_type = "Jarque-Bera", error = e$message)
  })
  
  # Shapiro-Wilk test (for smaller samples)
  sw_test <- if (length(clean_returns) <= 5000) {
    tryCatch({
      sw_result <- shapiro.test(clean_returns)
      list(
        statistic = sw_result$statistic,
        p_value = sw_result$p.value,
        test_type = "Shapiro-Wilk"
      )
    }, error = function(e) {
      list(statistic = NA, p_value = NA, test_type = "Shapiro-Wilk", error = e$message)
    })
  } else {
    list(statistic = NA, p_value = NA, test_type = "Shapiro-Wilk", note = "Sample too large")
  }
  
  # Kolmogorov-Smirnov test against normal
  ks_test <- tryCatch({
    ks_result <- ks.test(clean_returns, "pnorm", mean = mean(clean_returns), sd = sd(clean_returns))
    list(
      statistic = ks_result$statistic,
      p_value = ks_result$p.value,
      test_type = "Kolmogorov-Smirnov"
    )
  }, error = function(e) {
    list(statistic = NA, p_value = NA, test_type = "Kolmogorov-Smirnov", error = e$message)
  })
  
  # Tail index estimation (Hill estimator)
  tail_index <- tryCatch({
    # Use top 5% of absolute returns for tail estimation
    abs_returns <- abs(clean_returns)
    threshold <- quantile(abs_returns, 0.95)
    tail_data <- abs_returns[abs_returns > threshold]
    
    if (length(tail_data) > 10) {
      # Hill estimator
      sorted_tail <- sort(tail_data, decreasing = TRUE)
      k <- min(50, length(sorted_tail) - 1)  # Use top k observations
      hill_est <- 1 / mean(log(sorted_tail[1:k] / sorted_tail[k+1]))
      list(
        hill_estimator = hill_est,
        threshold = threshold,
        n_tail_obs = length(tail_data),
        k_used = k
      )
    } else {
      list(hill_estimator = NA, threshold = threshold, n_tail_obs = length(tail_data), note = "Insufficient tail data")
    }
  }, error = function(e) {
    list(hill_estimator = NA, error = e$message)
  })
  
  return(list(
    asset = asset_name,
    jarque_bera = jb_test,
    shapiro_wilk = sw_test,
    kolmogorov_smirnov = ks_test,
    tail_index = tail_index,
    n_obs = length(clean_returns)
  ))
}

# Function to perform autocorrelation tests
perform_autocorrelation_tests <- function(returns, asset_name) {
  cat("  Running autocorrelation tests for", asset_name, "\n")
  
  clean_returns <- na.omit(returns)
  squared_returns <- clean_returns^2
  
  # Ljung-Box test for raw returns
  lb_raw <- tryCatch({
    lb_result <- Box.test(clean_returns, lag = 10, type = "Ljung-Box")
    list(
      statistic = lb_result$statistic,
      p_value = lb_result$p.value,
      test_type = "Ljung-Box (raw returns)",
      lags = 10
    )
  }, error = function(e) {
    list(statistic = NA, p_value = NA, test_type = "Ljung-Box (raw returns)", error = e$message)
  })
  
  # Ljung-Box test for squared returns
  lb_squared <- tryCatch({
    lb_result <- Box.test(squared_returns, lag = 10, type = "Ljung-Box")
    list(
      statistic = lb_result$statistic,
      p_value = lb_result$p.value,
      test_type = "Ljung-Box (squared returns)",
      lags = 10
    )
  }, error = function(e) {
    list(statistic = NA, p_value = NA, test_type = "Ljung-Box (squared returns)", error = e$message)
  })
  
  # ARCH-LM test for heteroskedasticity
  arch_lm <- tryCatch({
    arch_result <- ArchTest(clean_returns, lags = 5)
    list(
      statistic = arch_result$statistic,
      p_value = arch_result$p.value,
      test_type = "ARCH-LM",
      lags = 5
    )
  }, error = function(e) {
    list(statistic = NA, p_value = NA, test_type = "ARCH-LM", error = e$message)
  })
  
  # Calculate ACF statistics
  acf_raw <- tryCatch({
    acf_result <- acf(clean_returns, lag.max = 20, plot = FALSE)
    list(
      acf1 = acf_result$acf[2],  # First lag autocorrelation
      acf5 = max(abs(acf_result$acf[2:6])),  # Max absolute ACF in first 5 lags
      acf10 = max(abs(acf_result$acf[2:11]))  # Max absolute ACF in first 10 lags
    )
  }, error = function(e) {
    list(acf1 = NA, acf5 = NA, acf10 = NA, error = e$message)
  })
  
  acf_squared <- tryCatch({
    acf_result <- acf(squared_returns, lag.max = 20, plot = FALSE)
    list(
      acf1 = acf_result$acf[2],  # First lag autocorrelation
      acf5 = max(abs(acf_result$acf[2:6])),  # Max absolute ACF in first 5 lags
      acf10 = max(abs(acf_result$acf[2:11]))  # Max absolute ACF in first 10 lags
    )
  }, error = function(e) {
    list(acf1 = NA, acf5 = NA, acf10 = NA, error = e$message)
  })
  
  return(list(
    asset = asset_name,
    ljung_box_raw = lb_raw,
    ljung_box_squared = lb_squared,
    arch_lm = arch_lm,
    acf_raw = acf_raw,
    acf_squared = acf_squared,
    n_obs = length(clean_returns)
  ))
}

# Function to test leverage effects
perform_leverage_tests <- function(returns, asset_name) {
  cat("  Running leverage effect tests for", asset_name, "\n")
  
  clean_returns <- na.omit(returns)
  n <- length(clean_returns)
  
  # Create lagged returns and squared returns
  returns_lag1 <- c(NA, clean_returns[-n])
  returns_squared <- clean_returns^2
  
  # Remove first observation due to lag
  valid_idx <- !is.na(returns_lag1)
  returns_clean <- clean_returns[valid_idx]
  returns_lag1_clean <- returns_lag1[valid_idx]
  returns_squared_clean <- returns_squared[valid_idx]
  
  # Sign bias test (Engle & Ng, 1993)
  sign_bias <- tryCatch({
    # Create indicator for negative returns
    neg_indicator <- as.numeric(returns_lag1_clean < 0)
    
    # Regress squared returns on lagged negative indicator
    lm_result <- lm(returns_squared_clean ~ neg_indicator)
    
    list(
      coefficient = coef(lm_result)[2],
      p_value = summary(lm_result)$coefficients[2, 4],
      r_squared = summary(lm_result)$r.squared,
      test_type = "Sign Bias"
    )
  }, error = function(e) {
    list(coefficient = NA, p_value = NA, r_squared = NA, test_type = "Sign Bias", error = e$message)
  })
  
  # Asymmetric volatility test
  asym_vol <- tryCatch({
    # Create positive and negative return indicators
    pos_returns <- pmax(returns_lag1_clean, 0)
    neg_returns <- pmax(-returns_lag1_clean, 0)
    
    # Regress squared returns on positive and negative components
    lm_result <- lm(returns_squared_clean ~ pos_returns + neg_returns)
    
    list(
      pos_coefficient = coef(lm_result)[2],
      neg_coefficient = coef(lm_result)[3],
      pos_p_value = summary(lm_result)$coefficients[2, 4],
      neg_p_value = summary(lm_result)$coefficients[3, 4],
      asymmetry_ratio = coef(lm_result)[3] / coef(lm_result)[2],
      r_squared = summary(lm_result)$r.squared,
      test_type = "Asymmetric Volatility"
    )
  }, error = function(e) {
    list(pos_coefficient = NA, neg_coefficient = NA, pos_p_value = NA, neg_p_value = NA, 
         asymmetry_ratio = NA, r_squared = NA, test_type = "Asymmetric Volatility", error = e$message)
  })
  
  # Volatility clustering measure
  vol_clustering <- tryCatch({
    # Calculate rolling volatility (20-day window)
    vol_window <- 20
    rolling_vol <- rollapply(returns_squared_clean, width = vol_window, FUN = mean, fill = NA, align = "right")
    
    # Calculate autocorrelation of volatility
    vol_acf <- acf(na.omit(rolling_vol), lag.max = 10, plot = FALSE)
    
    list(
      vol_acf1 = vol_acf$acf[2],
      vol_acf5 = max(abs(vol_acf$acf[2:6])),
      vol_acf10 = max(abs(vol_acf$acf[2:11])),
      window_size = vol_window,
      test_type = "Volatility Clustering"
    )
  }, error = function(e) {
    list(vol_acf1 = NA, vol_acf5 = NA, vol_acf10 = NA, window_size = 20, 
         test_type = "Volatility Clustering", error = e$message)
  })
  
  return(list(
    asset = asset_name,
    sign_bias = sign_bias,
    asymmetric_volatility = asym_vol,
    volatility_clustering = vol_clustering,
    n_obs = length(returns_clean)
  ))
}

# Create output directories
dir.create("outputs/eda/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/eda/tables", showWarnings = FALSE, recursive = TRUE)

# Generate summary statistics with dual splits
cat("Generating summary statistics with chronological and TS CV splits...\n")

# Function to calculate summary statistics for a given split
calculate_summary_stats <- function(train_data, test_data, split_type) {
  # Combine train and test for overall stats, but also calculate split-specific stats
  all_data <- rbind(train_data, test_data)
  
  summary_stats <- all_data %>%
  summarise_all(list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    skewness = ~skewness(., na.rm = TRUE),
    kurtosis = ~kurtosis(., na.rm = TRUE)
  )) %>%
  gather(key = "stat", value = "value") %>%
  separate(stat, into = c("asset", "statistic"), sep = "_") %>%
  spread(statistic, value)

  # Add split information
  summary_stats$Split_Type <- split_type
  summary_stats$Train_Size <- nrow(train_data)
  summary_stats$Test_Size <- nrow(test_data)
  
  return(summary_stats)
}

# Apply dual split analysis to FX data
fx_analysis_function <- function(train_data, test_data, split_type) {
  return(calculate_summary_stats(train_data, test_data, split_type))
}

# Apply dual split analysis to Equity data  
equity_analysis_function <- function(train_data, test_data, split_type) {
  return(calculate_summary_stats(train_data, test_data, split_type))
}

# Run simplified analysis for FX (chronological split only)
cat("Running chronological split analysis for FX data...\n")
fx_results <- list()
for (asset in fx_cols) {
  asset_data <- fx_returns[, asset, drop = FALSE]
  # Use simple chronological split
  chrono_split <- get_chronological_split(asset_data)
  fx_results[[asset]] <- list(
    chronological = fx_analysis_function(chrono_split$train, chrono_split$test, "chronological")
  )
}

# Run simplified analysis for Equity (chronological split only)
cat("Running chronological split analysis for Equity data...\n")
equity_results <- list()
for (asset in equity_cols) {
  asset_data <- equity_returns[, asset, drop = FALSE]
  # Use simple chronological split
  chrono_split <- get_chronological_split(asset_data)
  equity_results[[asset]] <- list(
    chronological = equity_analysis_function(chrono_split$train, chrono_split$test, "chronological")
  )
}

# Combine all results
all_fx_summaries <- list()
all_equity_summaries <- list()

# Process FX results (chronological only)
for (asset in names(fx_results)) {
  # Chronological results
  chrono_result <- fx_results[[asset]]$chronological
  chrono_result$Asset <- asset
  chrono_result$Asset_Type <- "FX"
  all_fx_summaries[[paste0(asset, "_chronological")]] <- chrono_result
}

# Process Equity results (chronological only)
for (asset in names(equity_results)) {
  # Chronological results
  chrono_result <- equity_results[[asset]]$chronological
  chrono_result$Asset <- asset
  chrono_result$Asset_Type <- "Equity"
  all_equity_summaries[[paste0(asset, "_chronological")]] <- chrono_result
}

# Combine and save results
fx_summary <- do.call(rbind, all_fx_summaries)
equity_summary <- do.call(rbind, all_equity_summaries)

# Save summary tables
write.csv(fx_summary, "outputs/eda/tables/fx_summary_stats_dual_splits.csv", row.names = FALSE)
write.csv(equity_summary, "outputs/eda/tables/equity_summary_stats_dual_splits.csv", row.names = FALSE)

# Generate histograms
cat("Generating histograms...\n")

# FX Histograms
for (asset in fx_cols) {
  # Create data frame for histogram function
  hist_data <- data.frame(Returns = fx_returns[[asset]])
  p <- create_enhanced_histogram(
    hist_data, 
    x_var = "Returns",
    title = paste("Return Distribution -", asset),
    color = "#1f77b4"
  )
  ggsave(paste0("outputs/eda/figures/", asset, "_histogram.png"), p, width = 10, height = 6, dpi = 300)
}

# Equity Histograms
for (asset in equity_cols) {
  # Create data frame for histogram function
  hist_data <- data.frame(Returns = equity_returns[[asset]])
  p <- create_enhanced_histogram(
    hist_data, 
    x_var = "Returns",
    title = paste("Return Distribution -", asset),
    color = "#2ca02c"
  )
  ggsave(paste0("outputs/eda/figures/", asset, "_histogram.png"), p, width = 10, height = 6, dpi = 300)
}

# Generate time series plots
cat("Generating time series plots...\n")

# FX Time Series
fx_returns_long <- fx_returns %>%
  mutate(Date = data$Date[-1]) %>%
  gather(key = "Asset", value = "Returns", -Date)

p <- create_enhanced_timeseries(
  fx_returns_long,
  x_var = "Date",
  y_var = "Returns",
  title = "FX Returns Time Series",
  color = "#1f77b4"
)
ggsave("outputs/eda/figures/fx_returns_timeseries.png", p, width = 12, height = 8, dpi = 300)

# Equity Time Series
equity_returns_long <- equity_returns %>%
  mutate(Date = data$Date[-1]) %>%
  gather(key = "Asset", value = "Returns", -Date)

p <- create_enhanced_timeseries(
  equity_returns_long,
  x_var = "Date",
  y_var = "Returns",
  title = "Equity Returns Time Series",
  color = "#2ca02c"
)
ggsave("outputs/eda/figures/equity_returns_timeseries.png", p, width = 12, height = 8, dpi = 300)

# Generate correlation heatmaps
cat("Generating correlation heatmaps...\n")

# FX Correlation
fx_corr <- cor(fx_returns, use = "complete.obs")
p <- create_enhanced_correlation_heatmap(
  fx_corr,
  title = "FX Returns Correlation Matrix"
)
ggsave("outputs/eda/figures/fx_correlation_heatmap.png", p, width = 10, height = 8, dpi = 300)

# Equity Correlation
equity_corr <- cor(equity_returns, use = "complete.obs")
p <- create_enhanced_correlation_heatmap(
  equity_corr,
  title = "Equity Returns Correlation Matrix"
)
ggsave("outputs/eda/figures/equity_correlation_heatmap.png", p, width = 10, height = 8, dpi = 300)

# Generate volatility clustering plots
cat("Generating volatility clustering plots...\n")

# FX Volatility Clustering
fx_vol <- fx_returns %>%
  mutate_all(~abs(.)) %>%
  mutate(Date = data$Date[-1]) %>%
  gather(key = "Asset", value = "Absolute_Returns", -Date)

p <- create_enhanced_timeseries(
  fx_vol,
  x_var = "Date",
  y_var = "Absolute_Returns",
  title = "FX Volatility Clustering",
  color = "#1f77b4"
)
ggsave("outputs/eda/figures/fx_volatility_clustering.png", p, width = 12, height = 8, dpi = 300)

# Equity Volatility Clustering
equity_vol <- equity_returns %>%
  mutate_all(~abs(.)) %>%
  mutate(Date = data$Date[-1]) %>%
  gather(key = "Asset", value = "Absolute_Returns", -Date)

p <- create_enhanced_timeseries(
  equity_vol,
  x_var = "Date",
  y_var = "Absolute_Returns",
  title = "Equity Volatility Clustering",
  color = "#2ca02c"
)
ggsave("outputs/eda/figures/equity_volatility_clustering.png", p, width = 12, height = 8, dpi = 300)

# Generate comparative analysis
cat("Generating comparative analysis...\n")

# Combine summary statistics
all_summary <- bind_rows(
  fx_summary %>% mutate(Asset_Type = "FX"),
  equity_summary %>% mutate(Asset_Type = "Equity")
)

# Comparative boxplots
all_returns_long <- bind_rows(
  fx_returns_long %>% mutate(Asset_Type = "FX"),
  equity_returns_long %>% mutate(Asset_Type = "Equity")
)

p <- create_enhanced_boxplot(
  all_returns_long,
  x_var = "Asset_Type",
  y_var = "Returns",
  fill_var = "Asset_Type",
  title = "Return Distribution Comparison: FX vs Equity",
  color_scheme = c("FX" = "#1f77b4", "Equity" = "#2ca02c")
)
ggsave("outputs/eda/figures/fx_vs_equity_comparison.png", p, width = 10, height = 6, dpi = 300)

# Save comprehensive summary
write.csv(all_summary, "outputs/eda/tables/comprehensive_summary_stats.csv", row.names = FALSE)

# =============================================================================
# COMPREHENSIVE STYLIZED FACTS ANALYSIS
# =============================================================================

cat("Running comprehensive stylized facts analysis...\n")

# Initialize results storage
stationarity_results <- list()
distributional_results <- list()
autocorrelation_results <- list()
leverage_results <- list()

# Run stylized facts tests for FX assets
cat("Analyzing FX assets for stylized facts...\n")
for (asset in fx_cols) {
  asset_returns <- fx_returns[[asset]]
  
  # Run all tests
  stationarity_results[[asset]] <- perform_stationarity_tests(asset_returns, asset)
  distributional_results[[asset]] <- perform_distributional_tests(asset_returns, asset)
  autocorrelation_results[[asset]] <- perform_autocorrelation_tests(asset_returns, asset)
  leverage_results[[asset]] <- perform_leverage_tests(asset_returns, asset)
}

# Run stylized facts tests for Equity assets
cat("Analyzing Equity assets for stylized facts...\n")
for (asset in equity_cols) {
  asset_returns <- equity_returns[[asset]]
  
  # Run all tests
  stationarity_results[[asset]] <- perform_stationarity_tests(asset_returns, asset)
  distributional_results[[asset]] <- perform_distributional_tests(asset_returns, asset)
  autocorrelation_results[[asset]] <- perform_autocorrelation_tests(asset_returns, asset)
  leverage_results[[asset]] <- perform_leverage_tests(asset_returns, asset)
}

# =============================================================================
# SAVE STYLIZED FACTS RESULTS
# =============================================================================

# Function to flatten and save test results
flatten_and_save_results <- function(results_list, filename) {
  flattened_data <- list()
  
  for (asset in names(results_list)) {
    result <- results_list[[asset]]
    
    # Create a row for each test type
    if ("adf" %in% names(result)) {
      # Stationarity tests
      adf_row <- data.frame(
        Asset = asset,
        Test_Type = "ADF",
        Statistic = result$adf$statistic,
        P_Value = result$adf$p_value,
        Critical_1pct = ifelse(is.null(result$adf$critical_values), NA, result$adf$critical_values[1]),
        Critical_5pct = ifelse(is.null(result$adf$critical_values), NA, result$adf$critical_values[2]),
        Critical_10pct = ifelse(is.null(result$adf$critical_values), NA, result$adf$critical_values[3]),
        N_Obs = result$n_obs,
        stringsAsFactors = FALSE
      )
      flattened_data[[paste0(asset, "_ADF")]] <- adf_row
      
      kpss_row <- data.frame(
        Asset = asset,
        Test_Type = "KPSS",
        Statistic = result$kpss$statistic,
        P_Value = result$kpss$p_value,
        Critical_1pct = ifelse(is.null(result$kpss$critical_values), NA, result$kpss$critical_values[1]),
        Critical_5pct = ifelse(is.null(result$kpss$critical_values), NA, result$kpss$critical_values[2]),
        Critical_10pct = ifelse(is.null(result$kpss$critical_values), NA, result$kpss$critical_values[3]),
        N_Obs = result$n_obs,
        stringsAsFactors = FALSE
      )
      flattened_data[[paste0(asset, "_KPSS")]] <- kpss_row
    }
    
    if ("jarque_bera" %in% names(result)) {
      # Distributional tests
      jb_row <- data.frame(
        Asset = asset,
        Test_Type = "Jarque-Bera",
        Statistic = result$jarque_bera$statistic,
        P_Value = result$jarque_bera$p_value,
        Critical_1pct = NA,
        Critical_5pct = NA,
        Critical_10pct = NA,
        Lags = NA,
        Coefficient = NA,
        Pos_Coefficient = NA,
        Neg_Coefficient = NA,
        Asymmetry_Ratio = NA,
        R_Squared = NA,
        N_Obs = result$n_obs,
        stringsAsFactors = FALSE
      )
      flattened_data[[paste0(asset, "_JB")]] <- jb_row
      
      ks_row <- data.frame(
        Asset = asset,
        Test_Type = "Kolmogorov-Smirnov",
        Statistic = result$kolmogorov_smirnov$statistic,
        P_Value = result$kolmogorov_smirnov$p_value,
        Critical_1pct = NA,
        Critical_5pct = NA,
        Critical_10pct = NA,
        Lags = NA,
        Coefficient = NA,
        Pos_Coefficient = NA,
        Neg_Coefficient = NA,
        Asymmetry_Ratio = NA,
        R_Squared = NA,
        N_Obs = result$n_obs,
        stringsAsFactors = FALSE
      )
      flattened_data[[paste0(asset, "_KS")]] <- ks_row
      
      tail_row <- data.frame(
        Asset = asset,
        Test_Type = "Tail_Index",
        Statistic = result$tail_index$hill_estimator,
        P_Value = NA,
        Critical_1pct = NA,
        Critical_5pct = NA,
        Critical_10pct = NA,
        Lags = NA,
        Coefficient = NA,
        Pos_Coefficient = NA,
        Neg_Coefficient = NA,
        Asymmetry_Ratio = NA,
        R_Squared = NA,
        N_Obs = result$n_obs,
        stringsAsFactors = FALSE
      )
      flattened_data[[paste0(asset, "_TAIL")]] <- tail_row
    }
    
    if ("ljung_box_raw" %in% names(result)) {
      # Autocorrelation tests
      lb_raw_row <- data.frame(
        Asset = asset,
        Test_Type = "Ljung-Box_Raw",
        Statistic = result$ljung_box_raw$statistic,
        P_Value = result$ljung_box_raw$p_value,
        Critical_1pct = NA,
        Critical_5pct = NA,
        Critical_10pct = NA,
        Lags = result$ljung_box_raw$lags,
        Coefficient = NA,
        Pos_Coefficient = NA,
        Neg_Coefficient = NA,
        Asymmetry_Ratio = NA,
        R_Squared = NA,
        N_Obs = result$n_obs,
        stringsAsFactors = FALSE
      )
      flattened_data[[paste0(asset, "_LB_RAW")]] <- lb_raw_row
      
      lb_sq_row <- data.frame(
        Asset = asset,
        Test_Type = "Ljung-Box_Squared",
        Statistic = result$ljung_box_squared$statistic,
        P_Value = result$ljung_box_squared$p_value,
        Critical_1pct = NA,
        Critical_5pct = NA,
        Critical_10pct = NA,
        Lags = result$ljung_box_squared$lags,
        Coefficient = NA,
        Pos_Coefficient = NA,
        Neg_Coefficient = NA,
        Asymmetry_Ratio = NA,
        R_Squared = NA,
        N_Obs = result$n_obs,
        stringsAsFactors = FALSE
      )
      flattened_data[[paste0(asset, "_LB_SQ")]] <- lb_sq_row
      
      arch_row <- data.frame(
        Asset = asset,
        Test_Type = "ARCH-LM",
        Statistic = result$arch_lm$statistic,
        P_Value = result$arch_lm$p_value,
        Critical_1pct = NA,
        Critical_5pct = NA,
        Critical_10pct = NA,
        Lags = result$arch_lm$lags,
        Coefficient = NA,
        Pos_Coefficient = NA,
        Neg_Coefficient = NA,
        Asymmetry_Ratio = NA,
        R_Squared = NA,
        N_Obs = result$n_obs,
        stringsAsFactors = FALSE
      )
      flattened_data[[paste0(asset, "_ARCH")]] <- arch_row
    }
    
    if ("sign_bias" %in% names(result)) {
      # Leverage tests
      sign_row <- data.frame(
        Asset = asset,
        Test_Type = "Sign_Bias",
        Statistic = NA,
        P_Value = result$sign_bias$p_value,
        Critical_1pct = NA,
        Critical_5pct = NA,
        Critical_10pct = NA,
        Lags = NA,
        Coefficient = result$sign_bias$coefficient,
        Pos_Coefficient = NA,
        Neg_Coefficient = NA,
        Asymmetry_Ratio = NA,
        R_Squared = result$sign_bias$r_squared,
        N_Obs = result$n_obs,
        stringsAsFactors = FALSE
      )
      flattened_data[[paste0(asset, "_SIGN")]] <- sign_row
      
      asym_row <- data.frame(
        Asset = asset,
        Test_Type = "Asymmetric_Volatility",
        Statistic = NA,
        P_Value = NA,
        Critical_1pct = NA,
        Critical_5pct = NA,
        Critical_10pct = NA,
        Lags = NA,
        Coefficient = NA,
        Pos_Coefficient = result$asymmetric_volatility$pos_coefficient,
        Neg_Coefficient = result$asymmetric_volatility$neg_coefficient,
        Asymmetry_Ratio = result$asymmetric_volatility$asymmetry_ratio,
        R_Squared = result$asymmetric_volatility$r_squared,
        N_Obs = result$n_obs,
        stringsAsFactors = FALSE
      )
      flattened_data[[paste0(asset, "_ASYM")]] <- asym_row
    }
  }
  
  # Combine and save
  if (length(flattened_data) > 0) {
    combined_data <- do.call(rbind, flattened_data)
    write.csv(combined_data, filename, row.names = FALSE)
    return(combined_data)
  } else {
    return(NULL)
  }
}

# Save all stylized facts results
cat("Saving stylized facts results...\n")
stationarity_summary <- flatten_and_save_results(stationarity_results, "outputs/eda/tables/stationarity_tests.csv")
distributional_summary <- flatten_and_save_results(distributional_results, "outputs/eda/tables/distributional_tests.csv")
autocorrelation_summary <- flatten_and_save_results(autocorrelation_results, "outputs/eda/tables/autocorrelation_tests.csv")
leverage_summary <- flatten_and_save_results(leverage_results, "outputs/eda/tables/leverage_tests.csv")

# =============================================================================
# GENERATE ADVANCED VISUALIZATIONS
# =============================================================================

cat("Generating advanced stylized facts visualizations...\n")

# Function to create QQ-plots
create_qq_plots <- function(returns_data, asset_cols, asset_type, color) {
  for (asset in asset_cols) {
    asset_returns <- returns_data[[asset]]
    clean_returns <- na.omit(asset_returns)
    
    # Create QQ-plot data
    qq_data <- data.frame(
      theoretical = qnorm(ppoints(length(clean_returns))),
      sample = sort(clean_returns)
    )
    
    # Create QQ-plot
    p <- ggplot(qq_data, aes(x = .data$theoretical, y = .data$sample)) +
      geom_point(alpha = 0.6, color = color) +
      geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
      labs(
        title = paste("Q-Q Plot:", asset, "Returns vs Normal Distribution"),
        x = "Theoretical Quantiles (Normal)",
        y = "Sample Quantiles"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
    
    ggsave(paste0("outputs/eda/figures/", asset, "_qqplot.png"), p, width = 10, height = 6, dpi = 300)
  }
}

# Function to create ACF/PACF plots
create_acf_plots <- function(returns_data, asset_cols, asset_type, color) {
  for (asset in asset_cols) {
    asset_returns <- returns_data[[asset]]
    clean_returns <- na.omit(asset_returns)
    squared_returns <- clean_returns^2
    
    # ACF plot for raw returns
    acf_raw <- acf(clean_returns, lag.max = 20, plot = FALSE)
    acf_data_raw <- data.frame(
      lag = 0:20,
      acf = as.numeric(acf_raw$acf)
    )
    
    p1 <- ggplot(acf_data_raw, aes(x = lag, y = acf)) +
      geom_col(width = 0.5, fill = color, alpha = 0.7) +
      geom_hline(yintercept = 0, color = "black") +
      geom_hline(yintercept = c(-1.96, 1.96) / sqrt(length(clean_returns)), 
                 color = "red", linetype = "dashed") +
      labs(
        title = paste("ACF of Raw Returns:", asset),
        x = "Lag",
        y = "Autocorrelation"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    
    ggsave(paste0("outputs/eda/figures/", asset, "_acf_raw.png"), p1, width = 10, height = 6, dpi = 300)
    
    # ACF plot for squared returns
    acf_squared <- acf(squared_returns, lag.max = 20, plot = FALSE)
    acf_data_squared <- data.frame(
      lag = 0:20,
      acf = as.numeric(acf_squared$acf)
    )
    
    p2 <- ggplot(acf_data_squared, aes(x = lag, y = acf)) +
      geom_col(width = 0.5, fill = color, alpha = 0.7) +
      geom_hline(yintercept = 0, color = "black") +
      geom_hline(yintercept = c(-1.96, 1.96) / sqrt(length(squared_returns)), 
                 color = "red", linetype = "dashed") +
      labs(
        title = paste("ACF of Squared Returns:", asset),
        x = "Lag",
        y = "Autocorrelation"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    
    ggsave(paste0("outputs/eda/figures/", asset, "_acf_squared.png"), p2, width = 10, height = 6, dpi = 300)
  }
}

# Generate QQ-plots
create_qq_plots(fx_returns, fx_cols, "FX", "#1f77b4")
create_qq_plots(equity_returns, equity_cols, "Equity", "#2ca02c")

# Generate ACF/PACF plots
create_acf_plots(fx_returns, fx_cols, "FX", "#1f77b4")
create_acf_plots(equity_returns, equity_cols, "Equity", "#2ca02c")

# =============================================================================
# CREATE STYLIZED FACTS SUMMARY TABLE
# =============================================================================

cat("Creating stylized facts summary table...\n")

# Function to create summary table
create_stylized_facts_summary <- function() {
  summary_data <- data.frame()
  
  all_assets <- c(fx_cols, equity_cols)
  all_returns <- cbind(fx_returns, equity_returns)
  
  for (asset in all_assets) {
    asset_returns <- all_returns[[asset]]
    clean_returns <- na.omit(asset_returns)
    
    # Basic statistics
    n_obs <- length(clean_returns)
    mean_ret <- mean(clean_returns)
    sd_ret <- sd(clean_returns)
    skewness_ret <- moments::skewness(clean_returns)
    kurtosis_ret <- moments::kurtosis(clean_returns)
    
    # ACF statistics
    acf_raw <- acf(clean_returns, lag.max = 1, plot = FALSE)
    acf1_raw <- acf_raw$acf[2]
    
    acf_squared <- acf(clean_returns^2, lag.max = 1, plot = FALSE)
    acf1_squared <- acf_squared$acf[2]
    
    # Leverage effect (sign bias)
    returns_lag1 <- c(NA, clean_returns[-length(clean_returns)])
    valid_idx <- !is.na(returns_lag1)
    leverage_coef <- if (sum(valid_idx) > 10) {
      lm_result <- lm(clean_returns[valid_idx]^2 ~ as.numeric(returns_lag1[valid_idx] < 0))
      coef(lm_result)[2]
    } else {
      NA
    }
    
    # Determine asset type
    asset_type <- if (asset %in% fx_cols) "FX" else "Equity"
    
    # Determine leverage presence
    leverage_present <- if (!is.na(leverage_coef)) leverage_coef > 0 else NA
    
    summary_row <- data.frame(
      Asset = asset,
      Asset_Type = asset_type,
      N_Obs = n_obs,
      Mean = mean_ret,
      SD = sd_ret,
      Skewness = skewness_ret,
      Kurtosis = kurtosis_ret,
      ACF1_Raw = acf1_raw,
      ACF1_Squared = acf1_squared,
      Leverage_Coefficient = leverage_coef,
      Leverage_Present = leverage_present,
      stringsAsFactors = FALSE
    )
    
    summary_data <- rbind(summary_data, summary_row)
  }
  
  return(summary_data)
}

# Create and save stylized facts summary
stylized_facts_summary <- create_stylized_facts_summary()
write.csv(stylized_facts_summary, "outputs/eda/tables/stylized_facts_summary.csv", row.names = FALSE)

# =============================================================================
# FINAL SUMMARY AND INTEGRATION
# =============================================================================

cat("=== ENHANCED EDA ANALYSIS COMPLETE ===\n")
cat("Generated comprehensive stylized facts analysis:\n")
cat("  Basic Statistics: outputs/eda/tables/comprehensive_summary_stats.csv\n")
cat("  Stationarity Tests: outputs/eda/tables/stationarity_tests.csv\n")
cat("  Distributional Tests: outputs/eda/tables/distributional_tests.csv\n")
cat("  Autocorrelation Tests: outputs/eda/tables/autocorrelation_tests.csv\n")
cat("  Leverage Tests: outputs/eda/tables/leverage_tests.csv\n")
cat("  Stylized Facts Summary: outputs/eda/tables/stylized_facts_summary.csv\n")
cat("  Advanced Visualizations: outputs/eda/figures/\n")
cat("  Total files:", length(list.files("outputs/eda", recursive = TRUE)), "\n")

# Create a manifest file for pipeline integration
manifest_data <- data.frame(
  File_Type = c("Summary_Stats", "Stationarity_Tests", "Distributional_Tests", 
                "Autocorrelation_Tests", "Leverage_Tests", "Stylized_Facts_Summary"),
  File_Path = c("outputs/eda/tables/comprehensive_summary_stats.csv",
                "outputs/eda/tables/stationarity_tests.csv",
                "outputs/eda/tables/distributional_tests.csv",
                "outputs/eda/tables/autocorrelation_tests.csv",
                "outputs/eda/tables/leverage_tests.csv",
                "outputs/eda/tables/stylized_facts_summary.csv"),
  Description = c("Basic statistical summary with dual splits",
                  "ADF and KPSS stationarity test results",
                  "Normality and distributional test results",
                  "Autocorrelation and heteroskedasticity tests",
                  "Leverage effect and asymmetric volatility tests",
                  "Comprehensive stylized facts summary table"),
  Generated_At = Sys.time(),
  stringsAsFactors = FALSE
)

write.csv(manifest_data, "outputs/eda/eda_manifest.csv", row.names = FALSE)

cat("EDA manifest created: outputs/eda/eda_manifest.csv\n")
cat("All outputs are ready for pipeline integration and dashboard display.\n")
