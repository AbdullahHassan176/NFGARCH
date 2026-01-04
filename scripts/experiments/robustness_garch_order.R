#!/usr/bin/env Rscript
# GARCH Order Robustness Experiment
# Tests whether NF gains persist when allowing higher-order GARCH models
# Compares best classical GARCH(p,q) vs NF-GARCH with same selected order

# Load centralized seed configuration
if (file.exists("scripts/core/config.R")) {
  source("scripts/core/config.R")
  set.seed(REPRODUCIBILITY_SEED)
} else {
  set.seed(123)
}

# Load required libraries
library(xts)
library(PerformanceAnalytics)
library(rugarch)
library(dplyr)
library(openxlsx)
library(stringr)

# Load utility functions
source("scripts/utils/safety_functions.R")
source("scripts/utils/standardize_residuals.R")

# =============================================================================
# CONFIGURATION
# =============================================================================

# GARCH orders to test
GARCH_ORDERS <- list(
  c(1, 1),
  c(2, 1),
  c(1, 2),
  c(2, 2)
)

# Model families to test (at least sGARCH; ideally also eGARCH and gjrGARCH)
MODEL_FAMILIES <- list(
  sGARCH = list(model = "sGARCH", description = "Standard GARCH"),
  eGARCH = list(model = "eGARCH", description = "Exponential GARCH"),
  gjrGARCH = list(model = "gjrGARCH", description = "GJR-GARCH")
)

# Distributions to test (use same as existing pipeline)
DISTRIBUTIONS <- c("norm", "sstd")

# Output directory
OUTPUT_DIR <- "outputs/robust_garch_order"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# DATA LOADING
# =============================================================================

cat("=== GARCH ORDER ROBUSTNESS EXPERIMENT ===\n")
cat("Loading data...\n")

# Load price data
raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1, stringsAsFactors = FALSE)
raw_price_data$Date <- lubridate::ymd(rownames(raw_price_data))
rownames(raw_price_data) <- NULL
raw_price_data <- raw_price_data %>% dplyr::select(Date, everything())

date_index <- raw_price_data$Date
price_data_matrix <- raw_price_data[, !(names(raw_price_data) %in% "Date")]

# Define assets (use subset for quick test, can be expanded)
fx_names <- c("EURUSD", "GBPUSD", "USDZAR")
equity_tickers <- c("NVDA", "MSFT", "AMZN")

# Convert to XTS and calculate returns
equity_xts <- lapply(equity_tickers, function(ticker) {
  if (ticker %in% names(price_data_matrix)) {
    xts(price_data_matrix[[ticker]], order.by = date_index)
  } else {
    NULL
  }
})
names(equity_xts) <- equity_tickers
equity_xts <- equity_xts[!sapply(equity_xts, is.null)]

fx_xts <- lapply(fx_names, function(name) {
  if (name %in% names(price_data_matrix)) {
    xts(price_data_matrix[[name]], order.by = date_index)
  } else {
    NULL
  }
})
names(fx_xts) <- fx_names
fx_xts <- fx_xts[!sapply(fx_xts, is.null)]

# Calculate returns
CalculateReturns <- function(x) {
  if (inherits(x, "xts")) {
    diff(log(x))
  } else {
    diff(log(as.numeric(x)))
  }
}

equity_returns <- lapply(equity_xts, function(x) CalculateReturns(x)[-1, ])
fx_returns <- lapply(fx_xts, function(x) diff(log(x))[-1, ])

# Combine all returns
all_returns <- c(equity_returns, fx_returns)

cat("Loaded", length(all_returns), "assets\n")

# Chronological split (65/35)
get_split_index <- function(x, split_ratio = 0.65) {
  floor(NROW(x) * split_ratio)
}

train_returns <- lapply(all_returns, function(x) x[1:get_split_index(x)])
test_returns <- lapply(all_returns, function(x) x[(get_split_index(x) + 1):NROW(x)])

# =============================================================================
# ORDER SELECTION FUNCTION
# =============================================================================

select_best_order_by_bic <- function(returns, model_family, dist, orders = GARCH_ORDERS) {
  # Select best GARCH order by BIC on training data
  # Returns: best (p,q), BIC, AIC, LLH, and any warnings
  
  best_order <- NULL
  best_bic <- Inf
  best_aic <- NA
  best_llh <- NA
  best_fit <- NULL
  warnings_list <- character(0)
  
  for (order in orders) {
    p <- order[1]
    q <- order[2]
    
    tryCatch({
      # Create GARCH spec with order (p,q)
      spec <- ugarchspec(
        mean.model = list(armaOrder = c(0, 0)),
        variance.model = list(
          model = model_family,
          garchOrder = c(p, q)
        ),
        distribution.model = dist
      )
      
      # Fit model
      fit <- ugarchfit(spec = spec, data = returns, solver = "hybrid")
      
      if (fit@fit$convergence == 0) {
        ic <- infocriteria(fit)
        bic <- ic[2]  # BIC is second element
        aic <- ic[1]  # AIC is first element
        llh <- likelihood(fit)
        
        if (bic < best_bic) {
          best_bic <- bic
          best_aic <- aic
          best_llh <- llh
          best_order <- c(p, q)
          best_fit <- fit
        }
      } else {
        warnings_list <- c(warnings_list, 
                          paste0("Order (", p, ",", q, "): convergence failed"))
      }
    }, error = function(e) {
      warnings_list <<- c(warnings_list, 
                         paste0("Order (", p, ",", q, "): ", e$message))
    })
  }
  
  if (is.null(best_order)) {
    return(list(
      order = NA,
      bic = NA,
      aic = NA,
      llh = NA,
      fit = NULL,
      warnings = warnings_list
    ))
  }
  
  return(list(
    order = best_order,
    bic = best_bic,
    aic = best_aic,
    llh = best_llh,
    fit = best_fit,
    warnings = warnings_list
  ))
}

# =============================================================================
# EVALUATION FUNCTIONS
# =============================================================================

evaluate_forecasts <- function(fit, test_returns, model_family) {
  # Evaluate GARCH forecasts on test set
  # Returns: MSE, MAE, LogLikelihood
  
  tryCatch({
    # Forecast
    forecast_obj <- ugarchforecast(fit, n.ahead = length(test_returns))
    forecast_sigma <- as.numeric(sigma(forecast_obj))
    forecast_mean <- as.numeric(fitted(forecast_obj))
    
    # Actual returns
    actual_returns <- as.numeric(test_returns)
    
    # Ensure same length
    min_len <- min(length(actual_returns), length(forecast_mean))
    actual_returns <- actual_returns[1:min_len]
    forecast_mean <- forecast_mean[1:min_len]
    forecast_sigma <- forecast_sigma[1:min_len]
    
    # Calculate metrics
    mse <- mean((actual_returns - forecast_mean)^2, na.rm = TRUE)
    mae <- mean(abs(actual_returns - forecast_mean), na.rm = TRUE)
    
    # Calculate log-likelihood on test set
    # Use fitted model parameters to compute likelihood
    residuals_test <- (actual_returns - forecast_mean) / forecast_sigma
    residuals_test <- residuals_test[is.finite(residuals_test)]
    
    # Get distribution parameters from fit
    dist_model <- fit@model$modeldesc$distribution
    if (dist_model == "norm") {
      loglik_test <- sum(dnorm(residuals_test, log = TRUE)) - sum(log(forecast_sigma[1:length(residuals_test)]))
    } else if (dist_model == "sstd") {
      # Skewed Student-t - approximate with normal for simplicity
      loglik_test <- sum(dnorm(residuals_test, log = TRUE)) - sum(log(forecast_sigma[1:length(residuals_test)]))
    } else {
      loglik_test <- sum(dnorm(residuals_test, log = TRUE)) - sum(log(forecast_sigma[1:length(residuals_test)]))
    }
    
    return(list(
      mse = mse,
      mae = mae,
      loglik = loglik_test
    ))
  }, error = function(e) {
    return(list(
      mse = NA,
      mae = NA,
      loglik = NA
    ))
  })
}

# =============================================================================
# NF RESIDUALS LOADING
# =============================================================================

cat("Loading NF residuals...\n")

nf_residuals_map <- list()
nf_files <- list.files("outputs/manual/nf_models", pattern = "*_synthetic_residuals.csv", full.names = TRUE, recursive = TRUE)

if (length(nf_files) > 0) {
  for (f in nf_files) {
    fname <- basename(f)
    fname_clean <- stringr::str_replace(fname, "_synthetic_residuals\\.csv$", "")
    
    # Parse model and asset from filename
    # Format: MODEL_ASSET_synthetic_residuals.csv
    parts <- strsplit(fname_clean, "_")[[1]]
    
    if (length(parts) >= 2) {
      model_name <- parts[1]
      asset_name <- paste(parts[-1], collapse = "_")
      
      tryCatch({
        residuals_data <- read.csv(f)
        residual_values <- if ("residual" %in% names(residuals_data)) {
          residuals_data$residual
        } else if (ncol(residuals_data) > 0) {
          residuals_data[[1]]
        } else {
          next
        }
        
        residual_values <- as.numeric(residual_values)
        residual_values <- residual_values[!is.na(residual_values)]
        
        if (length(residual_values) > 0) {
          # Standardize NF residuals
          residual_values <- standardize_residuals(residual_values, verify = TRUE)
          nf_residuals_map[[paste0(model_name, "_", asset_name)]] <- residual_values
        }
      }, error = function(e) {
        cat("WARNING: Failed to load NF residuals from", fname, ":", e$message, "\n")
      })
    }
  }
}

cat("Loaded", length(nf_residuals_map), "NF residual files\n")

# =============================================================================
# NF-GARCH SIMULATION WITH RUGARCH
# =============================================================================

simulate_nf_garch_rugarch <- function(fit, nf_residuals, horizon, model_family) {
  # Simulate NF-GARCH path using rugarch
  # fit: fitted GARCH model
  # nf_residuals: standardized NF residuals
  # horizon: forecast horizon
  # model_family: GARCH model family
  
  tryCatch({
    # Get model specification
    spec <- getspec(fit)
    
    # Get last state
    last_sigma <- as.numeric(tail(sigma(fit), 1))
    last_residual <- as.numeric(tail(residuals(fit), 1))
    last_return <- as.numeric(tail(fitted(fit), 1))
    
    # Use NF residuals for simulation (standardized innovations)
    nf_innovations <- head(nf_residuals, horizon)
    
    # Ensure we have enough innovations
    if (length(nf_innovations) < horizon) {
      nf_innovations <- rep(nf_innovations, length.out = horizon)
    }
    
    # Create path simulation with NF innovations
    # Note: innovations should be a matrix (n.sim x m.sim)
    sim <- ugarchpath(
      spec = spec,
      n.sim = horizon,
      m.sim = 1,
      presigma = matrix(last_sigma, nrow = 1, ncol = 1),
      preresiduals = matrix(last_residual, nrow = 1, ncol = 1),
      prereturns = matrix(last_return, nrow = 1, ncol = 1),
      innovations = matrix(nf_innovations, nrow = horizon, ncol = 1)
    )
    
    simulated_returns <- as.numeric(fitted(sim))
    return(simulated_returns)
  }, error = function(e) {
    warning("NF-GARCH simulation failed: ", e$message)
    return(rep(NA, horizon))
  })
}

# =============================================================================
# MAIN EXPERIMENT LOOP
# =============================================================================

cat("\n=== Running Robustness Experiment ===\n")

results_list <- list()

for (asset_name in names(train_returns)) {
  cat("\nProcessing asset:", asset_name, "\n")
  
  train_data <- train_returns[[asset_name]]
  test_data <- test_returns[[asset_name]]
  
  for (model_family_name in names(MODEL_FAMILIES)) {
    model_family <- MODEL_FAMILIES[[model_family_name]]$model
    
    for (dist in DISTRIBUTIONS) {
      cat("  Model:", model_family, "Distribution:", dist, "\n")
      
      # Step 1: Select best order by BIC on training data
      order_result <- select_best_order_by_bic(train_data, model_family, dist)
      
      if (is.null(order_result$order) || any(is.na(order_result$order))) {
        cat("    WARNING: Order selection failed\n")
        next
      }
      
      selected_p <- order_result$order[1]
      selected_q <- order_result$order[2]
      cat("    Selected order: (", selected_p, ",", selected_q, "), BIC:", round(order_result$bic, 2), "\n")
      
      # Step 2: Evaluate classical GARCH with selected order on test set
      classical_fit <- order_result$fit
      if (is.null(classical_fit)) {
        cat("    WARNING: Classical fit is NULL\n")
        next
      }
      
      classical_eval <- evaluate_forecasts(classical_fit, test_data, model_family)
      
      # Step 3: Fit NF-GARCH with same selected order
      # First, get NF residuals for this asset/model combination
      nf_key <- paste0(model_family, "_", asset_name)
      if (!nf_key %in% names(nf_residuals_map)) {
        # Try alternative key formats
        alt_keys <- c(
          paste0(model_family_name, "_", asset_name),
          paste0(tolower(model_family), "_", asset_name)
        )
        nf_key <- NULL
        for (alt_key in alt_keys) {
          if (alt_key %in% names(nf_residuals_map)) {
            nf_key <- alt_key
            break
          }
        }
      }
      
      if (is.null(nf_key) || !nf_key %in% names(nf_residuals_map)) {
        cat("    WARNING: NF residuals not found for", model_family, asset_name, "\n")
        # Store classical results only
        results_list[[length(results_list) + 1]] <- data.frame(
          asset = asset_name,
          model_family = model_family,
          dist = dist,
          selected_p = selected_p,
          selected_q = selected_q,
          classical_BIC = order_result$bic,
          classical_AIC = order_result$aic,
          classical_LL = order_result$llh,
          classical_MSE = classical_eval$mse,
          classical_MAE = classical_eval$mae,
          classical_LogLik = classical_eval$loglik,
          nf_MSE = NA,
          nf_MAE = NA,
          nf_LogLik = NA,
          delta_MSE = NA,
          delta_MAE = NA,
          delta_LogLik = NA,
          stringsAsFactors = FALSE
        )
        next
      }
      
      nf_residuals <- nf_residuals_map[[nf_key]]
      
      # Fit NF-GARCH with same order
      tryCatch({
        nf_spec <- ugarchspec(
          mean.model = list(armaOrder = c(0, 0)),
          variance.model = list(
            model = model_family,
            garchOrder = c(selected_p, selected_q)
          ),
          distribution.model = dist
        )
        
        nf_fit <- ugarchfit(spec = nf_spec, data = train_data, solver = "hybrid")
        
        if (nf_fit@fit$convergence != 0) {
          cat("    WARNING: NF-GARCH fit failed to converge\n")
          next
        }
        
        # Simulate NF-GARCH on test set
        nf_simulated <- simulate_nf_garch_rugarch(nf_fit, nf_residuals, length(test_data), model_family)
        
        if (all(is.na(nf_simulated))) {
          cat("    WARNING: NF-GARCH simulation failed\n")
          next
        }
        
        # Evaluate NF-GARCH
        actual_returns <- as.numeric(test_data)
        min_len <- min(length(actual_returns), length(nf_simulated))
        actual_returns <- actual_returns[1:min_len]
        nf_simulated <- nf_simulated[1:min_len]
        
        nf_mse <- mean((actual_returns - nf_simulated)^2, na.rm = TRUE)
        nf_mae <- mean(abs(actual_returns - nf_simulated), na.rm = TRUE)
        
        # Approximate log-likelihood for NF-GARCH
        nf_residuals_test <- (actual_returns - nf_simulated) / sd(nf_simulated, na.rm = TRUE)
        nf_residuals_test <- nf_residuals_test[is.finite(nf_residuals_test)]
        nf_loglik <- sum(dnorm(nf_residuals_test, log = TRUE))
        
        # Calculate deltas
        delta_mse <- nf_mse - classical_eval$mse
        delta_mae <- nf_mae - classical_eval$mae
        delta_loglik <- nf_loglik - classical_eval$loglik
        
        # Store results
        results_list[[length(results_list) + 1]] <- data.frame(
          asset = asset_name,
          model_family = model_family,
          dist = dist,
          selected_p = selected_p,
          selected_q = selected_q,
          classical_BIC = order_result$bic,
          classical_AIC = order_result$aic,
          classical_LL = order_result$llh,
          classical_MSE = classical_eval$mse,
          classical_MAE = classical_eval$mae,
          classical_LogLik = classical_eval$loglik,
          nf_MSE = nf_mse,
          nf_MAE = nf_mae,
          nf_LogLik = nf_loglik,
          delta_MSE = delta_mse,
          delta_MAE = delta_mae,
          delta_LogLik = delta_loglik,
          stringsAsFactors = FALSE
        )
        
        cat("    Classical MSE:", round(classical_eval$mse, 6), "NF MSE:", round(nf_mse, 6), 
            "Delta:", round(delta_mse, 6), "\n")
        
      }, error = function(e) {
        cat("    ERROR in NF-GARCH fitting:", e$message, "\n")
      })
    }
  }
}

# =============================================================================
# RESULTS OUTPUT
# =============================================================================

cat("\n=== Writing Results ===\n")

if (length(results_list) == 0) {
  cat("WARNING: No results to write!\n")
  quit(status = 1)
}

results_df <- do.call(rbind, results_list)

# Write CSV
csv_file <- file.path(OUTPUT_DIR, "garch_order_robustness_results.csv")
write.csv(results_df, csv_file, row.names = FALSE)
cat("CSV written:", csv_file, "\n")

# Write Excel
excel_file <- file.path(OUTPUT_DIR, "garch_order_robustness_results.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Results")
writeData(wb, "Results", results_df)
saveWorkbook(wb, excel_file, overwrite = TRUE)
cat("Excel written:", excel_file, "\n")

# Write LaTeX table
latex_file <- file.path(OUTPUT_DIR, "garch_order_robustness_table.tex")

# Create summary table (one row per asset per model family)
summary_df <- results_df %>%
  group_by(asset, model_family) %>%
  summarise(
    selected_order = paste0("(", first(selected_p), ",", first(selected_q), ")"),
    classical_MSE = first(classical_MSE),
    nf_MSE = first(nf_MSE),
    delta_MSE = first(delta_MSE),
    classical_MAE = first(classical_MAE),
    nf_MAE = first(nf_MAE),
    delta_MAE = first(delta_MAE),
    .groups = "drop"
  )

# Write LaTeX
cat("\\begin{table}[h]\n", file = latex_file)
cat("\\centering\n", file = latex_file, append = TRUE)
cat("\\caption{GARCH Order Robustness Results}\n", file = latex_file, append = TRUE)
cat("\\label{tab:garch_order_robustness}\n", file = latex_file, append = TRUE)
cat("\\begin{tabular}{lccccccc}\n", file = latex_file, append = TRUE)
cat("\\toprule\n", file = latex_file, append = TRUE)
cat("Asset & Model & Order & \\multicolumn{2}{c}{MSE} & \\multicolumn{2}{c}{MAE} \\\\\n", file = latex_file, append = TRUE)
cat(" & & & Classical & NF & Classical & NF \\\\\n", file = latex_file, append = TRUE)
cat("\\midrule\n", file = latex_file, append = TRUE)

for (i in 1:nrow(summary_df)) {
  row <- summary_df[i, ]
  cat(sprintf("%s & %s & %s & %.6f & %.6f & %.6f & %.6f \\\\\n",
              row$asset, row$model_family, row$selected_order,
              row$classical_MSE, row$nf_MSE, row$classical_MAE, row$nf_MAE),
      file = latex_file, append = TRUE)
}

cat("\\bottomrule\n", file = latex_file, append = TRUE)
cat("\\end{tabular}\n", file = latex_file, append = TRUE)
cat("\\end{table}\n", file = latex_file, append = TRUE)

cat("LaTeX written:", latex_file, "\n")

cat("\n=== Experiment Complete ===\n")
cat("Results written to:", OUTPUT_DIR, "\n")

