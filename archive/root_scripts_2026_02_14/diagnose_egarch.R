# Diagnose eGARCH residual extraction issue

library(xts)

# Load config and fitting functions
source("scripts/core/config.R")
source("scripts/manual_garch/fit_egarch_manual.R")
source("scripts/manual_garch/manual_garch_core.R")
source("scripts/engines/engine_selector.R")

# Load NVDA data
raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1)
raw_price_data$Date <- as.Date(rownames(raw_price_data))
rownames(raw_price_data) <- NULL
raw_price_data <- raw_price_data %>% dplyr::select(Date, everything())

price_data_matrix <- raw_price_data[, !(names(raw_price_data) %in% "Date")]
date_index <- raw_price_data$Date

# Get NVDA prices
nvda_prices <- xts(price_data_matrix[["NVDA"]], order.by = date_index)
nvda_returns <- diff(log(nvda_prices), na.pad = FALSE)
nvda_returns <- as.numeric(nvda_returns)

# Split 65/35
n_obs <- length(nvda_returns)
train_size <- floor(n_obs * 0.65)
train_returns <- nvda_returns[1:train_size]

cat("Training set size:", length(train_returns), "\n")
cat("Returns: mean =", mean(train_returns), ", sd =", sd(train_returns), "\n")

# Fit eGARCH
cat("\nFitting eGARCH...\n")
fit <- fit_egarch_manual(train_returns, dist = "norm")

cat("\nFit object structure:\n")
cat("Names:", names(fit), "\n")
cat("Convergence:", fit$convergence, "\n")
cat("LogLik:", fit$loglik, "\n")

cat("\nResiduals:\n")
cat("Length:", length(fit$residuals), "\n")
cat("Mean:", mean(fit$residuals), "\n")
cat("Std:", sd(fit$residuals), "\n")
cat("Min:", min(fit$residuals), "\n")
cat("Max:", max(fit$residuals), "\n")
cat("First 10:", head(fit$residuals, 10), "\n")

cat("\nStandardized Residuals:\n")
cat("Length:", length(fit$std_residuals), "\n")
cat("Mean:", mean(fit$std_residuals), "\n")
cat("Std:", sd(fit$std_residuals), "\n")
cat("Min:", min(fit$std_residuals), "\n")
cat("Max:", max(fit$std_residuals), "\n")
cat("First 10:", head(fit$std_residuals, 10), "\n")

cat("\nSigma:\n")
cat("Length:", length(fit$sigma), "\n")
cat("Mean:", mean(fit$sigma), "\n")
cat("Min:", min(fit$sigma), "\n")
cat("Max:", max(fit$sigma), "\n")
cat("First 10:", head(fit$sigma, 10), "\n")

# Check using engine_residuals
cat("\nUsing engine_residuals:\n")
std_res <- engine_residuals(fit, standardize = TRUE)
cat("Standardized (via engine):", length(std_res), "elements\n")
cat("Mean:", mean(std_res), "\n")
cat("Std:", sd(std_res), "\n")
cat("First 10:", head(std_res, 10), "\n")
