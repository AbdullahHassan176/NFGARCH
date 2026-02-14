# Direct test of eGARCH residual extraction
library(xts)

source("scripts/core/config.R")
source("scripts/manual_garch/manual_garch_core.R")
source("scripts/manual_garch/fit_egarch_manual.R")

# Load NVDA data
raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1)
raw_price_data$Date <- as.Date(rownames(raw_price_data))
rownames(raw_price_data) <- NULL

price_data_matrix <- raw_price_data[, !(names(raw_price_data) %in% "Date")]
date_index <- raw_price_data$Date

nvda_prices <- xts(price_data_matrix[["NVDA"]], order.by = date_index)
nvda_returns <- diff(log(nvda_prices), na.pad = FALSE)
nvda_returns <- as.numeric(nvda_returns)

n_obs <- length(nvda_returns)
train_size <- floor(n_obs * 0.65)
train_returns <- nvda_returns[1:train_size]

cat("=== DIRECT eGARCH TEST ===\n")
cat("Training set size:", length(train_returns), "\n\n")

# Fit eGARCH directly
cat("Fitting eGARCH...\n")
fit <- fit_egarch_manual(train_returns, dist = "norm")

cat("\n=== INSPECTING FIT OBJECT ===\n")
cat("Names in fit object:", paste(names(fit), collapse=", "), "\n\n")

cat("residuals (first 10):", head(fit$residuals, 10), "\n")
cat("residuals stats: mean=", mean(fit$residuals), ", sd=", sd(fit$residuals), 
    ", min=", min(fit$residuals), ", max=", max(fit$residuals), "\n\n")

cat("std_residuals (first 10):", head(fit$std_residuals, 10), "\n")
cat("std_residuals stats: mean=", mean(fit$std_residuals), ", sd=", sd(fit$std_residuals),
    ", min=", min(fit$std_residuals), ", max=", max(fit$std_residuals), "\n\n")

cat("sigma (first 10):", head(fit$sigma, 10), "\n")
cat("sigma stats: mean=", mean(fit$sigma), ", sd=", sd(fit$sigma),
    ", min=", min(fit$sigma), ", max=", max(fit$sigma), "\n\n")

# Check if residuals match the pattern
if (mean(fit$residuals) < -1 && all(fit$residuals < 0)) {
  cat("ERROR: residuals look like log_sigma2!\n")
  cat("Checking if residuals == log(sigma^2)...\n")
  log_sigma2_check <- log(fit$sigma^2)
  cat("log(sigma^2) first 10:", head(log_sigma2_check, 10), "\n")
  cat("Match?", all.equal(fit$residuals, log_sigma2_check), "\n")
}

# Now test what gets extracted when we save it
cat("\n=== TESTING CSV SAVE ===\n")
residuals_vec <- as.numeric(fit$std_residuals)
cat("residuals_vec (via std_residuals) first 10:", head(residuals_vec, 10), "\n")
cat("residuals_vec stats: mean=", mean(residuals_vec), ", sd=", sd(residuals_vec), "\n")

# Try saving and reloading
test_df <- data.frame(residuals = residuals_vec)
write.csv(test_df, "test_egarch_residuals.csv", row.names = FALSE)
reloaded <- read.csv("test_egarch_residuals.csv")
cat("\nReloaded from CSV (first 10):", head(reloaded$residuals, 10), "\n")
cat("Reloaded stats: mean=", mean(reloaded$residuals), ", sd=", sd(reloaded$residuals), "\n")
