# Manual Engine Execution Guide - Complete Walkthrough

This guide walks you through running the Financial-SDG-GARCH pipeline step-by-step using the **manual engine only**, with mathematical verification at each step.

## 📋 Prerequisites Check

### Step 1: Verify R Installation
```r
# In R Console or RStudio
R.version
# Should show R >= 4.0.0

# Install required packages
install.packages(c(
  "xts", "zoo", "PerformanceAnalytics", 
  "dplyr", "tidyr", "stringr", "lubridate",
  "parallel", "doParallel", "openxlsx", 
  "moments", "tseries", "forecast", 
  "ggplot2", "quantmod", "FinTS"
))
```

### Step 2: Verify Python Installation
```bash
python --version
# Should show Python >= 3.8

pip install torch nflows numpy pandas matplotlib scipy psutil
```

### Step 3: Verify Data File Exists
```r
file.exists("./data/processed/raw (FX + EQ).csv")
# Should return TRUE
```

---

## 🔬 Phase 1: Mathematical Verification (5 minutes)

**Purpose**: Verify the manual GARCH engine implements the correct mathematical equations.

### Step 1.1: Run Math Verification Script
```r
# In RStudio or R Console
source("scripts/manual/verify_manual_math.R")
```

**What this checks:**
- ✓ sGARCH variance equation: σ²_t = ω + αε²_{t-1} + βσ²_{t-1}
- ✓ eGARCH log-variance equation: log(σ²_t) = ω + α(|z_{t-1}| - E|z_t|) + βlog(σ²_{t-1}) + γz_{t-1}
- ✓ gjrGARCH variance equation: σ²_t = ω + αε²_{t-1} + γε²_{t-1}I_{ε<0} + βσ²_{t-1}
- ✓ TGARCH variance equation: σ²_t = ω + (α + ηI_{ε<0})ε²_{t-1} + βσ²_{t-1}
- ✓ Log-likelihood calculations for Normal and Student-t distributions
- ✓ Parameter constraints (ω > 0, α ≥ 0, β ≥ 0, α + β < 1)
- ✓ Variance stationarity conditions

**Expected Output:**
```
=== MANUAL ENGINE MATH VERIFICATION ===
✓ sGARCH variance recursion: PASSED
✓ eGARCH log-variance recursion: PASSED  
✓ gjrGARCH variance recursion: PASSED
✓ TGARCH variance recursion: PASSED
✓ Log-likelihood calculation: PASSED
✓ Parameter constraints: PASSED
✓ Stationarity conditions: PASSED
All mathematical verifications PASSED
```

**If verification fails:** Check the error messages and report issues.

---

## 📊 Phase 2: Data Loading & EDA (10 minutes)

### Step 2.1: Load Data and Verify
```r
# In RStudio
setwd("C:/Github/Financial-SDG-GARCH")  # Adjust path as needed

# Load data
raw_price_data <- read.csv("./data/processed/raw (FX + EQ).csv", row.names = 1)
raw_price_data$Date <- lubridate::ymd(rownames(raw_price_data))
rownames(raw_price_data) <- NULL
raw_price_data <- raw_price_data %>% dplyr::select(Date, everything())

# Verify data structure
cat("Rows:", nrow(raw_price_data), "\n")
cat("Columns:", ncol(raw_price_data), "\n")
cat("Date range:", min(raw_price_data$Date), "to", max(raw_price_data$Date), "\n")
```

**Expected:** ~4500 rows, 13 columns (1 Date + 12 assets)

### Step 2.2: Calculate Returns
```r
# Extract price matrix
price_data_matrix <- raw_price_data[, !(names(raw_price_data) %in% "Date")]

# Calculate log returns
returns_matrix <- apply(price_data_matrix, 2, function(x) {
  diff(log(x))[-1]
})

# Verify returns statistics
summary(returns_matrix)
# Should show:
# - Mean near 0 for each asset
# - Non-zero standard deviation
# - Some negative values (losses)
```

**Mathematical Check:** For log returns r_t = log(P_t/P_{t-1}), verify:
- Mean should be approximately 0 (or small drift)
- Standard deviation > 0 (volatility)
- Negative skewness typical for financial returns

---

## 🎯 Phase 3: Manual Optimized Configuration (2 minutes)

### Step 3.1: Load Optimized Configuration
```r
# Load manual optimization settings
source("scripts/manual/manual_optimized_config.R")

# Display configuration
print_optimization_summary()
```

**Expected Output:**
```
=== MANUAL EXECUTION OPTIMIZATION SUMMARY ===
Asset Reduction:  50 % ( 12  ->  6 )
Model Reduction:  40 % ( 5  ->  3 )
CV Optimization:  60 % time savings
Expected Total Time Savings: 70-80%
Expected Execution Time: 45-90 minutes
```

**Assets:** NVDA, MSFT, AMZN (Equity) + EURUSD, GBPUSD, USDZAR (FX)  
**Models:** sGARCH, eGARCH, TGARCH

---

## 🔧 Phase 4: GARCH Model Fitting (20-30 minutes)

### Step 4.1: Run Optimized GARCH Fitting
```r
# Run the manual GARCH fitting script
source("scripts/manual/manual_garch_fitting.R")
```

**What happens:**
1. Loads 6 optimized assets (3 FX + 3 Equity)
2. Calculates log returns for each asset
3. Fits 3 GARCH models (sGARCH, eGARCH, TGARCH) using **manual engine**
4. Extracts standardized residuals: z_t = ε_t / σ_t
5. Saves residuals for NF training

**Mathematical Verification During Fitting:**

For each model-asset combination, verify:
```r
# After fitting, check:
fit <- your_fit_object

# 1. Parameters satisfy constraints
fit$coef["omega"] > 0
fit$coef["alpha"] >= 0
fit$coef["beta"] >= 0
fit$coef["alpha"] + fit$coef["beta"] < 1  # Stationarity

# 2. Standardized residuals have mean ≈ 0, variance ≈ 1
mean(fit$std_residuals, na.rm = TRUE)
var(fit$std_residuals, na.rm = TRUE)

# 3. Residuals are uncorrelated (Ljung-Box test)
Box.test(fit$std_residuals, type = "Ljung-Box", lag = 10)

# 4. Squared residuals show no ARCH effects
Box.test(fit$std_residuals^2, type = "Ljung-Box", lag = 10)
```

**Expected Outputs:**
- `outputs/manual/garch_fitting/model_summary.csv` - Convergence status for each model
- `outputs/manual/residuals_by_model/` - Residuals organized by model type

**Key Metrics to Check:**
- Convergence rate should be > 90% for all models
- AIC/BIC values should be reasonable (no extreme values)
- Log-likelihood should be negative (normal for log-likelihood)

---

## 🤖 Phase 5: Normalizing Flow Training (20-30 minutes)

### Step 5.1: Train NF Models
```python
# In Python/Jupyter Notebook or command line
python scripts/manual/manual_nf_training.py
```

**What happens:**
1. Loads standardized residuals from Phase 4
2. Trains a Normalizing Flow model for each model-asset combination
3. Generates synthetic residuals that preserve statistical properties
4. Saves trained models and synthetic residuals

**Mathematical Verification After Training:**

```python
import numpy as np
from scipy.stats import ks_2samp, wasserstein_distance

# Load real and synthetic residuals
real_residuals = np.loadtxt('outputs/manual/residuals_by_model/sGARCH/NVDA_Manual_Optimized_residuals.csv')
synthetic_residuals = np.loadtxt('outputs/manual/nf_models/sGARCH_NVDA_synthetic_residuals.csv')

# Check statistical properties
print(f"Real - Mean: {np.mean(real_residuals):.4f}, Std: {np.std(real_residuals):.4f}")
print(f"Synth - Mean: {np.mean(synthetic_residuals):.4f}, Std: {np.std(synthetic_residuals):.4f}")

# Kolmogorov-Smirnov test (should not reject H0: same distribution)
ks_stat, ks_pval = ks_2samp(real_residuals, synthetic_residuals)
print(f"KS test: stat={ks_stat:.4f}, p-value={ks_pval:.4f}")

# Wasserstein distance (should be small)
wd = wasserstein_distance(real_residuals, synthetic_residuals)
print(f"Wasserstein distance: {wd:.4f}")
```

**Expected:**
- Mean differences < 0.1
- Std differences < 0.2
- KS p-value > 0.05 (same distribution)
- Wasserstein distance < 0.3

**Outputs:**
- `outputs/manual/nf_models/*_synthetic_residuals.csv` - Synthetic residuals
- `outputs/manual/nf_models/*/nf_model.pth` - Trained PyTorch models

---

## 🔄 Phase 6: NF-GARCH Simulation (15-20 minutes)

### Step 6.1: Run NF-GARCH Simulation
```r
# In RStudio or R Console
# Load engine selector
source("scripts/engines/engine_selector.R")
source("scripts/utils/cli_parser.R")

# Run simulation
Rscript scripts/simulation_forecasting/simulate_nf_garch_engine.R --engine manual
```

**What happens:**
1. Loads trained GARCH models and synthetic residuals
2. For each model-asset combination:
   - Fits GARCH model to historical returns
   - Generates synthetic path using: r_t = μ + σ_t * z_synthetic
   - Where σ_t follows GARCH recursion with synthetic innovations
3. Compares simulated vs actual returns (MSE, MAE)
4. Performs time-series cross-validation

**Mathematical Verification During Simulation:**

For NF-GARCH simulation, verify the path generation:
```r
# After simulation, check:
sim_result <- your_simulation_result

# 1. Variance follows GARCH equation
# σ²_{t+1} should equal: ω + α*ε²_t + β*σ²_t
sigma2_manual <- omega + alpha*residuals^2 + beta*sigma2_lag
all.equal(sigma2_garch, sigma2_manual, tolerance = 1e-6)

# 2. Returns follow: r_t = μ + σ_t * z_t
returns_sim <- mu + sigma * synthetic_z
cor(returns_sim, actual_returns)  # Should be positive but not 1.0

# 3. Synthetic returns preserve volatility clustering
acf(returns_sim^2)  # Should show autocorrelation
acf(actual_returns^2)  # Should show similar pattern
```

**Expected Outputs:**
- `results/consolidated/NF_GARCH_Results_manual.xlsx` - Complete results
- Comparison tables showing:
  - Chronological split performance
  - Time-series CV performance
  - Model rankings

**Key Metrics:**
- MSE: Mean Squared Error between simulated and actual
- MAE: Mean Absolute Error
- AIC/BIC: Model selection criteria

---

## 📈 Phase 7: Results Verification (10 minutes)

### Step 7.1: Check Output Files
```r
# Verify all outputs exist
output_files <- list(
  garch_summary = "outputs/manual/garch_fitting/model_summary.csv",
  nf_training = "outputs/manual/nf_models/training_summary.json",
  simulation_results = "results/consolidated/NF_GARCH_Results_manual.xlsx"
)

sapply(output_files, file.exists)
# All should return TRUE
```

### Step 7.2: Validate Mathematical Properties

```r
# Load results
library(openxlsx)
results <- read.xlsx("results/consolidated/NF_GARCH_Results_manual.xlsx", 
                     sheet = "Chrono_Split_NF_GARCH")

# Check each model-asset combination:
# 1. AIC and BIC are finite and reasonable
all(is.finite(results$AIC))
all(is.finite(results$BIC))
summary(results$AIC)  # Should show reasonable range

# 2. MSE and MAE are positive
all(results$MSE > 0)
all(results$MAE > 0)

# 3. Log-likelihood is negative (typical)
all(results$LogLikelihood < 0)

# 4. Compare models - lower AIC/BIC is better
results %>%
  group_by(Model) %>%
  summarise(
    Mean_AIC = mean(AIC, na.rm = TRUE),
    Mean_BIC = mean(BIC, na.rm = TRUE),
    Mean_MSE = mean(MSE, na.rm = TRUE)
  ) %>%
  arrange(Mean_AIC)
```

---

## 🎓 Mathematical Reference Checklist

Use this checklist to verify correctness at each step:

### GARCH Model Equations ✓

**sGARCH(1,1):**
- Return: r_t = μ + ε_t
- Error: ε_t = σ_t * z_t (where z_t ~ N(0,1) or t-distribution)
- Variance: σ²_t = ω + αε²_{t-1} + βσ²_{t-1}
- Constraints: ω > 0, α ≥ 0, β ≥ 0, α + β < 1

**eGARCH(1,1):**
- Return: r_t = μ + ε_t
- Log-variance: log(σ²_t) = ω + α(|z_{t-1}| - E|z_t|) + γz_{t-1} + βlog(σ²_{t-1})
- Constraints: β < 1 (stationarity)

**gjrGARCH(1,1):**
- Variance: σ²_t = ω + αε²_{t-1} + γε²_{t-1}I_{ε<0} + βσ²_{t-1}
- Constraints: ω > 0, α ≥ 0, γ ≥ 0, β ≥ 0, α + γ/2 + β < 1

**TGARCH(1,1):**
- Variance: σ²_t = ω + (α + ηI_{ε<0})ε²_{t-1} + βσ²_{t-1}
- Constraints: Similar to gjrGARCH

### Likelihood Functions ✓

**Normal Distribution:**
- Log-likelihood: LL = -½Σ[log(2π) + log(σ²_t) + (ε_t/σ_t)²]

**Student-t Distribution:**
- Log-likelihood: Includes gamma functions and degrees of freedom ν

### Residual Standardization ✓

- Standardized residual: z_t = ε_t / σ_t
- Should have: E[z_t] ≈ 0, Var(z_t) ≈ 1
- Should be: i.i.d. (independent and identically distributed)

### NF-GARCH Simulation ✓

1. Initialize: σ²_0 = unconditional variance
2. For each time step t:
   - Draw synthetic innovation: z_synth ~ NF(learned distribution)
   - Update variance: σ²_t = GARCH_recursion(σ²_{t-1}, ε_{t-1})
   - Generate return: r_t = μ + σ_t * z_synth
   - Calculate error: ε_t = r_t - μ

---

## 🐛 Troubleshooting

### If GARCH fitting fails:
1. Check data quality (no NAs, sufficient observations > 500)
2. Try different initial parameter values
3. Check convergence code (should be 0 for success)
4. Verify returns are stationary

### If NF training fails:
1. Verify residual files exist and have data
2. Check Python packages are installed correctly
3. Ensure enough memory (NF training can be memory-intensive)
4. Try reducing batch size or epochs

### If simulation fails:
1. Verify synthetic residuals are loaded correctly
2. Check GARCH model fits exist
3. Ensure file paths are correct
4. Verify engine selector is using "manual" engine

---

## ✅ Final Verification

After completing all phases, verify:

1. ✓ All output files exist
2. ✓ All models converged (convergence rate > 90%)
3. ✓ Residuals are properly standardized (mean ≈ 0, var ≈ 1)
4. ✓ Synthetic residuals preserve distribution (KS test p > 0.05)
5. ✓ Simulation produces realistic returns (volatility clustering, fat tails)
6. ✓ Results Excel file contains all expected sheets
7. ✓ Mathematical equations are correctly implemented (verification script passes)

**You're done!** The manual engine pipeline has been successfully executed with mathematical verification at each step.

For NF-GARCH simulation, verify the path generation:
```r
# After simulation, check:
sim_result <- your_simulation_result

# 1. Variance follows GARCH equation
# σ²_{t+1} should equal: ω + α*ε²_t + β*σ²_t
sigma2_manual <- omega + alpha*residuals^2 + beta*sigma2_lag
all.equal(sigma2_garch, sigma2_manual, tolerance = 1e-6)

# 2. Returns follow: r_t = μ + σ_t * z_t
returns_sim <- mu + sigma * synthetic_z
cor(returns_sim, actual_returns)  # Should be positive but not 1.0

# 3. Synthetic returns preserve volatility clustering
acf(returns_sim^2)  # Should show autocorrelation
acf(actual_returns^2)  # Should show similar pattern
```

**Expected Outputs:**
- `results/consolidated/NF_GARCH_Results_manual.xlsx` - Complete results
- Comparison tables showing:
  - Chronological split performance
  - Time-series CV performance
  - Model rankings

**Key Metrics:**
- MSE: Mean Squared Error between simulated and actual
- MAE: Mean Absolute Error
- AIC/BIC: Model selection criteria

---

## 📈 Phase 7: Results Verification (10 minutes)

### Step 7.1: Check Output Files
```r
# Verify all outputs exist
output_files <- list(
  garch_summary = "outputs/manual/garch_fitting/model_summary.csv",
  nf_training = "outputs/manual/nf_models/training_summary.json",
  simulation_results = "results/consolidated/NF_GARCH_Results_manual.xlsx"
)

sapply(output_files, file.exists)
# All should return TRUE
```

### Step 7.2: Validate Mathematical Properties

```r
# Load results
library(openxlsx)
results <- read.xlsx("results/consolidated/NF_GARCH_Results_manual.xlsx", 
                     sheet = "Chrono_Split_NF_GARCH")

# Check each model-asset combination:
# 1. AIC and BIC are finite and reasonable
all(is.finite(results$AIC))
all(is.finite(results$BIC))
summary(results$AIC)  # Should show reasonable range

# 2. MSE and MAE are positive
all(results$MSE > 0)
all(results$MAE > 0)

# 3. Log-likelihood is negative (typical)
all(results$LogLikelihood < 0)

# 4. Compare models - lower AIC/BIC is better
results %>%
  group_by(Model) %>%
  summarise(
    Mean_AIC = mean(AIC, na.rm = TRUE),
    Mean_BIC = mean(BIC, na.rm = TRUE),
    Mean_MSE = mean(MSE, na.rm = TRUE)
  ) %>%
  arrange(Mean_AIC)
```

---

## 🎓 Mathematical Reference Checklist

Use this checklist to verify correctness at each step:

### GARCH Model Equations ✓

**sGARCH(1,1):**
- Return: r_t = μ + ε_t
- Error: ε_t = σ_t * z_t (where z_t ~ N(0,1) or t-distribution)
- Variance: σ²_t = ω + αε²_{t-1} + βσ²_{t-1}
- Constraints: ω > 0, α ≥ 0, β ≥ 0, α + β < 1

**eGARCH(1,1):**
- Return: r_t = μ + ε_t
- Log-variance: log(σ²_t) = ω + α(|z_{t-1}| - E|z_t|) + γz_{t-1} + βlog(σ²_{t-1})
- Constraints: β < 1 (stationarity)

**gjrGARCH(1,1):**
- Variance: σ²_t = ω + αε²_{t-1} + γε²_{t-1}I_{ε<0} + βσ²_{t-1}
- Constraints: ω > 0, α ≥ 0, γ ≥ 0, β ≥ 0, α + γ/2 + β < 1

**TGARCH(1,1):**
- Variance: σ²_t = ω + (α + ηI_{ε<0})ε²_{t-1} + βσ²_{t-1}
- Constraints: Similar to gjrGARCH

### Likelihood Functions ✓

**Normal Distribution:**
- Log-likelihood: LL = -½Σ[log(2π) + log(σ²_t) + (ε_t/σ_t)²]

**Student-t Distribution:**
- Log-likelihood: Includes gamma functions and degrees of freedom ν

### Residual Standardization ✓

- Standardized residual: z_t = ε_t / σ_t
- Should have: E[z_t] ≈ 0, Var(z_t) ≈ 1
- Should be: i.i.d. (independent and identically distributed)

### NF-GARCH Simulation ✓

1. Initialize: σ²_0 = unconditional variance
2. For each time step t:
   - Draw synthetic innovation: z_synth ~ NF(learned distribution)
   - Update variance: σ²_t = GARCH_recursion(σ²_{t-1}, ε_{t-1})
   - Generate return: r_t = μ + σ_t * z_synth
   - Calculate error: ε_t = r_t - μ

---

## 🐛 Troubleshooting

### If GARCH fitting fails:
1. Check data quality (no NAs, sufficient observations > 500)
2. Try different initial parameter values
3. Check convergence code (should be 0 for success)
4. Verify returns are stationary

### If NF training fails:
1. Verify residual files exist and have data
2. Check Python packages are installed correctly
3. Ensure enough memory (NF training can be memory-intensive)
4. Try reducing batch size or epochs

### If simulation fails:
1. Verify synthetic residuals are loaded correctly
2. Check GARCH model fits exist
3. Ensure file paths are correct
4. Verify engine selector is using "manual" engine

---

## ✅ Final Verification

After completing all phases, verify:

1. ✓ All output files exist
2. ✓ All models converged (convergence rate > 90%)
3. ✓ Residuals are properly standardized (mean ≈ 0, var ≈ 1)
4. ✓ Synthetic residuals preserve distribution (KS test p > 0.05)
5. ✓ Simulation produces realistic returns (volatility clustering, fat tails)
6. ✓ Results Excel file contains all expected sheets
7. ✓ Mathematical equations are correctly implemented (verification script passes)

**You're done!** The manual engine pipeline has been successfully executed with mathematical verification at each step.

This guide provides step-by-step instructions for running the Financial-SDG-GARCH pipeline manually in R Studio and Jupyter Notebook with significant optimizations.

## 🚀 Optimization Summary

- **Asset Reduction**: 50% time savings (12 → 6 assets)
- **Model Reduction**: 40% time savings (5 → 3 models)  
- **CV Optimization**: 60% time savings (5 → 3 folds, optimized windows)
- **NF Training**: 25% time savings (100 → 75 epochs, optimized architecture)
- **Total Expected Time Savings**: 70-80%
- **Expected Execution Time**: 45-90 minutes

## 📋 Prerequisites

### R Packages Required
```r
install.packages(c(
  "rugarch", "quantmod", "xts", "PerformanceAnalytics", 
  "FinTS", "tidyverse", "dplyr", "tidyr", "stringr", 
  "ggplot2", "openxlsx", "moments", "tseries", 
  "forecast", "lmtest", "parallel", "doParallel"
))
```

### Python Packages Required
```bash
pip install torch nflows numpy pandas matplotlib scipy psutil
```

## 🔧 Manual Execution Steps

### Phase 1: R Studio - Data Preparation & GARCH Fitting (30 minutes)

#### Step 1: Load Configuration
```r
# Load optimized configuration
source("scripts/manual/manual_optimized_config.R")

# Verify optimization settings
print_optimization_summary()
```

#### Step 2: Run Optimized GARCH Fitting
```r
# Run the optimized GARCH fitting script
source("scripts/manual/manual_garch_fitting.R")
```

**What this does:**
- Loads 6 optimized assets (3 FX + 3 Equity)
- Fits 3 optimized GARCH models (sGARCH, eGARCH, TGARCH)
- Runs optimized time-series cross-validation (3 folds instead of 5)
- Saves residuals for NF training

**Expected Output:**
- `outputs/manual/garch_fitting/model_summary.csv`
- `outputs/manual/residuals_by_model/` (residuals for each model-asset combination)

### Phase 2: Jupyter Notebook - NF Training (20 minutes)

#### Step 1: Open Jupyter Notebook
```bash
jupyter notebook
```

#### Step 2: Create New Notebook or Use Existing
Create a new notebook or use: `archive/Manual Scripts/Python - NF Main Training/NFGARCH - Train all Residuals.ipynb`

#### Step 3: Run Optimized NF Training
```python
# Cell 1: Import and setup
exec(open('scripts/manual/manual_nf_training.py').read())

# Cell 2: Run main training pipeline
training_results, all_samples = main()
```

**What this does:**
- Trains NF models on GARCH residuals (75 epochs instead of 100)
- Uses optimized architecture (4 layers, 64 hidden features)
- Implements early stopping and validation
- Generates synthetic residuals

**Expected Output:**
- `outputs/manual/nf_models/` (trained NF models)
- `outputs/manual/nf_models/*_synthetic_residuals.csv` (synthetic data)

### Phase 3: R Studio - Simulation & Evaluation (15 minutes)

#### Step 1: Run NF-GARCH Simulation
```r
# Load the main simulation script with manual optimizations
source("scripts/simulation_forecasting/simulate_nf_garch_engine.R")
```

#### Step 2: Run Core Evaluation
```r
# Run only essential evaluation metrics
source("scripts/evaluation/core_metrics.R")
```

**What this does:**
- Simulates NF-GARCH models with synthetic residuals
- Evaluates core metrics (RMSE, MAE, KS distance, VaR)
- Skips non-essential evaluations for speed

## 📊 Optimization Details

### Asset Selection (50% Reduction)
**Selected Assets:**
- **FX**: EURUSD, GBPUSD, USDZAR (3 most liquid)
- **Equity**: NVDA, MSFT, AMZN (3 most volatile)

**Excluded Assets:**
- **FX**: GBPCNY, GBPZAR, EURZAR
- **Equity**: PG, CAT, WMT

### Model Selection (40% Reduction)
**Selected Models:**
- **sGARCH**: Standard GARCH with skewed t-distribution
- **eGARCH**: Exponential GARCH with asymmetric effects
- **TGARCH**: Threshold GARCH with regime-dependent behavior

**Excluded Models:**
- **sGARCH_norm**: Standard GARCH with normal distribution
- **gjrGARCH**: Glosten-Jagannathan-Runkle GARCH

### CV Optimization (60% Time Savings)
**Optimized Parameters:**
- **Folds**: 3 (reduced from 5)
- **Window Size**: 50% (reduced from 65%)
- **Step Size**: 15% (increased from 10%)
- **Max Windows**: 3 (reduced from 4)
- **Forecast Horizon**: 15 (reduced from 20)

### NF Training Optimization (25% Time Savings)
**Optimized Parameters:**
- **Epochs**: 75 (reduced from 100)
- **Batch Size**: 512 (increased from 256)
- **Layers**: 4 (reduced from 5)
- **Hidden Features**: 64 (reduced from 128)
- **Early Stopping**: Enabled (patience = 15)

## 🔍 Monitoring and Debugging

### Performance Monitoring
```r
# In R Studio - Monitor execution time
start_time <- Sys.time()
# Your code here
end_time <- Sys.time()
cat("Execution time:", end_time - start_time, "\n")
```

```python
# In Jupyter - Monitor memory usage
monitor_memory()
clear_memory()  # Clear GPU/CPU memory
```

### Common Issues and Solutions

#### Issue 1: Memory Errors
**Solution:**
```r
# Clear memory between operations
gc()
```

```python
# Clear GPU memory
clear_memory()
```

#### Issue 2: Convergence Failures
**Solution:**
- Check data quality
- Reduce model complexity
- Increase tolerance parameters

#### Issue 3: Slow Execution
**Solution:**
- Enable parallel processing
- Reduce batch sizes
- Skip non-essential components

## 📈 Expected Results

### Performance Metrics
- **Total Execution Time**: 45-90 minutes
- **Memory Usage**: < 8GB
- **Success Rate**: > 90%
- **Convergence Rate**: > 85%

### Output Files
```
outputs/manual/
├── garch_fitting/
│   ├── model_summary.csv
│   └── detailed_results.rds
├── residuals_by_model/
│   ├── sGARCH/
│   ├── eGARCH/
│   └── TGARCH/
├── nf_models/
│   ├── sGARCH_*_synthetic_residuals.csv
│   ├── eGARCH_*_synthetic_residuals.csv
│   └── TGARCH_*_synthetic_residuals.csv
└── evaluation/
    ├── core_metrics.csv
    └── simulation_results.csv
```

## 🎯 Quick Start Commands

### Ultra-Fast Execution (45 minutes)
```r
# R Studio
source("scripts/manual/manual_optimized_config.R")
source("scripts/manual/manual_garch_fitting.R")
```

```python
# Jupyter Notebook
exec(open('scripts/manual/manual_nf_training.py').read())
training_results, all_samples = main()
```

### Balanced Execution (90 minutes)
```r
# R Studio - Full pipeline with more assets/models
source("scripts/core/config.R")  # Use full config
source("scripts/model_fitting/fit_garch_models.R")
source("scripts/model_fitting/extract_residuals.R")
source("scripts/simulation_forecasting/simulate_nf_garch_engine.R")
```

## 📝 Notes

1. **First Run**: May take longer due to package compilation
2. **Memory**: Ensure at least 8GB RAM available
3. **GPU**: Optional but recommended for NF training
4. **Parallel Processing**: Automatically enabled where possible
5. **Checkpoints**: Results are saved incrementally

## 🆘 Troubleshooting

### If Scripts Fail
1. Check file paths and working directory
2. Verify all required packages are installed
3. Check memory usage and clear if needed
4. Review error messages in console output

### If Results Are Incomplete
1. Check convergence rates in model summary
2. Verify residual files were generated
3. Ensure NF training completed successfully
4. Check output directories for missing files

## 📞 Support

For issues or questions:
1. Check the `outputs/manual/` directory for logs
2. Review error messages in console output
3. Verify optimization settings with `print_optimization_summary()`
4. Check memory usage with monitoring functions

