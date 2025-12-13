# NF-GARCH Mathematical Correctness Audit

**Generated**: 2025-01-XX  
**Examiner**: Senior Academic Reviewer  
**Severity Levels**: 🔴 CRITICAL / ⚠️ MAJOR / ⚠️ MINOR

---

## Executive Summary

This audit examines the mathematical correctness of GARCH model definitions, NF residual handling, evaluation metrics, and data leakage prevention. **Multiple critical issues identified** that could invalidate results.

---

## 1. GARCH Model Definitions

### 1.1 Parameter Constraints

**File**: `scripts/manual_garch/manual_garch_core.R` (lines 8-82)

#### sGARCH Parameter Transformation

**Code** (lines 9-25):
```r
mu <- theta[1]                                    # Mean parameter (unconstrained)
omega <- exp(theta[2])                            # Constant term (ω > 0)
alpha <- 1 / (1 + exp(-theta[3]))                 # ARCH parameter (α ∈ (0,1))
beta_raw <- 1 / (1 + exp(-theta[4]))              # Raw GARCH parameter (β_raw ∈ (0,1))
beta <- (1 - 1e-4) * (1 - alpha) * beta_raw      # Constrained β ensuring α + β < 1
```

**Verification**:
- ✅ **ω > 0**: Correctly enforced via `exp(theta[2])`
- ✅ **α ∈ (0,1)**: Correctly enforced via sigmoid
- ⚠️ **Stationarity Constraint**: `beta <- (1 - 1e-4) * (1 - alpha) * beta_raw`
  - **Issue**: This ensures `α + β < 1 - 1e-4`, but the constraint is applied to `beta_raw` which is already in (0,1)
  - **Mathematical Correctness**: The constraint `α + β < 1` is satisfied, but the implementation is non-standard
  - **Impact**: MINOR - Works but may limit parameter space unnecessarily

#### GJR-GARCH Parameter Transformation

**Code** (lines 26-44):
```r
mu <- theta[1]
omega <- exp(theta[2])
alpha <- 1 / (1 + exp(-theta[3]))
gamma <- theta[4]                                 # Leverage parameter (unconstrained)
beta_raw <- 1 / (1 + exp(-theta[5]))
beta <- (1 - 1e-4) * (1 - alpha) * beta_raw
```

**Verification**:
- ✅ **ω > 0**: Correct
- ✅ **α ∈ (0,1)**: Correct
- ⚠️ **γ (leverage)**: Unconstrained - this is correct for GJR-GARCH
- ⚠️ **Stationarity**: Same issue as sGARCH - constraint may be too restrictive

#### eGARCH Parameter Transformation

**Code** (lines 45-62):
```r
mu <- theta[1]
omega <- theta[2]                                 # Constant term (log-variance, unconstrained)
alpha <- theta[3]                                 # ARCH parameter (unconstrained)
gamma <- theta[4]                                 # Leverage parameter (unconstrained)
beta <- 1 / (1 + exp(-theta[5]))                 # GARCH parameter (β ∈ (0,1))
```

**Verification**:
- ✅ **eGARCH Parameters**: All unconstrained except β, which is correct for eGARCH
- ✅ **β ∈ (0,1)**: Correctly enforced

#### TGARCH Parameter Transformation

**Code** (lines 63-81):
```r
mu <- theta[1]
omega <- exp(theta[2])
alpha <- 1 / (1 + exp(-theta[3]))
eta <- theta[4]                                   # Threshold parameter (unconstrained)
beta <- 1 / (1 + exp(-theta[5]))
```

**Verification**:
- ✅ **Parameters**: Correctly transformed
- ⚠️ **Stationarity**: No explicit stationarity constraint for TGARCH - need to verify

### 1.2 GARCH Recursion Equations

#### sGARCH Recursion

**File**: `scripts/utils/utils_nf_garch.R` (lines 14-26)

**Code**:
```r
h_t <- max(omega + alpha * (eps_prev^2) + beta * h_prev, var_floor)
eps_t <- sqrt(h_t) * z[t]
r[t]  <- mu + eps_t
```

**Mathematical Verification**:
- ✅ **Correct**: `h_t = ω + α·ε²_{t-1} + β·h_{t-1}` matches standard GARCH(1,1)
- ✅ **Variance Floor**: `var_floor=1e-12` prevents numerical issues

#### GJR-GARCH Recursion

**File**: `scripts/utils/utils_nf_garch.R` (lines 28-41)

**Code**:
```r
Ineg <- as.numeric(eps_prev < 0)
h_t <- max(omega + (alpha + gamma * Ineg) * (eps_prev^2) + beta * h_prev, var_floor)
```

**Mathematical Verification**:
- ✅ **Correct**: `h_t = ω + (α + γ·I_{t-1})·ε²_{t-1} + β·h_{t-1}` matches GJR-GARCH
- ✅ **Indicator**: `Ineg = 1` if `eps_prev < 0`, correct

#### eGARCH Recursion

**File**: `scripts/utils/utils_nf_garch.R` (lines 43-59)

**Code**:
```r
logh_t <- omega + beta * log(h_prev) + alpha * (abs(z_prev) - Ezabs) + gamma * z_prev
h_t <- max(exp(logh_t), var_floor)
```

**Mathematical Verification**:
- ✅ **Correct**: `log(σ²_t) = ω + β·log(σ²_{t-1}) + α·(|z_{t-1}| - E|z|) + γ·z_{t-1}`
- ⚠️ **E|z| Calculation**: `Ezabs <- mean(abs(z_nf), na.rm = TRUE)` (line 124)
  - **Issue**: For eGARCH, E|z| should be the theoretical expectation, not the sample mean
  - **For Normal**: E|z| = √(2/π) ≈ 0.798
  - **For Student-t**: E|z| = √(ν/π) · Γ((ν-1)/2) / Γ(ν/2)
  - **Impact**: MAJOR - Using sample mean instead of theoretical expectation may bias eGARCH estimates
  - **Fix**: Use theoretical E|z| based on distribution assumption

**File**: `scripts/manual_garch/manual_garch_core.R` (lines 196-200):
```r
E_abs_t <- function(nu) {
  if (nu <= 2) stop("nu must be > 2")
  sqrt(nu/pi) * gamma((nu-1)/2) / gamma(nu/2)
}
```
- ✅ **Correct**: Theoretical E|z| for Student-t is implemented
- ⚠️ **Issue**: Not consistently used in eGARCH simulation

#### TGARCH Recursion

**File**: `scripts/utils/utils_nf_garch.R` (lines 200-214)

**Code**:
```r
sigma_t <- omega + alpha1*abs(eps_tm1) + eta11*as.numeric(eps_tm1 < 0)*abs(eps_tm1) + beta1*sqrt(pmax(sigma2_tm1, var_floor))
```

**Mathematical Verification**:
- ✅ **Correct**: TGARCH uses absolute residuals: `σ_t = ω + α·|ε_{t-1}| + η·I(ε_{t-1}<0)·|ε_{t-1}| + β·σ_{t-1}`
- ⚠️ **Note**: This is the absolute-value TGARCH specification, which is correct

### 1.3 Log-Likelihood Calculations

**File**: `scripts/manual_garch/manual_garch_core.R` (lines 104-118)

**Code**:
```r
compute_ll_normal <- function(returns, sigma, mu) {
  residuals <- returns - mu
  z <- residuals / sigma
  sum(dnorm_ll(z) - log(sigma))
}

dnorm_ll <- function(z) {
  -0.5 * (log(2 * pi) + z^2)
}
```

**Mathematical Verification**:
- ✅ **Correct**: Log-likelihood for normal: `LL = Σ[log(φ(z_t)) - log(σ_t)]`
  - Where `φ(z)` is standard normal density: `-0.5·(log(2π) + z²)`
  - The `-log(sigma)` term accounts for the Jacobian of the transformation

**Student-t Log-Likelihood** (lines 112-118):
```r
compute_ll_student_t <- function(returns, sigma, mu, nu) {
  residuals <- returns - mu
  z <- residuals / sigma
  sum(dt_ll(z, nu) - log(sigma))
}

dt_ll <- function(z, nu) {
  lgamma((nu + 1) / 2) - lgamma(nu / 2) - 0.5 * log(pi * nu) - 
    ((nu + 1) / 2) * log(1 + z^2 / nu)
}
```

**Mathematical Verification**:
- ✅ **Correct**: Student-t log-likelihood formula is correct

---

## 2. NF Residual Training & Standardization

### 2.1 🔴 CRITICAL: Multiple Standardization Points

**Issue**: NF residual standardization appears in **3+ locations** with inconsistent logic.

#### Location 1: `scripts/utils/utils_nf_garch.R` (lines 4-10)

```r
.standardize_nf <- function(z) {
  z <- as.numeric(z)
  z <- z - mean(z, na.rm = TRUE)
  sdv <- sd(z, na.rm = TRUE)
  if (!is.finite(sdv) || sdv == 0) stop("NF shocks have zero/invalid variance after centering.")
  z / sdv
}
```

**Analysis**: This function standardizes: `z_std = (z - mean(z)) / sd(z)`
- ✅ **Formula**: Correct
- ⚠️ **Usage**: Not consistently called

#### Location 2: `scripts/simulation_forecasting/simulate_nf_garch_engine.R` (lines 360-373)

```r
# CRITICAL FIX: Standardize NF residuals (mean ≈ 0, SD ≈ 1)
residual_values <- as.numeric(residual_values)
residual_values <- residual_values[!is.na(residual_values)]
if (length(residual_values) > 0) {
  resid_mean <- mean(residual_values, na.rm = TRUE)
  resid_sd <- sd(residual_values, na.rm = TRUE)
  if (!is.finite(resid_sd) || resid_sd == 0) {
    cat("WARNING: NF residuals have zero/invalid variance for", fname_clean, "- skipping\n")
    next
  }
  residual_values <- (residual_values - resid_mean) / resid_sd
}
```

**Analysis**:
- ✅ **Formula**: Correct `(z - mean) / sd`
- ⚠️ **Issue**: This is applied when **loading** NF residuals from CSV files
- ⚠️ **Question**: Why are NF residuals not already standardized when saved?

#### Location 3: `scripts/simulation_forecasting/simulate_nf_garch_engine.R` (lines 203-215, 419-433)

**Code** (lines 211-215):
```r
# Double-check standardization (should already be done, but ensure it)
resid_mean <- mean(nf_resid_vec, na.rm = TRUE)
resid_sd <- sd(nf_resid_vec, na.rm = TRUE)
if (abs(resid_mean) > 0.1 || abs(resid_sd - 1) > 0.1) {
  nf_resid_vec <- (nf_resid_vec - resid_mean) / resid_sd
}
```

**Analysis**:
- ⚠️ **Issue**: "Double-check" standardization suggests uncertainty about whether residuals are standardized
- ⚠️ **Threshold**: `abs(resid_mean) > 0.1 || abs(resid_sd - 1) > 0.1` - arbitrary thresholds
- ⚠️ **Impact**: If residuals are already standardized, this should be a no-op, but the logic suggests they might not be

### 2.2 NF Training Standardization

**File**: `scripts/manual/manual_nf_training.py` (lines 152-154)

**Code**:
```r
residuals = pd.read_csv(file_path).values.astype(np.float32)
residuals = residuals[~np.isnan(residuals)].flatten().reshape(-1, 1)
```

**Analysis**:
- ⚠️ **Issue**: NF training loads residuals **without explicit standardization**
- ⚠️ **Assumption**: Residuals are assumed to be already standardized (from GARCH fitting)
- ⚠️ **Question**: Are GARCH standardized residuals guaranteed to have mean=0, SD=1?

### 2.3 GARCH Residual Standardization

**File**: `scripts/model_fitting/extract_residuals.R` (line 403)

**Code**:
```r
resid_vec <- residuals(fit, standardize = TRUE)
```

**File**: `scripts/manual_garch/fit_sgarch_manual.R` (line 128)

**Code**:
```r
std_residuals <- residuals / sigma
```

**Mathematical Verification**:
- ✅ **Correct**: Standardized residuals = `(r_t - μ) / σ_t` = `ε_t / σ_t` = `z_t`
- ✅ **Expected Properties**: `E[z_t] = 0`, `Var(z_t) = 1` (for correctly specified model)

**Issue**: However, **sample** mean and SD of standardized residuals may not be exactly 0 and 1 due to:
1. Estimation error in GARCH parameters
2. Finite sample effects
3. Model misspecification

### 2.4 🔴 CRITICAL: Standardization Inconsistency

**Root Cause Analysis**:

1. **GARCH residuals** are standardized as `z = ε / σ` (correct)
2. **NF training** assumes residuals are already standardized (reasonable)
3. **NF generation** produces synthetic residuals (should be standardized)
4. **NF-GARCH simulation** re-standardizes residuals **multiple times** (problematic)

**Impact**: 
- 🔴 **CRITICAL**: If NF residuals are standardized multiple times, they will be over-standardized
- 🔴 **CRITICAL**: Inconsistent standardization across code paths
- ⚠️ **MAJOR**: "CRITICAL FIX" comments suggest this was a known bug that was patched, not properly fixed

**Recommendation**:
1. **Standardize once** when GARCH residuals are extracted
2. **Verify** NF training receives standardized residuals
3. **Verify** NF generation produces standardized residuals
4. **Remove** all "double-check" standardization logic
5. **Add** unit tests to verify standardization at each step

---

## 3. NF Residual Injection into GARCH

### 3.1 Residual Injection Logic

**File**: `scripts/utils/utils_nf_garch.R` (lines 14-26)

**Code**:
```r
h_t <- max(omega + alpha * (eps_prev^2) + beta * h_prev, var_floor)
eps_t <- sqrt(h_t) * z[t]
r[t]  <- mu + eps_t
```

**Mathematical Verification**:
- ✅ **Correct**: `ε_t = σ_t · z_t` where `z_t` is standardized innovation
- ✅ **Correct**: Volatility recursion uses `ε_{t-1}` (not `z_{t-1}`) correctly

### 3.2 Initial Conditions

**File**: `scripts/utils/utils_nf_garch.R` (lines 17, 31, 47)

**Code**:
```r
h_prev <- max(as.numeric(sigma0)^2, var_floor)
eps_prev <- as.numeric(eps0)
```

**Analysis**:
- ✅ **Correct**: Initial variance and residual are extracted from fitted model
- ✅ **Safety Floor**: `var_floor` prevents numerical issues

### 3.3 Variance Floors

**File**: `scripts/utils/utils_nf_garch.R` (var_floor=1e-12)

**Analysis**:
- ✅ **Reasonable**: `1e-12` is small enough to not bias results significantly
- ⚠️ **Note**: Should verify that variance floors don't accumulate bias in long simulations

---

## 4. Evaluation Metrics

### 4.1 Kupiec Test

**File**: `scripts/evaluation/var_backtesting_comprehensive.R` (lines 18-52)

**Code**:
```r
kupiec_test <- function(exceedances, total_obs, confidence_level = 0.95) {
  expected_rate <- 1 - confidence_level
  observed_rate <- exceedances / total_obs
  
  if (exceedances == 0) {
    LR_stat <- -2 * total_obs * log(expected_rate)
  } else if (exceedances == total_obs) {
    LR_stat <- -2 * total_obs * log(1 - expected_rate)
  } else {
    LR_stat <- -2 * (
      exceedances * log(observed_rate) + 
      (total_obs - exceedances) * log(1 - observed_rate) -
      exceedances * log(expected_rate) - 
      (total_obs - exceedances) * log(1 - expected_rate)
    )
  }
  
  pvalue <- 1 - pchisq(LR_stat, df = 1)
  reject <- pvalue < 0.05
}
```

**Mathematical Verification**:
- ✅ **Correct**: Likelihood ratio test statistic: `LR = -2·log(L_0 / L_1)`
  - `L_0`: Likelihood under H₀ (expected rate)
  - `L_1`: Likelihood under H₁ (observed rate)
- ✅ **Correct**: `LR ~ χ²(1)` under H₀
- ✅ **Correct**: Edge cases (0 or total_obs exceedances) handled

### 4.2 Christoffersen Test

**File**: `scripts/evaluation/var_backtesting_comprehensive.R` (lines 55-99)

**Code**:
```r
christoffersen_test <- function(exceedances_vec) {
  # Count transitions
  n00 <- sum(exceedances_vec[1:(length(exceedances_vec) - 1)] == 0 & 
             exceedances_vec[2:length(exceedances_vec)] == 0)
  n01 <- sum(exceedances_vec[1:(length(exceedances_vec) - 1)] == 0 & 
             exceedances_vec[2:length(exceedances_vec)] == 1)
  n10 <- sum(exceedances_vec[1:(length(exceedances_vec) - 1)] == 1 & 
             exceedances_vec[2:length(exceedances_vec)] == 0)
  n11 <- sum(exceedances_vec[1:(length(exceedances_vec) - 1)] == 1 & 
             exceedances_vec[2:length(exceedances_vec)] == 1)
  
  pi_01 <- if (n00 + n01 > 0) n01 / (n00 + n01) else 0
  pi_11 <- if (n10 + n11 > 0) n11 / (n10 + n11) else 0
  pi_total <- sum(exceedances_vec) / length(exceedances_vec)
  
  LR_stat <- -2 * (
    n00 * log(1 - pi_01) + n01 * log(pi_01) +
    n10 * log(1 - pi_11) + n11 * log(pi_11) -
    (n00 + n10) * log(1 - pi_total) - (n01 + n11) * log(pi_total)
  )
  
  pvalue <- 1 - pchisq(LR_stat, df = 1)
}
```

**Mathematical Verification**:
- ✅ **Correct**: Transition matrix counting is correct
- ✅ **Correct**: Likelihood ratio test for independence
- ✅ **Correct**: `LR ~ χ²(1)` under H₀ (independence)
- ⚠️ **Edge Case**: `if (n00 + n01 == 0 || n10 + n11 == 0)` returns `LR_stat = 0`, which may not be correct
  - **Impact**: MINOR - Rare edge case

### 4.3 Forecast Loss Metrics

**File**: `scripts/simulation_forecasting/simulate_nf_garch_engine.R` (lines 447-448)

**Code**:
```r
mse <- mean((asset_returns - fitted_values)^2, na.rm = TRUE)
mae <- mean(abs(asset_returns - fitted_values), na.rm = TRUE)
```

**Mathematical Verification**:
- ✅ **MSE**: Correct definition
- ✅ **MAE**: Correct definition
- ⚠️ **Issue**: `fitted_values` are **simulated returns**, not forecasts
  - **Impact**: MAJOR - MSE/MAE are comparing actual returns to simulated returns, not forecasts
  - **Question**: Is this the intended evaluation? Should be comparing forecasts to actuals

---

## 5. Data Leakage Checks

### 5.1 Chronological Split

**File**: `scripts/model_fitting/extract_residuals.R` (lines 138-148)

**Code**:
```r
get_split_index <- function(x, split_ratio = 0.65) {
  return(floor(nrow(x) * split_ratio))
}

fx_train_returns <- lapply(fx_returns, function(x) x[1:get_split_index(x)])
fx_test_returns  <- lapply(fx_returns, function(x) x[(get_split_index(x) + 1):nrow(x)])
```

**Verification**:
- ✅ **Correct**: Strictly chronological split (65% train, 35% test)
- ✅ **No Future Data**: Training set ends before test set begins
- ✅ **Deterministic**: Split is deterministic (no randomness)

### 5.2 Time-Series Cross-Validation

**File**: `scripts/model_fitting/extract_residuals.R` (lines 153-207)

**Code**:
```r
ts_cross_validate <- function(returns, model_type, dist_type = "sstd", submodel = NULL, 
                              window_size = 500, step_size = 50, forecast_horizon = 20) {
  for (start_idx in seq(1, n - window_size - forecast_horizon, by = step_size)) {
    train_set <- returns[start_idx:(start_idx + window_size - 1)]
    test_set  <- returns[(start_idx + window_size):(start_idx + window_size + forecast_horizon - 1)]
```

**Verification**:
- ✅ **Correct**: Sliding windows don't overlap test sets with training data
- ✅ **No Leakage**: Test set starts after training set ends
- ⚠️ **Note**: Windows may overlap in training data (acceptable for time-series CV)

### 5.3 NF Training Data

**File**: `scripts/manual/manual_nf_training.py` (lines 152-159)

**Code**:
```r
residuals = pd.read_csv(file_path).values.astype(np.float32)
# Split data for validation
n_train = int(len(residuals) * (1 - config["validation_split"]))
train_residuals = residuals[:n_train]
val_residuals = residuals[n_train:]
```

**Verification**:
- ✅ **Correct**: NF training uses only training residuals (from GARCH fitting on training data)
- ✅ **No Leakage**: Test residuals are not used for NF training

### 5.4 Normalization Statistics

**Verification**:
- ✅ **No Leakage**: No normalization statistics computed on full sample before splitting
- ✅ **Correct**: Standardization is done per-model, per-asset, on training data only

---

## 6. Summary of Issues

### 🔴 CRITICAL Issues

1. **Multiple Standardization Points** (Section 2.1, 2.4)
   - NF residuals standardized 3+ times
   - Inconsistent logic with "CRITICAL FIX" comments
   - **Impact**: Results may be invalid due to over-standardization
   - **Fix**: Standardize once, verify at each step, remove redundant standardization

2. **eGARCH E|z| Calculation** (Section 1.2)
   - Using sample mean instead of theoretical expectation
   - **Impact**: May bias eGARCH parameter estimates
   - **Fix**: Use theoretical E|z| based on distribution assumption

### ⚠️ MAJOR Issues

1. **Forecast vs Simulation Confusion** (Section 4.3)
   - MSE/MAE comparing actual returns to simulated returns, not forecasts
   - **Impact**: Evaluation metrics may not measure what is claimed
   - **Fix**: Clarify whether this is intended or use proper forecast evaluation

2. **Stationarity Constraints** (Section 1.1)
   - Non-standard implementation may unnecessarily restrict parameter space
   - **Impact**: MINOR - Works but may limit model flexibility

### ⚠️ MINOR Issues

1. **Christoffersen Test Edge Case** (Section 4.2)
   - Edge case handling may not be mathematically correct
   - **Impact**: MINOR - Rare edge case

2. **Variance Floor Bias** (Section 3.3)
   - Need to verify variance floors don't accumulate bias
   - **Impact**: MINOR - Likely negligible but should be verified

---

## 7. Recommendations

### Immediate Actions (Before Publication)

1. **Fix Standardization Logic**
   - Create single standardization function
   - Verify standardization at each step (GARCH → NF training → NF generation → NF-GARCH simulation)
   - Remove all "double-check" standardization
   - Add unit tests

2. **Fix eGARCH E|z|**
   - Use theoretical E|z| based on distribution assumption
   - Document which E|z| is used for each distribution

3. **Clarify Evaluation Metrics**
   - Document whether MSE/MAE are comparing forecasts or simulations
   - If simulations, explain why this is the correct evaluation

### Before MDPI Submission

1. **Add Mathematical Verification Tests**
   - Unit tests for GARCH recursions
   - Unit tests for standardization
   - Unit tests for statistical tests

2. **Document Mathematical Assumptions**
   - Document all GARCH model equations
   - Document standardization procedures
   - Document evaluation metric definitions

---

**Next Steps**: Proceed to Software Quality Audit

