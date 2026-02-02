# Deep Dive: Why NF-GARCH Fails for sGARCH_norm

## Additional Investigations to Run

We've established that distributional incompatibility is the issue. But let's get more specific about **HOW** and **WHERE** the failure manifests.

---

## 1. Residual Quality Comparison

**Question:** Are NF residuals "worse" than standard residuals for sGARCH_norm?

### **Analysis A: Residual Diagnostics**

```r
library(rugarch)
library(moments)

# Compare residual quality
analyze_residual_quality <- function(asset, model = "sGARCH_norm") {
  
  # Load standard GARCH residuals
  std_resid <- read.csv(paste0("outputs/manual/residuals_by_model/", 
                                model, "/", asset, "_Manual_Optimized_residuals.csv"))
  
  # Load NF synthetic residuals  
  nf_resid <- read.csv(paste0("outputs/manual/nf_models/", 
                               model, "_", asset, "_synthetic_residuals.csv"))
  
  # Key diagnostics
  diagnostics <- data.frame(
    Asset = asset,
    
    # Moments
    std_mean = mean(std_resid$residuals),
    nf_mean = mean(nf_resid$synthetic_residuals),
    std_sd = sd(std_resid$residuals),
    nf_sd = sd(nf_resid$synthetic_residuals),
    std_skew = skewness(std_resid$residuals),
    nf_skew = skewness(nf_resid$synthetic_residuals),
    std_kurt = kurtosis(std_resid$residuals),
    nf_kurt = kurtosis(nf_resid$synthetic_residuals),
    
    # Autocorrelation (should be zero for white noise)
    std_acf1 = acf(std_resid$residuals, plot=FALSE)$acf[2],
    nf_acf1 = acf(nf_resid$synthetic_residuals, plot=FALSE)$acf[2],
    std_acf5 = mean(abs(acf(std_resid$residuals, lag.max=5, plot=FALSE)$acf[2:6])),
    nf_acf5 = mean(abs(acf(nf_resid$synthetic_residuals, lag.max=5, plot=FALSE)$acf[2:6])),
    
    # Squared autocorrelation (ARCH effects - should be zero)
    std_acf2_sq = acf(std_resid$residuals^2, plot=FALSE)$acf[2],
    nf_acf2_sq = acf(nf_resid$synthetic_residuals^2, plot=FALSE)$acf[2],
    
    # Ljung-Box tests (p-value > 0.05 = good)
    std_lb = Box.test(std_resid$residuals, lag=10, type="Ljung-Box")$p.value,
    nf_lb = Box.test(nf_resid$synthetic_residuals, lag=10, type="Ljung-Box")$p.value,
    
    # ARCH LM test (p-value > 0.05 = no ARCH effects, good)
    std_arch = ArchTest(std_resid$residuals, lags=5)$p.value,
    nf_arch = ArchTest(nf_resid$synthetic_residuals, lags=5)$p.value
  )
  
  return(diagnostics)
}

# Run for all assets
assets <- c("NVDA", "MSFT", "AMZN", "EURUSD", "GBPUSD", "USDZAR")
results <- lapply(assets, analyze_residual_quality)
results_df <- do.call(rbind, results)

print(results_df)
```

**Expected Insight:** 
- If NF residuals have higher autocorrelation → NF is adding spurious temporal patterns
- If NF residuals have residual ARCH effects → NF isn't capturing conditional heteroskedasticity correctly
- This would explain forecast degradation

---

## 2. Forecast Path Analysis

**Question:** Do NF forecasts diverge systematically from actuals in specific ways?

### **Analysis B: Forecast Error Decomposition**

```r
# Compare forecast paths
compare_forecast_paths <- function(asset, model = "sGARCH_norm") {
  
  # Load comparison results
  combined <- read.xlsx("results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx",
                        sheet = "Combined_Results")
  
  # Filter for this asset/model
  nf_results <- combined %>% filter(Asset == asset, Model == model, Source == "NF_GARCH")
  std_results <- combined %>% filter(Asset == asset, Model == model, Source == "Standard")
  
  # Load actual returns for test period
  data <- read.csv("data/processed/raw (FX + EQ).csv")
  actual_returns <- tail(data[[asset]], 1581)  # Test set size
  
  # Analyze forecast errors
  analysis <- data.frame(
    Asset = asset,
    
    # Bias (mean error)
    std_bias = mean(std_forecast - actual_returns),
    nf_bias = mean(nf_forecast - actual_returns),
    
    # Variance (forecast volatility)
    std_forecast_vol = sd(std_forecast),
    nf_forecast_vol = sd(nf_forecast),
    actual_vol = sd(actual_returns),
    
    # Over/under-prediction
    std_overpred_rate = mean(std_forecast > actual_returns),
    nf_overpred_rate = mean(nf_forecast > actual_returns),
    
    # Tail behavior
    std_capture_5pct_tails = sum(abs(actual_returns) > quantile(abs(actual_returns), 0.95)) / 
                               sum(abs(std_forecast) > quantile(abs(std_forecast), 0.95)),
    nf_capture_5pct_tails = sum(abs(actual_returns) > quantile(abs(actual_returns), 0.95)) / 
                             sum(abs(nf_forecast) > quantile(abs(nf_forecast), 0.95))
  )
  
  return(analysis)
}
```

**Expected Insight:**
- If NF forecasts are too volatile → Overfitting, adding noise
- If NF forecasts are too smooth → Underfitting, losing information
- If NF misses tail events → Distributional mismatch manifesting in extremes

---

## 3. Information Loss Analysis

**Question:** Is NF losing critical information during transformation?

### **Analysis C: Mutual Information & Entropy**

```r
library(infotheo)

# Measure information preservation
analyze_information_loss <- function(asset, model = "sGARCH_norm") {
  
  # Original residuals
  original <- read.csv(paste0("outputs/manual/residuals_by_model/", 
                               model, "/", asset, "_Manual_Optimized_residuals.csv"))
  
  # NF-transformed residuals
  nf <- read.csv(paste0("outputs/manual/nf_models/", 
                         model, "_", asset, "_synthetic_residuals.csv"))
  
  # Discretize for mutual information calculation
  orig_disc <- discretize(original$residuals, nbins = 20)
  nf_disc <- discretize(nf$synthetic_residuals, nbins = 20)
  
  # Calculate information metrics
  info <- data.frame(
    Asset = asset,
    
    # Entropy (uncertainty)
    orig_entropy = entropy(orig_disc),
    nf_entropy = entropy(nf_disc),
    entropy_ratio = entropy(nf_disc) / entropy(orig_disc),
    
    # Mutual information (how much info is preserved)
    mutual_info = mutinformation(orig_disc, nf_disc),
    
    # Normalized mutual information (0 = no info, 1 = perfect preservation)
    norm_mi = mutinformation(orig_disc, nf_disc) / min(entropy(orig_disc), entropy(nf_disc)),
    
    # KL divergence (distribution difference)
    kl_div = sum(hist(original$residuals, plot=FALSE)$density * 
                 log(hist(original$residuals, plot=FALSE)$density / 
                     hist(nf$synthetic_residuals, plot=FALSE)$density), na.rm=TRUE)
  )
  
  return(info)
}
```

**Expected Insight:**
- If `norm_mi < 0.7` → NF is losing critical information
- If `kl_div` is large → NF is significantly changing distribution
- Lower mutual information for assets where NF fails worse

---

## 4. Temporal Dynamics Analysis

**Question:** Does NF change the time-series structure in harmful ways?

### **Analysis D: Dynamic Properties**

```r
# Test if NF preserves temporal structure
analyze_temporal_dynamics <- function(asset, model = "sGARCH_norm") {
  
  original <- read.csv(paste0("outputs/manual/residuals_by_model/", 
                               model, "/", asset, "_Manual_Optimized_residuals.csv"))
  nf <- read.csv(paste0("outputs/manual/nf_models/", 
                         model, "_", asset, "_synthetic_residuals.csv"))
  
  # Time series properties
  dynamics <- data.frame(
    Asset = asset,
    
    # Autocorrelation structure (up to 20 lags)
    orig_acf_sum = sum(abs(acf(original$residuals, lag.max=20, plot=FALSE)$acf[-1])),
    nf_acf_sum = sum(abs(acf(nf$synthetic_residuals, lag.max=20, plot=FALSE)$acf[-1])),
    
    # Runs test (randomness)
    orig_runs_p = runs.test(as.factor(sign(original$residuals)))$p.value,
    nf_runs_p = runs.test(as.factor(sign(nf$synthetic_residuals)))$p.value,
    
    # Turning points test
    orig_turning = turning.point.test(original$residuals)$p.value,
    nf_turning = turning.point.test(nf$synthetic_residuals)$p.value,
    
    # Variance ratio test (mean reversion)
    orig_vr = Auto.VR(original$residuals)$stat,
    nf_vr = Auto.VR(nf$synthetic_residuals)$stat
  )
  
  return(dynamics)
}
```

**Expected Insight:**
- If NF adds autocorrelation → Introduces spurious patterns
- If NF changes runs/turning points → Alters fundamental dynamics
- These changes could be harmful during multi-step forecasting

---

## 5. Volatility Forecast Quality

**Question:** Does NF produce worse conditional volatility forecasts?

### **Analysis E: Volatility Forecast Evaluation**

```r
# Evaluate σ_t forecasts separately from return forecasts
evaluate_volatility_forecasts <- function(asset, model = "sGARCH_norm") {
  
  # Load actual realized volatility (squared returns)
  data <- read.csv("data/processed/raw (FX + EQ).csv")
  actual_returns <- tail(data[[asset]], 1581)
  realized_vol <- actual_returns^2  # Realized variance proxy
  
  # Load volatility forecasts (from simulation results)
  # These would need to be saved during simulation
  std_vol_forecast <- readRDS(paste0("outputs/manual/forecasts/", 
                                     asset, "_", model, "_standard_vol.rds"))
  nf_vol_forecast <- readRDS(paste0("outputs/manual/forecasts/", 
                                    asset, "_", model, "_nf_vol.rds"))
  
  # Volatility forecast metrics
  vol_metrics <- data.frame(
    Asset = asset,
    
    # MSE for volatility
    std_vol_mse = mean((std_vol_forecast - realized_vol)^2),
    nf_vol_mse = mean((nf_vol_forecast - realized_vol)^2),
    
    # QLIKE (robust volatility metric)
    std_qlike = mean(realized_vol/std_vol_forecast - log(realized_vol/std_vol_forecast) - 1),
    nf_qlike = mean(realized_vol/nf_vol_forecast - log(realized_vol/nf_vol_forecast) - 1),
    
    # R² for volatility predictions
    std_vol_r2 = 1 - sum((realized_vol - std_vol_forecast)^2) / sum((realized_vol - mean(realized_vol))^2),
    nf_vol_r2 = 1 - sum((realized_vol - nf_vol_forecast)^2) / sum((realized_vol - mean(realized_vol))^2),
    
    # Hit rate (correct direction)
    std_hit_rate = mean(sign(std_vol_forecast - mean(std_vol_forecast)) == 
                         sign(realized_vol - mean(realized_vol))),
    nf_hit_rate = mean(sign(nf_vol_forecast - mean(nf_vol_forecast)) == 
                        sign(realized_vol - mean(realized_vol)))
  )
  
  return(vol_metrics)
}
```

**Expected Insight:**
- If NF volatility forecasts are worse → Problem is in variance equation, not just residuals
- Could reveal whether issue is distributional or structural

---

## 6. Rolling Window Performance

**Question:** Is NF failure consistent or does it vary over time?

### **Analysis F: Time-Varying Performance**

```r
# Rolling window comparison
rolling_performance_analysis <- function(asset, model = "sGARCH_norm", 
                                         window_size = 250) {
  
  # Load forecast results
  combined <- read.xlsx("results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx",
                        sheet = "Combined_Results")
  
  # This requires individual forecast paths (would need to save during simulation)
  # For now, conceptual framework:
  
  n_windows <- floor(1581 / window_size)  # Test set size
  rolling_results <- data.frame()
  
  for(i in 1:n_windows) {
    start_idx <- (i-1) * window_size + 1
    end_idx <- min(i * window_size, 1581)
    
    # Calculate MSE for this window
    window_std_mse <- calculate_window_mse(std_forecast[start_idx:end_idx], 
                                           actual[start_idx:end_idx])
    window_nf_mse <- calculate_window_mse(nf_forecast[start_idx:end_idx], 
                                          actual[start_idx:end_idx])
    
    # Calculate realized volatility for this window
    window_vol <- sd(actual[start_idx:end_idx])
    
    rolling_results <- rbind(rolling_results, data.frame(
      window = i,
      window_vol = window_vol,
      std_mse = window_std_mse,
      nf_mse = window_nf_mse,
      nf_worse = window_nf_mse > window_std_mse
    ))
  }
  
  # Analyze relationship between volatility regime and NF failure
  regime_analysis <- rolling_results %>%
    mutate(regime = cut(window_vol, breaks = 3, labels = c("Low", "Med", "High"))) %>%
    group_by(regime) %>%
    summarise(
      n = n(),
      pct_nf_worse = mean(nf_worse),
      mean_std_mse = mean(std_mse),
      mean_nf_mse = mean(nf_mse)
    )
  
  return(list(rolling = rolling_results, regime = regime_analysis))
}
```

**Expected Insight:**
- If NF fails more in specific regimes → Conditional failure, not universal
- If NF fails consistently → Fundamental incompatibility
- Could reveal if NF works better in high-vol periods but fails in low-vol

---

## 7. Model Confidence Analysis

**Question:** Is NF overconfident in its forecasts?

### **Analysis G: Prediction Intervals**

```r
# Compare forecast uncertainty
analyze_forecast_uncertainty <- function(asset, model = "sGARCH_norm") {
  
  # From 1000-path Monte Carlo, calculate prediction intervals
  # (Would need to save individual paths during simulation)
  
  std_paths <- readRDS(paste0("outputs/manual/paths/", 
                              asset, "_", model, "_standard_paths.rds"))
  nf_paths <- readRDS(paste0("outputs/manual/paths/", 
                             asset, "_", model, "_nf_paths.rds"))
  actual <- read.csv("data/processed/raw (FX + EQ).csv")[[asset]]
  actual_test <- tail(actual, 1581)
  
  # Calculate prediction intervals (5th, 50th, 95th percentiles)
  std_lower <- apply(std_paths, 1, quantile, 0.05)
  std_median <- apply(std_paths, 1, quantile, 0.50)
  std_upper <- apply(std_paths, 1, quantile, 0.95)
  
  nf_lower <- apply(nf_paths, 1, quantile, 0.05)
  nf_median <- apply(nf_paths, 1, quantile, 0.50)
  nf_upper <- apply(nf_paths, 1, quantile, 0.95)
  
  # Calibration metrics
  calibration <- data.frame(
    Asset = asset,
    
    # Coverage (should be 90% for 5-95% interval)
    std_coverage = mean(actual_test >= std_lower & actual_test <= std_upper),
    nf_coverage = mean(actual_test >= nf_lower & actual_test <= nf_upper),
    
    # Interval width (narrower = more confident)
    std_width = mean(std_upper - std_lower),
    nf_width = mean(nf_upper - nf_lower),
    
    # Sharpness (narrow AND accurate)
    std_sharpness = std_coverage / std_width,
    nf_sharpness = nf_coverage / nf_width,
    
    # Overconfidence index (coverage < nominal = overconfident)
    std_overconf = max(0, 0.90 - std_coverage),
    nf_overconf = max(0, 0.90 - nf_coverage)
  )
  
  return(calibration)
}
```

**Expected Insight:**
- If NF has narrower intervals but worse coverage → Overconfident
- If NF intervals don't contain actuals → Systematic bias
- Overconfidence would explain why point forecasts are worse

---

## 8. Feature Importance Analysis

**Question:** What features does NF learn, and are they helpful?

### **Analysis H: NF Interpretability**

```r
library(torch)

# Analyze what NF learned
interpret_nf_transformations <- function(asset, model = "sGARCH_norm") {
  
  # Load trained NF model
  nf_model <- torch_load(paste0("outputs/manual/nf_models/", 
                                model, "_", asset, "/nf_model.pth"))
  
  # Load original residuals
  original <- read.csv(paste0("outputs/manual/residuals_by_model/", 
                              model, "/", asset, "_Manual_Optimized_residuals.csv"))
  
  # Sample residuals across distribution
  test_points <- seq(-4, 4, length.out = 100)
  
  # Forward pass to see transformation
  with_no_grad({
    transformed <- nf_model$forward(torch_tensor(test_points)$unsqueeze(-1))
  })
  
  # Analyze transformation characteristics
  transformation_analysis <- data.frame(
    input = test_points,
    output = as.numeric(transformed),
    transformation = as.numeric(transformed) - test_points
  )
  
  # Key patterns
  patterns <- data.frame(
    Asset = asset,
    
    # Non-linearity strength
    nonlinearity = cor(test_points, transformation_analysis$transformation)^2,
    
    # Tail behavior
    left_tail_boost = mean(transformation_analysis$transformation[test_points < -2]),
    right_tail_boost = mean(transformation_analysis$transformation[test_points > 2]),
    
    # Center compression/expansion
    center_effect = mean(abs(transformation_analysis$transformation[abs(test_points) < 1]))
  )
  
  return(list(transformation = transformation_analysis, patterns = patterns))
}
```

**Expected Insight:**
- If NF heavily modifies tails → Learning fat-tail features (good!)
- But if GARCH assumes Gaussian → Mismatch in forecasting (bad!)
- Could visualize what transformations NF learns and why they conflict

---

## 9. Comparative Ablation Study

**Question:** What if we used NF residuals with a better-matched GARCH model?

### **Analysis I: Cross-Model Testing**

```r
# Test NF residuals from sGARCH_norm with sGARCH_sstd dynamics
test_cross_model_compatibility <- function(asset) {
  
  # Get NF residuals trained on sGARCH_norm
  nf_norm_residuals <- read.csv(paste0("outputs/manual/nf_models/", 
                                       "sGARCH_norm_", asset, 
                                       "_synthetic_residuals.csv"))
  
  # Fit sGARCH_sstd on training data
  data <- read.csv("data/processed/raw (FX + EQ).csv")
  train <- head(data[[asset]], 2934)
  test <- tail(data[[asset]], 1581)
  
  spec_sstd <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0)),
    distribution.model = "sstd"
  )
  
  fit_sstd <- ugarchfit(spec_sstd, train)
  
  # Forecast using:
  # 1. Standard sGARCH_norm + standard residuals
  # 2. NF sGARCH_norm + NF residuals (original - fails)
  # 3. sGARCH_sstd + NF residuals from norm (test compatibility)
  
  forecast_1 <- forecast_with_residuals(fit_norm, standard_residuals)
  forecast_2 <- forecast_with_residuals(fit_norm, nf_norm_residuals)  # Fails
  forecast_3 <- forecast_with_residuals(fit_sstd, nf_norm_residuals)  # Test
  
  # Compare MSE
  comparison <- data.frame(
    Asset = asset,
    std_norm_mse = mean((forecast_1 - test)^2),
    nf_norm_mse = mean((forecast_2 - test)^2),  # Should be worse
    nf_norm_sstd_mse = mean((forecast_3 - test)^2),  # Better?
    
    improvement_with_sstd = (mean((forecast_2 - test)^2) - mean((forecast_3 - test)^2)) / 
                            mean((forecast_2 - test)^2)
  )
  
  return(comparison)
}
```

**Expected Insight:**
- If using sstd dynamics improves NF-norm residuals → Confirms distributional mismatch
- Shows that NF learns correct patterns, but wrong GARCH specification ruins it
- **This would be a POWERFUL demonstration of the compatibility hypothesis!**

---

## 10. Synthetic Data Validation

**Question:** Can we reproduce the failure in controlled settings?

### **Analysis J: Simulation Study**

```r
# Generate synthetic data where we KNOW the truth
synthetic_validation <- function(n = 5000, true_dist = "sstd") {
  
  # Simulate from sGARCH with known parameters
  spec_true <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0)),
    distribution.model = true_dist,  # True distribution
    fixed.pars = list(mu = 0, omega = 0.00001, alpha1 = 0.1, beta1 = 0.85)
  )
  
  sim_data <- ugarchpath(spec_true, n.sim = n)
  returns <- as.numeric(fitted(sim_data))
  
  # Split train/test
  train <- returns[1:floor(0.65*n)]
  test <- returns[(floor(0.65*n)+1):n]
  
  # Fit WRONG model (sGARCH_norm) to data generated from sstd
  spec_wrong <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0)),
    distribution.model = "norm"  # WRONG!
  )
  
  fit_wrong <- ugarchfit(spec_wrong, train)
  
  # Extract residuals and train NF
  residuals <- residuals(fit_wrong, standardize = TRUE)
  # ... train NF on these residuals ...
  # ... forecast and compare ...
  
  # Test:
  # 1. If true_dist = "norm", does NF still fail? (Should not - correctly specified)
  # 2. If true_dist = "sstd", does NF fail for norm model? (Should - misspecified)
  
  return(results)
}

# Run with different true distributions
for(true_dist in c("norm", "std", "sstd", "ged")) {
  results <- synthetic_validation(true_dist = true_dist)
  # ... analyze ...
}
```

**Expected Insight:**
- If NF fails only when model is misspecified → Confirms hypothesis
- If NF fails even when correctly specified → Something else is wrong
- Provides controlled validation of distributional compatibility theory

---

## Recommended Execution Order

### **Quick Wins (1-2 days each):**
1. **Residual Diagnostics** (Analysis A) - Check ACF, ARCH effects
2. **Information Loss** (Analysis C) - Measure mutual information
3. **Volatility Forecast Quality** (Analysis E) - Separate vol from returns

### **Deep Investigations (1 week each):**
4. **Cross-Model Testing** (Analysis I) - Use NF-norm residuals with sstd dynamics ⭐
5. **Temporal Dynamics** (Analysis D) - Check if NF changes time structure
6. **Model Confidence** (Analysis G) - Check prediction interval calibration

### **Advanced (2+ weeks):**
7. **Synthetic Validation** (Analysis J) - Controlled experiments
8. **Feature Importance** (Analysis H) - Interpret NF transformations
9. **Rolling Window** (Analysis F) - Time-varying performance

---

## Expected Findings Summary

| Analysis | What It Tests | Expected Result for sGARCH_norm Failure |
|----------|---------------|----------------------------------------|
| **Residual Diagnostics** | Quality of NF residuals | Higher ACF, residual ARCH |
| **Forecast Paths** | Where forecasts diverge | NF misses tail events |
| **Information Loss** | NF preserves info? | Low mutual information |
| **Temporal Dynamics** | NF changes structure? | Adds autocorrelation |
| **Volatility Forecasts** | σ_t quality | Worse vol forecasts |
| **Rolling Window** | Consistent failure? | Fails in all regimes |
| **Model Confidence** | Overconfident? | Narrower, worse intervals |
| **Feature Importance** | What NF learns | Fat-tail features |
| **Cross-Model Test** | Compatibility | ⭐ Works with sstd dynamics! |
| **Synthetic Data** | Controlled validation | Fails only when misspecified |

---

## Most Valuable Analysis: Cross-Model Testing

**Why Analysis I (Cross-Model) is the smoking gun:**

If you can show that:
1. NF residuals from sGARCH_norm + sGARCH_norm dynamics = **Bad** (MSE +2%)
2. NF residuals from sGARCH_norm + sGARCH_sstd dynamics = **Good** (MSE improves)

This **proves** the distributional compatibility hypothesis!

**Implementation:**
```r
# Pseudo-code for the killer experiment:
nf_norm_resid <- train_nf(sGARCH_norm_residuals)

# Test 1: Original (fails)
forecast_norm_norm <- forecast(sGARCH_norm_model, nf_norm_resid)
mse_1 <- MSE(forecast_norm_norm, actual)  # High MSE

# Test 2: Compatible dynamics
forecast_norm_sstd <- forecast(sGARCH_sstd_model, nf_norm_resid)
mse_2 <- MSE(forecast_norm_sstd, actual)  # Lower MSE?

# If mse_2 < mse_1: NF learned correctly, model was wrong!
```

---

## Dissertation Impact

Adding these analyses would:

1. ✅ **Deepen understanding** - Show HOW failure manifests, not just that it does
2. ✅ **Strengthen argument** - Multiple lines of evidence for compatibility hypothesis
3. ✅ **Practical value** - Diagnostics practitioners can use pre-deployment
4. ✅ **Methodological rigor** - Shows thorough investigation
5. ✅ **Publishability** - Cross-model test is a novel contribution

**Recommended additions to dissertation:**
- Section 5.4: "Failure Mode Analysis for sGARCH_norm"
  - Subsection 5.4.1: Residual Quality Diagnostics
  - Subsection 5.4.2: Cross-Model Compatibility Test ⭐
  - Subsection 5.4.3: Temporal Structure Analysis

---

## Next Steps

### **This Week:**
1. Run Residual Diagnostics (Analysis A)
2. Run Information Loss (Analysis C)
3. Document findings

### **Next Week:**
1. Implement Cross-Model Test (Analysis I) ⭐⭐⭐
2. If it works → Major finding!
3. Write up as key result

### **Following Week:**
1. Run 2-3 additional analyses for robustness
2. Create diagnostic plots
3. Add to dissertation

Want me to create the R scripts to run these analyses?
