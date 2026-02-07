# COMPLETE METHODOLOGY ISSUES - ROOT CAUSE ANALYSIS

## Date: 2026-02-07

## Summary

After comprehensive investigation, I've identified **FOUR MAJOR ISSUES** in the NF-GARCH methodology that explain why it doesn't outperform Standard GARCH.

---

## ISSUE #1: FILENAME MISMATCH (Pipeline Disconnect)

### The Problem
- **NF Training expects**: `*_Manual_Optimized_residuals.csv` (line 436 of `manual_nf_training.py`)
- **GARCH Fitting saves**: `{asset}_Manual_residuals.csv` (line 415 of `manual_garch_fitting.R`)

### Impact
- NF training is using **OLD residuals from a previous pipeline run**
- These old residuals likely come from when Bug #1 (non-standardized residuals) existed
- NF was trained on INCORRECTLY standardized data

### Evidence
- No files matching `*_Manual_Optimized_residuals.csv` exist in current workspace
- `outputs/manual/residuals_by_model/` directory exists but is empty
- NF models exist in `outputs/manual/nf_models/` suggesting training completed at some point

---

## ISSUE #2: TRAINING RESIDUALS NOT STANDARDIZED (Historical Bug)

### The Problem
Empirical analysis of NF training data shows:

```
TRAINING RESIDUALS (AMZN sGARCH_norm):
  Mean = -0.463  (should be ~0.00)
  Std = 1.503    (should be ~1.00)
  Skew = 2.22
  Kurt = 28.12   (extremely leptokurtic!)
```

These residuals are **NOT properly standardized**.

### Theoretical Expected Values
For standardized GARCH residuals: z_t = eps_t / sigma_t
- Mean(z_t) should be ≈ 0
- Std(z_t) should be ≈ 1
- Skew/Kurt can vary but should be reasonable

### Root Cause
This was **Bug #1** that we supposedly fixed in `fit_garch_chronological.R` and `fit_garch_tscv.R`. However:
1. The fix was applied to the **additional_analysis** pipeline
2. The **manual pipeline** residuals were generated BEFORE this fix
3. NF training used the OLD, incorrectly saved residuals

### Code Path (What Should Happen)
```
fit_sgarch_manual.R line 217: std_residuals <- residuals / sigma
engine_selector.R line 268: return(fit$std_residuals)
manual_garch_fitting.R line 204: residuals <- engine_residuals(garch_fit, standardize = TRUE)
manual_garch_fitting.R line 405: residuals_vec <- as.numeric(fit_result$residuals)
manual_garch_fitting.R line 416: write.csv(residuals_df, residuals_file, row.names = FALSE)
```

This should save properly standardized residuals, but the empirical data shows it didn't.

---

## ISSUE #3: FORCED STANDARDIZATION OF NF SAMPLES

### The Problem
`manual_nf_training.py` lines 374-384 applies forced standardization:

```python
samples_mean = samples.mean()
samples_std = samples.std()
if abs(samples_mean) > 0.01 or abs(samples_std - 1) > 0.01:
    samples = (samples - samples_mean) / samples_std  # FORCED
```

### NF Sample Statistics (BEFORE forced standardization)
```
NF GENERATED (AMZN sGARCH_norm):
  Mean = -0.378
  Std = 1.49
  Skew = -0.014
  Kurt = -0.14
```

These samples are NOT naturally standardized!

### Why This Happens
1. NF was trained on non-standardized residuals (mean=-0.463, std=1.503)
2. NF learned this non-standard distribution
3. NF generates samples from what it learned
4. Samples naturally have mean≠0, std≠1

### Why Forced Standardization Is Wrong
1. **Destroys learned patterns**: If NF learned asset-specific distributional features, forcing standardization erases them
2. **Makes NF equivalent to bootstrap**: After forced standardization, NF samples are just rescaled noise
3. **Unfair comparison**: Standard GARCH uses parametric samples (naturally standardized), NF uses learned samples then forced standardization

### Impact on Higher Moments
```
TRAINING RESIDUALS:     Skew = 2.22,    Kurt = 28.12
NF SAMPLES (raw):       Skew = -0.014,  Kurt = -0.14
NF SAMPLES (forced):    Skew = -0.014,  Kurt = -0.14
```

**NF completely FAILED to capture skewness and kurtosis!**
- Training data has extreme positive skew (2.22) and leptokurtosis (28.12)
- NF samples are nearly symmetric (skew≈0) and mesokurtic (kurt≈0)
- This suggests NF didn't learn the distribution properly

---

## ISSUE #4: NF TRAINING LOSS CONVERGENCE WITHOUT LEARNING

### The Evidence
From `outputs/manual/nf_models/sGARCH_norm_AMZN/training_history.csv`:
- Epochs: 75
- Initial loss: 2.21
- Final loss: 1.89
- **Convergence: Yes** (loss decreased by 0.32)

### The Problem
- NF training loss converged
- BUT NF failed to learn the actual distribution (skew, kurtosis)
- **Hypothesis**: NF learned to minimize loss by approximating a simple Gaussian, ignoring higher moments

### Possible Explanations
1. **NF Architecture Too Simple**: 4 layers, 64 hidden features may not be enough to capture complex distributions
2. **Loss Function Issue**: Negative log-likelihood doesn't heavily penalize higher moment mismatches
3. **Training Data Issue**: If training residuals weren't standardized, NF learned the wrong target
4. **Optimization Issue**: NF may have converged to a local minimum that approximates N(mu, sigma²) but misses tail behavior

---

## THE COMPLETE FAILURE CHAIN

### What Was Supposed to Happen:
1. Fit GARCH → extract standardized residuals (mean=0, std=1)
2. Train NF on standardized residuals → NF learns P(z) where z~(0,1) with asset-specific shape
3. Sample from NF → get z_nf ~ P(z) naturally standardized
4. Use in forecasting: r_t = mu_t + sigma_t * z_nf
5. NF captures higher moments → better forecasts

### What Actually Happened:
1. Fit GARCH → **extracted NON-standardized residuals** (mean=-0.46, std=1.50) [Bug #1 + Filename mismatch]
2. Train NF on non-standardized residuals → NF learns wrong distribution
3. NF training converges BUT fails to capture higher moments (skew, kurt)
4. Sample from NF → get z_nf with mean≠0, std≠1
5. **Force standardize** z_nf → destroys any learned patterns [Issue #3]
6. Use in forecasting: r_t = mu_t + sigma_t * z_nf_forced
7. NF-GARCH effectively becomes bootstrap resampling from normalized noise
8. Standard GARCH (with parametric sampling) uses theoretically correct distributions
9. **Result: Standard GARCH performs AS WELL OR BETTER than NF-GARCH**

---

## WHY STANDARD GARCH WINS

### Predictive Log-Likelihood
- **Standard GARCH**: Parametric density is analytical (exact formula)
- **NF-GARCH**: Density estimated via KDE with 200 paths (noisy estimate)
- **Winner**: Standard GARCH (more precise density estimation)

### MSE/MAE
- **Standard GARCH**: Samples from theoretical distribution (N or t)
- **NF-GARCH**: Samples from forced-standardized NF (learned nothing useful)
- **Winner**: Tie or Standard GARCH (NF provides no benefit)

### AIC
- **Standard GARCH**: 5-6 parameters
- **NF-GARCH**: 5-6 GARCH parameters + thousands of NF parameters
- **Winner**: Standard GARCH (simpler model, equal performance)

---

## WHAT NEEDS TO BE FIXED

### Immediate Fixes:

1. **Fix Filename Mismatch**
   - Update `manual_nf_training.py` to match the correct residual file pattern
   - OR update `manual_garch_fitting.R` to use "_Manual_Optimized_" naming

2. **Ensure Residuals Are Standardized**
   - Verify that `engine_residuals(fit, standardize = TRUE)` returns properly standardized data
   - Add validation checks: abs(mean) < 0.01, abs(std - 1) < 0.01
   - Save diagnostics with each residual file

3. **Remove or Justify Forced Standardization**
   - If residuals are properly standardized, NF should output standardized samples naturally
   - Remove forced standardization (lines 374-384 of `manual_nf_training.py`)
   - If removal causes issues, investigate NF training process

### Deeper Fixes:

4. **Improve NF Architecture**
   - Increase complexity: 8+ layers, 128+ hidden features
   - Add constraints to preserve standardization
   - Experiment with different flow architectures (Coupling flows, etc.)

5. **Add Validation Metrics**
   - Track higher moments (skew, kurtosis) during NF training
   - Add loss penalty for moment mismatches
   - Implement KS test / Wasserstein distance in training loop

6. **Rerun Complete Pipeline**
   - Clean all outputs
   - Run manual GARCH fitting with verified standardization
   - Train NF on correct residuals
   - Run comparison with NO forced standardization

---

## EXPECTED OUTCOME AFTER FIXES

### If Fixes Work:
- NF samples will be naturally standardized (mean≈0, std≈1)
- NF will capture higher moments (skew, kurtosis) from training data
- NF-GARCH forecasts will use asset-specific distributional information
- **NF-GARCH should outperform Standard GARCH on Predictive Log-Likelihood**

### If Fixes Don't Help:
- May indicate that for these assets, Student-t parametric is sufficient
- NF complexity is not needed if returns are well-modeled by standard distributions
- Consider testing on assets with more extreme/complex return distributions

---

## CONCLUSION

The current NF-GARCH implementation has **FUNDAMENTAL FLAWS**:
1. Training on incorrectly standardized residuals
2. NF failing to learn higher moments
3. Forced standardization destroying learned patterns
4. Filename mismatch causing pipeline disconnect

**These issues completely negate NF's theoretical advantages**, making it perform no better than (or worse than) parametric GARCH.

**The comparison showing Standard GARCH performing better is CORRECT given the current broken implementation**, but does NOT necessarily mean NF-GARCH is inherently inferior. The methodology must be fixed before drawing scientific conclusions.
