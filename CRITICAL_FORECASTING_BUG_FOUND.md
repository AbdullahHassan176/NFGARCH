# CRITICAL BUG: Standard GARCH is NOT Using Parametric Distributions for Forecasting

## Summary

Both NF-GARCH and Standard GARCH are using the **exact same simulation approach** for forecasting: resampling from **training residuals** (bootstrap).

This is **incorrect for Standard GARCH** - it should be sampling from the **parametric distribution** (Normal or Student-t) with fitted parameters, not resampling from empirical residuals.

---

## The Bug

### Location: `scripts/evaluation/compare_nf_vs_standard_garch.R`, lines 184-195

```r
standard_residuals <- engine_residuals(fit, standardize = TRUE)
# Use evaluate_return_forecasts with n_paths=200 for balanced speed/accuracy
# Point forecast = mean across paths, matching NF-GARCH methodology
eval_result <- evaluate_return_forecasts(
  fit = fit,
  nf_residuals = standard_residuals,  # <-- BUG: Resampling from empirical residuals!
  actual_returns = test_returns,
  horizon = length(test_returns),
  model_type = cfg$model,
  submodel = cfg$submodel,
  engine = "manual",
  n_paths = 200L
)
```

### What's Happening

1. **Standard GARCH extracts standardized training residuals** (e.g., 2934 values)
2. **Resamples from these empirical residuals** during simulation (bootstrap)
3. Uses the same `evaluate_return_forecasts()` as NF-GARCH

### What SHOULD Happen

1. **Standard GARCH should sample from the parametric distribution**:
   - For `Distribution = "norm"` → sample from `N(0, 1)` using `rnorm(n, 0, 1)`
   - For `Distribution = "std"` → sample from Student-t using `rt(n, df=fitted_df) / sqrt(df/(df-2))`
2. **NOT resample from empirical training residuals**

---

## Why This Matters

### Impact on Predictive Log-Likelihood

- **Current method** (bootstrap from training residuals):
  - Limited by the empirical distribution shape from training data
  - KDE with only 100-200 paths is very noisy
  - Doesn't properly represent the parametric assumptions

- **Correct method** (parametric sampling):
  - Uses the full parametric distribution (infinite support)
  - Properly represents tails and density
  - Should give Standard GARCH a STRONG advantage over bootstrap methods

### Impact on Comparison

**Currently**: NF-GARCH vs. Standard GARCH is comparing:
- NF (flexible learned distribution from training) 
- vs. Standard (bootstrap from training residuals)

**Should be**: NF-GARCH vs. Standard GARCH comparing:
- NF (flexible learned distribution)
- vs. Standard (parametric normal/Student-t distribution)

**This is why NF-GARCH and Standard GARCH have nearly identical performance!** They're using essentially the same method (resampling from training residuals).

---

## Secondary Issue: Only 100 Valid Paths

The results show `NPaths = 100`, but the code specifies `n_paths = 200`. This means:
- **50% of simulation paths are failing**
- KDE with only 100 paths is very imprecise for predictive log-likelihood
- Need to investigate why half the paths fail

---

## The Fix

### 1. Create a parametric sampling function for Standard GARCH

```r
sample_parametric_residuals <- function(n, distribution, df = NULL) {
  if (distribution == "norm") {
    return(rnorm(n, 0, 1))
  } else if (distribution == "std" || distribution == "sstd") {
    if (is.null(df)) df <- 5  # Default if not provided
    # Standardize Student-t to have unit variance
    z <- rt(n, df)
    z <- z / sqrt(df / (df - 2))
    return(z)
  } else {
    # Fallback to normal
    return(rnorm(n, 0, 1))
  }
}
```

### 2. Modify Standard GARCH evaluation to use parametric sampling

In `compare_nf_vs_standard_garch.R`:

```r
# BEFORE (WRONG):
standard_residuals <- engine_residuals(fit, standardize = TRUE)
eval_result <- evaluate_return_forecasts(
  fit = fit,
  nf_residuals = standard_residuals,  # Bootstrap
  ...
)

# AFTER (CORRECT):
# For Standard GARCH, generate parametric residuals
n_total_needed <- length(test_returns) * 200  # horizon * n_paths
parametric_residuals <- sample_parametric_residuals(
  n = n_total_needed,
  distribution = cfg$distribution,
  df = if (cfg$distribution == "std") fit$manual_fit$shape else NULL
)
eval_result <- evaluate_return_forecasts(
  fit = fit,
  nf_residuals = parametric_residuals,  # Parametric
  ...
)
```

### 3. Investigate path failures

Debug why `n_valid_paths = 100` when `n_paths = 200`:
- Check `generate_multiple_paths()` error messages
- Look for NAs or infinities in simulated paths
- Verify GARCH recursion stability

---

## Expected Outcome After Fix

### If NF is working correctly:
- **Predictive Log-Lik**: NF-GARCH should **outperform** Standard GARCH (more flexible distribution)
- **Point Forecasts (MSE/MAE)**: Should remain similar (both are 1-step ahead)
- **Distributional metrics**: NF should better capture tail behavior and skewness

### If Standard parametric is better:
- **Predictive Log-Lik**: Standard GARCH may **outperform** NF-GARCH (stronger assumptions, when correct)
- This would suggest: NF isn't learning a better distribution, OR there's a bug in NF training/sampling

Either way, this fix will give us an **apples-to-apples comparison** instead of the current apples-to-apples (both using bootstrap).

---

## Next Steps

1. ✅ Document the bug (this file)
2. ⬜ Implement parametric sampling for Standard GARCH
3. ⬜ Debug path failures (why only 100/200 paths succeed)
4. ⬜ Re-run comparison with fixed method
5. ⬜ Analyze results to determine if NF is truly learning a better distribution
