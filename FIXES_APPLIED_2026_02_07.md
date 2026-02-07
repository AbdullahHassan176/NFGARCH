# Critical Methodology Fixes Applied - 2026-02-07

## Summary

Applied comprehensive fixes to address the root causes of NF-GARCH underperformance identified in `COMPLETE_METHODOLOGY_ISSUES.md`.

---

## FIX #1: Filename Mismatch Resolution

### Problem
- NF training expected: `*_Manual_Optimized_residuals.csv`
- GARCH fitting saved: `{asset}_Manual_residuals.csv`
- Result: NF was trained on OLD residuals from previous runs

### Fix Applied
**File**: `scripts/manual/manual_garch_fitting.R` (line 415)

**Changed from**:
```R
residuals_file <- file.path(model_dir, paste0(asset_name, "_Manual_residuals.csv"))
```

**Changed to**:
```R
residuals_file <- file.path(model_dir, paste0(asset_name, "_Manual_Optimized_residuals.csv"))
```

**Impact**: NF training will now use freshly generated, correctly standardized residuals.

---

## FIX #2: Residual Standardization Validation

### Problem
- Training residuals had mean=-0.463, std=1.503 (should be ~0, ~1)
- No validation to catch non-standardized residuals
- NF learned from incorrectly scaled data

### Fix Applied
**File**: `scripts/manual/manual_garch_fitting.R` (lines 413-425)

**Added validation**:
```R
# Validate residuals are standardized
resid_mean <- mean(residuals_vec, na.rm = TRUE)
resid_sd <- sd(residuals_vec, na.rm = TRUE)

if (abs(resid_mean) > 0.05 || abs(resid_sd - 1.0) > 0.1) {
  cat("    WARNING: Residuals not properly standardized!\n")
  cat("      Mean =", resid_mean, "(should be ~0)\n")
  cat("      SD =", resid_sd, "(should be ~1)\n")
  cat("      Skipping NF training for this model\n")
  next
}

cat("    Residuals validated: mean =", round(resid_mean, 4), 
    ", sd =", round(resid_sd, 4), "\n")
```

**Impact**: 
- Pipeline will fail-fast if residuals are not standardized
- Prevents training NF on incorrect data
- Provides clear diagnostics for debugging

---

## FIX #3: Removed Forced Standardization

### Problem
- NF training script forced standardization: `samples = (samples - mean) / std`
- This destroyed any distributional patterns NF learned
- Made NF-GARCH equivalent to bootstrap resampling

### Fix Applied
**File**: `scripts/manual/manual_nf_training.py` (lines 374-384)

**Changed from** (forced standardization):
```python
samples_mean = samples.mean()
samples_std = samples.std()
if abs(samples_mean) > 0.01 or abs(samples_std - 1) > 0.01:
    print(f"  Standardizing NF samples: mean={samples_mean:.6f}, std={samples_std:.6f}")
    samples = (samples - samples_mean) / samples_std  # FORCED
    print(f"  After standardization: mean={samples.mean():.6f}, std={samples.std():.6f}")
```

**Changed to** (validation only):
```python
samples_mean = samples.mean()
samples_std = samples.std()

print(f"  NF sample statistics: mean={samples_mean:.6f}, std={samples_std:.6f}")

# Check if samples are properly standardized
if abs(samples_mean) > 0.1 or abs(samples_std - 1) > 0.15:
    print(f"  WARNING: NF samples NOT standardized! This indicates NF training issue.")
    print(f"    Expected: mean~0, std~1")
    print(f"    Got: mean={samples_mean:.4f}, std={samples_std:.4f}")
    print(f"  Recommendation: Check training residuals and NF architecture")
    # Don't force standardization - let it fail so we know there's an issue
elif abs(samples_mean) > 0.05 or abs(samples_std - 1) > 0.05:
    print(f"  Note: Slight deviation from perfect standardization (acceptable)")
else:
    print(f"  [OK] NF samples properly standardized")
```

**Impact**:
- NF samples will retain learned distributional properties
- If NF fails to generate standardized samples, we'll know immediately
- Fair comparison with parametric GARCH (no post-processing)

---

## FIX #4: Verified std_residuals Calculation

### Verification
Confirmed all manual GARCH scripts correctly calculate standardized residuals:

**Files checked**:
- `scripts/manual_garch/fit_sgarch_manual.R` (lines 168, 217)
- `scripts/manual_garch/fit_egarch_manual.R` (lines 285, 335)
- `scripts/manual_garch/fit_tgarch_manual.R` (lines 191, 233)
- `scripts/manual_garch/fit_gjr_manual.R` (lines 151, 195)

**Formula** (all correct):
```R
std_residuals <- residuals / sigma
```

**Impact**: Residual standardization formula is mathematically correct across all models.

---

## Expected Outcomes After Fixes

### If NF Training Works Correctly:

1. **Standardized Residuals**
   - Mean ≈ 0.00 (within ±0.05)
   - Std ≈ 1.00 (within ±0.10)

2. **NF Samples (Natural)**
   - Mean ≈ 0.00 (within ±0.05)
   - Std ≈ 1.00 (within ±0.05)
   - Skewness/Kurtosis preserved from training data

3. **NF-GARCH Performance**
   - Should capture asset-specific higher moments
   - Should outperform on Predictive Log-Likelihood
   - May outperform on MAE/MSE if tails are important

### If NF Still Fails:

**Indicators**:
- NF samples have mean≠0 or std≠1 (validation will warn)
- Skewness/Kurtosis not preserved

**Possible causes**:
1. NF architecture too simple (4 layers, 64 features)
2. Training epochs insufficient (75 epochs)
3. Loss function doesn't capture higher moments well
4. Asset returns are well-modeled by Student-t (NF not needed)

**Next steps**:
- Increase NF complexity (8 layers, 128+ features)
- Add moment-matching loss terms
- Test on assets with more extreme distributions

---

## Cleanup Performed

Removed old outputs to ensure fresh run:
- `outputs/manual/nf_models/` - Old NF models trained on bad data
- `outputs/manual/residuals_by_model/` - Old residuals with wrong names
- `outputs/manual/garch_fitting/` - Old GARCH fits

---

## Files Modified

1. `scripts/manual/manual_garch_fitting.R` - Lines 413-425
   - Added residual standardization validation
   - Fixed filename to match NF training expectations

2. `scripts/manual/manual_nf_training.py` - Lines 374-384
   - Removed forced standardization
   - Added validation warnings

---

## Next Steps

1. Run manual GARCH fitting: `Rscript scripts/manual/manual_garch_fitting.R`
2. Verify residuals are standardized (check console output)
3. Run NF training: `python scripts/manual/manual_nf_training.py`
4. Verify NF samples are standardized (check console output)
5. Run full comparison pipeline
6. Analyze new results

---

## Success Criteria

### GARCH Fitting
- [ ] All models converge
- [ ] Residuals pass validation (mean≈0, std≈1)
- [ ] Files saved with correct naming (*_Manual_Optimized_residuals.csv)

### NF Training
- [ ] Loads correct residual files
- [ ] Training loss converges
- [ ] NF samples are naturally standardized (no warnings)
- [ ] Skewness/Kurtosis preserved

### Comparison Results
- [ ] NF-GARCH outperforms on Predictive Log-Likelihood
- [ ] NF-GARCH competitive or better on MAE/MSE
- [ ] Clear evidence NF captures distributional features

---

## Rollback Plan (If Fixes Break Pipeline)

If pipeline fails with new fixes:

1. Check residual validation output - if all fail, issue is in std_residuals extraction
2. Check NF sample validation - if all fail, issue is in NF architecture/training
3. Temporarily re-enable forced standardization (but note in dissertation!)
4. File bug report with diagnostics

---

## Documentation Status

- [x] Root cause analysis documented: `COMPLETE_METHODOLOGY_ISSUES.md`
- [x] Fixes documented: This file
- [x] Bug history tracked: Previous `CRITICAL_BUG_*.md` files
- [ ] Dissertation section on methodology needs update after results
