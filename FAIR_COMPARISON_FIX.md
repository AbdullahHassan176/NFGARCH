# Fair Comparison Fix - February 7, 2026

## Problem Identified

The chronological pipeline was producing **opposite results** from the main pipeline:

### Main Pipeline (OLD run with sstd labels):
- **NF-GARCH WINS**: 19 out of 25 comparisons
- MSE: NF=0.000365 vs Standard=0.000370 → NF is 1.5% better

### Chronological Pipeline (AFTER methodology fixes):
- **Standard GARCH WINS**: 2 out of 6 comparisons  
- MSE: NF=0.000371 vs Standard=0.000356 → NF is 4.2% worse

## Root Cause: **UNFAIR COMPARISON**

The chronological pipeline was comparing:
- **NF-GARCH** trained on models with `norm` (Normal) distribution
- **Standard GARCH** using `std` (Student-t) distribution

This is like comparing:
- A car optimized for city driving (NF trained on Normal residuals)
- Against a truck built for highways (Standard GARCH with fat-tailed Student-t)
- And declaring the truck "better" because it goes faster on highways

### Evidence from Results:

```
CHRONOLOGICAL (UNFAIR):
   Model Distribution  NF_MSE  Standard_MSE
  TGARCH         norm  0.000357      NaN       ← NF trained on norm
  TGARCH          std       NaN  0.000355      ← Compared to std!
  
  sGARCH         norm  0.000357  0.000358      ← ONLY fair comparison
```

The **only fair comparison** (sGARCH_norm vs sGARCH_norm): **NF-GARCH WINS!**

## The Fix

Updated two files to ensure **apples-to-apples** comparison:

### 1. `scripts/core/config.R`
Added BOTH distributions for ALL models:
- `sGARCH_norm` and `sGARCH_std`
- `eGARCH_norm` and `eGARCH_std`
- `TGARCH_norm` and `TGARCH_std`
- `gjrGARCH_norm` and `gjrGARCH_std`

### 2. `scripts/evaluation/compare_nf_vs_standard_garch.R`
Updated model configurations to include all distribution variants.

## Fair Comparison Logic

For each model/distribution combination:
1. Fit GARCH model with that distribution
2. Extract standardized residuals
3. Train NF on those residuals
4. Compare:
   - **NF-GARCH**: GARCH dynamics + NF-generated innovations
   - **Standard GARCH**: SAME GARCH dynamics + Parametric innovations from SAME distribution

This ensures we're testing **ONLY** whether NF improves over the parametric distribution assumption, holding GARCH dynamics constant.

## Expected Outcome

With fair comparison:
- Both NF-GARCH and Standard GARCH use the same base GARCH model
- Both use the same base distribution assumption (norm or std)
- The ONLY difference is: parametric vs NF-learned innovation distribution
- We can now properly assess if NF's flexibility adds value

## Next Steps

1. Clean old chronological outputs
2. Rerun chronological pipeline with fair comparison
3. Compare results across pipelines with consistent methodology
