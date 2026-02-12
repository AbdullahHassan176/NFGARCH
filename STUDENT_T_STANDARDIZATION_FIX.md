# Critical Fix: Student-t Standardization

## Problem
Student-t standardized residuals were NOT properly scaled to have variance = 1.

## Root Cause
Student-t(nu) distribution has variance = nu/(nu-2), not 1.

So standardized residuals z_t = ε_t / σ_t follow Student-t(nu) with Var(z) = nu/(nu-2).

## Solution
After computing `std_residuals = residuals / sigma`, apply additional scaling:

```r
# Student-t(nu) has Var(z) = nu/(nu-2)
# To get Var=1, multiply by sqrt((nu-2)/nu)
if (nu > 2) {
  std_residuals <- std_residuals * sqrt((nu - 2) / nu)
}
```

## Files to Fix
- [x] fit_sgarch_manual.R
- [ ] fit_egarch_manual.R  
- [ ] fit_gjr_manual.R
- [ ] fit_tgarch_manual.R

## Expected Result
After fix, all Student-t models should have:
- Mean ≈ 0
- SD ≈ 1.0 (not 1.2-1.6!)

This will allow NF training to proceed correctly.
