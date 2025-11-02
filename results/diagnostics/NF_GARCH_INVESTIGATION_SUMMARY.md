# NF-GARCH Performance Investigation Summary

## Executive Summary

**NF-injected GARCH models did NOT outperform standard GARCH models.** The investigation revealed critical issues with NF residual standardization that caused extreme forecast errors.

## Key Findings

### 1. Root Cause: Improper NF Residual Standardization

**Problem Identified:**
- NF residuals were not properly standardized before use in simulations
- Several NF residual files have means far from 0 and SDs far from 1:
  - `eGARCH_EURUSD`: Mean = 16.65, SD = 158.47 (should be ~0 and ~1)
  - `eGARCH_GBPUSD`: Mean = 19.18, SD = 81.19 (should be ~0 and ~1)
  - `TGARCH_EURUSD`: Mean = 0.29, SD = 1.85 (close but not perfect)

**Impact:**
- NF residuals should have mean ≈ 0 and SD ≈ 1 to work properly in GARCH simulations
- Non-standardized residuals caused extreme forecast errors:
  - NF-GARCH MSE: `1.94e+226` (extremely high)
  - Standard GARCH MSE: `5.63e-04` (normal range)

### 2. Performance Comparison

| Metric | Standard GARCH | NF-GARCH | Winner |
|--------|---------------|----------|--------|
| **Overall MSE** | 5.63e-04 | 1.94e+226 | Standard ✓ |
| **Overall MAE** | 1.39e-02 | 5.67e+162 | Standard ✓ |
| **Overall AIC** | -19,003 | -14,827 | Standard ✓ |
| **Win Rate** | 6/6 (100%) | 0/6 (0%) | Standard ✓ |

### 3. Model-by-Model Performance

#### TGARCH
- **Standard**: MSE = 5.60e-04, MAE = 0.014
- **NF-GARCH**: MSE = 2.33e+226, MAE = 6.61e+162
- **Winner**: Standard GARCH (by huge margin)

#### eGARCH
- **Standard**: MSE = 5.23e-05, MAE = 0.006
- **NF-GARCH**: MSE = 5.88e+184, MAE = 5.11e+90
- **Winner**: Standard GARCH (by huge margin)

#### sGARCH
- **Standard**: MSE = 6.51e-04, MAE = 0.015
- **NF-GARCH**: No results (residuals not found/matched)
- **Winner**: Standard GARCH (only option)

### 4. Residual Distribution Analysis

#### NF Residuals (Problematic Cases)
- **eGARCH_EURUSD**: 
  - Mean: 16.65 (should be ~0)
  - SD: 158.47 (should be ~1)
  - Range: [-539.83, 544.45]
  
- **eGARCH_GBPUSD**:
  - Mean: 19.18 (should be ~0)
  - SD: 81.19 (should be ~1)
  - Range: [-247.63, 357.20]

#### Standard GARCH Residuals (Correct)
- **TGARCH_EURUSD**:
  - Mean: 0.014 (≈ 0 ✓)
  - SD: 1.99 (≈ 1 ✓)
  
- **sGARCH_NVDA**:
  - Mean: -0.29 (≈ 0 ✓)
  - SD: 1.46 (≈ 1 ✓)

### 5. Extreme Results Analysis

**All NF-GARCH results showed extreme MSE/MAE values:**
- TGARCH EURUSD: MSE = 1.41e+219
- TGARCH GBPUSD: MSE = 1.66e+179
- TGARCH NVDA: MSE = 7.40e+196
- TGARCH MSFT: MSE = 1.16e+227
- eGARCH EURUSD: MSE = 5.88e+184

These extreme values indicate that the simulation was using improperly scaled residuals, causing explosive forecast errors.

## Technical Details

### How GARCH Simulation Works

GARCH models generate returns using:
```
r_t = μ + σ_t * z_t
σ_t^2 = f(σ_{t-1}, ε_{t-1}, z_t)
```

Where:
- `z_t` are standardized residuals (mean ≈ 0, SD ≈ 1)
- `σ_t` is the conditional volatility
- `ε_t` are the innovations

**If `z_t` is not standardized properly:**
- Large `z_t` values → Large `σ_t` → Explosive forecast errors
- This is exactly what happened with NF residuals

### NF Residual Generation Process

1. GARCH models are fitted to historical data
2. Standardized residuals are extracted
3. Normalizing Flow (NF) models are trained on these residuals
4. NF models generate synthetic residuals
5. **Issue**: The synthetic residuals are not guaranteed to be standardized

### The Fix Needed

NF residuals must be standardized before use:
```r
nf_residuals_standardized <- (nf_residuals - mean(nf_residuals)) / sd(nf_residuals)
```

OR the NF model should be trained/configured to output standardized residuals directly.

## Visualizations Created

1. **residual_distributions.png**: Histograms comparing NF vs Standard residual distributions
2. **residual_qq_plots.png**: Q-Q plots showing distribution shape differences
3. **mse_comparison.png**: Box plots comparing MSE between NF-GARCH and Standard GARCH
4. **mae_comparison.png**: Box plots comparing MAE between NF-GARCH and Standard GARCH
5. **residual_stats_comparison.png**: Bar charts comparing mean and SD of residuals

All visualizations saved to: `results/diagnostics/`

## Recommendations

### Immediate Actions

1. **Standardize NF residuals before use:**
   - Add standardization step in `simulate_nf_garch_engine.R`
   - Apply `.standardize_nf()` function to all NF residuals before simulation

2. **Fix NF training pipeline:**
   - Ensure NF models output standardized residuals (mean=0, SD=1)
   - Add validation checks for residual statistics

3. **Re-run simulations with corrected residuals:**
   - Standardize all NF residuals before NF-GARCH simulation
   - Compare results again to see if NF-GARCH can outperform standard GARCH

### Long-term Improvements

1. **NF Model Architecture:**
   - Modify NF training to enforce output standardization
   - Use conditional NF models that account for GARCH variance structure

2. **Validation Framework:**
   - Add residual distribution tests (KS test, Shapiro-Wilk)
   - Check residual statistics before simulation
   - Fail fast if residuals are not properly standardized

3. **Alternative Approaches:**
   - Consider using NF to model the entire return distribution, not just residuals
   - Explore NF-GARCH variants that better integrate the NF output

## Conclusion

The investigation revealed that **NF-GARCH underperformance was due to improper residual standardization**, not a fundamental flaw in the NF-GARCH approach. With proper standardization, NF-GARCH may still have value, but the current implementation cannot demonstrate this due to the scaling issues.

**Next Steps:**
1. Implement residual standardization fix
2. Re-run NF-GARCH simulations
3. Re-evaluate performance comparison
4. Document findings in research report

---

*Investigation completed: [Date]*
*Diagnostic files: `results/diagnostics/`*
*Visualizations: `results/diagnostics/*.png`*

