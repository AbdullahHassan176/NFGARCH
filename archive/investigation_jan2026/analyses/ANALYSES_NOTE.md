# Analysis Status

## ✅ RUNNABLE ANALYSES (Data Available)

### Analysis 1: Residual Diagnostics
**Status:** ✅ Implemented and ready to run  
**Data Required:** Standard GARCH residuals, NF synthetic residuals  
**Location:** `analyses/analysis_1_residual_diagnostics.R`  
**Output:** ACF, ARCH tests, Ljung-Box tests

### Analysis 3: Information Loss
**Status:** ✅ Implemented and ready to run  
**Data Required:** Standard GARCH residuals, NF synthetic residuals  
**Location:** `analyses/analysis_3_information_loss.R`  
**Output:** Entropy, KL divergence, distribution similarity

### Analysis 4: Temporal Dynamics
**Status:** ✅ Implemented and ready to run  
**Data Required:** Standard GARCH residuals, NF synthetic residuals  
**Location:** `analyses/analysis_4_temporal_dynamics.R`  
**Output:** Runs tests, turning points, variance ratios

---

## ❌ NOT RUNNABLE (Data Not Saved During Pipeline)

### Analysis 2: Forecast Path Analysis
**Status:** ❌ Cannot run without modifications  
**Data Required:** Individual forecast paths (1000 paths × 1581 steps)  
**Missing:** Forecast paths were not saved, only aggregated MSE/MAE  
**To Enable:** Modify `simulate_nf_garch_engine.R` to save individual paths

### Analysis 5: Volatility Forecast Quality
**Status:** ❌ Cannot run without modifications  
**Data Required:** σ_t forecasts (conditional volatility forecasts)  
**Missing:** Only return forecasts saved, not volatility forecasts  
**To Enable:** Modify forecast functions to return and save σ_t

### Analysis 6: Rolling Window Performance
**Status:** ❌ Cannot run without modifications  
**Data Required:** Individual forecasts for each time point  
**Missing:** Only final aggregated metrics available  
**To Enable:** Save forecast_t for each t during simulation

### Analysis 7: Model Confidence Analysis
**Status:** ❌ Cannot run without modifications  
**Data Required:** 1000 MC paths for each asset/model  
**Missing:** Paths aggregated to mean, not saved individually  
**To Enable:** Save full path distributions during simulation

---

## RECOMMENDATION

**For this investigation run:**
- Execute Analyses 1, 3, 4 (available data)
- Document findings
- Note limitations for future runs

**For future comprehensive investigation:**
- Modify pipeline to save:
  1. Individual forecast paths (for Analyses 2, 6, 7)
  2. Conditional volatility forecasts (for Analysis 5)
  3. Individual time-step forecasts (for Analysis 6)

**Alternative approach:**
- Re-run comparison script with modified save logic
- Focus on specific assets (e.g., sGARCH_norm worst performers)
- Save detailed outputs for those cases only (reduces storage)

---

## EXECUTION PLAN

Run feasible analyses now:
```r
source("analyses/run_all_analyses.R")
```

This will execute:
1. Residual Diagnostics (Analysis 1)
2. Information Loss (Analysis 3)
3. Temporal Dynamics (Analysis 4)

Results will be saved to: `analyses/results/`
