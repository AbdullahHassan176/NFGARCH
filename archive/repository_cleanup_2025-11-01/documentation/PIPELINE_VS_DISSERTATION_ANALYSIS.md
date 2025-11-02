# Pipeline vs Dissertation Requirements Analysis

## 📊 Executive Summary

Your **Manual branch pipeline** is well-optimized for execution time but **missing several key metrics and evaluations** required by your dissertation. This document identifies gaps and provides recommendations.

---

## ✅ What Your Pipeline Currently Performs

### Assets & Models
- ✅ **6 assets** (3 FX: EURUSD, GBPUSD, USDZAR; 3 Equity: NVDA, MSFT, AMZN)
- ✅ **3 GARCH models** (sGARCH, eGARCH, TGARCH)
- ❌ **Missing**: gjrGARCH model (dissertation includes GJR-GARCH)

### Data Splitting
- ✅ **Chronological split** (65:35 train/test)
- ✅ **Time-Series Cross-Validation** (3 windows, optimized)
- ✅ Reduced TS CV windows for faster execution

### Core Metrics Currently Calculated
- ✅ **Forecasting metrics**: MSE, MAE, RMSE, Log-Likelihood, AIC, BIC
- ✅ **Model comparison**: NF-GARCH vs Standard GARCH
- ✅ **Convergence rates**: Model fitting success rates

### What Gets Saved
- ✅ NF-GARCH results (`NF_GARCH_Results_manual.xlsx`)
- ✅ Comparison results (`NF_vs_Standard_GARCH_Comparison.xlsx`)
- ✅ Dashboard (`Final_Dashboard.xlsx`)
- ✅ GARCH fitting summary

---

## ❌ What's MISSING from Dissertation Requirements

### 1. **Missing Model: gjrGARCH**
**Dissertation requirement**: sGARCH, EGARCH, TGARCH, **GJR-GARCH**
- **Current**: Only sGARCH, eGARCH, TGARCH
- **Impact**: Cannot compare all 4 models as specified
- **Fix**: Add gjrGARCH to `MANUAL_MODELS` in `scripts/manual/manual_optimized_config.R`

### 2. **Missing Distributional Metrics**
**Dissertation requirements** (Chapter 3, Section 3.1):
- ❌ **Kolmogorov-Smirnov (KS) distance** - Not aggregated/compared in dashboard
- ❌ **Wasserstein distance** - Not calculated or displayed
- ❌ **Tail index** - Not calculated
- ❌ **Skewness** - Not included in results
- ❌ **Kurtosis** - Not included in results
- ❌ **Distributional Y-metric (DY)** - Not calculated

**Note**: KS statistic is calculated in NF training (`scripts/manual/manual_nf_training.py`) but **not saved** to results or dashboard.

### 3. **Missing Stylized Facts Analysis**
**Dissertation requirements** (Chapter 3, Section 3.1):
- ❌ **Volatility clustering** - Not quantified
- ❌ **Leverage effects** - Not measured
- ❌ **Autocorrelation decay** - Not analyzed
- ❌ **Heavy tails** - Not quantified (tail index missing)
- ❌ **Gain/loss asymmetry** - Not measured
- ❌ **Aggregational Gaussianity** - Not tested

**Current config says**: `skip_stylized_facts = TRUE` (line 176 in `manual_optimized_config.R`)

### 4. **Missing VaR Backtesting**
**Dissertation requirements** (Chapter 4, Section 4.2; RQ4):
- ❌ **Kupiec test** (unconditional coverage) - Not performed
- ❌ **Christoffersen test** (independence) - Not performed
- ❌ **VaR exceedance rates** - Not calculated
- ❌ **Conditional coverage tests** - Not performed
- ❌ **VaR at 95% and 99%** - Not backtested

**Current config says**: `skip_stress_testing = TRUE` (line 177)

### 5. **Missing Stress Testing**
**Dissertation requirements** (Chapter 3, Section 3.7):
- ❌ **Historical crises** (2008 GFC, 2020 COVID) - Not tested
- ❌ **Hypothetical shocks** (50% devaluation, volatility spikes) - Not tested
- ❌ **Robustness under extreme events** - Not evaluated
- ❌ **Stress test summaries** - Not generated

**Current config says**: `skip_stress_testing = TRUE` (line 177)

### 6. **Missing Asset-Class Analysis**
**Dissertation requirement**: Compare performance by asset class (FX vs Equity)
- ❌ **Aggregated FX performance** - Not calculated
- ❌ **Aggregated Equity performance** - Not calculated
- ❌ **Asset-class specific winners** - Not identified

**Current**: Results are per-asset but not aggregated by class.

### 7. **Missing Wilcoxon Signed-Rank Test**
**Dissertation requirement** (Chapter 3, Section 3.3):
- ❌ **Statistical significance testing** - Not performed
- ❌ **Wilcoxon test** on matched comparisons - Not calculated

---

## 📊 What's Calculated but NOT in Dashboard

### From NF Training (`scripts/manual/manual_nf_training.py`):
1. **KS statistic & p-value** (line 289-295)
   - Calculated during NF training
   - **Not saved** to results or dashboard
   - Should be included in dashboard

### From GARCH Fitting (`scripts/model_fitting/fit_garch_models.R`):
2. **Q-statistic (Ljung-Box test)** (line 286)
   - Tests for residual autocorrelation
   - **Not included** in dashboard
   - Should be included for stylized facts

3. **ARCH-LM test p-value** (line 287)
   - Tests for ARCH effects
   - **Not included** in dashboard
   - Should be included for stylized facts

### From Comparison Script (`scripts/evaluation/compare_nf_vs_standard_garch.R`):
4. **Win rate analysis** (lines 228-246)
   - Calculated but **only in comparison file**
   - Should be included in main dashboard

5. **Improvement percentages** (lines 202-207)
   - MSE/MAE/AIC improvements calculated
   - **Not aggregated** in dashboard
   - Should be prominently displayed

---

## 🎯 Recommended Dashboard Metrics

### Essential Metrics (Must Include)

#### 1. **Forecasting Metrics** ✅ (Already included)
- MSE, MAE, RMSE, Log-Likelihood, AIC, BIC

#### 2. **Distributional Metrics** ❌ (Missing - Add these)
```r
# Add to dashboard:
- KS_distance (mean, median by model)
- Wasserstein_distance (mean, median by model)
- Tail_index (calculated from residuals)
- Skewness (mean, median by model/asset)
- Kurtosis (mean, median by model/asset)
```

#### 3. **Stylized Facts Metrics** ❌ (Missing - Add these)
```r
# Add to dashboard:
- Volatility_clustering_index (ACF of squared returns)
- Leverage_effect_measure (asymmetric response)
- Autocorrelation_decay (ACF decay rate)
- Heavy_tail_index (tail index estimate)
- Gain_loss_asymmetry (skewness of losses vs gains)
```

#### 4. **Risk Metrics** ❌ (Missing - Add these)
```r
# Add to dashboard:
- VaR_95_exceedance_rate
- VaR_99_exceedance_rate
- Kupiec_test_pvalue
- Christoffersen_test_pvalue
- ES_95 (Expected Shortfall)
- ES_99 (Expected Shortfall)
```

#### 5. **Statistical Significance** ❌ (Missing - Add these)
```r
# Add to dashboard:
- Wilcoxon_test_statistic
- Wilcoxon_pvalue
- Win_rate_NF_vs_Standard
- Improvement_percentage_MSE
- Improvement_percentage_MAE
```

#### 6. **Asset-Class Aggregation** ❌ (Missing - Add these)
```r
# Add to dashboard:
- FX_aggregated_performance
- Equity_aggregated_performance
- Best_model_per_asset_class
- Asset_class_specific_improvements
```

---

## 📋 Dashboard Structure Recommendations

### Recommended Sheets for `Final_Dashboard.xlsx`:

1. **Executive_Summary** ✅ (Already exists)
   - Add: Overall KS/Wasserstein, VaR exceedance rates, Wilcoxon results

2. **Performance_Chrono** ✅ (Already exists)
   - Add columns: KS_distance, Wasserstein, Tail_index, Skewness, Kurtosis

3. **Performance_TS_CV** ✅ (Already exists)
   - Add columns: KS_distance, Wasserstein, Tail_index

4. **Distributional_Fit** ❌ (NEW - Create this)
   - KS/Wasserstein by model
   - Tail index by model
   - Skewness/Kurtosis by model

5. **Stylized_Facts** ❌ (NEW - Create this)
   - Volatility clustering metrics
   - Leverage effects
   - Autocorrelation analysis

6. **Risk_Calibration** ❌ (NEW - Create this)
   - VaR backtesting results
   - Kupiec/Christoffersen test results
   - Exceedance rates by model

7. **Asset_Class_Analysis** ❌ (NEW - Create this)
   - FX vs Equity aggregated metrics
   - Best models per asset class
   - Class-specific improvements

8. **Statistical_Significance** ❌ (NEW - Create this)
   - Wilcoxon test results
   - Win rates
   - Improvement percentages

9. **Stress_Testing** ❌ (NEW - Create this)
   - Historical crisis performance
   - Hypothetical shock responses
   - Robustness scores

---

## 🔧 Action Items to Align with Dissertation

### High Priority (Required for dissertation):

1. **Add gjrGARCH model**
   ```r
   # In scripts/manual/manual_optimized_config.R
   MANUAL_MODELS <- c("sGARCH", "eGARCH", "TGARCH", "gjrGARCH")  # Add gjrGARCH
   ```

2. **Enable distributional metrics calculation**
   ```r
   # Change in manual_optimized_config.R:
   skip_stylized_facts = FALSE,  # Was TRUE
   distributional_metrics = c("KS_distance", "Wasserstein", "Tail_index", "Skewness", "Kurtosis")
   ```

3. **Enable VaR backtesting**
   ```r
   # Change in manual_optimized_config.R:
   skip_stress_testing = FALSE,  # Was TRUE
   risk_metrics = c("VaR_95", "VaR_99", "ES_95", "ES_99", "Kupiec", "Christoffersen")
   ```

4. **Add Wilcoxon test to comparison script**
   ```r
   # In scripts/evaluation/compare_nf_vs_standard_garch.R
   # Add: wilcox.test() for matched comparisons
   ```

5. **Create distributional metrics calculator**
   - New script: `scripts/evaluation/calculate_distributional_metrics.R`
   - Calculate: KS, Wasserstein, tail index, skewness, kurtosis

6. **Create VaR backtesting script**
   - New script: `scripts/evaluation/var_backtesting_comprehensive.R`
   - Calculate: VaR, exceedance rates, Kupiec, Christoffersen

7. **Update dashboard creation**
   - Add missing metrics to `scripts/core/create_final_dashboard.R`
   - Add new sheets: Distributional_Fit, Stylized_Facts, Risk_Calibration, etc.

### Medium Priority (Enhancements):

8. **Add stylized facts analysis**
   - Quantify volatility clustering, leverage effects, etc.

9. **Add stress testing**
   - Historical crises and hypothetical shocks

10. **Asset-class aggregation**
    - Summarize by FX vs Equity

---

## 📈 Current vs Required Metrics Summary

| Metric Category | Current | Required | Status |
|----------------|---------|----------|--------|
| **Forecasting** | ✅ MSE, MAE, LogLik, AIC, BIC | ✅ MSE, MAE, LogLik, AIC, BIC | ✅ Complete |
| **Distributional** | ❌ Missing | ✅ KS, Wasserstein, Tail, Skew, Kurt | ❌ Missing |
| **Stylized Facts** | ❌ Missing | ✅ Vol clustering, Leverage, ACF decay | ❌ Missing |
| **Risk (VaR)** | ❌ Missing | ✅ VaR, Kupiec, Christoffersen | ❌ Missing |
| **Stress Testing** | ❌ Missing | ✅ Historical & hypothetical shocks | ❌ Missing |
| **Statistical Tests** | ❌ Missing | ✅ Wilcoxon signed-rank | ❌ Missing |
| **Asset-Class** | ❌ Missing | ✅ FX vs Equity aggregation | ❌ Missing |

---

## ✅ What You're Doing Right

1. **Optimized execution** - 6 assets, 3 models, reduced TS CV windows
2. **Proper standardization** - NF residuals are now properly standardized
3. **NF vs Standard comparison** - Comparison script works correctly
4. **Basic metrics** - MSE, MAE, AIC, BIC are calculated and displayed
5. **Results consolidation** - Results are saved to Excel files

---

## 🎯 Next Steps

1. **Review this analysis** against your dissertation requirements
2. **Prioritize missing metrics** based on dissertation chapters
3. **Implement high-priority metrics** (distributional, VaR, statistical tests)
4. **Update dashboard** to include all required metrics
5. **Re-run pipeline** with all metrics enabled
6. **Verify results** match dissertation requirements

---

## 📝 Notes

- Your **6-asset, 3-model configuration** is appropriate for faster execution
- **Missing gjrGARCH** is a significant gap (dissertation includes 4 models)
- **Distributional metrics** are critical for dissertation Chapter 4
- **VaR backtesting** is required for RQ4
- **Stylized facts** are mentioned in methodology but not implemented

---

*Generated: 2024*
*Pipeline Version: Manual Branch (Optimized)*

