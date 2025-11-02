# Reproducibility Check - Will `run_all.bat` Replicate Your Results?

## ✅ YES - But With Important Caveats

### **Good News: Reproducible Elements**

1. **Random Seeds Set** ✅
   - R scripts: `set.seed(123)` in multiple places
   - Python NF training: `torch.manual_seed(123)`, `np.random.seed(123)`
   - Location: `scripts/simulation_forecasting/simulate_nf_garch_engine.R` (line 39)
   - Location: `scripts/manual/manual_nf_training.py` (lines 75-76)

2. **Critical Fix Preserved** ✅
   - **NF residual standardization fix** is still in place
   - Location: `scripts/simulation_forecasting/simulate_nf_garch_engine.R` (lines 360-372, 419-431)
   - This ensures proper standardization (mean ≈ 0, SD ≈ 1)

3. **Same Data Split** ✅
   - Chronological split: 65:35 (train:test)
   - Time-Series Cross-Validation: Same logic
   - Same data file: `./data/processed/raw (FX + EQ).csv`

4. **Same Assets** ✅
   - 6 assets: EURUSD, GBPUSD, USDZAR, NVDA, MSFT, AMZN
   - Same order and selection

5. **Same Optimization Settings** ✅
   - CV: 3 folds, max 3 windows
   - NF Training: 75 epochs, batch size 512
   - Reduced TS CV windows

---

## ⚠️ Important Differences (Will Cause Minor Variations)

### **1. Model Count Changed**

**Current Results (Your Previous Run):**
- 3 models: sGARCH, eGARCH, TGARCH

**New `run_all.bat` (After Fixes):**
- **4 models**: sGARCH, eGARCH, TGARCH, **gjrGARCH** ← NEW

**Impact:**
- Will take longer (additional model to fit)
- Results will include gjrGARCH comparisons
- **Your previous results won't have gjrGARCH**
- Other model results should match if seeds are the same

### **2. Additional Evaluation Scripts**

**New Scripts (Will Generate Extra Results):**
- Distributional metrics (KS, Wasserstein, Tail index)
- Stylized facts (volatility clustering, leverage effects)
- VaR backtesting (Kupiec, Christoffersen)
- Stress testing (historical crises, hypothetical shocks)
- Wilcoxon tests
- Asset-class aggregation

**Impact:**
- More output files
- More comprehensive dashboard
- **Your previous results won't have these**
- Core forecasting metrics (MSE, MAE, AIC, BIC) should match

### **3. Dashboard Structure**

**Previous Dashboard:**
- ~7 sheets

**New Dashboard:**
- **20+ sheets** (includes all new metrics)

**Impact:**
- More comprehensive
- Previous dashboard simpler

---

## 🎯 What WILL Replicate Exactly

### If you run `run_all.bat` with the SAME configuration:

1. **Core Forecasting Metrics** (for sGARCH, eGARCH, TGARCH)
   - MSE, MAE, RMSE should match (within numerical precision)
   - AIC, BIC, Log-Likelihood should match
   - **Reason**: Same seeds, same data, same models, same splits

2. **NF Residual Standardization** ✅
   - Fix is preserved
   - NF-GARCH performance should match your good results
   - **Reason**: Standardization code unchanged

3. **Time-Series CV Results**
   - Same 3 windows
   - Same forecast horizons
   - Results should match

4. **NF vs Standard Comparison** (for the 3 original models)
   - Win rates should match
   - Improvement percentages should match

---

## ⚠️ What Will NOT Match (Expected Differences)

### 1. **gjrGARCH Results** (NEW)
   - Your previous run didn't include gjrGARCH
   - New run will have gjrGARCH results
   - This is **intentional** (dissertation requirement)

### 2. **Additional Metrics** (NEW)
   - Distributional metrics (KS, Wasserstein, Tail index)
   - Stylized facts
   - VaR backtesting
   - Stress testing
   - These weren't in your previous run
   - This is **intentional** (dissertation requirement)

### 3. **Dashboard Sheets** (EXPANDED)
   - Previous: ~7 sheets
   - New: 20+ sheets
   - This is **intentional** (comprehensive dashboard)

---

## 🔍 How to Ensure Exact Replication

### Option 1: Match Your Previous Configuration

If you want to replicate your **exact previous results** (3 models only):

1. **Temporarily remove gjrGARCH** (if you only want 3 models):
   ```r
   # In scripts/manual/manual_optimized_config.R
   MANUAL_MODELS <- c("sGARCH", "eGARCH", "TGARCH")  # Remove gjrGARCH
   ```

2. **Skip new evaluation scripts** (if you only want core metrics):
   - Comment out Steps 6-9 in `run_all.bat`
   - Or use `run_manual.bat` instead (simpler version)

### Option 2: Use Current Configuration (Recommended)

Run `run_all.bat` as-is to get:
- ✅ All 4 models (dissertation requirement)
- ✅ All metrics (dissertation requirement)
- ✅ Comprehensive dashboard
- ✅ Reproducible core results (same seeds)

---

## 📊 Expected Results Comparison

### For the 3 Original Models (sGARCH, eGARCH, TGARCH):

| Metric | Previous Run | New Run (`run_all.bat`) | Match? |
|--------|-------------|-------------------------|--------|
| **MSE** | Your values | Same (with seed 123) | ✅ Should match |
| **MAE** | Your values | Same (with seed 123) | ✅ Should match |
| **AIC** | Your values | Same (with seed 123) | ✅ Should match |
| **BIC** | Your values | Same (with seed 123) | ✅ Should match |
| **NF vs Standard Win Rate** | Your values | Same | ✅ Should match |
| **NF Residual Standardization** | Fixed | Fixed | ✅ Should match |

### For gjrGARCH (NEW):

| Metric | Previous Run | New Run (`run_all.bat`) |
|--------|-------------|-------------------------|
| **Results** | ❌ Not included | ✅ Will be generated |

### For Additional Metrics (NEW):

| Metric Category | Previous Run | New Run (`run_all.bat`) |
|-----------------|-------------|-------------------------|
| **Distributional (KS, Wasserstein)** | ❌ Not calculated | ✅ Will be calculated |
| **Stylized Facts** | ❌ Not calculated | ✅ Will be calculated |
| **VaR Backtesting** | ❌ Not calculated | ✅ Will be calculated |
| **Stress Testing** | ❌ Not calculated | ✅ Will be calculated |
| **Wilcoxon Tests** | ❌ Not calculated | ✅ Will be calculated |
| **Asset-Class Analysis** | ❌ Not calculated | ✅ Will be calculated |

---

## ✅ Final Answer

### **Yes, `run_all.bat` WILL replicate your core results for the 3 original models** ✅

**With these conditions:**
1. ✅ Same random seeds (`set.seed(123)`)
2. ✅ Same data split (65:35)
3. ✅ Same TS CV windows (3 windows)
4. ✅ Same NF residual standardization fix
5. ✅ Same optimization settings

**BUT:**
- Will also include **gjrGARCH** (4th model) - **NEW**
- Will also calculate **additional metrics** - **NEW**
- Will create **more comprehensive dashboard** - **NEW**

### **Recommendation:**

1. **If you want EXACT previous results** (3 models only):
   - Temporarily comment out gjrGARCH in config
   - Skip new evaluation steps
   - Use simpler pipeline

2. **If you want COMPLETE dissertation results** (Recommended):
   - Run `run_all.bat` as-is
   - Core metrics will match (3 original models)
   - Plus you'll get all required metrics for dissertation

---

## 🧪 Verification Steps

After running `run_all.bat`, compare:

1. **Core metrics for sGARCH, eGARCH, TGARCH**:
   ```r
   # Compare MSE, MAE, AIC, BIC from previous run vs new run
   # Should match (within numerical precision)
   ```

2. **NF vs Standard win rates**:
   ```r
   # Should match your previous results
   ```

3. **NF residual standardization**:
   ```r
   # Should see mean ≈ 0, SD ≈ 1 in console output
   # This was the critical fix
   ```

---

## 📝 Notes

- **Seeds**: All scripts use `set.seed(123)` for reproducibility
- **Standardization Fix**: Preserved in `simulate_nf_garch_engine.R`
- **Configuration**: Now includes 4 models (was 3)
- **Execution**: `run_all.bat` clears outputs first, so it's a fresh run
- **Time**: Will take longer due to additional model and metrics

---

*Bottom Line: Core results will replicate. Additional models and metrics will extend your results for dissertation requirements.*

