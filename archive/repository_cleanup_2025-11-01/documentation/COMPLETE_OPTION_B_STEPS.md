# Complete Option B Steps - Train gjrGARCH NF Models

## 🔍 Current Situation

**Issue Found:**
- ❌ gjrGARCH residuals are missing
- ❌ gjrGARCH models were not fitted (0 gjrGARCH fits in summary)
- ✅ gjrGARCH is in configuration (`manual_optimized_config.R`)
- ✅ NF training script exists and can handle gjrGARCH

**Solution:**
You need to run GARCH fitting FIRST to generate gjrGARCH residuals, THEN train NF models.

---

## 📋 Complete Option B Steps

### Step 1: Fit gjrGARCH Models (R Studio)

**In R Studio:**

```r
# Set working directory
setwd("C:/Github/Financial-SDG-GARCH")

# Load required libraries
library(xts)
library(dplyr)
library(tidyr)

# Load configuration and engine
source("scripts/manual/manual_optimized_config.R")
source("scripts/engines/engine_selector.R")

# Run GARCH fitting (will fit all 4 models including gjrGARCH)
source("scripts/manual/manual_garch_fitting.R")
```

**Expected output:**
- Fits sGARCH, eGARCH, TGARCH, and **gjrGARCH** for all 6 assets
- Saves residuals to `outputs/manual/residuals_by_model/gjrGARCH/`
- Creates 6 residual files (one per asset)

**Time:** ~10-15 minutes

**Verification:**
```r
# After fitting completes, verify gjrGARCH residuals exist
list.files("outputs/manual/residuals_by_model/gjrGARCH/")
# Should show 6 files: EURUSD, GBPUSD, USDZAR, NVDA, MSFT, AMZN
```

---

### Step 2: Train NF Models for gjrGARCH (Cursor/Python)

**In Cursor (or terminal):**

```bash
# Navigate to project directory
cd C:/Github/Financial-SDG-GARCH

# Run NF training (auto-discovers gjrGARCH residuals)
python scripts/manual/manual_nf_training.py
```

**OR in Cursor Python script:**

```python
import sys
sys.path.append('.')
from scripts.manual.manual_nf_training import main

# Run main training pipeline (auto-discovers ALL residuals including gjrGARCH)
training_results, all_samples = main()
```

**Expected behavior:**
- Finds all residual files (including gjrGARCH)
- Trains NF models for gjrGARCH (6 models: one per asset)
- Saves synthetic residuals to `outputs/manual/nf_models/gjrGARCH_*_synthetic_residuals.csv`

**Time:** ~5-10 minutes

**Verification:**
```r
# In R Studio, verify gjrGARCH NF residuals exist
list.files("outputs/manual/nf_models/", pattern = "*gjrGARCH*")
# Should show 6 files: one for each asset
```

---

### Step 3: Re-run Distributional Metrics (R Studio)

**In R Studio:**

```r
# Re-run distributional metrics (will now include gjrGARCH)
source("scripts/evaluation/calculate_distributional_metrics.R")
```

**Expected result:**
- KS_distance and Wasserstein_distance will have values for gjrGARCH
- Summary statistics will show complete metrics for all 4 models
- No NaN values for gjrGARCH

**Time:** ~5 minutes

---

### Step 4: Continue with Remaining Steps

After completing Option B, proceed with the remaining steps from `MANUAL_EXECUTION_STEPS.md`:

```r
# Step 3: Stylized facts
source("scripts/evaluation/calculate_stylized_facts.R")

# Step 4: VaR backtesting
source("scripts/evaluation/var_backtesting_comprehensive.R")

# Step 5: Stress testing
source("scripts/evaluation/stress_testing_comprehensive.R")

# Step 6: Final dashboard
source("scripts/core/create_final_dashboard.R")
```

---

## 📊 Complete Execution Sequence

### Full Option B + Remaining Steps:

```r
# In R Studio - Step 1: Fit gjrGARCH models
setwd("C:/Github/Financial-SDG-GARCH")
source("scripts/manual/manual_garch_fitting.R")
# ⏱️ ~10-15 minutes

# Verify gjrGARCH residuals
list.files("outputs/manual/residuals_by_model/gjrGARCH/")
```

```python
# In Cursor - Step 2: Train NF models for gjrGARCH
python scripts/manual/manual_nf_training.py
# ⏱️ ~5-10 minutes
```

```r
# In R Studio - Step 3: Re-run distributional metrics
source("scripts/evaluation/calculate_distributional_metrics.R")
# ⏱️ ~5 minutes

# Step 4: Continue with remaining steps
source("scripts/evaluation/calculate_stylized_facts.R")
# ⏱️ ~10 minutes

source("scripts/evaluation/var_backtesting_comprehensive.R")
# ⏱️ ~15 minutes

source("scripts/evaluation/stress_testing_comprehensive.R")
# ⏱️ ~10 minutes

source("scripts/core/create_final_dashboard.R")
# ⏱️ ~5 minutes
```

**Total Time:** ~60-75 minutes (includes gjrGARCH fitting and NF training)

---

## ⚠️ Important Notes

### 1. Why Step 1 is Needed

gjrGARCH models were added to the configuration but GARCH fitting wasn't re-run after adding gjrGARCH. You need to:
- Fit gjrGARCH models first (generates residuals)
- Then train NF models on those residuals

### 2. Will This Overwrite Existing Results?

**GARCH Fitting:**
- Will fit gjrGARCH models (new)
- Will re-fit sGARCH, eGARCH, TGARCH (will overwrite if run)
- **Option:** You can modify the script to only fit gjrGARCH if you want to avoid re-fitting other models

**NF Training:**
- Will train NF models for ALL residual files found
- Including gjrGARCH (new) and existing models (will re-train)
- **Option:** You can modify to skip existing NF models if you want

### 3. Quick Option (If You Don't Want to Re-fit)

If you want to avoid re-fitting existing models:

**Option A: Skip existing models in GARCH fitting**
- Modify `manual_garch_fitting.R` to check if fits exist
- Only fit gjrGARCH models

**Option B: Fit only gjrGARCH**
- Create a minimal script that only fits gjrGARCH models
- Extract only gjrGARCH residuals

---

## ✅ Verification Checklist

After completing Option B:

- [ ] gjrGARCH residuals exist: `outputs/manual/residuals_by_model/gjrGARCH/` (6 files)
- [ ] gjrGARCH NF residuals exist: `outputs/manual/nf_models/` (6 files starting with "gjrGARCH_")
- [ ] Distributional metrics include gjrGARCH: No NaN for gjrGARCH KS/Wasserstein
- [ ] All 4 models have complete metrics in final dashboard

---

## 🚀 Recommended Approach

**If you have time (~60-75 minutes):**
1. Run Step 1 (GARCH fitting) - fits all models including gjrGARCH
2. Run Step 2 (NF training) - trains NF for all models including gjrGARCH
3. Run Step 3 (Distributional metrics) - re-run to include gjrGARCH
4. Continue with Steps 4-6 (Stylized facts, VaR, Stress, Dashboard)

**If you want to minimize time (~20 minutes):**
1. Modify script to only fit gjrGARCH (skip other models)
2. Run Step 2 (NF training) - will only train for gjrGARCH (if other residuals exist, will train those too)
3. Run Step 3 (Distributional metrics) - re-run
4. Continue with remaining steps

---

*After Option B, you'll have complete results for all 4 models (sGARCH, eGARCH, TGARCH, gjrGARCH)!*

