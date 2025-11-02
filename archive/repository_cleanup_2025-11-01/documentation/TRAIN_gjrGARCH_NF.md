# Train NF Models for gjrGARCH - Step by Step

## 🎯 Goal
Train Normalizing Flow models for gjrGARCH residuals to get complete distributional metrics.

## ✅ Prerequisites Check

Before starting, verify:
1. ✅ gjrGARCH standard residuals exist in `outputs/manual/residuals_by_model/gjrGARCH/`
2. ✅ NF training script exists: `scripts/manual/manual_nf_training.py`
3. ✅ Python environment with required packages (torch, nflows, numpy, pandas)

---

## 📋 Option B: Train NF Models for gjrGARCH

### Step 1: Verify gjrGARCH Residuals Exist

**In R Studio:**
```r
# Check if gjrGARCH residuals exist
list.files("outputs/manual/residuals_by_model/gjrGARCH/")
# Should show 6 files: one for each asset (EURUSD, GBPUSD, USDZAR, NVDA, MSFT, AMZN)
```

**Expected output:**
```
[1] "EURUSD_Manual_Optimized_residuals.csv"
[2] "GBPUSD_Manual_Optimized_residuals.csv"
[3] "USDZAR_Manual_Optimized_residuals.csv"
[4] "NVDA_Manual_Optimized_residuals.csv"
[5] "MSFT_Manual_Optimized_residuals.csv"
[6] "AMZN_Manual_Optimized_residuals.csv"
```

If these files don't exist, you need to:
1. Re-run GARCH fitting for gjrGARCH models
2. Extract residuals for gjrGARCH

---

### Step 2: Train NF Models for gjrGARCH (Cursor/Python)

**In Cursor (or Jupyter Notebook):**

The NF training script should auto-discover gjrGARCH residuals if they exist. Simply run:

```python
# Option A: Run the main script (auto-discovers all residual files)
python scripts/manual/manual_nf_training.py
```

**OR if using Jupyter Notebook:**

```python
# Import and run
import sys
sys.path.append('.')
from scripts.manual.manual_nf_training import main, train_optimized_nf

# Run main training pipeline (auto-discovers gjrGARCH residuals)
training_results, all_samples = main()
```

**Expected behavior:**
- Script will find all residual files in `outputs/manual/residuals_by_model/`
- This includes gjrGARCH residuals (if they exist)
- Will train NF models for all models, including gjrGARCH
- Will save synthetic residuals to `outputs/manual/nf_models/gjrGARCH_*_synthetic_residuals.csv`

**Time:** ~5-10 minutes (depending on system)

---

### Step 3: Verify gjrGARCH NF Residuals Generated

**After training completes, verify:**

**In R Studio:**
```r
# Check for gjrGARCH NF residuals
list.files("outputs/manual/nf_models/", pattern = "*gjrGARCH*")
# Should show 6 files: one for each asset
```

**Expected output:**
```
[1] "gjrGARCH_AMZN_synthetic_residuals.csv"
[2] "gjrGARCH_EURUSD_synthetic_residuals.csv"
[3] "gjrGARCH_GBPUSD_synthetic_residuals.csv"
[4] "gjrGARCH_MSFT_synthetic_residuals.csv"
[5] "gjrGARCH_NVDA_synthetic_residuals.csv"
[6] "gjrGARCH_USDZAR_synthetic_residuals.csv"
```

---

### Step 4: Re-run Distributional Metrics (R Studio)

Once gjrGARCH NF residuals exist, re-run the distributional metrics script:

```r
# Re-run distributional metrics (will now include gjrGARCH)
source("scripts/evaluation/calculate_distributional_metrics.R")
```

**Expected result:**
- KS_distance and Wasserstein_distance will now have values for gjrGARCH
- Summary statistics will show complete metrics for all 4 models
- No NaN values for gjrGARCH comparisons

---

### Step 5: Continue with Remaining Steps

After completing Option B, proceed with the remaining steps from MANUAL_EXECUTION_STEPS.md:

```r
# Step 3: Stylized facts (already in guide)
source("scripts/evaluation/calculate_stylized_facts.R")

# Step 4: VaR backtesting (already in guide)
source("scripts/evaluation/var_backtesting_comprehensive.R")

# Step 5: Stress testing (already in guide)
source("scripts/evaluation/stress_testing_comprehensive.R")

# Step 6: Final dashboard (already in guide)
source("scripts/core/create_final_dashboard.R")
```

---

## ⚠️ Important Notes

### If gjrGARCH Residuals Don't Exist:

If `outputs/manual/residuals_by_model/gjrGARCH/` is empty or missing:

1. **Re-run GARCH fitting for gjrGARCH:**
   ```r
   # In R Studio
   source("scripts/manual/manual_garch_fitting.R")
   # This should fit all 4 models including gjrGARCH
   ```

2. **Or extract gjrGARCH residuals from existing fits:**
   - Check if gjrGARCH fits exist in `outputs/manual/garch_fitting/`
   - If yes, extract residuals using the extraction script

### If NF Training Fails:

1. **Check Python packages:**
   ```python
   import torch
   import nflows
   import numpy as np
   import pandas as pd
   # All should import without errors
   ```

2. **Check CUDA availability (optional):**
   ```python
   import torch
   print(f"CUDA available: {torch.cuda.is_available()}")
   # Works on CPU too, just slower
   ```

3. **Check memory:**
   - NF training can be memory-intensive
   - If you run out of memory, reduce batch size in `MANUAL_NF_CONFIG`

---

## 🚀 Quick Start (Complete Process)

### In Cursor (Python):

```python
# 1. Verify gjrGARCH residuals exist
import os
gjr_dir = "outputs/manual/residuals_by_model/gjrGARCH"
if os.path.exists(gjr_dir):
    files = os.listdir(gjr_dir)
    print(f"Found {len(files)} gjrGARCH residual files")
else:
    print("gjrGARCH residuals not found - need to run GARCH fitting first")

# 2. Run NF training (auto-discovers gjrGARCH)
python scripts/manual/manual_nf_training.py

# 3. Verify output
output_dir = "outputs/manual/nf_models"
gjr_nf_files = [f for f in os.listdir(output_dir) if "gjrGARCH" in f]
print(f"Generated {len(gjr_nf_files)} gjrGARCH NF residual files")
```

### In R Studio (After Python):

```r
# 1. Verify gjrGARCH NF residuals
list.files("outputs/manual/nf_models/", pattern = "*gjrGARCH*")

# 2. Re-run distributional metrics
source("scripts/evaluation/calculate_distributional_metrics.R")

# 3. Continue with remaining steps
source("scripts/evaluation/calculate_stylized_facts.R")
source("scripts/evaluation/var_backtesting_comprehensive.R")
source("scripts/evaluation/stress_testing_comprehensive.R")
source("scripts/core/create_final_dashboard.R")
```

---

## 📊 Expected Outcome

After completing Option B:

✅ **All 4 models have complete distributional metrics:**
- sGARCH: ✅ Complete
- eGARCH: ✅ Complete
- TGARCH: ✅ Complete
- gjrGARCH: ✅ Complete (newly added)

✅ **No NaN values** in KS_distance or Wasserstein_distance

✅ **Complete dashboard** with all metrics for all models

---

## ⏱️ Time Estimate

- **NF Training for gjrGARCH:** ~5-10 minutes
- **Re-run Distributional Metrics:** ~2-3 minutes
- **Total additional time:** ~10-15 minutes

---

*After Option B, you'll have complete results for all 4 models!*

