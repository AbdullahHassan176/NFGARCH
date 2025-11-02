# Manual Execution Steps - Using Existing Outputs

## 🎯 Overview

This guide shows how to run the **remaining evaluation scripts** using **existing outputs** in R Studio and Cursor (for Python), without re-running the entire pipeline.

---

## ✅ Prerequisites Check

### What You Already Have:

1. **GARCH Fitting Results** ✅
   - Location: `outputs/manual/garch_fitting/model_summary.csv`
   - Needed for: All evaluation scripts

2. **Standard Residuals** ✅
   - Location: `outputs/manual/residuals_by_model/*/`
   - Needed for: Distributional metrics, Stylized facts

3. **NF Residuals (Synthetic)** ✅
   - Location: `outputs/manual/nf_models/*_synthetic_residuals.csv`
   - Needed for: Distributional metrics

4. **NF-GARCH Simulation Results** ✅
   - Location: `results/consolidated/NF_GARCH_Results_manual.xlsx`
   - Needed for: VaR backtesting, Dashboard

5. **NF vs Standard Comparison** ✅
   - Location: `results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx`
   - Needed for: Dashboard, Asset-class analysis

6. **Raw Data** ✅
   - Location: `data/processed/raw (FX + EQ).csv`
   - Needed for: Stylized facts, Stress testing, VaR backtesting

---

## 📋 Execution Order

### Phase 1: Update NF vs Standard Comparison (R Studio)

**Why first?** Updates comparison with Wilcoxon test and asset-class analysis.

**In R Studio:**

```r
# Step 1: Set working directory
setwd("C:/Github/Financial-SDG-GARCH")

# Step 2: Load required libraries
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(xts)

# Step 3: Run updated comparison script
source("scripts/evaluation/compare_nf_vs_standard_garch.R")
```

**Expected Output:**
- Updates `results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx`
- Adds sheets: `Wilcoxon_Test`, `Asset_Class_Summary`, `Best_Model_By_Class`

**Time:** ~5 minutes

---

### Phase 2: Distributional Metrics (R Studio)

**Why?** Calculates KS distance, Wasserstein, Tail index, Skewness, Kurtosis.

**In R Studio:**

```r
# Step 1: Set working directory
setwd("C:/Github/Financial-SDG-GARCH")

# Step 2: Install required package (if needed)
# install.packages("transport")  # Optional - will use manual calculation if not available
# install.packages("moments")    # Optional - will use manual calculation if not available

# Step 3: Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

# Step 4: Run distributional metrics script
source("scripts/evaluation/calculate_distributional_metrics.R")
```

**Expected Output:**
- Creates `results/consolidated/Distributional_Metrics.xlsx`
- Sheets: `Distributional_Metrics`, `Summary_Statistics`

**Time:** ~10 minutes

**Note:** If `transport` package not available, will use manual Wasserstein calculation.

---

### Phase 3: Stylized Facts (R Studio)

**Why?** Calculates volatility clustering, leverage effects, autocorrelation decay.

**In R Studio:**

```r
# Step 1: Set working directory
setwd("C:/Github/Financial-SDG-GARCH")

# Step 2: Load required libraries
library(xts)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

# Step 3: Run stylized facts script
source("scripts/evaluation/calculate_stylized_facts.R")
```

**Expected Output:**
- Creates `results/consolidated/Stylized_Facts.xlsx`
- Sheets: `Stylized_Facts`, `Summary_By_Asset_Class`

**Time:** ~10 minutes

---

### Phase 4: VaR Backtesting (R Studio)

**Why?** Implements Kupiec test, Christoffersen test, exceedance rates.

**In R Studio:**

```r
# Step 1: Set working directory
setwd("C:/Github/Financial-SDG-GARCH")

# Step 2: Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(xts)

# Step 3: Run VaR backtesting script
source("scripts/evaluation/var_backtesting_comprehensive.R")
```

**Expected Output:**
- Creates `results/consolidated/VaR_Backtesting.xlsx`
- Sheets: `VaR_Backtesting`, `Summary_Statistics`

**Time:** ~15 minutes

---

### Phase 5: Stress Testing (R Studio)

**Why?** Tests historical crises and hypothetical shocks.

**In R Studio:**

```r
# Step 1: Set working directory
setwd("C:/Github/Financial-SDG-GARCH")

# Step 2: Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(xts)

# Step 3: Run stress testing script
source("scripts/evaluation/stress_testing_comprehensive.R")
```

**Expected Output:**
- Creates `results/consolidated/Stress_Testing.xlsx`
- Sheets: `Stress_Test_Results`, `Summary_Statistics`

**Time:** ~10 minutes

---

### Phase 6: Create Final Dashboard (R Studio)

**Why?** Consolidates all results into comprehensive dashboard.

**In R Studio:**

```r
# Step 1: Set working directory
setwd("C:/Github/Financial-SDG-GARCH")

# Step 2: Load required libraries
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)

# Step 3: Run dashboard creation script
source("scripts/core/create_final_dashboard.R")
```

**Expected Output:**
- Updates `results/consolidated/Final_Dashboard.xlsx`
- Adds all new sheets: Distributional_Fit, Stylized_Facts, Risk_Calibration, Stress_Testing, etc.
- Dashboard now has 20+ sheets

**Time:** ~5 minutes

---

## 🐍 Python Files (None Required for Remaining Scripts)

**Good news:** All remaining scripts are R scripts! No Python execution needed for the evaluation phase.

**Note:** NF training was already completed. The synthetic residuals are already generated and saved in `outputs/manual/nf_models/`.

---

## 📊 Complete Execution Sequence

### Quick Reference - All Steps

```r
# In R Studio - Run all sequentially:

# 1. Update comparison (Wilcoxon, asset-class)
setwd("C:/Github/Financial-SDG-GARCH")
source("scripts/evaluation/compare_nf_vs_standard_garch.R")

# 2. Distributional metrics
source("scripts/evaluation/calculate_distributional_metrics.R")

# 3. Stylized facts
source("scripts/evaluation/calculate_stylized_facts.R")

# 4. VaR backtesting
source("scripts/evaluation/var_backtesting_comprehensive.R")

# 5. Stress testing
source("scripts/evaluation/stress_testing_comprehensive.R")

# 6. Final dashboard
source("scripts/core/create_final_dashboard.R")
```

**Total Time:** ~45-60 minutes

---

## 🔍 What Each Script Uses (Existing Outputs)

### 1. `compare_nf_vs_standard_garch.R`
**Uses:**
- ✅ `results/consolidated/NF_GARCH_Results_manual.xlsx` (already exists)
- ✅ `outputs/manual/garch_fitting/model_summary.csv` (already exists)
- ✅ Raw data for standard GARCH simulation

**Generates:**
- Updates `results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx`
- Adds: Wilcoxon_Test, Asset_Class_Summary, Best_Model_By_Class

---

### 2. `calculate_distributional_metrics.R`
**Uses:**
- ✅ `outputs/manual/residuals_by_model/*/` (standard residuals)
- ✅ `outputs/manual/nf_models/*_synthetic_residuals.csv` (NF residuals)
- ✅ `results/consolidated/NF_GARCH_Results_manual.xlsx` (optional)

**Generates:**
- `results/consolidated/Distributional_Metrics.xlsx`

---

### 3. `calculate_stylized_facts.R`
**Uses:**
- ✅ `data/processed/raw (FX + EQ).csv` (raw price data)

**Generates:**
- `results/consolidated/Stylized_Facts.xlsx`

---

### 4. `var_backtesting_comprehensive.R`
**Uses:**
- ✅ `results/consolidated/NF_GARCH_Results_manual.xlsx` (NF-GARCH results)
- ✅ `data/processed/raw (FX + EQ).csv` (actual returns for backtesting)

**Generates:**
- `results/consolidated/VaR_Backtesting.xlsx`

---

### 5. `stress_testing_comprehensive.R`
**Uses:**
- ✅ `data/processed/raw (FX + EQ).csv` (raw price data)

**Generates:**
- `results/consolidated/Stress_Testing.xlsx`

---

### 6. `create_final_dashboard.R`
**Uses:**
- ✅ `results/consolidated/NF_GARCH_Results_manual.xlsx`
- ✅ `results/consolidated/Distributional_Metrics.xlsx` (from Step 2)
- ✅ `results/consolidated/Stylized_Facts.xlsx` (from Step 3)
- ✅ `results/consolidated/VaR_Backtesting.xlsx` (from Step 4)
- ✅ `results/consolidated/Stress_Testing.xlsx` (from Step 5)
- ✅ `results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx` (from Step 1)
- ✅ `outputs/manual/garch_fitting/model_summary.csv`

**Generates:**
- Updates `results/consolidated/Final_Dashboard.xlsx` (20+ sheets)

---

## ⚠️ Important Notes

### 1. **Execution Order Matters**
- Run scripts in the order listed above
- Step 6 (Dashboard) must come last (it loads results from Steps 1-5)

### 2. **Dependencies**
- Some scripts depend on outputs from previous scripts
- If a script fails, check if previous scripts completed successfully

### 3. **Optional Packages**
- `transport` package: Optional for Wasserstein distance (will use manual calculation if not available)
- `moments` package: Optional for skewness/kurtosis (will use manual calculation if not available)
- Scripts will work without these packages (using manual calculations)

### 4. **Error Handling**
- All scripts have error handling
- Scripts will continue even if some metrics fail
- Check console output for warnings

---

## 🚀 Quick Start (Copy-Paste)

### Option 1: Run All Sequentially in R Studio

```r
# Set working directory
setwd("C:/Github/Financial-SDG-GARCH")

# Load all required libraries once
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(xts)

# Run all scripts in sequence
source("scripts/evaluation/compare_nf_vs_standard_garch.R")      # Step 1
source("scripts/evaluation/calculate_distributional_metrics.R") # Step 2
source("scripts/evaluation/calculate_stylized_facts.R")         # Step 3
source("scripts/evaluation/var_backtesting_comprehensive.R")    # Step 4
source("scripts/evaluation/stress_testing_comprehensive.R")      # Step 5
source("scripts/core/create_final_dashboard.R")                   # Step 6

# Verify completion
cat("\n✅ All evaluation scripts completed!\n")
cat("Results saved to: results/consolidated/\n")
cat("Dashboard: results/consolidated/Final_Dashboard.xlsx\n")
```

---

### Option 2: Run Individual Scripts (As Needed)

Run each script individually in R Studio as needed, in the order listed.

---

## 📁 Expected Output Files

After running all scripts, you should have:

```
results/consolidated/
├── NF_GARCH_Results_manual.xlsx              ✅ (existing)
├── NF_vs_Standard_GARCH_Comparison.xlsx      ✅ (updated)
├── Distributional_Metrics.xlsx               ✅ (new)
├── Stylized_Facts.xlsx                       ✅ (new)
├── VaR_Backtesting.xlsx                      ✅ (new)
├── Stress_Testing.xlsx                       ✅ (new)
└── Final_Dashboard.xlsx                      ✅ (updated - 20+ sheets)
```

---

## 🔍 Verification After Each Step

### After Step 1 (Comparison):
```r
file.exists("results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx")
# Check for new sheets: Wilcoxon_Test, Asset_Class_Summary
```

### After Step 2 (Distributional):
```r
file.exists("results/consolidated/Distributional_Metrics.xlsx")
# Should have: Distributional_Metrics, Summary_Statistics sheets
```

### After Step 3 (Stylized Facts):
```r
file.exists("results/consolidated/Stylized_Facts.xlsx")
# Should have: Stylized_Facts, Summary_By_Asset_Class sheets
```

### After Step 4 (VaR):
```r
file.exists("results/consolidated/VaR_Backtesting.xlsx")
# Should have: VaR_Backtesting, Summary_Statistics sheets
```

### After Step 5 (Stress):
```r
file.exists("results/consolidated/Stress_Testing.xlsx")
# Should have: Stress_Test_Results, Summary_Statistics sheets
```

### After Step 6 (Dashboard):
```r
file.exists("results/consolidated/Final_Dashboard.xlsx")
# Should have 20+ sheets (verify by opening in Excel)
```

---

## 🆘 Troubleshooting

### If Script Fails:

1. **Check Dependencies:**
   ```r
   # Verify required files exist
   file.exists("results/consolidated/NF_GARCH_Results_manual.xlsx")
   file.exists("outputs/manual/garch_fitting/model_summary.csv")
   file.exists("data/processed/raw (FX + EQ).csv")
   ```

2. **Check Packages:**
   ```r
   # Install missing packages
   install.packages(c("openxlsx", "dplyr", "tidyr", "stringr", "xts"))
   ```

3. **Check Working Directory:**
   ```r
   getwd()  # Should be: C:/Github/Financial-SDG-GARCH
   ```

4. **View Error Messages:**
   - Check R Studio console for detailed error messages
   - Scripts have error handling and will show warnings

---

## 📊 Final Result

After completing all steps, you will have:

✅ **Complete dissertation results** with:
- 4 GARCH models (sGARCH, eGARCH, TGARCH, gjrGARCH)
- Distributional metrics (KS, Wasserstein, Tail index, Skewness, Kurtosis)
- Stylized facts (volatility clustering, leverage effects, etc.)
- VaR backtesting (Kupiec, Christoffersen)
- Stress testing (historical & hypothetical)
- Wilcoxon statistical tests
- Asset-class aggregation
- Comprehensive dashboard (20+ sheets)

---

*All scripts use existing outputs - no need to re-run GARCH fitting or NF training!*

