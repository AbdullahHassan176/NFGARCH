# R Studio Manual Execution Guide

This guide shows you exactly what to run in R Studio, step-by-step.

## 🎯 Quick Start: Copy-Paste Execution Order

Run these scripts in R Studio in this exact order:

---

## Step 1: Set Working Directory (Required First)

```r
# Set your working directory - IMPORTANT!
setwd("C:/Github/Financial-SDG-GARCH")

# Verify you're in the right place
list.files("data/processed")  # Should show CSV files
```

---

## Step 2: Verify Mathematics (Optional but Recommended)

**Script:** `scripts/manual/verify_manual_math.R`

```r
# Run math verification
source("scripts/manual/verify_manual_math.R")
```

**What it does:**
- Tests that all GARCH equations are implemented correctly
- Verifies parameter constraints
- Checks stationarity conditions

**Expected output:**
```
✓ All mathematical verifications PASSED
```

**Time:** ~2-5 minutes

---

## Step 3: Load Configuration

**Script:** `scripts/manual/manual_optimized_config.R`

```r
# Load optimized configuration
source("scripts/manual/manual_optimized_config.R")

# Display settings
print_optimization_summary()
```

**What it does:**
- Sets up 6 assets (NVDA, MSFT, AMZN, EURUSD, GBPUSD, USDZAR)
- Configures 3 models (sGARCH, eGARCH, TGARCH)
- Sets optimized CV parameters

**Expected output:**
```
=== MANUAL EXECUTION OPTIMIZATION SUMMARY ===
Asset Reduction:  50 % ( 12  ->  6 )
Model Reduction:  40 % ( 5  ->  3 )
CV Optimization:  60 % time savings
```

**Time:** ~10 seconds

---

## Step 4: Fit GARCH Models (Main Step)

**Script:** `scripts/manual/manual_garch_fitting.R`

```r
# Run GARCH fitting
source("scripts/manual/manual_garch_fitting.R")
```

**What it does:**
1. Loads price data
2. Calculates log returns
3. Fits 3 GARCH models (sGARCH, eGARCH, TGARCH) to 6 assets
4. Extracts standardized residuals
5. Saves results

**Expected outputs:**
- `outputs/manual/garch_fitting/model_summary.csv`
- `outputs/manual/residuals_by_model/sGARCH/*.csv`
- `outputs/manual/residuals_by_model/eGARCH/*.csv`
- `outputs/manual/residuals_by_model/TGARCH/*.csv`

**Verify after completion:**
```r
# Check convergence rate
summary <- read.csv("outputs/manual/garch_fitting/model_summary.csv")
cat("Convergence rate:", mean(summary$Converged == TRUE), "\n")
# Should be > 0.9 (90%)

# Check number of residual files
residual_files <- list.files("outputs/manual/residuals_by_model", 
                             recursive = TRUE, pattern = "*.csv")
cat("Residual files created:", length(residual_files), "\n")
# Should be 18 (3 models × 6 assets)
```

**Time:** ~20-30 minutes

---

## Step 5: Run NF-GARCH Simulation (After Python NF Training)

**Script:** `scripts/simulation_forecasting/simulate_nf_garch_engine.R`

**⚠️ Note:** Run this AFTER completing Python NF training (see below)

```r
# Load utilities first
source("scripts/utils/cli_parser.R")
source("scripts/engines/engine_selector.R")
source("scripts/utils/safety_functions.R")
source("scripts/utils/conflict_resolution.R")

# Initialize pipeline
initialize_pipeline()

# Run simulation
source("scripts/simulation_forecasting/simulate_nf_garch_engine.R")
```

**What it does:**
1. Loads trained GARCH models
2. Loads synthetic residuals from NF training
3. Generates NF-GARCH simulated returns
4. Compares simulated vs actual (MSE, MAE)
5. Performs time-series cross-validation
6. Saves results to Excel

**Expected output:**
- `results/consolidated/NF_GARCH_Results_manual.xlsx`

**Verify after completion:**
```r
# Check results file exists
file.exists("results/consolidated/NF_GARCH_Results_manual.xlsx")
# Should return TRUE

# Load and inspect results
library(openxlsx)
wb <- loadWorkbook("results/consolidated/NF_GARCH_Results_manual.xlsx")
sheets <- names(wb)
cat("Excel sheets:", paste(sheets, collapse = ", "), "\n")

# Read results
results <- read.xlsx("results/consolidated/NF_GARCH_Results_manual.xlsx", 
                     sheet = "Chrono_Split_NF_GARCH")
head(results)
```

**Time:** ~15-20 minutes

---

## 📝 Complete R Studio Workflow

Here's the complete workflow you can copy-paste into R Studio:

```r
# ============================================================================
# COMPLETE MANUAL EXECUTION WORKFLOW FOR R STUDIO
# ============================================================================

# Step 1: Set working directory
setwd("C:/Github/Financial-SDG-GARCH")

# Step 2: Verify mathematics (optional)
source("scripts/manual/verify_manual_math.R")

# Step 3: Load configuration
source("scripts/manual/manual_optimized_config.R")
print_optimization_summary()

# Step 4: Fit GARCH models
source("scripts/manual/manual_garch_fitting.R")

# Verify GARCH fitting results
summary <- read.csv("outputs/manual/garch_fitting/model_summary.csv")
cat("Convergence rate:", mean(summary$Converged == TRUE), "\n")
cat("Total models fitted:", nrow(summary), "\n")

# Step 5: (Run Python NF training here - see below)
# Then come back and run:
source("scripts/utils/cli_parser.R")
source("scripts/engines/engine_selector.R")
source("scripts/utils/safety_functions.R")
source("scripts/utils/conflict_resolution.R")
initialize_pipeline()
source("scripts/simulation_forecasting/simulate_nf_garch_engine.R")

# Verify simulation results
file.exists("results/consolidated/NF_GARCH_Results_manual.xlsx")
```

---

## 🐍 Python Step (Between R Steps 4 and 5)

After Step 4 (GARCH fitting), you need to run Python for NF training:

**In PowerShell or Command Prompt:**
```bash
cd C:\Github\Financial-SDG-GARCH
python scripts/manual/manual_nf_training.py
```

**Or in Jupyter Notebook:**
```python
exec(open('scripts/manual/manual_nf_training.py').read())
training_results, all_samples = main()
```

**Time:** ~20-30 minutes

---

## 🔍 Individual Scripts You Can Run

### Data Exploration Scripts

```r
# EDA Summary Statistics
source("scripts/eda/eda_summary_stats.R")
# Outputs: outputs/eda/tables/ and outputs/eda/figures/
```

### Model Evaluation Scripts

```r
# Wilcoxon Win Rate Analysis
source("scripts/evaluation/wilcoxon_winrate_analysis.R")

# VaR Backtesting
source("scripts/evaluation/var_backtesting.R")

# Stylized Facts Tests
source("scripts/evaluation/stylized_fact_tests.R")
```

### Utilities

```r
# Pipeline Diagnostics
source("scripts/utils/pipeline_diagnostic.R")

# Conflict Resolution
source("scripts/utils/conflict_resolution.R")
initialize_pipeline()
```

---

## ✅ Quick Verification Checklist

After each major step, verify:

### After Step 3 (Configuration):
- [ ] `print_optimization_summary()` shows expected settings
- [ ] Working directory is correct

### After Step 4 (GARCH Fitting):
- [ ] `outputs/manual/garch_fitting/model_summary.csv` exists
- [ ] Convergence rate > 90%
- [ ] 18 residual CSV files exist (3 models × 6 assets)

### After Step 5 (Simulation):
- [ ] `results/consolidated/NF_GARCH_Results_manual.xlsx` exists
- [ ] Excel file has multiple sheets
- [ ] Results have finite AIC, BIC, MSE, MAE values

---

## 🚨 Common Issues and Fixes

### Issue: "Cannot open file" or "No such file or directory"
**Fix:** Check working directory
```r
getwd()  # Should show: "C:/Github/Financial-SDG-GARCH"
setwd("C:/Github/Financial-SDG-GARCH")  # Fix if needed
```

### Issue: Package not found
**Fix:** Install missing package
```r
install.packages("PACKAGE_NAME")
library(PACKAGE_NAME)
```

### Issue: Convergence failures
**Fix:** Check data quality
```r
# Check for NAs
sum(is.na(your_data))

# Check sample size (need > 500 observations)
nrow(your_data)
```

---

## 📊 What Gets Created

### After GARCH Fitting:
```
outputs/manual/
├── garch_fitting/
│   ├── model_summary.csv
│   └── detailed_results.rds
└── residuals_by_model/
    ├── sGARCH/
    │   ├── NVDA_Manual_Optimized_residuals.csv
    │   ├── MSFT_Manual_Optimized_residuals.csv
    │   └── ... (6 files total)
    ├── eGARCH/
    │   └── ... (6 files)
    └── TGARCH/
        └── ... (6 files)
```

### After NF-GARCH Simulation:
```
results/consolidated/
└── NF_GARCH_Results_manual.xlsx
    ├── Chrono_Split_NF_GARCH (sheet)
    ├── TS_CV_NF_GARCH (sheet)
    ├── Chrono_Summary (sheet)
    └── ... (additional sheets)
```

---

## 🎯 Recommended Execution Order

1. **First Time Setup:**
   - Step 1 (Set WD)
   - Step 2 (Verify Math)
   - Step 3 (Config)

2. **Main Analysis:**
   - Step 4 (GARCH Fitting)
   - Python NF Training
   - Step 5 (Simulation)

3. **Optional Evaluation:**
   - Evaluation scripts (if needed)
   - Additional analysis

---

## 💡 Pro Tips

1. **Run scripts line-by-line** in R Studio to see what each section does
2. **Check outputs after each step** before proceeding
3. **Save workspace** periodically: `save.image("checkpoint.RData")`
4. **Use RStudio's source panel** to navigate and understand scripts
5. **Check console output** for warnings or errors at each step

---

## 📚 Additional Resources

- **Full Guide:** `scripts/manual/MANUAL_EXECUTION_GUIDE.md`
- **Quick Reference:** `scripts/manual/QUICK_REFERENCE.md`
- **Math Verification:** `scripts/manual/verify_manual_math.R`

