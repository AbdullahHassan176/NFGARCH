# Issue Found: Missing gjrGARCH NF Residuals

## 🔍 Problem Identified

### Issue 1: Missing gjrGARCH NF Residuals

**Root Cause:**
- Script processes 4 models: `sGARCH`, `eGARCH`, `TGARCH`, `gjrGARCH`
- NF training was only run for 3 models: `sGARCH`, `eGARCH`, `TGARCH`
- **Missing:** All `gjrGARCH` NF residual files (6 files: one per asset)

**Impact:**
- KS_distance and Wasserstein_distance show as NaN for gjrGARCH
- Summary statistics show NaN for gjrGARCH comparisons

### Issue 2: CSV File Reading

The NF residual CSV files may have headers, but the script reads them as header=FALSE, which might cause issues.

---

## ✅ Fix Required

### Option 1: Skip gjrGARCH in Distributional Metrics (Quick Fix)

Modify `scripts/evaluation/calculate_distributional_metrics.R` to only process models that have NF residuals:

```r
# Process each model and asset combination
models <- c("sGARCH", "eGARCH", "TGARCH")  # Remove gjrGARCH temporarily
assets <- c("EURUSD", "GBPUSD", "USDZAR", "NVDA", "MSFT", "AMZN")
```

### Option 2: Train NF Models for gjrGARCH (Complete Fix)

Run NF training for gjrGARCH residuals:

```python
# In Cursor/Jupyter:
# 1. Ensure gjrGARCH residuals exist in outputs/manual/residuals_by_model/gjrGARCH/
# 2. Run NF training for gjrGARCH (should auto-discover if residuals exist)
```

### Option 3: Make Script More Robust (Best Fix)

Update the script to handle missing files gracefully:

```r
# In calculate_distributional_metrics.R, around line 219:
# Compare standard vs NF residuals if both available
if (file.exists(standard_residual_file) && file.exists(nf_residual_file)) {
  tryCatch({
    standard_residuals <- read.csv(standard_residual_file, header = FALSE)[[1]]
    nf_residuals <- read.csv(nf_residual_file, header = FALSE)[[1]]
    
    # Check for headers and skip if present
    if (is.character(standard_residuals[1]) && standard_residuals[1] == "residual") {
      standard_residuals <- standard_residuals[-1]
    }
    if (is.character(nf_residuals[1]) && nf_residuals[1] == "synthetic_residuals") {
      nf_residuals <- nf_residuals[-1]
    }
    
    standard_residuals <- as.numeric(standard_residuals[!is.na(standard_residuals)])
    nf_residuals <- as.numeric(nf_residuals[!is.na(nf_residuals)])
    
    if (length(standard_residuals) > 10 && length(nf_residuals) > 10) {
      # Standardize both
      standard_residuals <- (standard_residuals - mean(standard_residuals)) / sd(standard_residuals)
      nf_residuals <- (nf_residuals - mean(nf_residuals)) / sd(nf_residuals)
      
      metrics$KS_distance <- calculate_ks_distance(standard_residuals, nf_residuals)
      metrics$Wasserstein_distance <- calculate_wasserstein_distance(standard_residuals, nf_residuals)
    }
  }, error = function(e) {
    cat("  Warning: Could not calculate KS/Wasserstein for", model_name, "-", asset_name, ":", e$message, "\n")
  })
}
```

---

## 📊 Current Status

### What's Working:
- ✅ Tail index calculated (for all models with standard residuals)
- ✅ Skewness calculated (for all models with standard residuals)
- ✅ Kurtosis calculated (for all models with standard residuals)
- ✅ Standard residual metrics complete

### What's Missing:
- ❌ KS_distance for gjrGARCH (missing NF residuals)
- ❌ Wasserstein_distance for gjrGARCH (missing NF residuals)
- ❌ KS_distance for other models (may be CSV reading issue)

---

## 🔧 Recommended Action

**Immediate Fix:** Update script to handle missing gjrGARCH files gracefully (Option 3)

**Long-term:** Train NF models for gjrGARCH if you want complete distributional comparison

---

## 🔍 Verification Steps

After applying fix:

1. Check that gjrGARCH standard residuals exist:
   ```r
   list.files("outputs/manual/residuals_by_model/gjrGARCH/")
   ```

2. Check that gjrGARCH NF residuals are generated (or script skips them):
   ```r
   list.files("outputs/manual/nf_models/", pattern = "*gjrGARCH*")
   ```

3. Re-run distributional metrics:
   ```r
   source("scripts/evaluation/calculate_distributional_metrics.R")
   ```

4. Verify results:
   ```r
   dist_metrics <- read.xlsx("results/consolidated/Distributional_Metrics.xlsx", sheet = "Distributional_Metrics")
   # Check for NaN values
   sum(is.nan(dist_metrics$KS_distance))
   sum(is.nan(dist_metrics$Wasserstein_distance))
   ```

---

*Issue identified: gjrGARCH NF residuals not generated during NF training phase*

