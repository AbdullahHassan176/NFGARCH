# Final Pipeline Run Configuration - 2026-02-07

## Run Started
**Time**: 2026-02-07 ~09:45 UTC
**Command**: `run_both_pipelines.bat`

---

## Configuration Changes

### Simulation Paths: 200 → 100
**Reason**: Faster execution while maintaining statistical validity

**Files Modified**:
1. `scripts/simulation_forecasting/simulate_nf_garch_engine.R`
   - Line 304: `n_paths = 100` (was 200)
   - Line 588: `n_paths = 100` (was 200)

2. `scripts/evaluation/compare_nf_vs_standard_garch.R`
   - Line 189: `n_paths <- 100L` (was 200L)

3. `scripts/evaluation/stress_testing_comprehensive.R`
   - All occurrences: `n_paths = 100` (was 200)

**Impact**:
- ~50% faster execution
- Still sufficient for KDE-based predictive log-likelihood
- 100 paths >> recommended minimum of 50 for stable estimates

---

## All Fixes Applied

### Critical Fixes (Session 2026-02-07):
1. ✅ **Residual Standardization Validation** - Added strict checks
2. ✅ **Removed Forced Standardization** - NF samples no longer modified
3. ✅ **Filename Mismatch** - Fixed `_Manual_Optimized_residuals.csv`
4. ✅ **Logical Operator Bug** - Fixed `isTRUE()` handling
5. ✅ **Cleaned All Outputs** - Fresh start for all pipelines

### Previous Critical Fixes:
6. ✅ **sGARCH Optimizer** - L-BFGS-B with proper bounds
7. ✅ **Parametric Sampling** - Standard GARCH uses theoretical distributions
8. ✅ **Parameter Handling** - Batch file `/Y` logic fixed

---

## Pipeline Structure

### 1. Chronological Pipeline (`run_chronological.bat`)
**Steps**:
1. Fit GARCH models on 65% train data
2. Extract & validate standardized residuals
3. Train NF on validated residuals (NO forced standardization)
4. Simulate NF-GARCH forecasts (100 paths)
5. Compare NF-GARCH vs Standard GARCH
6. Generate evaluation metrics

**Output**: `outputs/chronological/`

---

### 2. Time-Series CV Pipeline (`run_tscv.bat`)
**Steps**:
1. Rolling window setup (3 windows, 65% size, 15% step)
2. For each window:
   - Fit GARCH models
   - Extract & validate residuals
   - Train NF
   - Simulate forecasts (100 paths)
3. Aggregate results across windows
4. Generate robustness metrics

**Output**: `outputs/tscv/`

---

## Expected Runtime
- **Chronological**: ~30-45 minutes
- **TS-CV**: ~90-120 minutes
- **Total**: ~2-2.5 hours (with 100 paths)

**Note**: With 200 paths, total was ~3-4 hours

---

## Success Criteria

### GARCH Fitting:
- [ ] All models converge
- [ ] Residuals pass validation: `|mean| < 0.05`, `|std - 1| < 0.1`
- [ ] No "SKIPPING NF training" warnings

### NF Training:
- [ ] All NF models train successfully
- [ ] NF samples naturally standardized (no warnings)
- [ ] Loss converges (decreases over epochs)

### Evaluation:
- [ ] All metrics calculated (MSE, MAE, Predictive LogLik)
- [ ] Valid paths: ~80-100 out of 100
- [ ] No validation errors in comparison script

---

## Key Files to Monitor

### Progress Logs:
- Terminal output: `terminals/57568.txt`
- R console output (inline)

### Results Files:
- `outputs/chronological/consolidated/NF_GARCH_Results_manual.xlsx`
- `outputs/tscv/consolidated/NF_GARCH_Results_tscv.xlsx`
- `outputs/evaluation/Comparison_Results.xlsx`

### Validation Checkpoints:
- Residual statistics during GARCH fitting
- NF sample statistics during NF training
- Valid paths count in evaluation

---

## What Changed vs Previous Runs

### Methodology:
1. **Residuals are validated** before NF training
2. **NF samples are NOT forced** to be standardized
3. **Standard GARCH uses parametric** sampling (not bootstrap)

### Configuration:
1. **100 paths** instead of 200 (faster)
2. **Optimized mode** (4 layers, 64 features, 75 epochs)
3. **All old outputs cleared** (fresh start)

---

## Expected Results

### If Fixes Work:
- ✓ Residuals will be properly standardized (mean≈0, std≈1)
- ✓ NF samples will be naturally standardized
- ✓ NF-GARCH may show improved performance
- ✓ Fair comparison between NF and Standard GARCH

### If NF Still Doesn't Outperform:
- Conclusion: For these assets, Student-t parametric is sufficient
- NF-GARCH complexity not justified
- Valid scientific finding (not a bug)

---

## Monitoring Commands

Check progress:
```powershell
Get-Content terminals/57568.txt -Tail 50
```

Check for errors:
```powershell
Select-String "ERROR|WARNING|SKIPPING" terminals/57568.txt
```

Check running processes:
```powershell
Get-Process | Where-Object { $_.ProcessName -like "*Rscript*" -or $_.ProcessName -like "*python*" }
```

---

## Post-Run Analysis

After completion, analyze:
1. **Residual statistics** - Were they standardized?
2. **NF sample statistics** - Were they naturally standardized?
3. **Performance metrics** - Did NF outperform?
4. **Valid paths** - How many succeeded per model?
5. **Predictive LogLik** - Where NF should excel

---

**Status**: Running...  
**Terminal**: `terminals/57568.txt`  
**Estimated Completion**: ~12:00-12:30 UTC
