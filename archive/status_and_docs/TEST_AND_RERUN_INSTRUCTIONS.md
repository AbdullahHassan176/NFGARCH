# Test and Rerun Instructions

## ✅ Code Changes Verified

All code changes have been implemented and verified:
- ✅ No linter errors
- ✅ Functions properly documented
- ✅ Error handling included
- ✅ Consistent with existing code style

## 🧪 Testing Steps

### Step 1: Quick Test (Recommended First)

Run the test script to verify everything works on a single asset/model:

```bash
# If Rscript is in PATH:
Rscript scripts/utils/test_return_forecast_evaluation.R

# Or if using full path:
"C:\Program Files\R\R-4.x.x\bin\Rscript.exe" scripts/utils/test_return_forecast_evaluation.R
```

**Expected Output:**
- Model fits successfully
- Generates 100 paths (reduced for testing)
- Calculates MSE, MAE, PredictiveLogLik
- Shows ~100 valid paths
- All validation checks pass

**If test passes:** Proceed to full pipeline rerun
**If test fails:** Check error messages and fix issues

### Step 2: Verify Test Results

The test should output:
```
=== TEST RESULTS ===
MSE: [number]
MAE: [number]
Predictive Log-Likelihood: [number]
Number of valid paths: ~100
Point forecast length: 20
Sigma forecast length: 20

=== VALIDATION CHECKS ===
  [PASS] MSE and MAE are valid
  [PASS] Sufficient valid paths: ~100
  [PASS] Point forecast length correct
  [PASS] All point forecasts are valid

=== TEST PASSED ===
```

## 🔄 Full Pipeline Rerun

### Option A: Run Complete Pipeline (Recommended)

This runs everything from scratch:

```bash
run_all.bat
```

**What it does:**
1. Clears previous outputs
2. Fits GARCH models
3. Trains NF models
4. Runs NF-GARCH simulation (with new return forecast evaluation)
5. Compares NF-GARCH vs Standard GARCH
6. Calculates all metrics
7. Generates results and tables

**Expected time:** 60-120 minutes

### Option B: Run Only Simulation (Faster)

If you only want to regenerate forecast results:

```bash
# Run simulation with new evaluation
Rscript scripts/simulation_forecasting/simulate_nf_garch_engine.R --engine manual

# Then run comparison
Rscript scripts/evaluation/compare_nf_vs_standard_garch.R

# Then consolidate
Rscript -e "source('scripts/core/consolidation.R'); consolidate_all_results('results/consolidated')"
```

**Expected time:** 15-30 minutes

## 📊 What Will Change

### Results Files
- `results/consolidated/NF_GARCH_Results_manual.xlsx`
  - New columns: `PredictiveLogLik`, `NPaths`
  - Updated MSE/MAE values (proper evaluation)
  
- `results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx`
  - Updated comparison metrics
  - Both models use same evaluation method

- `results/consolidated/Stress_Testing.xlsx`
  - Updated stress test results
  - Uses multiple paths for both NF-GARCH and Standard GARCH

### Expected Changes
1. **MSE/MAE values will be different**
   - Old: Single path comparison (unstable, random)
   - New: Point forecast evaluation (stable, interpretable)
   - Values should be more stable across runs

2. **New metric: PredictiveLogLik**
   - Tests density forecast quality
   - Higher = better predictive distribution

3. **More stable results**
   - Not dependent on random seed
   - Multiple paths average out randomness

## ⚠️ Important Notes

### Before Rerunning
- **Backup existing results** if you want to compare
- Ensure you have **4-6 hours** for full pipeline
- Check that **NF models are already trained** (or allow time for training)

### After Rerunning
1. **Check results** for reasonable values
2. **Compare** new MSE/MAE to old values (they will differ)
3. **Update dissertation** with new values and interpretation
4. **Regenerate tables** if using automated table generation

## 🔍 Verification Checklist

After rerun, verify:

- [ ] Results files generated successfully
- [ ] MSE/MAE values are reasonable (not NaN, not extreme)
- [ ] PredictiveLogLik values present
- [ ] NPaths shows ~1000 for most results
- [ ] Both NF-GARCH and Standard GARCH have results
- [ ] No major errors in logs

## 📝 Next Steps After Rerun

1. **Update Dissertation Tables**
   - Regenerate LaTeX tables with new values
   - Update interpretation sections

2. **Update Dissertation Text**
   - Change "conditional variance forecasts" → "return forecasts"
   - Add methodology section on return forecast evaluation
   - Update results interpretation

3. **Review Results**
   - Check if improvements are still present
   - Verify statistical significance (Wilcoxon tests)
   - Update discussion based on new results

## 🐛 Troubleshooting

### If test fails:
1. Check R is installed and in PATH
2. Check data file exists: `data/processed/raw (FX + EQ).csv`
3. Check NF residuals exist for test asset
4. Review error messages in test output

### If pipeline fails:
1. Check logs in `logs/` directory
2. Verify all dependencies are installed
3. Check disk space (results can be large)
4. Review error messages in batch output

### If results look wrong:
1. Check NPaths column (should be ~1000)
2. Verify MSE/MAE are not NaN
3. Compare to old results (should be different but reasonable)
4. Check that both NF-GARCH and Standard GARCH have results

## 📞 Support

If you encounter issues:
1. Check `IMPLEMENTATION_SUMMARY.md` for what was changed
2. Review `docs/RETURN_FORECAST_EVALUATION.md` for methodology
3. Check code comments in updated files
4. Review error messages carefully
