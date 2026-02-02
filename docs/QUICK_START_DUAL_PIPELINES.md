# Quick Start Guide: Dual Pipeline System

## What Was Implemented

A complete dual validation system for your NF-GARCH dissertation with:

✅ **Chronological Split Pipeline** - Pure 65/35 split with NO data leakage  
✅ **Time-Series CV Pipeline** - Rolling window validation with 5 folds  
✅ **Comparative Analysis** - Meta-analysis comparing both methods  
✅ **Batch Files** - Ready-to-run Windows scripts  
✅ **Documentation** - Complete README and this quick start guide

## Quick Start (Choose One)

### Option 1: Test with Chronological (Fastest - ~2 hours)
```batch
run_chronological.bat
```
**Best for**: Initial testing, iterative development

### Option 2: Full Dissertation Analysis (8-10 hours)
```batch
run_both_pipelines.bat
```
**Best for**: Final dissertation results, publication-ready validation

### Option 3: Just TS CV (~6-8 hours)
```batch
run_tscv.bat
```
**Best for**: Temporal stability analysis

## What Each Pipeline Does

### Chronological Pipeline
1. Splits data: 65% train / 35% test (one time)
2. Fits GARCH on training set
3. Trains NF on training residuals (NO validation split - key fix!)
4. Evaluates on test set
5. Generates results in `results/chronological/`

### TS CV Pipeline
1. Creates 5 rolling windows
2. Fits GARCH per window
3. Trains NF per window (each window gets its own model)
4. Evaluates per window
5. Aggregates results in `results/tscv/`

### Comparison Analysis
1. Loads results from both pipelines
2. Compares performance metrics
3. Analyzes temporal stability
4. Identifies winner consistency
5. Saves to `results/comparison/`

## Key Files Created

### Configuration
- `scripts/config/chrono_split_config.R` - Chronological settings
- `scripts/config/tscv_split_config.R` - TS CV settings

### GARCH Fitting
- `scripts/chronological/fit_garch_chronological.R`
- `scripts/tscv/fit_garch_tscv.R`

### NF Training
- `scripts/chronological/train_nf_chronological.py`
- `scripts/tscv/train_nf_tscv.py`

### Evaluation
- `scripts/evaluation/evaluation_split_config.R` - Helper for all evaluation scripts
- `scripts/evaluation/compare_chronological_vs_tscv.R` - Meta-analysis

### Batch Files
- `run_chronological.bat` - Run chronological pipeline
- `run_tscv.bat` - Run TS CV pipeline
- `run_both_pipelines.bat` - Run both + comparison

### Documentation
- `DUAL_PIPELINE_README.md` - Complete documentation
- `QUICK_START_DUAL_PIPELINES.md` - This file

## Critical Fix: Data Leakage Prevention

### Previous Issue
Your old pipeline had **nested splits** that created data leakage:
- GARCH residuals from 65% of data
- NF training used 80% of those (= 52% of original)
- NF validation used 20% of residuals (= 13% of original)
- This 13% overlapped with simulation training period!

### New Solution
**Chronological Pipeline**: NF trains on 100% of training residuals (no validation split)
**TS CV Pipeline**: Each window is independent (no overlap)

This ensures **NO data leakage** and **valid evaluation**.

## Expected Outputs

### Chronological Results
```
results/chronological/
├── consolidated/
│   └── NF_GARCH_Results_chronological.xlsx
├── dissertation_tables/
├── figures/
└── diagnostics/
```

### TS CV Results
```
results/tscv/
├── consolidated/
│   └── NF_GARCH_Results_tscv.xlsx
├── dissertation_tables/
├── figures/
└── diagnostics/
```

### Comparison Results
```
results/comparison/
└── Chronological_vs_TSCV_Analysis.xlsx
```

## Interpreting Results

### If Both Methods Agree on Best Model
✅ **Strong Validation**
- Model is robust across validation strategies
- Safe to recommend for practical use
- Emphasize in dissertation: "Consistent across temporal conditions"

### If Methods Disagree
⚠️ **Important Finding**
- Model performance varies with market conditions
- Discuss temporal instability in dissertation
- Provide nuanced recommendations

### Temporal Stability (from TS CV)
- **Low CV** (<0.1): High stability across time
- **Medium CV** (0.1-0.3): Moderate variation
- **High CV** (>0.3): Significant temporal instability

## Troubleshooting

### "GARCH fitting failed"
- Check: `data/processed/raw (FX + EQ).csv` exists
- Verify: R packages installed (xts, rugarch, etc.)
- Review: `outputs/chronological/garch_fitting/model_summary.csv`

### "NF training failed"
- Check: Python packages (torch, nflows)
- Verify: Residuals exist in `outputs/*/residuals_by_model/`
- Review: `outputs/*/nf_models/training_summary.json`

### "Comparison script failed"
- Ensure: Both pipelines completed
- Check: Result files exist in `results/*/consolidated/`
- Run: Each pipeline separately first

## Next Steps

1. **Run Chronological First** (test the system)
   ```batch
   run_chronological.bat
   ```

2. **Review Results** (check outputs)
   ```
   results/chronological/consolidated/
   ```

3. **If Successful, Run Both** (full validation)
   ```batch
   run_both_pipelines.bat
   ```

4. **Analyze Comparison** (meta-analysis)
   ```
   results/comparison/Chronological_vs_TSCV_Analysis.xlsx
   ```

5. **Update Dissertation** (integrate findings)
   - Chapter 3: Methodology
   - Chapter 4: Results
   - Chapter 5: Discussion

## Time Estimates

- **Chronological**: ~2 hours
- **TS CV**: ~6-8 hours
- **Both + Comparison**: ~8-10 hours

**Recommendation**: Run overnight or during a work day

## Getting Help

1. Check `DUAL_PIPELINE_README.md` for details
2. Review plan file in `.cursor/plans/`
3. Examine batch file outputs for error messages
4. Check log files in `outputs/*/`

## Academic Impact

This dual pipeline approach:
- ✅ Addresses reviewer concerns about overfitting
- ✅ Demonstrates methodological rigor
- ✅ Provides robust validation
- ✅ Enables temporal stability analysis
- ✅ Strengthens publication chances

**Result**: A dissertation-quality validation strategy that goes beyond typical single-split approaches.

---

**Ready to start?**

```batch
# Test first with chronological (fastest)
run_chronological.bat

# Then run full analysis (overnight)
run_both_pipelines.bat
```

Good luck with your dissertation! 🎓
