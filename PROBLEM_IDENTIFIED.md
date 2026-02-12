# ROOT CAUSE IDENTIFIED - DIFFERENT PIPELINES USE DIFFERENT SCRIPTS!

## The Problem

The main pipeline (`run_full_dissertation.bat` → `run_all.bat`) uses:
- **Scripts**: `scripts/manual/`
- **Results**: NF-GARCH WINS (19 out of 25)
- **MSE**: NF=0.000365 vs Standard=0.000370

The chronological/TSCV pipelines use:
- **Scripts**: `additional_analysis/scripts/chronological/` and `additional_analysis/scripts/tscv/`
- **Results**: Standard GARCH WINS  
- **MSE**: NF=0.000370 vs Standard=0.000355

## Why This Is Wrong

These are **COMPLETELY DIFFERENT IMPLEMENTATIONS**!

The user is right - we should use the SAME methodology (manual scripts) but just change the DATA SPLITTING strategy.

## What Should Happen

All three pipelines should use:
1. **Same GARCH fitting**: `scripts/manual/manual_garch_fitting.R` (or equivalent logic)
2. **Same NF training**: `scripts/manual/manual_nf_training.py`
3. **Same simulation**: `scripts/simulation_forecasting/simulate_nf_garch_engine.R`
4. **Same comparison**: `scripts/evaluation/compare_nf_vs_standard_garch.R`

**ONLY DIFFERENCE**: Data splitting method
- Main: CV-based splitting (existing)
- Chronological: 65/35 chronological split
- TS-CV: Rolling window TS-CV

## The Fix

Need to either:
1. Unify all pipelines to use `scripts/manual/` with different split configs
2. Or fix the `additional_analysis/` scripts to match the manual pipeline's methodology exactly

The user wants option 1: Use the working manual pipeline, just change data splitting.
