# GARCH Order Robustness Experiment - Changes Summary

## What Changed

### New Files Created

1. **`scripts/experiments/robustness_garch_order.R`**
   - Main experiment script
   - Tests GARCH orders: (1,1), (2,1), (1,2), (2,2)
   - Selects best order by BIC on training data
   - Compares classical vs NF-GARCH with same selected order
   - Outputs CSV, Excel, and LaTeX results

2. **`scripts/experiments/README_robustness.md`**
   - Documentation for the robustness experiment
   - Explains purpose, methodology, and how to run

3. **`scripts/experiments/CHANGES_SUMMARY.md`** (this file)
   - Summary of changes made

4. **`run_robustness_garch_order.bat`**
   - Windows batch script to run the experiment

### Modified Files

None (experiment is isolated in separate branch)

### Output Directory

- `outputs/robust_garch_order/` (created automatically)

## Key Implementation Details

### Order Selection
- Uses `rugarch` package for GARCH fitting (supports different orders)
- Tests all orders on training data
- Selects best by BIC (lower is better)
- Handles convergence failures gracefully

### Evaluation Approach
- **Classical GARCH**: Uses `ugarchforecast` for point forecasts
- **NF-GARCH**: Uses `ugarchpath` with NF-generated innovations for simulation
- Both evaluated on same test set (chronological 65/35 split)
- Metrics: MSE, MAE, LogLikelihood

### NF Residuals
- Loads from `outputs/manual/nf_models/*_synthetic_residuals.csv`
- Standardizes residuals before use
- Handles missing files gracefully (stores NA for NF metrics)

## How to Run

### Quick Start
```bash
# Windows
run_robustness_garch_order.bat

# Or from R
Rscript scripts/experiments/robustness_garch_order.R
```

### Prerequisites
- R packages: `rugarch`, `xts`, `PerformanceAnalytics`, `dplyr`, `openxlsx`, `stringr`, `lubridate`
- NF residual files in `outputs/manual/nf_models/`
- Data file: `data/processed/raw (FX + EQ).csv`

## Expected Runtime

- ~5-15 minutes depending on:
  - Number of assets (default: 6)
  - Number of model families (3: sGARCH, eGARCH, gjrGARCH)
  - Number of distributions (2: norm, sstd)
  - Number of orders tested (4 per combination)

## Output Files

1. **CSV**: `outputs/robust_garch_order/garch_order_robustness_results.csv`
   - Full results with all metrics

2. **Excel**: `outputs/robust_garch_order/garch_order_robustness_results.xlsx`
   - Same data in Excel format

3. **LaTeX**: `outputs/robust_garch_order/garch_order_robustness_table.tex`
   - Summary table for dissertation

## Interpretation

- **Negative delta_MSE/MAE**: NF-GARCH performs better
- **Positive delta_MSE/MAE**: Classical GARCH performs better
- **Selected order**: Shows if higher-order GARCH was selected (if not (1,1))

## Notes

- This experiment uses `rugarch` (not manual engine) for order flexibility
- Main pipeline uses manual engine (GARCH(1,1) only)
- This is acceptable for a separate robustness test
- Failed fits are logged and skipped; experiment continues

## Branch

All changes are in branch: `robust-garch-order-quicktest`

