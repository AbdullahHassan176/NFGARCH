# GARCH Order Robustness Experiment - Quick Guide

## Overview

This experiment tests whether NF-GARCH gains persist when allowing higher-order GARCH models, addressing examiner feedback that baseline GARCH(1,1) might be too simple.

## What Was Created

### Branch
- **Branch name**: `robust-garch-order-quicktest`
- **Status**: Ready to run

### Files Created
1. `scripts/experiments/robustness_garch_order.R` - Main experiment script
2. `scripts/experiments/README_robustness.md` - Detailed documentation
3. `scripts/experiments/CHANGES_SUMMARY.md` - Changes summary
4. `run_robustness_garch_order.bat` - Windows batch runner

## How to Run

### Option 1: Batch Script (Windows)
```bash
run_robustness_garch_order.bat
```

### Option 2: R Script
```r
# In R or RStudio
setwd("C:/Experimentation/NFGARCH")
source("scripts/experiments/robustness_garch_order.R")
```

### Option 3: Command Line
```bash
Rscript scripts/experiments/robustness_garch_order.R
```

## What It Tests

- **GARCH Orders**: (1,1), (2,1), (1,2), (2,2)
- **Model Families**: sGARCH, eGARCH, gjrGARCH
- **Distributions**: norm, sstd
- **Selection**: BIC on training data (65% split)
- **Evaluation**: MSE, MAE, LogLikelihood on test set (35% split)

## Output Files

Results are written to `outputs/robust_garch_order/`:

1. **`garch_order_robustness_results.csv`** - Full results
2. **`garch_order_robustness_results.xlsx`** - Excel format
3. **`garch_order_robustness_table.tex`** - LaTeX table for dissertation

## Output Columns

- `asset`: Asset name
- `model_family`: sGARCH, eGARCH, or gjrGARCH
- `dist`: norm or sstd
- `selected_p`, `selected_q`: Best order selected by BIC
- `classical_BIC`, `classical_AIC`, `classical_LL`: Training metrics
- `classical_MSE`, `classical_MAE`, `classical_LogLik`: Test metrics (classical)
- `nf_MSE`, `nf_MAE`, `nf_LogLik`: Test metrics (NF-GARCH)
- `delta_MSE`, `delta_MAE`, `delta_LogLik`: NF - Classical

## Interpretation

- **Negative delta**: NF-GARCH performs better
- **Positive delta**: Classical GARCH performs better
- **Selected order ≠ (1,1)**: Higher-order GARCH was selected

## Prerequisites

- R packages: `rugarch`, `xts`, `PerformanceAnalytics`, `dplyr`, `openxlsx`, `stringr`, `lubridate`
- NF residual files in `outputs/manual/nf_models/*_synthetic_residuals.csv`
- Data file: `data/processed/raw (FX + EQ).csv`

## Expected Runtime

5-15 minutes depending on:
- Number of assets (default: 6)
- Number of model families (3)
- Number of distributions (2)
- Number of orders tested (4 per combination)

## Example Command

```bash
# Run the experiment
run_robustness_garch_order.bat

# Or from R
Rscript scripts/experiments/robustness_garch_order.R
```

## Notes

- Uses `rugarch` for GARCH fitting (supports different orders)
- Main pipeline uses manual engine (GARCH(1,1) only)
- This is acceptable for a separate robustness test
- Failed fits are logged and skipped; experiment continues

## Next Steps

1. Run the experiment: `run_robustness_garch_order.bat`
2. Check results in `outputs/robust_garch_order/`
3. Review LaTeX table for dissertation inclusion
4. Analyze whether NF gains persist with higher-order GARCH

## Branch Status

All changes are committed to `robust-garch-order-quicktest` branch. No changes to main branch.



