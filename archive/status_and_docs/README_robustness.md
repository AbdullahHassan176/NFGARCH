# GARCH Order Robustness Experiment

## Purpose

This experiment addresses examiner feedback by testing whether NF-GARCH gains persist when allowing higher-order GARCH models. The experiment compares the best classical GARCH(p,q) model (selected by BIC) against an NF-GARCH model using the same selected order.

## What is Tested

- **GARCH Orders**: (1,1), (2,1), (1,2), (2,2)
- **Model Families**: sGARCH, eGARCH, gjrGARCH
- **Distributions**: norm, sstd (same as existing pipeline)
- **Order Selection**: BIC on training data
- **Evaluation**: MSE, MAE, LogLikelihood on test set (chronological 65/35 split)

## How It Works

1. For each asset/model family/distribution combination:
   - Test all GARCH orders on training data
   - Select best order by BIC
   - Evaluate classical GARCH with selected order on test set
   - Fit NF-GARCH with same selected order
   - Evaluate NF-GARCH on test set
   - Calculate deltas (NF - Classical)

2. Results are written to:
   - CSV: `outputs/robust_garch_order/garch_order_robustness_results.csv`
   - Excel: `outputs/robust_garch_order/garch_order_robustness_results.xlsx`
   - LaTeX: `outputs/robust_garch_order/garch_order_robustness_table.tex`

## How to Run

### From R/RStudio:

```r
setwd("C:/Experimentation/NFGARCH")  # Adjust path as needed
source("scripts/experiments/robustness_garch_order.R")
```

### From Command Line:

```bash
Rscript scripts/experiments/robustness_garch_order.R
```

### Prerequisites

- R packages: `rugarch`, `xts`, `PerformanceAnalytics`, `dplyr`, `openxlsx`, `stringr`, `lubridate`
- NF residual files must exist in `outputs/manual/nf_models/` (pattern: `*_synthetic_residuals.csv`)
- Data file: `data/processed/raw (FX + EQ).csv`

## Output Columns

- `asset`: Asset name
- `model_family`: GARCH model family (sGARCH, eGARCH, gjrGARCH)
- `dist`: Distribution (norm, sstd)
- `selected_p`, `selected_q`: Best GARCH order selected by BIC
- `classical_BIC`, `classical_AIC`, `classical_LL`: Information criteria from training
- `classical_MSE`, `classical_MAE`, `classical_LogLik`: Test set metrics for classical GARCH
- `nf_MSE`, `nf_MAE`, `nf_LogLik`: Test set metrics for NF-GARCH
- `delta_MSE`, `delta_MAE`, `delta_LogLik`: NF - Classical (negative = NF better)

## Interpretation

- **Negative delta_MSE/MAE**: NF-GARCH performs better
- **Positive delta_MSE/MAE**: Classical GARCH performs better
- **Selected order**: Shows whether higher-order GARCH was selected (if not (1,1))

## Notes

- This experiment uses `rugarch` for GARCH fitting (supports different orders natively)
- The main pipeline uses manual engine (GARCH(1,1) only), but this is acceptable for a separate robustness test
- Failed fits are caught and logged; the experiment continues
- NF residuals must be pre-trained and available in the outputs directory

## Branch

This experiment is in branch: `robust-garch-order-quicktest`



