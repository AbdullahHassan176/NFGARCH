# Pipeline Execution Guide

## Main Dissertation Pipeline

### `run_all.bat`
**Purpose:** Core manual pipeline used for main dissertation results

**What it does:**
1. GARCH fitting (`scripts/manual/manual_garch_fitting.R`)
2. NF training (`scripts/manual/manual_nf_training.py`)
3. NF-GARCH simulation (`scripts/simulation_forecasting/simulate_nf_garch_engine.R`)
4. Comparison analysis (`scripts/evaluation/compare_nf_vs_standard_garch.R`)
5. Distributional metrics
6. Stylized facts
7. VaR backtesting
8. Stress testing
9. Residual stationarity tests
10. Extract dissertation tables

**Runtime:** 60-120 minutes

**Outputs:**
- `outputs/manual/` - All model outputs
- `results/consolidated/` - Excel dashboards
- `results/dissertation_tables/` - CSV tables for LaTeX

---

### `run_full_dissertation.bat`
**Purpose:** Complete dissertation pipeline with all analyses

**What it does:**
1. Runs `run_all.bat /Y` (main pipeline)
2. Runs GARCH order robustness (`run_robustness_garch_order.bat`)
3. Runs complete analysis consolidation
4. Exports to `overleaf_export/`

**Runtime:** 60-150+ minutes

**This is the ONE-COMMAND full dissertation run.**

---

## Alternative Validation Pipelines

### `run_chronological.bat`
**Purpose:** Alternative validation using pure chronological split (no CV)

**Uses:** `additional_analysis/scripts/chronological/`

**Difference from main:** Simpler chronological split instead of CV-based model selection

**Runtime:** ~60-90 minutes

**Outputs:** `outputs/chronological/`, `results/chronological/`

---

### `run_tscv.bat`
**Purpose:** Alternative validation using rolling TS-CV windows

**Uses:** `additional_analysis/scripts/tscv/`

**Difference from main:** Rolling window TS-CV for temporal robustness

**Runtime:** ~4-8 hours

**Outputs:** `outputs/tscv/`, `results/tscv/`

---

### `run_both_pipelines.bat`
**Purpose:** Run both alternative validation pipelines sequentially

**Runtime:** ~8-10 hours

**Outputs:** Both chronological and TS-CV results

---

## Additional Experiments

### `run_robustness_garch_order.bat`
**Purpose:** Test GARCH order selection robustness

**Runtime:** ~30 minutes

**Outputs:** `outputs/robust_garch_order/`

---

## Coverage Confirmation

### What `run_full_dissertation.bat` covers:
 GARCH fitting (6 assets, 4 models)
 NF training (all models)
 NF-GARCH simulation
 Comparison analysis
 Distributional metrics
 Stylized facts
 VaR backtesting
 Stress testing (GFC 2008, COVID-19)
 Residual stationarity tests
 GARCH order robustness
 Dissertation tables generation
 Overleaf export

### What the alternative pipelines add:
 Alternative validation strategy (chronological)
 Alternative validation strategy (TS-CV)
 Robustness across different data splitting methods

---

## Quick Start

### For dissertation submission:
```bash
run_full_dissertation.bat
```
This single command runs everything needed for your dissertation.

### For additional validation (optional):
```bash
run_chronological.bat # Quick alternative validation
run_tscv.bat # Comprehensive temporal validation
run_both_pipelines.bat # Both alternatives (long runtime)
```

---

## Summary

**Main pipeline** (`run_all.bat`, `run_full_dissertation.bat`):
- Uses `scripts/manual/` 
- Your dissertation results
- Core analysis

**Alternative pipelines** (`run_chronological.bat`, `run_tscv.bat`):
- Use `additional_analysis/scripts/`
- Optional validation experiments
- Show robustness across validation strategies

**All scripts and data are in the main repo.**
**The `additional_analysis/` folder just contains alternative pipeline scripts.**
