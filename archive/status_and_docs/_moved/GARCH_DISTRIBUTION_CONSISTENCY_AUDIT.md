# GARCH Distribution (norm vs sstd) – Consistency Audit

## Summary

GARCH models can use **norm** (normal) or **sstd** (skewed Student-t; in the manual engine **sstd is mapped to std**, i.e. symmetric Student-t). This audit lists where each is used and what was changed for consistency.

---

## 1. Manual engine mapping

In `scripts/engines/engine_selector.R`:

```r
manual_dist <- if (dist == "sstd") "std" else dist
```

- **norm** → fitted as Normal.
- **sstd** → fitted as **std** (symmetric Student-t). Skewed-t is not implemented in the manual engine.

So all configs that request **sstd** are effectively fitted with **std** in the main pipeline.

---

## 2. Main pipeline

### 2.1 `scripts/manual/manual_optimized_config.R` (MANUAL_MODEL_CONFIG)

**Used by:** `manual_garch_fitting.R` (Step 2 GARCH fitting and residuals for NF training).

| Model       | Before   | After   |
|------------|----------|---------|
| sGARCH_norm  | norm     | norm    |
| sGARCH_sstd  | sstd     | sstd    |
| eGARCH     | **norm** | **sstd** |
| TGARCH     | **norm** | **sstd** |
| gjrGARCH   | **norm** | **sstd** |

**Change:** eGARCH, TGARCH, gjrGARCH set from `"norm"` to `"sstd"` so they match simulate, compare, stress, and extract_residuals.

---

### 2.2 `scripts/simulation_forecasting/simulate_nf_garch_engine.R` (model_configs)

**Used by:** NF-GARCH simulation (Step 4), Chrono and TS CV.

| Config       | distribution |
|-------------|--------------|
| sGARCH_norm | norm         |
| sGARCH_sstd | sstd         |
| gjrGARCH    | sstd         |
| eGARCH      | sstd         |
| TGARCH      | sstd         |

No change. Same as extract_residuals and (after the fix) manual_optimized_config for e/T/gjr.

---

### 2.3 `scripts/evaluation/compare_nf_vs_standard_garch.R` (model_configs)

**Used by:** Standard GARCH baseline and NF vs Standard comparison.

| Model     | distribution | Note                           |
|----------|--------------|---------------------------------|
| sGARCH   | sstd         | Single sGARCH variant only      |
| eGARCH   | sstd         |                                 |
| TGARCH   | sstd         |                                 |
| gjrGARCH | sstd         |                                 |

No sGARCH_norm. Compared to simulate, which has both sGARCH_norm and sGARCH_sstd. Left as is; compare’s “sGARCH” is aligned with sGARCH_sstd.

---

### 2.4 `scripts/evaluation/stress_testing_comprehensive.R` (model_configs)

| Model     | distribution |
|----------|--------------|
| sGARCH   | sstd         |
| eGARCH   | sstd         |
| TGARCH   | sstd         |
| gjrGARCH | sstd         |

No change.

---

### 2.5 `scripts/model_fitting/extract_residuals.R` (model_configs)

| Config       | distribution |
|-------------|--------------|
| sGARCH_norm | norm         |
| sGARCH_sstd | sstd         |
| gjrGARCH    | sstd         |
| eGARCH      | sstd         |
| TGARCH      | sstd         |

No change. Matches simulate.

---

### 2.6 `scripts/model_fitting/fit_garch_models.R` (model_configs)

| Model   | distribution | Note        |
|--------|--------------|-------------|
| sGARCH | sstd         | One variant |
| eGARCH | sstd         |             |
| TGARCH | sstd         |             |

No gjrGARCH, no sGARCH_norm. Left as is; this script is not in the main run_all flow.

---

## 3. Scripts that do not fit GARCH

- **`var_backtesting_comprehensive.R`**: Empirical VaR/ES on actual returns; no GARCH distribution.
- **`calculate_distributional_metrics.R`**: Reads residuals from disk; no GARCH fitting.
- **`calculate_stylized_facts.R`**: Works on returns/residuals; no GARCH dist config.
- **`complete_analysis.R`**: `Distribution = "sstd"` in `failed_models` is a label only.

---

## 4. Additional / experimental scripts

### 4.1 `scripts/experiments/robustness_garch_order.R`

- **DISTRIBUTIONS** = `c("norm", "sstd")` with **rugarch**.
- Intentionally runs both norm and sstd. No change.

### 4.2 `scripts/experiments/synthetic_recovery/` (run_synthetic_recovery, run_audit, run_full_audit)

- Use **norm** and **std** for **sGARCH** in a controlled DGP.
- Purpose: recovery of a known innovation distribution. Deliberately norm vs std; no change.

---

## 5. Manual fit backends (`scripts/manual_garch/`)

- **fit_sgarch_manual.R**, **fit_gjr_manual.R**, **fit_egarch_manual.R**, **fit_tgarch_manual.R**:  
  `dist = c("norm", "std")` only. **sstd** is turned into **std** in `engine_selector.R` before calling these.

---

## 6. Consistency after the fix

- **eGARCH, TGARCH, gjrGARCH**: `sstd` in  
  - manual_optimized_config (and thus manual_garch_fitting),  
  - simulate_nf_garch_engine,  
  - compare_nf_vs_standard_garch,  
  - stress_testing_comprehensive,  
  - extract_residuals.
- **sGARCH**:  
  - **norm**: `sGARCH_norm` in manual_optimized_config, simulate, extract_residuals.  
  - **sstd**: `sGARCH_sstd` in manual_optimized_config, simulate, extract_residuals; and the single **sGARCH** in compare and stress.

---

## 7. Files touched

- **`scripts/manual/manual_optimized_config.R`**: In `MANUAL_MODEL_CONFIG`, set  
  `distribution` for **eGARCH**, **TGARCH**, **gjrGARCH** from `"norm"` to `"sstd"`.
