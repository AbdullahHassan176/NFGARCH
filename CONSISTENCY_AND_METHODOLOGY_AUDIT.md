# Consistency and Methodology Audit

This document records issues found during a full pass over the NF-GARCH pipeline and the fixes applied, so the analysis is mathematically consistent and academically sound.

---

## 1. Return forecast evaluation – re-standardization (FIXED)

**File:** `scripts/utils/return_forecast_evaluation.R`

**Issue:** In `generate_multiple_paths`, after sampling `path_residuals` from already-standardized `nf_residuals`, the code re-standardized the sample to mean 0 and SD 1. The GARCH recursion expects innovations \(z_t\) with \(\mathbb{E}[z]=0\), \(\mathrm{Var}(z)=1\) in the population. The *population* of residuals is already standardized; re-standardizing the *sample* rescales the innovations and is incorrect.

**Fix:** Removed the re-standardization. Sampled path residuals are used as-is (with only `path_residuals[is.na(path_residuals)] <- 0`). A comment was added to document why re-standardization is wrong.

---

## 2. VaR backtesting – `calculate_es` and methodology note (FIXED)

**File:** `scripts/evaluation/var_backtesting_comprehensive.R`

**Issue 1:** `calculate_es(returns, var_level, confidence_level)` ignored `var_level` and recomputed VaR via `quantile(returns, 1 - confidence_level)`. ES should be \(\mathbb{E}[\text{returns} \mid \text{returns} \leq \text{VaR}]\) using the provided VaR.

**Fix:** `calculate_es` now uses `exceedances <- returns[returns <= var_level]` and no longer recomputes VaR.

**Issue 2:** The script uses *empirical* VaR (quantile of the test returns) as the VaR level for each day. That characterises tail risk in the data but is **not** model-based VaR backtesting. Proper backtesting would use one-step-ahead VaR forecasts from GARCH/NF-GARCH for each test date, then apply Kupiec/Christoffersen to those forecasts.

**Fix:** A NOTE was added in the script header describing this. Kupiec and Christoffersen remain implemented as in the literature; their use with a constant empirical VaR is documented as a limitation.

---

## 3. NF vs Standard comparison – model alignment and pairing (FIXED)

**File:** `scripts/evaluation/compare_nf_vs_standard_garch.R`

**Issue 1:** Standard GARCH baseline had only one sGARCH variant (sstd). NF-GARCH has `sGARCH_norm` and `sGARCH_sstd`. Win-rate and Wilcoxon paired (Model, Asset) but Standard had no `sGARCH_norm`, so sGARCH pairings were inconsistent.

**Fix:** Standard `model_configs` now include `sGARCH_norm` and `sGARCH_sstd` (and eGARCH, TGARCH, gjrGARCH) so (Model, Distribution) matches NF. Each Standard result row uses `Model = cfg$model` and `Distribution = cfg$distribution`.

**Issue 2:** Win-rate and Wilcoxon used `(Model, Asset)` (and for Wilcoxon, `Model` only). For sGARCH, NF has two distributions, so pairing must be by `(Model, Distribution, Asset)`.

**Fix:**  
- Win-rate: `group_by(Model, Distribution, Asset)` for pairing, then `group_by(Model, Distribution)` for the summary.  
- Wilcoxon: loop over `distinct(Model, Distribution)` and filter `Model == m, Distribution == d` before running the test. The Wilcoxon table includes `Model` and `Distribution`.

**Issue 3:** `comparison_by_model` grouped only by `(Model, Source)`, mixing sGARCH norm and sstd.

**Fix:** `comparison_by_model` now groups by `(Model, Distribution, Source)` so the Model_Comparison sheet is per (Model, Distribution).

---

## 4. eGARCH multi-step forecast – E|z| for Student-t (FIXED)

**File:** `scripts/manual_garch/manual_garch_core.R`

**Issue:** In `forecast_one_step` for eGARCH, \(E|z|\) was set to \(\sqrt{2/\pi}\) (normal) regardless of the fitted distribution. When the model is fitted with Student-t (`sstd` → `std` in the manual engine), the eGARCH recursion \(\ln\sigma_t^2 = \omega + \beta\ln\sigma_{t-1}^2 + \alpha(|z_{t-1}| - E|z|) + \gamma z_{t-1}\) requires \(E|z|\) for the Student-t, i.e. \(E|z| = \sqrt{\nu/\pi}\,\Gamma((\nu-1)/2)/\Gamma(\nu/2)\).

**Fix:** In the eGARCH branch of `forecast_one_step`, if `fit$distribution == "std"` and `"nu" %in% names(fit$coef)` with finite \(\nu > 2\), use `E_abs_t(nu)`; otherwise use \(\sqrt{2/\pi}\).

---

## 5. Kupiec test – edge cases (FIXED)

**File:** `scripts/evaluation/var_backtesting_comprehensive.R`

**Issue:** For the Kupiec unconditional coverage LR statistic, the edge cases `exceedances == 0` and `exceedances == total_obs` used the wrong formulas. Correct: LR = −2(log L(π) − log L(p̂)); when x=0, L(π)=(1−π)^n, L(p̂)=1 ⇒ LR = −2n·log(1−π); when x=n, L(π)=π^n, L(p̂)=1 ⇒ LR = −2n·log(π). The code had these reversed (and used the wrong π vs 1−π).

**Fix:** `exceedances == 0`: `LR_stat <- -2 * total_obs * log(1 - expected_rate)`. `exceedances == total_obs`: `LR_stat <- -2 * total_obs * log(expected_rate)`.

---

## 6. Compare `bind_rows` and VaR dead code (FIXED)

**Files:** `scripts/evaluation/compare_nf_vs_standard_garch.R`, `scripts/evaluation/var_backtesting_comprehensive.R`

**Issue (compare):** `select(-SplitType)` errors if `SplitType` is missing (e.g. older Excel). **Fix:** `select(-any_of("SplitType"))` so the column is dropped only when present.

**Issue (VaR):** The script loaded `NF_GARCH_Results_manual.xlsx` and `nf_chrono` but never used them; VaR/ES are purely empirical from test returns. **Fix:** Removed the unused load. Clarified in a comment that no GARCH/NF-GARCH outputs are used.

---

## 7. Previously audited (no code changes)

- **GARCH distribution (norm vs sstd):** `GARCH_DISTRIBUTION_CONSISTENCY_AUDIT.md` – consistency across manual_optimized_config, simulate, compare, stress, extract_residuals.
- **Return vs variance evaluation:** `RETURN_VS_VARIANCE_EVALUATION_AUDIT.md` – compare uses `evaluate_return_forecasts` with `n_paths=1000` and 65/35 split for both NF and Standard.

---

## 8. Files modified in this audit

| File | Changes |
|------|---------|
| `scripts/utils/return_forecast_evaluation.R` | Removed re-standardization of sampled path residuals; added comment; `seq_along` in `calculate_predictive_loglik`. |
| `scripts/evaluation/var_backtesting_comprehensive.R` | `calculate_es` uses `var_level`; NOTE on empirical vs model-based VaR; Kupiec x=0 and x=n edge cases corrected; removed unused NF-GARCH load. |
| `scripts/evaluation/compare_nf_vs_standard_garch.R` | `model_configs` with sGARCH_norm/sstd; `Model`/`Distribution` in Standard; win_rate and Wilcoxon by (Model, Distribution); `comparison_by_model` by (Model, Distribution, Source); `select(-any_of("SplitType"))` for robust `bind_rows`. |
| `scripts/manual_garch/manual_garch_core.R` | eGARCH `forecast_one_step`: E\|z\| from `E_abs_t(nu)` when `fit$distribution == "std"` and `nu` in `fit$coef`. |
| `scripts/evaluation/generate_dashboard_visualizations.R` | Conditional read of `TS_CV_NF_GARCH` via `getSheetNames`; `tscv_results <- data.frame()` when missing. |
| `scripts/evaluation/extract_dissertation_tables.R` | `model_comparison` groups by `(Model, Distribution, Source)` when `Distribution` present. |
| `scripts/model_fitting/extract_residuals.R` | `cfg$dist` → `cfg$distribution` in `fit_models` and `ts_cross_validate`; `residuals(fit, ...)` → `engine_residuals(fit, standardize=TRUE)`. |

---

## 9. Recommendations

1. **Re-run the pipeline** (`run_all.bat` or `run_full_dissertation.bat`) after these changes so all outputs (NF-GARCH, compare, VaR, stress, dissertation tables) reflect the fixes.
2. **VaR:** For dissertation work, consider adding a separate, model-based VaR backtest (one-step VaR from GARCH/NF-GARCH per test date, then Kupiec/Christoffersen) and reporting it alongside the current empirical VaR/ES characterisation.
3. **Reporting:** When discussing NF vs Standard, prefer PredictiveLogLik and VaR/tail metrics for NF benefits; do not over-interpret small MSE/MAE differences (see `RETURN_VS_VARIANCE_EVALUATION_AUDIT.md`).

---

## 10. Additional pass – TS_CV_NF_GARCH, extract_dissertation, extract_residuals (FIXED)

**generate_dashboard_visualizations.R:** Unconditionally read `TS_CV_NF_GARCH`, which may be missing when the simulate step had no TS CV runs. **Fix:** Use `getSheetNames`; if the sheet exists, read it; otherwise `tscv_results <- data.frame()`.

**extract_dissertation_tables.R:** `model_comparison` grouped only by `(Model, Source)`, mixing sGARCH norm and sstd. **Fix:** If `Distribution` exists in `Combined_Results`, group by `(Model, Distribution, Source)`; otherwise fall back to `(Model, Source)`.

**extract_residuals.R:** (1) `model_configs` uses `distribution` but the code referenced `cfg$dist` (NULL), so the wrong distribution was passed to `fit_models` and `ts_cross_validate`. **Fix:** Use `cfg$distribution` in both places. (2) `residuals(fit, standardize = TRUE)` was used with the engine’s fit object; `residuals.default` ignores `standardize` and returns raw residuals. **Fix:** Use `engine_residuals(fit, standardize = TRUE)` so standardized residuals are saved.
