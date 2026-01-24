# Return vs Variance Evaluation – Audit

## Summary

The pipeline moved from forecasting **variance** to forecasting **returns**. This audit documents what was found and what was fixed.

---

## 1. What Is Actually Being Forecast and Evaluated

- **Return forecast**: point forecast = E[r_t | info]. Implemented as the **mean of N simulated return paths** (N=1000 in `evaluate_return_forecasts`). MSE/MAE are on (actual return − point forecast).
- **Variance forecast**: not used in the current return-forecast evaluation. `manual_path` and `engine_path` produce both `returns` and `sigma`; only `returns` are used for MSE/MAE. `sigma_forecast` is computed but not written into the comparison or NF-GARCH results.

So the current design is **return forecasting and return-MSE/MAE**, not variance.

---

## 2. Inconsistencies Found (and Fixes)

### 2.1 `compare_nf_vs_standard_garch.R` – Standard GARCH

| Issue | Before | After |
|-------|--------|-------|
| **Point forecast** | One path from `engine_path(..., head(standard_residuals, test_size), ...)` treated as the “forecast” | Use `evaluate_return_forecasts(..., n_paths=1000)` so the point forecast is the **mean of 1000 paths**, same as NF-GARCH. |
| **Train/test** | Fit on **full** sample; test on last `min(40, 0.35*n)` | Fit on **first 65%**, test on **last 35%**, no 40 cap – same as `simulate_nf_garch_engine.R`. |
| **Residuals** | `head(standard_residuals, test_size)` – one fixed in-sample block | `evaluate_return_forecasts` **samples** from standardized residuals for each path; many paths → proper conditional-mean forecast. |
| **Metrics** | MSE, MAE only | MSE, MAE, `PredictiveLogLik`, `NPaths` for parity with NF-GARCH. |

**Why it distorted results**

- NF-GARCH: point forecast = mean of 1000 paths ≈ E[r_t] = μ (for constant-mean GARCH). MSE ≈ mean over test of (r − μ)².
- Standard (old): “forecast” = one random path. E[MSE] is much larger because of path variance. So **Standard looked unfairly worse** than NF-GARCH.

**Fix:** Standard GARCH in `compare_nf_vs_standard_garch.R` now uses `evaluate_return_forecasts` with `n_paths=1000` and the same 65/35 split as NF-GARCH.

---

### 2.2 `stress_testing_comprehensive.R`

- **NF-GARCH and Standard GARCH** both use `evaluate_return_forecasts(..., n_paths=1000)`.
- No change needed here; comparison is already on the same footing.

---

### 2.3 `simulate_nf_garch_engine.R`

- Uses `evaluate_return_forecasts` with `n_paths=1000` for both Chrono and TS CV.
- `manual_path` and `engine_path` correctly produce **returns**; the mean over paths is the return point forecast.  
- No change needed.

---

## 3. Interpretation of Return MSE/MAE (GARCH with constant μ)

For a constant-mean GARCH, **E[r_t | F_{t-1}] = μ**. So:

- The **theoretical** return point forecast is μ at all horizons.
- With many paths, the **mean of simulated paths** tends to μ.
- Both NF-GARCH and Standard GARCH (with the same μ) therefore produce **the same** point forecast when both use many paths. So **return MSE and MAE can be almost identical** for the two once the comparison is fair.

Where NF can differ:

- **PredictiveLogLik** (density): different residual distributions (NF vs Gaussian/Student) change the predictive density.
- **VaR / tail risk**: different tail shapes.
- **Volatility forecasting** (if we evaluated σ): NF changes the innovation distribution, so it can affect σ paths; that is not currently in the MSE/MAE.

So: after the fix, **similar MSE/MAE for NF vs Standard is expected**. A clear NF advantage is more likely to show up in PredictiveLogLik, VaR, or variance-focused metrics, not in return MSE/MAE.

---

## 4. Files Touched

- **`scripts/evaluation/compare_nf_vs_standard_garch.R`**: Standard GARCH block rewritten to use the same 65/35 split, `evaluate_return_forecasts` with `n_paths=1000`, and to add `PredictiveLogLik` and `NPaths`.

---

## 5. Recommendation

1. **Re-run** `run_all.bat` (or at least: GARCH fit → NF train → `simulate_nf_garch_engine.R` → `compare_nf_vs_standard_garch.R`) so NF-GARCH and Standard GARCH are compared under the corrected setup.
2. When reporting:
   - Use **PredictiveLogLik** and **VaR/tail** as the main places to look for an NF benefit.
   - Do **not** over-interpret small MSE/MAE differences in favour of NF; similar values are expected for return point forecasts.
