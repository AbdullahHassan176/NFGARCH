# Dissertation claims vs codebase verification

## Verified (done as claimed)

| Claim | Location in code |
|-------|------------------|
| Two-stage NF-GARCH: GARCH then MAF on residuals | manual_garch_fitting.R (chrono), manual_nf_training.py |
| MAF implemented with MaskedAffineAutoregressiveTransform (nflows) | manual_nf_training.py |
| Four GARCH variants: sGARCH, eGARCH, TGARCH, GJR-GARCH | config.R GARCH_MODELS; manual fit_*_manual.R |
| 65% in-sample / 35% out-of-sample chronological split | config.R TSCV_OPTIMIZED/FULL window_size = 0.65; manual_garch_fitting.R train_size = floor(n_obs * 0.65) |
| Rolling TSCV: 20-step forecast horizon | config.R forecast_horizon = 20 (TSCV_OPTIMIZED); some scripts use 500/20 |
| Minimum 520 observations for TSCV | fit_garch_models.R, extract_residuals.R, simulate_nf_garch_engine.R: nrow > 520 |
| GARCH fit via maximum likelihood (optim) | manual_garch/fit_*_manual.R use optim() |
| NF: 4 layers, 64 hidden (optimized), batch 512, lr 0.001, validation 20% | config.R NF_OPTIMIZED; nf_config.json (when optimized); manual_nf_training.py |
| Early stopping: patience 15 epochs (optimized) | config.R patience = 15; manual_nf_training.py |
| MSE, MAE, AIC, BIC, log-likelihood | compare_nf_vs_standard_garch.R; dissertation_tables |
| Wilcoxon signed-rank test | evaluation scripts / dissertation_tables wilcoxon_test_results |
| KS distance, Wasserstein, tail index, skewness | manual_nf_training.py (scipy.stats); distributional_metrics |
| VaR exceedance, Kupiec, Christoffersen | stress_testing / var backtest scripts |
| Stress: GFC 2008 (1 Sep 2008–31 Mar 2009), COVID-19 (1 Feb–30 Apr 2020) | stress_testing_comprehensive.R crisis_periods |
| Hypothetical shocks: price drop 30%/50%, vol spike 2x/3x, mean shift | stress_testing_comprehensive.R apply_shock |
| GARCH order robustness: (1,1), (2,1), (1,2), (2,2), BIC selection | robustness_garch_order.R GARCH_ORDERS |
| sGARCH with normal and Student-t (norm + std) | extract_residuals.R sGARCH_norm, sGARCH_std; manual fit_*_manual.R support norm/std |
| Full mode 13 assets: 6 FX + 7 equity | config.R FULL_ASSETS; nf_config.json (after update) |

## Corrected in dissertation

| Issue | Fix |
|-------|-----|
| Early stopping "20 epochs" | Code uses patience = 15 → dissertation updated to 15 where stated |
| Data section listed only 3 FX + 3 equity | Updated to list all 6 FX + 7 equity for full mode |
| "Six high-liquidity assets" in limitations | Updated to 13 assets |
| NF hyperparameters in methodology | Clarified optimized (4/64/75) vs full (8/256/150) |

## Design vs results (current state)

- **Design**: Full pipeline mode (13 assets) is configured and documented.
- **Tables**: Current dissertation_tables were generated from a run with 6 assets (optimized). N in tables = 6 (or lower per model). After a full 13-asset run, regenerate tables so N and statistics match.
- **Table 4.5 (detailed NF vs Standard)**: Missing values (--) occur where the Standard GARCH run for that asset–model–distribution did not converge or was excluded due to extreme forecast errors (see compare_nf_vs_standard_garch.R: Standard row is only added when evaluation succeeds). The detailed wide table is exported by `extract_dissertation_tables.R` as `results/dissertation_tables/detailed_nf_vs_standard.csv` and `.tex` so the thesis table can be refreshed from the pipeline.

## Methodology vs config (two setups)

- **Chronological split**: 65% train, 35% test (by proportion). Implemented in manual_garch_fitting.R.
- **Rolling TSCV**: Some scripts use fixed 500 training obs + 20-step horizon and 520 min length; config TSCV uses 65% window and 20-step horizon. Dissertation correctly describes both (65/35 and 500/20 TSCV).
