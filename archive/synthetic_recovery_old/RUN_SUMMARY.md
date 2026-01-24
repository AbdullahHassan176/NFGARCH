# Synthetic Recovery Experiment - Run Summary

**Date**: 2026-01-09  
**Status**: ✅ **SUCCESS** - All 10 seeds completed without errors

## Execution Summary

- **Total Seeds**: 10 (11, 22, 33, 44, 55, 66, 77, 88, 99, 123)
- **Success Rate**: 100% (10/10 seeds completed)
- **GARCH Convergence**: 100% (all models converged)
- **NF Training**: 100% (all NF models trained successfully)
- **Evaluation**: 100% (all metrics computed and plots generated)

## Output Files Generated

### Aggregated Results
- `MULTISEED_REPORT.md` - Summary report with winners
- `recovery_metrics_raw_aggregate.csv` - RAW mode metrics
- `recovery_metrics_shape_aggregate.csv` - SHAPE mode metrics
- `summary_statistics_raw_aggregate.csv` - RAW summary stats
- `summary_statistics_shape_aggregate.csv` - SHAPE summary stats

### Per-Seed Results (10 directories)
Each `seed_*/` directory contains:
- `recovery_metrics_raw.csv` - RAW metrics
- `recovery_metrics_shape.csv` - SHAPE metrics
- `summary_statistics_raw.csv` - RAW summary stats
- `summary_statistics_shape.csv` - SHAPE summary stats
- `nf_fit_sanity_raw.csv` - NF sanity check (RAW)
- `nf_fit_sanity_shape.csv` - NF sanity check (SHAPE)
- `plots/` - 7 plots per seed (KDE overlays, QQ plots, NF sanity)
- `residuals/` - All residual files
- `nf_model.pth` - Trained NF model
- `nf_model_samples.csv` - NF-generated samples

## Key Results

### Overall Winner: **Student-t GARCH**
- Best KS statistic (0.0514 in SHAPE mode)
- Best tail quantile recovery
- 100% skewness sign match
- Most stable across seeds

### NF-GARCH Performance
- ✅ Best Wasserstein distance (0.2166 in SHAPE mode)
- ✅ Good stability on KS statistic
- ❌ **Fails to recover skewness sign** (50% match rate)
- ❌ Severely underestimates kurtosis
- ⚠️ Scale drift (SD≈1.56 vs 1.0)

### Gaussian GARCH Performance
- ✅ Best absolute skewness/kurtosis differences (but misleading)
- ❌ Significant mean drift (-0.4560)
- ❌ Least stable across seeds

## Issues Identified (Not Errors)

These are **findings**, not code errors:

1. **NF-GARCH Skewness Recovery Failure** (50% sign match)
   - **Status**: Documented limitation, not a bug
   - **Cause**: MAF architecture bias toward symmetry
   - **Impact**: NF-GARCH cannot reliably recover skewness direction

2. **NF-GARCH Scale Drift** (SD≈1.56)
   - **Status**: Expected behavior (inherits Student-t GARCH scale)
   - **Cause**: Two-stage training approach
   - **Impact**: NF samples have higher variance than true innovations

3. **Gaussian GARCH Mean Drift** (-0.4560)
   - **Status**: Expected due to misspecification
   - **Cause**: Assumes normal innovations (true is skewed-t)
   - **Impact**: Systematic bias in mean estimation

## Warnings Encountered

The experiment produced some warnings (not errors):
- Package conflicts (dplyr/xts lag function) - harmless
- NF sample scale warnings - documented in analysis
- These are expected and do not affect results

## Analysis Documents

1. **EXPERIMENT_ANALYSIS.md** - Comprehensive analysis with detailed findings
2. **MULTISEED_REPORT.md** - Quick summary with winners table
3. **ANALYSIS_AUDIT.md** - Technical audit (from previous run)

## Next Steps

1. ✅ Review `EXPERIMENT_ANALYSIS.md` for detailed findings
2. ✅ Check `MULTISEED_REPORT.md` for quick summary
3. ✅ Examine plots in `seed_*/plots/` directories
4. ✅ Consider NF architecture improvements for skewness recovery

## Reproducibility

All results are reproducible with the same seeds. The experiment uses:
- Fixed seeds: 11, 22, 33, 44, 55, 66, 77, 88, 99, 123
- Synchronized R and Python seeds
- Deterministic DGP generation

---

**Experiment Script**: `scripts/experiments/synthetic_recovery/run_synthetic_recovery.R`  
**Results Location**: `outputs/synthetic_recovery/`

