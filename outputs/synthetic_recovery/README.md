# Synthetic Distribution Recovery Experiment - Results Summary

## Experiment Overview

This experiment evaluates how well different GARCH models recover the **true innovation distribution** from synthetic GARCH(1,1) data.

## Data Generating Process (DGP)

### GARCH Parameters
- **Sample size (T):** 2000
- **Omega (ω):** 1e-04
- **Alpha (α):** 0.1
- **Beta (β):** 0.85
- **Stationarity:** α + β = 0.95 ✓ (stationary)

### Innovation Distribution
- **Type:** skewed_t
- **Parameters:** nu=5, xi=1.5
- **Seed:** 123

## Models Fitted

1. **Gaussian GARCH(1,1)**: Standard GARCH with normal innovations
2. **Student-t GARCH(1,1)**: GARCH with Student-t innovations
3. **NF-GARCH(1,1)**: Two-stage approach
   - Fit Gaussian GARCH(1,1) to extract standardized residuals
   - Train normalizing flow on residuals
   - Sample from fitted flow

## Distribution Recovery Metrics

The following metrics compare each method's recovered distribution against the **true innovation distribution**:

| Method | KS Statistic | Wasserstein Distance | Skewness Diff | Kurtosis Diff |
|--------|--------------|---------------------|---------------|---------------|
| Gaussian_GARCH | 0.3310 | 1.2847 | 0.0714 | 1.3959 |
| Student_t_GARCH | 0.1155 | 1.3127 | 0.3922 | 4.7542 |
| NF_GARCH | 0.1585 | 0.5197 | 1.4941 | 4.9575 |

### Interpretation

- **KS Statistic**: Lower is better (0 = perfect match)
- **Wasserstein Distance**: Lower is better (0 = perfect match)
- **Skewness/Kurtosis Diff**: Lower is better (0 = perfect match)

## Summary Statistics

| Method | Mean | SD | Skewness | Kurtosis | Q(0.01) | Q(0.99) |
|--------|------|----|----------|----------|---------|---------|
| True | -0.0039 | 0.9886 | 1.4550 | 8.1568 | -1.8451 | 3.1739 |
| Gaussian GARCH | -0.5595 | 1.2728 | 1.3836 | 9.5526 | -3.4580 | 3.4463 |
| Student-t GARCH | 0.0325 | 1.5613 | 1.8473 | 12.9109 | -3.0563 | 5.3054 |
| NF-GARCH | -0.0198 | 1.5041 | -0.0391 | 3.1993 | -3.5817 | 3.4758 |

## Plots

All plots are saved in the `plots/` directory:

- `kde_overlay.png`: KDE overlay comparing all methods
- `qq_gaussian.png`: QQ plot for Gaussian GARCH
- `qq_student_t.png`: QQ plot for Student-t GARCH
- `qq_nf.png`: QQ plot for NF-GARCH

## Files Generated

- `recovery_metrics.csv`: Detailed recovery metrics
- `summary_statistics.csv`: Summary statistics table
- `residuals/z_true.csv`: True innovations (ground truth)
- `residuals/z_hat_gaussian.csv`: Gaussian GARCH residuals
- `residuals/z_hat_student_t.csv`: Student-t GARCH residuals
- `nf_model.pth`: Trained normalizing flow model

## Notes

- This experiment focuses on **distribution recovery**, not forecast accuracy
- The true innovation distribution `z_true` is the ground truth
- Lower metric values indicate better recovery
- NF-GARCH uses a two-stage approach: GARCH fit → NF training → sampling

