# Synthetic Distribution Recovery Experiment

## Overview

This experiment evaluates how well different GARCH models recover the **true innovation distribution** from synthetic GARCH(1,1) data. Unlike typical forecast evaluation, this focuses on **distribution recovery** - how well each approach captures the true innovation distribution.

## Goal

Create a controlled synthetic experiment that:
1. Simulates returns from a known non-Gaussian innovation distribution through GARCH(1,1)
2. Fits competing models (Gaussian GARCH, Student-t GARCH, NF-GARCH)
3. Evaluates how well each approach recovers the TRUE innovation distribution

## Quick Start

### Run the Experiment

```bash
# From repository root
Rscript scripts/experiments/synthetic_recovery/run_synthetic_recovery.R
```

Or on Windows:
```batch
cd scripts\experiments\synthetic_recovery
Rscript run_synthetic_recovery.R
```

### Expected Runtime

- **Data generation**: < 1 second
- **GARCH fitting**: 10-30 seconds
- **NF training**: 1-3 minutes
- **Evaluation & plotting**: 5-10 seconds
- **Total**: ~2-4 minutes

## Experiment Structure

### Step 1: Synthetic DGP Generation

- Simulates GARCH(1,1) returns: `r_t = μ + ε_t`, `ε_t = σ_t z_t`
- Variance recursion: `σ²_t = ω + α ε²_{t-1} + β σ²_{t-1}`
- Innovation distribution: Configurable (skewed-t, Student-t, or mixture Gaussian)
- **Saves true innovations `z_t`** as ground truth

### Step 2: Model Fitting

Fits three competing approaches:

1. **Gaussian GARCH(1,1)**: Standard GARCH with normal innovations
   - Uses `engine_fit(model="sGARCH", dist="norm")`
   - Extracts standardized residuals `z_hat_gaussian`

2. **Student-t GARCH(1,1)**: GARCH with Student-t innovations
   - Uses `engine_fit(model="sGARCH", dist="std")`
   - Extracts standardized residuals `z_hat_student_t`

3. **NF-GARCH(1,1)**: Two-stage approach
   - Fit Student-t GARCH(1,1) as base
   - Extract standardized residuals
   - Train normalizing flow on residuals
   - Sample `z_nf` from fitted flow

### Step 3: Distribution Recovery Evaluation

Computes metrics comparing recovered distributions to true:

- **Kolmogorov-Smirnov (KS) statistic**: Distribution distance
- **Wasserstein distance**: Optimal transport distance
- **Tail diagnostics**: Quantile differences (1%, 5%, 95%, 99%)
- **Moment differences**: Skewness and excess kurtosis

Generates plots:
- **KDE overlay**: Density plots of all methods vs true
- **QQ plots**: Quantile-quantile plots for each method

### Step 4: Report Generation

Creates markdown report with:
- DGP configuration
- Model specifications
- Recovery metrics table
- Summary statistics
- Links to plots

## Output Files

All outputs are saved to `outputs/synthetic_recovery/`:

### Data Files
- `residuals/z_true.csv`: True innovations (ground truth)
- `residuals/z_hat_gaussian.csv`: Gaussian GARCH residuals
- `residuals/z_hat_student_t.csv`: Student-t GARCH residuals
- `nf_model.pth`: Trained normalizing flow model
- `nf_model_samples.csv`: NF-generated samples

### Results Files
- `recovery_metrics.csv`: Detailed recovery metrics
- `summary_statistics.csv`: Summary statistics table
- `README.md`: Experiment report

### Plots
- `plots/kde_overlay.png`: KDE overlay comparison
- `plots/qq_gaussian.png`: QQ plot for Gaussian GARCH
- `plots/qq_student_t.png`: QQ plot for Student-t GARCH
- `plots/qq_nf.png`: QQ plot for NF-GARCH

## Configuration

Default DGP configuration (in `synthetic_dgp.R`):

```r
list(
  T = 2000,              # Sample size
  omega = 0.0001,        # GARCH intercept
  alpha = 0.1,           # ARCH parameter
  beta = 0.85,           # GARCH parameter
  mu = 0,               # Mean return
  innovation_type = "skewed_t",  # "student_t", "skewed_t", or "mixture_gaussian"
  innovation_params = list(
    nu = 5,             # Degrees of freedom
    xi = 1.5            # Skewness parameter
  ),
  seed = 123
)
```

To modify, edit `run_synthetic_recovery.R`:

```r
DGP_CONFIG <- get_default_dgp_config()
DGP_CONFIG$T <- 3000  # Change sample size
DGP_CONFIG$innovation_type <- "student_t"  # Change innovation type
```

## Dependencies

### R Packages
- `xts`, `dplyr`, `ggplot2`
- `moments` (for skewness/kurtosis)
- `transport` (optional, for Wasserstein distance)

### Python Packages
- `torch`, `nflows`
- `numpy`, `pandas`

All packages should already be installed if the main pipeline runs successfully.

## Interpretation

### Recovery Metrics

- **KS Statistic**: Lower is better (0 = perfect match)
  - Measures maximum difference between CDFs
  - Range: [0, 1]

- **Wasserstein Distance**: Lower is better (0 = perfect match)
  - Measures optimal transport cost
  - Units: Same as data

- **Skewness/Kurtosis Diff**: Lower is better (0 = perfect match)
  - Absolute difference in moments
  - Measures tail behavior recovery

- **Quantile Differences**: Lower is better (0 = perfect match)
  - Measures tail quantile recovery
  - Important for risk management

### Expected Results

- **Gaussian GARCH**: Should perform poorly if true distribution is non-Gaussian
- **Student-t GARCH**: Should perform better for heavy-tailed distributions
- **NF-GARCH**: Should perform best overall, especially for complex distributions

## Notes

- This experiment is **deterministic** (uses fixed seed)
- Focus is on **distribution recovery**, not forecast accuracy
- NF-GARCH uses **two-stage** approach (not joint training)
- Stationarity is checked: `alpha + beta < 1`

## Troubleshooting

### NF Training Fails

If NF training fails:
1. Check Python environment has `torch` and `nflows`
2. Verify residuals file exists and has data
3. Check `outputs/synthetic_recovery/residuals/z_hat_for_nf.csv`

### GARCH Fitting Fails

If GARCH fitting fails:
1. Check stationarity: `alpha + beta < 1`
2. Try different starting values
3. Increase sample size `T`

### Plots Not Generated

If plots fail:
1. Check `ggplot2` is installed
2. Verify data files exist
3. Check `outputs/synthetic_recovery/plots/` directory exists

## Future Extensions

- [ ] Joint NF-GARCH training (currently two-stage only)
- [ ] Additional innovation distributions (e.g., GED, skewed GED)
- [ ] Multiple DGP scenarios (different parameter combinations)
- [ ] PIT (Probability Integral Transform) histograms
- [ ] Bootstrap confidence intervals for metrics

