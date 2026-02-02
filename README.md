# Normalizing Flow-GARCH for Financial Volatility Forecasting

## Overview

This repository implements a two-stage Normalizing Flow-GARCH (NF-GARCH) framework that replaces parametric innovation distributions in GARCH models with learned densities. The method is evaluated on six daily financial series (three FX pairs, three equities) and demonstrates substantial improvements in forecast accuracy.

**Key Results:**
- Mean squared error reduction from 0.348 to 0.000358 (>99%)
- Statistically significant improvements (Wilcoxon p = 0.03125)
- Win rates of 83-100% across model specifications
- 10.2% reduction in Wasserstein distance

## Requirements

**Hardware:**
- 8GB RAM minimum (16GB recommended)
- 5GB disk space
- GPU optional (CUDA-compatible recommended for faster training)

**Software:**
- R 4.0+ (tested on R 4.5.1)
- Python 3.8+ (tested on Python 3.8-3.10)

**R Packages:** rugarch, xts, PerformanceAnalytics, tidyverse, openxlsx, moments, tseries, forecast, lmtest

**Python Packages:** numpy, pandas, torch, nflows, matplotlib, seaborn, scikit-learn, pyyaml, openpyxl

## Installation

### Automated Setup
```batch
setup.bat
```

### Manual Installation
1. Install R from https://cran.r-project.org/
2. Install Python from https://python.org/
3. Install R packages:
 ```r
 install.packages("renv")
 renv::restore()
 ```
4. Install Python packages:
 ```bash
 pip install -r environment/requirements.txt
 ```

## Usage

### Full Analysis
Run the complete pipeline including all robustness tests:
```batch
run_full_dissertation.bat
```
Runtime: 60-120 minutes

### Main Pipeline Only
Run core analysis without additional experiments:
```batch
run_all.bat
```
Runtime: 60-90 minutes

### Individual Experiments
- GARCH order robustness: `run_robustness_garch_order.bat`
- Synthetic recovery: `run_synthetic_recovery.bat`

## Output Structure

**Dissertation Tables:** `results/dissertation_tables/`
- Overall performance, by-model comparison, asset class analysis
- Statistical tests, win rates
- All tables in CSV format (LaTeX-ready)

**Figures:** `results/figures/`
- Stylized facts (ACF/PACF)
- Distribution diagnostics (histograms, Q-Q plots)
- Residual diagnostics
- Stress testing results
- Win rate visualizations

**Excel Dashboards:** `results/consolidated/`
- Comprehensive results dashboard
- Distributional metrics
- VaR backtesting
- Stress testing summaries

**Overleaf Export:** `overleaf_export/`
- Tables and figures ready for LaTeX import
- Import instructions in OVERLEAF_IMPORT.txt

## Methodology

### Two-Stage Framework

**Stage 1:** Fit standard GARCH variants (sGARCH, TGARCH, GJR-GARCH, EGARCH) with parametric distributions (Normal, Student-t). Extract standardized residuals from training data.

**Stage 2:** Train Masked Autoregressive Flow on GARCH residuals to learn flexible innovation distribution. Generate samples from fitted flow.

**Forecasting:** Use flow samples as innovations in original GARCH recursion, preserving interpretability while improving distributional accuracy.

### Data

**Assets:** EURUSD, GBPUSD, USDZAR (FX); NVDA, MSFT, AMZN (equities) 
**Split:** 65% training, 35% testing (chronological)

### Evaluation

- Forecast accuracy: MSE, MAE, Log-Likelihood
- Distributional fit: Kolmogorov-Smirnov distance, Wasserstein distance
- Statistical tests: Wilcoxon signed-rank tests
- Risk metrics: VaR backtesting (Kupiec, Christoffersen)
- Stress testing: Historical crises (GFC, COVID-19)

## Reproducibility

All results use fixed random seed (123). Dependencies are version-locked in `environment/renv.lock` (R) and `environment/requirements.txt` (Python). Minor numerical differences (<0.1% in metrics) may occur due to hardware variations, floating-point precision, or solver convergence paths.

## Project Structure

```
NFGARCH/
├── data/processed/ # Price data (FX + equities)
├── environment/ # Dependency specifications
├── scripts/
│ ├── core/ # Configuration and utilities
│ ├── model_fitting/ # GARCH estimation
│ ├── manual/ # Optimized pipeline
│ ├── simulation_forecasting/ # NF-GARCH forecasting
│ ├── evaluation/ # Analysis and metrics
│ └── experiments/ # Robustness tests
├── results/
│ ├── dissertation_tables/ # LaTeX-ready tables
│ ├── figures/ # Publication figures
│ └── consolidated/ # Excel dashboards
├── outputs/manual/ # Model outputs and diagnostics
└── overleaf_export/ # Ready for LaTeX import
```

## Troubleshooting

**R not found:** Ensure R is installed and in system PATH, or edit `scripts/utils/find_r_executable.bat` to specify installation path.

**Python packages fail:** Use conda environment: `conda env create -f environment/environment.yml`

**CUDA errors:** NF training falls back to CPU automatically (slower but identical results).

**GARCH convergence failures:** Normal for some model/asset/distribution combinations. Pipeline continues with available results.

**Memory errors:** Close other applications or reduce parallel processing in `scripts/core/config.R`.

## License

MIT License - see LICENSE file

## Citation

```
Hassan, A., Mlambo, F., & Mongwe, W.T. (2026). 
Normalizing Flow-GARCH for Financial Volatility Forecasting.
```
