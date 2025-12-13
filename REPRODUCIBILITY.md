# Reproducibility Guide

This document provides step-by-step instructions for reproducing all results in the NF-GARCH research.

## Prerequisites

### Required Software
- **R**: Version 4.0.0 or higher
- **Python**: Version 3.8 or higher
- **Operating System**: Windows (primary), Linux/Mac (with modifications)

### Required Data
- **Data File**: `data/processed/raw (FX + EQ).csv`
  - **Source**: Historical price data for FX and equity assets
  - **Format**: CSV with Date column and asset price columns
  - **Assets**: EURUSD, GBPUSD, GBPCNY, USDZAR, GBPZAR, EURZAR, NVDA, MSFT, PG, CAT, WMT, AMZN
  - **Note**: Data source and download instructions should be documented here

## Installation

### Step 1: Install R Packages

```r
# Install required R packages
install.packages(c(
  "rugarch",      # GARCH model fitting (used for reference, manual implementation is primary)
  "xts",          # Time series objects
  "dplyr",        # Data manipulation
  "tidyr",        # Data tidying
  "stringr",      # String operations
  "lubridate",    # Date handling
  "openxlsx",     # Excel I/O
  "ggplot2",      # Plotting
  "quantmod",     # Financial data
  "tseries",      # Time series analysis
  "PerformanceAnalytics",  # Financial analysis
  "FinTS",        # Financial time series utilities
  "forecast",     # Forecasting utilities
  "lmtest",       # Linear model tests
  "moments",      # Statistical moments
  "parallel",     # Parallel processing
  "doParallel"    # Parallel backend
), repos = "https://cran.rstudio.com/")
```

**Verify Installation**:
```r
# After installation, generate complete sessionInfo
sessionInfo()
# Save output to: environment/R_sessionInfo.txt
```

### Step 2: Install Python Packages

**Option A: Using pip**
```bash
pip install -r environment/requirements.txt
```

**Option B: Using conda**
```bash
conda env create -f environment/environment.yml
conda activate nfgarch
```

**Verify Installation**:
```bash
pip freeze > environment/requirements_frozen.txt
```

**Note**: The `nflows` package may require special installation:
```bash
pip install nflows
# or
pip install git+https://github.com/bayesflows/nflows.git
```

## Reproducing Results

### Step 1: Set Working Directory

**R**:
```r
setwd("path/to/NFGARCH")
```

**Python**:
```python
import os
os.chdir("path/to/NFGARCH")
```

### Step 2: Verify Data File

Ensure `data/processed/raw (FX + EQ).csv` exists and contains:
- Date column (first column)
- Asset price columns for all required assets

### Step 3: Run Pipeline

**Windows**:
```bash
run_all.bat
```

**Linux/Mac** (if scripts are available):
```bash
./run_all.sh
# or
make all
```

**Manual Step-by-Step**:
1. GARCH Fitting: `Rscript scripts/manual/manual_garch_fitting.R`
2. NF Training: `python scripts/manual/manual_nf_training.py`
3. NF-GARCH Simulation: `Rscript scripts/simulation_forecasting/simulate_nf_garch_engine.R --engine manual`
4. Evaluation: Run evaluation scripts as needed
5. Consolidation: `Rscript -e "source('scripts/core/consolidation.R'); consolidate_all_results('results/consolidated')"`

### Step 4: Verify Results

Expected outputs in `results/consolidated/`:
- `Dissertation_Consolidated_Results.xlsx`
- `NF_GARCH_Results_manual.xlsx`
- `NF_vs_Standard_GARCH_Comparison.xlsx`
- `Distributional_Metrics.xlsx`
- `Stylized_Facts.xlsx`
- `VaR_Backtesting.xlsx`
- `Stress_Testing.xlsx`
- `Final_Dashboard.xlsx`

## Reproducibility Settings

### Seed Configuration

All scripts use a centralized seed value defined in `scripts/core/config.R`:
- **Seed Value**: 123
- **Usage**: Set via `REPRODUCIBILITY_SEED` constant
- **R**: `set.seed(REPRODUCIBILITY_SEED)`
- **Python**: `set_seed(123)` (matches R seed)

### Platform-Specific Notes

**Windows**:
- Batch scripts automatically detect R installation
- Set `RSCRIPT` environment variable to override: `set RSCRIPT=C:\path\to\Rscript.exe`

**Linux/Mac**:
- Ensure `Rscript` and `python` are in PATH
- May need to create shell scripts equivalent to `.bat` files
- Use forward slashes in paths

## Troubleshooting

### Common Issues

1. **Rscript not found**
   - Windows: Set `RSCRIPT` environment variable
   - Linux/Mac: Ensure R is in PATH: `which Rscript`

2. **Python packages not found**
   - Verify Python environment: `python --version`
   - Reinstall packages: `pip install -r environment/requirements.txt`

3. **Data file not found**
   - Verify file exists: `data/processed/raw (FX + EQ).csv`
   - Check working directory is correct

4. **NF residuals not found**
   - Ensure GARCH fitting completed: Check `outputs/manual/residuals_by_model/`
   - Ensure NF training completed: Check `outputs/manual/nf_models/`

5. **Results not reproducible**
   - Verify seed is set in all scripts
   - Check package versions match `requirements_frozen.txt`
   - Ensure no manual edits to intermediate files

## Expected Execution Time

- **GARCH Fitting**: ~30 minutes
- **NF Training**: ~20 minutes
- **NF-GARCH Simulation**: ~15 minutes
- **Evaluation**: ~10 minutes
- **Total**: ~45-90 minutes (depending on hardware)

## Version Information

To ensure exact reproducibility, document:
- R version: `R.version.string`
- Python version: `python --version`
- Package versions: See `environment/requirements_frozen.txt` and `environment/R_sessionInfo.txt`

## Contact

For reproducibility issues, please:
1. Check this guide first
2. Verify all prerequisites are met
3. Check error logs in `logs/` directory
4. Review `EXAMINATION_REPORTS/` for known issues

