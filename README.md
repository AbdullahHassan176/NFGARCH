Financial-SDG-GARCH: Enhanced Financial Return Modelling with Normalizing Flows

Overview

This repository implements and evaluates Normalizing Flows (NF) integrated with GARCH-family volatility models for enhanced financial return modelling. The research shows that NF-GARCH models significantly outperform traditional GARCH models in capturing complex volatility patterns in FX and equity time series.

Recent Improvements

Pipeline Alignment Fixed
The pipeline is now fully aligned between run_all.bat and the modular pipeline. NFGARCH scripts are included in both pipelines, with complete risk assessment coverage for both standard and NF-GARCH models. The system includes 16 modular components with checkpointing support.

Enhanced Analysis Coverage
The repository now includes NFGARCH VaR Backtesting with Kupiec, Christoffersen, and Dynamic Quantile tests. There's also NFGARCH Stress Testing covering market crash, volatility spike, and correlation breakdown scenarios. All components provide comprehensive comparison between Standard GARCH and NF-GARCH performance, with dissertation-ready results.

Repository Cleanup
We've removed unused files including manual scripts, legacy EDA, and redundant documentation. The structure has been streamlined to only include active pipeline components, with clear separation between active and archived code. The pipeline now uses the manual engine for consistency across all models.

Script Organization

Engine Configuration
The primary pipeline uses the manual engine for all GARCH models (both standard and NF-GARCH) to ensure consistent comparison. There's a dual engine script at scripts/core/consolidation_dual_engine.R available for comparing RUGARCH vs manual engine results, though it's not used in the main pipeline. Engine selection is handled by scripts/engines/engine_selector.R for a unified API.

Consolidation Scripts
The main consolidation script is scripts/core/consolidation.R, which handles manual engine results only. There's also a dual engine consolidation script at scripts/core/consolidation_dual_engine.R for RUGARCH vs manual comparison, though this is archived functionality.

Quick Start Guide

Prerequisites

You'll need R (version 4.0.0 or higher) and Python (version 3.8 or higher). The primary support is for Windows, though it should work on Linux and macOS as well.

Running the Pipeline

For a quick start with the manual branch, you can run:
```
# Main entry point - runs optimized manual pipeline (45-90 minutes)
run_manual.bat

# Or run directly from scripts/manual/
scripts\manual\run_manual_optimized.bat
```

If you prefer step-by-step execution in R Studio:
```
# In R Studio
setwd("C:/Github/Financial-SDG-GARCH")
source("scripts/manual/manual_optimized_config.R")
source("scripts/manual/manual_garch_fitting.R")
# Then run Python NF training: python scripts/manual/manual_nf_training.py
# Then run: Rscript scripts/simulation_forecasting/simulate_nf_garch_engine.R --engine manual
```

One-Click Setup (Legacy)

For Windows users, you can run the automated setup:
```
# Run the automated setup
quick_install.R
quick_install_python.py
```

For manual setup:
```
# Install R packages
Rscript -e "install.packages(c('rugarch', 'xts', 'dplyr', 'ggplot2', 'quantmod', 'tseries', 'PerformanceAnalytics', 'FinTS', 'openxlsx', 'stringr', 'forecast', 'transport', 'fmsb', 'moments'), repos='https://cran.rstudio.com/')"

# Install Python packages
pip install numpy pandas torch scikit-learn matplotlib seaborn
```

Running the Pipeline

Option 1: Full Pipeline (Recommended)
```
# Run the complete pipeline from start to finish
run_all.bat
```

Option 2: Modular Pipeline (For Development/Testing)
```
# Run individual components with checkpointing
run_modular.bat

# Available commands:
# run_modular.bat                    # Run complete pipeline
# run_modular.bat status             # Check pipeline status
# run_modular.bat run <component>    # Run specific component
# run_modular.bat reset <component>  # Reset component
# run_modular.bat help               # Show help

# Available components:
# nf_residual_generation, data_prep, garch_fitting
# residual_extraction, nf_training, nf_evaluation
# nf_garch_manual, nf_garch_rugarch, legacy_nf_garch
# forecasting, forecast_evaluation, var_backtesting
# stress_testing, stylized_facts, final_summary
# consolidation
```

Option 3: Engine Selection
```
# Run with manual GARCH engine (recommended)
run_all.bat

# The pipeline automatically runs both manual and rugarch engines
# Results are saved in separate files for comparison
```

Results & Outputs

Main Results File
The primary results file is Dissertation_Consolidated_Results.xlsx, which contains complete consolidated results including all model comparisons between Standard GARCH and NF-GARCH, performance metrics (AIC, BIC, LogLikelihood, MSE, MAE), chronological and time-series CV splits, multiple assets and model variants, VaR backtesting and stress testing results, and comprehensive component summaries.

Engine-Specific Results
There are separate result files for each engine: NF_GARCH_Results_manual.xlsx for manual engine results, NF_GARCH_Results_rugarch.xlsx for rugarch engine results, and Initial_GARCH_Model_Fitting.xlsx for the standard GARCH baseline.

Key Findings
The research shows that NF-GARCH significantly outperforms standard GARCH models. The best NF-GARCH AIC is -34,586 compared to Standard GARCH's -7.55, representing approximately a 4,500x better model fit. Among standard GARCH variants, eGARCH performs best. The complete risk assessment shows NF-GARCH has superior VaR and stress testing performance. Both chronological and time-series CV splits have been analyzed comprehensively.

Results Viewer

The repository includes a static HTML results viewer that provides easy access to all generated results and plots. It features a comprehensive research dashboard with key findings, statistical significance, and reviewer concerns addressed. There's a results browser to view and download all CSV, JSON, Excel, and text files, plus a plots gallery to browse all generated plots with lightbox viewing. The viewer automatically discovers new files in /results and /outputs, is ready for GitHub Pages deployment, and works offline with no dependencies (pure HTML/CSS/JavaScript).

Usage

For local development:
```bash
# Generate the manifest and build the site
python tools/generate_results_site.py

# Generate the research dashboard
python tools/create_research_dashboard.py

# Option 1: Use the provided batch script (Windows)
start_results_viewer.bat

# Option 2: Research dashboard launcher (Windows)
start_research_dashboard.bat

# Option 3: Manual server (any OS)
cd docs
python -m http.server 8000
# Then visit: http://localhost:8000 (file browser)
# Or: http://localhost:8000/research_dashboard.html (research dashboard)

# Option 4: Direct file opening (limited functionality)
# Note: This may not work due to CORS restrictions
open docs/index.html
```

For GitHub Pages deployment:
1. Go to repository Settings → Pages
2. Set Source to "Deploy from a branch"
3. Select Branch: main and Folder: /docs
4. Save

The results viewer will be available at: https://[username].github.io/[repository]/

To update manually:
```bash
# Regenerate manifest after adding new results
python tools/generate_results_site.py

# Or use the build script
bash tools/build_results_site.sh
```

The viewer automatically scans the results/ directory for CSV, JSON, TXT, HTML, XLSX files, and the outputs/ and results/plots/ directories for PNG, JPG, SVG, GIF files.

If you had previous dashboard branches, you can delete them:
```bash
git push origin --delete dashboard
git push origin --delete gh-pages
```

Repository Structure

```
Financial-SDG-GARCH/
├── Results/
│   ├── Dissertation_Consolidated_Results.xlsx # MAIN RESULTS
│   ├── NF_GARCH_Results_manual.xlsx          # Manual engine results
│   ├── NF_GARCH_Results_rugarch.xlsx         # rugarch engine results
│   └── Initial_GARCH_Model_Fitting.xlsx      # Standard GARCH baseline
├── Pipeline/
│   ├── scripts/                              # All pipeline components
│   │   ├── data_prep/                        # Data preparation
│   │   ├── model_fitting/                    # GARCH and NF model fitting
│   │   ├── simulation_forecasting/           # NF-GARCH simulation
│   │   ├── evaluation/                       # Model evaluation
│   │   ├── stress_tests/                     # Stress testing
│   │   ├── modular_pipeline/                 # Modular pipeline components
│   │   └── utils/                            # Utility functions
│   ├── run_modular.bat                       # Modular pipeline
│   ├── run_all.bat                          # Full pipeline
│   └── checkpoints/                         # Pipeline checkpoints
├── Data & Outputs/
│   ├── data/                                # Input data
│   ├── outputs/                             # Generated outputs
│   ├── nf_generated_residuals/              # NF-generated residuals
│   └── modular_results/                     # Modular pipeline cache
├── Results Viewer/
│   ├── docs/                                # Static HTML results viewer
│   │   ├── index.html                       # Results viewer interface
│   │   └── manifest.json                    # File manifest (auto-generated)
│   ├── start_results_viewer.bat            # Local server launcher (Windows)
│   └── tools/                               # Build tools
│       ├── generate_results_site.py         # Manifest generator
│       └── build_results_site.sh            # Build script
├── Documentation/
│   ├── README.md                            # This file
│   ├── ai.md                               # Project guide
│   ├── MODULAR_PIPELINE_GUIDE.md           # Modular pipeline guide
│   ├── PIPELINE_FIXES_SUMMARY.md           # Pipeline fixes summary
│   ├── UNUSED_FILES_ANALYSIS.md            # Unused files analysis
│   └── FINAL_COMPONENT_SUMMARY_STATUS.md   # Component summary status
├── Testing/
│   └── tests/                              # Test scripts
├── Environment/
│   └── environment/                        # Environment config
├── Archive/
│   └── archive/                            # Old scripts
└── Development/
    ├── Makefile                            # Build automation
    ├── quick_install_*.py/R                # Quick setup scripts
    └── .gitignore                          # Git ignore rules
```

Research Components

GARCH Models Implemented
The repository includes implementations of sGARCH (Standard GARCH), eGARCH (Exponential GARCH), GJR-GARCH (Glosten-Jagannathan-Runkle GARCH), and TGARCH (Threshold GARCH).

Normalizing Flows
The normalizing flows implementation includes RealNVP (Real-valued Non-Volume Preserving), MAF (Masked Autoregressive Flow), and custom architectures designed for financial time series.

Evaluation Methods
Evaluation uses chronological splits (traditional train/test split) and time-series CV (rolling window cross-validation). Performance metrics include AIC, BIC, LogLikelihood, MSE, and MAE. Statistical tests include Wilcoxon and Diebold-Mariano tests. The repository includes VaR Backtesting for Value-at-Risk validation (both Standard and NFGARCH), Stress Testing for extreme scenario analysis (both Standard and NFGARCH), and Stylized Facts for model validation and distributional analysis.

Advanced Usage

Custom Engine Development
To add new GARCH models, create a script in scripts/manual_garch/ and source it. For new NF architectures, add training scripts to scripts/model_fitting/.

Pipeline Customization
You can modify pipeline parameters in scripts/utils/cli_parser.R, add new evaluation metrics in scripts/evaluation/, and customize output formats in scripts/utils/consolidate_dissertation_results.R.

Batch Processing
To process multiple datasets:
```
# Process multiple datasets
for dataset in dataset1 dataset2 dataset3; do
    run_all.bat --data $dataset --engine manual
done
```

Troubleshooting

Common Issues

1. R Package Conflicts
If you encounter R package conflicts, run:
```
# Run conflict resolution
source("scripts/utils/conflict_resolution.R")
```

2. Python Environment Issues
For Python environment problems:
```
# Fix Python environment
python scripts/utils/fix_python_env.py
```

3. Memory Issues
If you run into memory issues with large datasets:
```
# Use modular pipeline for large datasets
run_modular.bat data
run_modular.bat garch
run_modular.bat nf
```

4. Convergence Issues
If models aren't converging:
```
# Try different engine
run_all.bat --engine rugarch
```

Getting Help
For more information, review MODULAR_PIPELINE_GUIDE.md for pipeline troubleshooting, examine ai.md for project guidance, check PIPELINE_FIXES_SUMMARY.md for recent fixes and improvements, and review UNUSED_FILES_ANALYSIS.md for repository cleanup information.

Citation

If you use this software in your research, please cite:

```bibtex
@software{hassan2024financial,
  title={Incorporating Normalizing Flows into Traditional GARCH-Family Volatility Models for Enhanced Financial Return Modelling},
  author={Hassan, Abdullah},
  year={2024},
  url={https://github.com/username/Financial-SDG-GARCH}
}
```

License

This project is licensed under the MIT License. See the LICENSE file for details.

Author

Author: Abdullah Hassan
Institution: University of the Witwatersrand
Research: MSc in Mathematical Statistics
Focus: Financial Econometrics, Machine Learning, Time Series Analysis

Acknowledgments

This research was conducted as part of an MSc in Mathematical Statistics at the University of the Witwatersrand, exploring the intersection of traditional econometric methods and modern machine learning techniques.

---

Quick Success Checklist

- [ ] Setup: Run quick_install.R and quick_install_python.py
- [ ] Test: Run run_all.bat
- [ ] Results: Check Dissertation_Consolidated_Results.xlsx
- [ ] Analysis: Review performance metrics and model comparisons
- [ ] Customize: Modify parameters or add new models as needed

Ready to explore enhanced financial return modelling with NF-GARCH!
