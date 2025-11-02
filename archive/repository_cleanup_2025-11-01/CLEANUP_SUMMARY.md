# Repository Cleanup Summary - November 1, 2025

## Overview
Comprehensive cleanup of the repository to streamline for research use. All unneeded files have been moved to this archive directory.

## Files Archived

### 1. Batch Files (5 files)
- `run_modular.bat` - Modular pipeline script (replaced by run_all.bat)
- `run_manual.bat` - Manual pipeline script (functionality merged into run_all.bat)
- `start_results_viewer.bat` - Excel results viewer (replaced by HTML dashboard)
- `run_remaining_evaluation.R` - Temporary evaluation script
- `scripts/manual/run_manual_optimized.bat` - Old manual script

**Kept:** `run_all.bat`, `start_research_dashboard.bat`

### 2. Documentation Files (13 files)
Temporary process documentation moved to archive:
- `COMPLETE_OPTION_B_STEPS.md` - Process documentation
- `CLEANUP_COMPLETE.md` - Previous cleanup notes
- `RUGARCH_REMOVAL_CONFIRMED.md` - Removal confirmation
- `TRAIN_gjrGARCH_NF.md` - Training guide
- `FIX_DISTRIBUTIONAL_METRICS.md` - Fix documentation
- `MANUAL_EXECUTION_STEPS.md` - Manual execution guide
- `REPRODUCIBILITY_CHECK.md` - Reproducibility analysis
- `FIXES_COMPLETE.md` - Fixes documentation
- `PIPELINE_VS_DISSERTATION_ANALYSIS.md` - Analysis document
- `results/DASHBOARD_README.md` - Dashboard guide
- `scripts/manual/manual_execution_guide.md` - Manual guide
- `scripts/manual/QUICK_REFERENCE.md` - Quick reference
- `scripts/manual/RSTUDIO_EXECUTION_GUIDE.md` - R Studio guide

**Kept:** `ai.md` (project documentation), `README.md` (main readme)

### 3. Old Residual Directories (2 directories)
- `residuals_by_model/` - Old residual structure (replaced by outputs/manual/residuals_by_model/)
- `nf_generated_residuals/` - Old NF residuals (replaced by outputs/manual/nf_models/)

**Kept:** `outputs/manual/residuals_by_model/`, `outputs/manual/nf_models/`

### 4. Old Output Directories (6 directories)
- `outputs/eda/` - EDA outputs (not used in final pipeline)
- `outputs/model_eval/` - Model evaluation outputs (consolidated into results/)
- `outputs/stress_tests/` - Stress test outputs (consolidated into results/)
- `outputs/var_backtest/` - VaR backtest outputs (consolidated into results/)
- `outputs/diagnostic/` - Diagnostic outputs (consolidated into results/diagnostics/)
- `outputs/supplementary/` - Supplementary outputs (not used)

**Kept:** `outputs/manual/` (current pipeline outputs)

### 5. Empty/Unused Directories (6 directories)
- `checkpoints/` - Empty checkpoint directory
- `tools/` - Empty tools directory
- `docs/` - Empty docs directory
- `scripts/eda/` - Empty EDA scripts directory
- `scripts/models/` - Empty models directory
- `scripts/stress_tests/` - Empty stress tests directory

### 6. Old Results Plots (1 directory)
- `results/plots/` - Old plot structure (replaced by results/dashboard_plots/)

**Kept:** `results/dashboard_plots/` (current dashboard visualizations)

## Repository Structure After Cleanup

```
Financial-SDG-GARCH/
├── ai.md                              # Project documentation
├── README.md                          # Main readme
├── LICENSE                            # License file
│
├── run_all.bat                        # Main pipeline script (KEEP)
├── start_research_dashboard.bat      # Dashboard launcher (KEEP)
│
├── data/                              # All data files (KEEP)
│   ├── processed/
│   └── raw/
│
├── scripts/                           # Active scripts (KEEP)
│   ├── core/                          # Core utilities
│   ├── engines/                       # Engine selector
│   ├── evaluation/                    # Evaluation scripts
│   ├── manual/                        # Manual execution
│   ├── manual_garch/                  # Manual GARCH implementation
│   ├── model_fitting/                 # Model fitting
│   ├── simulation_forecasting/        # NF-GARCH simulation
│   └── utils/                         # Utilities
│
├── results/                           # All results (KEEP)
│   ├── consolidated/                  # Excel files
│   ├── dashboard_plots/               # Visualization plots
│   ├── dashboard_visualizations.html  # HTML dashboard
│   └── diagnostics/                   # Diagnostic files
│
├── outputs/                           # Current outputs (KEEP)
│   └── manual/                        # Manual pipeline outputs
│       ├── garch_fitting/
│       ├── nf_models/
│       └── residuals_by_model/
│
├── environment/                       # Config files (KEEP)
│   ├── environment.yml
│   ├── requirements.txt
│   └── R_sessionInfo.txt
│
└── archive/                           # Archived files
    └── repository_cleanup_2025-11-01/  # This cleanup
```

## Files Kept for Research

### Essential Files
1. **Pipeline Scripts:**
   - `run_all.bat` - Complete pipeline execution
   - `start_research_dashboard.bat` - Interactive dashboard

2. **Documentation:**
   - `ai.md` - Project overview and architecture
   - `README.md` - Main repository documentation

3. **Scripts:**
   - All active scripts in `scripts/` directory
   - Core, engines, evaluation, manual, model_fitting, simulation_forecasting, utils

4. **Data:**
   - All data files in `data/` directory
   - Processed and raw data

5. **Results:**
   - `results/consolidated/` - All Excel result files
   - `results/dashboard_plots/` - Visualization plots
   - `results/dashboard_visualizations.html` - HTML dashboard
   - `results/diagnostics/` - Diagnostic files

6. **Outputs:**
   - `outputs/manual/` - Current manual pipeline outputs
   - GARCH fitting results
   - NF model files
   - Residuals

7. **Environment:**
   - All environment configuration files

## Archive Location
All archived files are located in: `archive/repository_cleanup_2025-11-01/`

## Notes
- All files were moved (not deleted) for safety
- Archive can be restored if needed
- Repository is now streamlined for research use
- All essential pipeline functionality is preserved

