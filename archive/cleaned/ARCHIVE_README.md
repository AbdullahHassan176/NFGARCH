# Archived Files - Manual Branch Cleanup

This directory contains files that were archived during the Manual branch cleanup.

## Archive Date
[Current Date]

## Reason for Archiving
These files are not required for the Manual branch pipeline execution:
- Old batch scripts replaced by `scripts/manual/run_manual_optimized.bat`
- Documentation files that are no longer needed
- Evaluation scripts that were one-off analyses
- Old NF training scripts replaced by `scripts/manual/manual_nf_training.py`
- Utility scripts not used by the Manual pipeline
- Stress testing and comprehensive evaluation scripts

## Essential Files Kept

The following files are **essential** for the Manual pipeline and were **NOT** archived:

### Core Pipeline Scripts
- `scripts/manual/manual_optimized_config.R` - Configuration
- `scripts/manual/manual_garch_fitting.R` - GARCH fitting
- `scripts/manual/manual_nf_training.py` - NF training
- `scripts/simulation_forecasting/simulate_nf_garch_engine.R` - NF-GARCH simulation

### Manual GARCH Implementation
- `scripts/manual_garch/fit_sgarch_manual.R`
- `scripts/manual_garch/fit_egarch_manual.R`
- `scripts/manual_garch/fit_tgarch_manual.R`
- `scripts/manual_garch/fit_gjr_manual.R`
- `scripts/manual_garch/manual_garch_core.R`
- `scripts/manual_garch/forecast_manual.R`

### Core Utilities
- `scripts/engines/engine_selector.R`
- `scripts/utils/cli_parser.R`
- `scripts/utils/safety_functions.R`
- `scripts/utils/utils_nf_garch.R`
- `scripts/core/config.R`
- `scripts/core/utils.R`
- `scripts/core/parallel_execution.R`

### Essential Documentation
- `README.md` - Main project readme
- `ai.md` - Project information for AI assistants
- `scripts/manual/manual_execution_guide.md`
- `scripts/manual/QUICK_REFERENCE.md`
- `scripts/manual/RSTUDIO_EXECUTION_GUIDE.md`

### Essential Scripts
- `scripts/manual/verify_manual_math.R` - Mathematical verification
- `scripts/evaluation/verify_all_results.R` - Results verification
- `scripts/model_fitting/fit_garch_models.R` - Core GARCH fitting
- `scripts/model_fitting/extract_residuals.R` - Residual extraction

## Archive Structure

```
archive/cleaned/
├── documentation/          # .md files
├── batch_files/           # .bat files
├── scripts/
│   ├── evaluation/       # Evaluation scripts
│   ├── core/             # Core scripts
│   ├── model_fitting/    # Old NF training
│   ├── stress_tests/     # Stress testing
│   ├── utils/            # Utility scripts
│   ├── simulation_forecasting/  # Old simulation
│   ├── models/           # Old models
│   └── eda/              # EDA scripts
├── tools/                # Tools directory
└── docs/                # Docs directory
```

## To Restore Files

If you need to restore any archived files:
1. Locate the file in the archive structure above
2. Copy it back to its original location
3. Update any dependencies if needed

## Note

These files are archived, not deleted. They can be restored if needed for future reference or if the Manual branch is merged back into main.

