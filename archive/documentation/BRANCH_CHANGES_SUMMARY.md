# Changes Between Current Branch and Origin/Main

**Branch**: `rerun_academic_run_all`  
**Comparison**: `origin/main`  
**Date**: 2025-01-XX

---

## Summary Statistics

- **Total Files Changed**: 178 files
- **Insertions**: +145,716 lines
- **Deletions**: -187,452 lines
- **Net Change**: -41,736 lines (mostly output files)

---

## New Files Added

### Documentation
- `CHANGELOG_FIXES.md` - Detailed changelog of all fixes
- `FIXES_SUMMARY.md` - Summary of fixes applied
- `REPRODUCIBILITY.md` - Complete reproducibility guide
- `REPRODUCIBILITY_MAIN_BRANCH.md` - Comparison with main branch
- `ACADEMIC_VALIDATION_STEPS.md` - Academic validation steps added
- `DISSERTATION_OUTPUTS_CHECKLIST.md` - Checklist of all outputs
- `ACADEMIC_RERUN_VERIFICATION.md` - Verification summary
- `BRANCH_CHANGES_SUMMARY.md` - This file

### Code Files
- `scripts/utils/standardize_residuals.R` - Centralized standardization function
- `scripts/utils/get_e_z.R` - Theoretical E|z| calculation utilities
- `scripts/utils/find_r_executable.bat` - R detection utility

### Environment
- `environment/requirements_frozen.txt` - Template for frozen requirements

---

## Modified Source Code Files

### Core Configuration
- **`scripts/core/config.R`**
  - Added `REPRODUCIBILITY_SEED = 123` constant
  - Added `get_r_executable()` and `get_python_executable()` functions
  - Updated `print_config_summary()` to include seed info

### Critical Fixes
- **`scripts/utils/utils_nf_garch.R`**
  - Changed eGARCH E|z| from sample mean to theoretical value: `sqrt(2/pi)`
  - Updated to use centralized `standardize_residuals()` function
  - Added comments explaining theoretical basis

- **`scripts/simulation_forecasting/simulate_nf_garch_engine.R`**
  - Removed redundant "CRITICAL FIX" standardization code
  - Uses centralized `standardize_residuals()` function
  - Added `is_standardized()` verification checks
  - Loads seed from config

- **`scripts/manual_garch/fit_egarch_manual.R`**
  - Added documentation for E|z| theoretical values
  - Clarified Normal vs Student-t E|z| calculations

### Seed Management (25+ files)
All R scripts now load seed from config:
- `scripts/model_fitting/extract_residuals.R`
- `scripts/model_fitting/fit_garch_models.R`
- `scripts/manual/manual_garch_fitting.R`
- `scripts/manual/verify_manual_math.R`
- All evaluation scripts (15+ files)
- All core scripts

### Python Scripts
- **`scripts/manual/manual_nf_training.py`**
  - Updated `set_seed()` to handle None and use seed 123
  - Added multi-GPU seed support

- **`scripts/evaluation/nf_hyperparameter_sensitivity.py`**
  - Updated seed setting with multi-GPU support

### Platform Independence
- **`run_all.bat`**
  - Removed hardcoded R paths
  - Uses `%RSCRIPT%` variable (from `find_r_executable.bat`)
  - Added automatic R detection
  - Added Steps 10-11, 14, 17-18 for academic validation
  - Updated summary to include all new outputs

- **`run_modular.bat`**
  - Removed hardcoded R path
  - Uses `%RSCRIPT%` variable

### Evaluation Scripts
All evaluation scripts updated with:
- Seed loading from config
- Consistent error handling
- Better documentation

### Documentation
- **`README.md`**
  - Fixed AIC claim (removed "4,500x better")
  - Updated installation instructions
  - Added reproducibility note

- **`ai.md`**
  - Updated with recent fixes
  - Added reproducibility requirements
  - Added standardization requirements
  - Added eGARCH E|z| requirements

### Environment
- **`environment/environment.yml`**
  - Added missing packages: `openpyxl`, `plotly`, `nflows`

---

## Files Deleted

### Output Files (Expected - will be regenerated)
- Some eGARCH model outputs (AMZN, MSFT, USDZAR) - will be regenerated
- Some residual files - will be regenerated
- Some old result files - replaced by new consolidated results

---

## Key Changes by Category

### 1. Critical Bug Fixes
- ✅ eGARCH E|z| calculation (sample mean → theoretical)
- ✅ Standardization consistency (centralized function)
- ✅ Seed management (centralized in config)

### 2. Platform Independence
- ✅ Automatic R detection
- ✅ Cross-platform batch scripts
- ✅ Environment variable support

### 3. Academic Validation
- ✅ Added methodology validation steps (10-11)
- ✅ Added methodology consolidation (14)
- ✅ Added dissertation table extraction (17)
- ✅ Added dissertation figure generation (18)

### 4. Documentation
- ✅ Complete reproducibility guide
- ✅ Fix documentation
- ✅ Academic validation documentation

### 5. Code Quality
- ✅ Removed deprecated functions
- ✅ Improved error handling
- ✅ Better code organization
- ✅ Consistent seed usage

---

## Impact on Results

### Will NOT Change Results
- GARCH fitting (same seed, same logic)
- NF training (same seed, same parameters)
- sGARCH, TGARCH, gjrGARCH simulations
- Most evaluation metrics

### May Slightly Change Results
- **eGARCH simulations**: Due to E|z| bug fix (more correct)
- **Standardization**: More consistent (should be identical if original was correct)

### Will Improve Reproducibility
- Centralized seed management
- Consistent standardization
- Better error handling

---

## Files Modified (Source Code Only)

### Core Scripts (7 files)
1. `scripts/core/config.R`
2. `scripts/core/consolidation.R`
3. `scripts/core/create_final_dashboard.R`

### Model Fitting (3 files)
4. `scripts/model_fitting/extract_residuals.R`
5. `scripts/model_fitting/fit_garch_models.R`
6. `scripts/manual/manual_garch_fitting.R`

### Simulation (2 files)
7. `scripts/simulation_forecasting/simulate_nf_garch_engine.R`
8. `scripts/utils/utils_nf_garch.R`

### Manual GARCH (1 file)
9. `scripts/manual_garch/fit_egarch_manual.R`

### Evaluation (20+ files)
10-30. All evaluation scripts (seeds, standardization)

### Python (2 files)
31. `scripts/manual/manual_nf_training.py`
32. `scripts/evaluation/nf_hyperparameter_sensitivity.py`

### Batch Files (2 files)
33. `run_all.bat`
34. `run_modular.bat`

### Utilities (1 file)
35. `scripts/utils/safety_functions.R`

### Documentation (3 files)
36. `README.md`
37. `ai.md`
38. `.gitignore`

### Environment (1 file)
39. `environment/environment.yml`

---

## Output Files Changed

Most output files (`.rds`, `.pth`, `.csv`, `.xlsx`) are modified because:
- They will be regenerated with the fixes applied
- Some may have slightly different values (eGARCH due to bug fix)
- Most should be identical (same seed, same logic)

---

## Summary

**Total Source Code Changes**: ~40 files modified, 3 new files created

**Key Improvements**:
1. ✅ Fixed critical mathematical bugs (eGARCH E|z|)
2. ✅ Improved standardization consistency
3. ✅ Centralized seed management
4. ✅ Added platform independence
5. ✅ Added academic validation steps
6. ✅ Improved documentation

**Result Impact**:
- Most results: ✅ Identical
- eGARCH results: ⚠️ Slightly different (more correct)
- Reproducibility: ✅ Much improved

---

**Status**: Ready for academic rerun with improved correctness and reproducibility

