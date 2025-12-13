# NF-GARCH Reproducibility Dry-Run Plan

**Generated**: 2025-01-XX  
**Examiner**: Senior Academic Reviewer  
**Purpose**: Verify end-to-end reproducibility for third-party replication

---

## Executive Summary

This document outlines the reproducibility verification plan for the NF-GARCH codebase. The goal is to identify all blocking issues that would prevent a third party from reproducing results on a clean machine.

**Status**: ⚠️ **BLOCKING ISSUES IDENTIFIED** (see below)

---

## 1. Dependency Installation Verification

### 1.1 Python Dependencies

**File**: `environment/requirements.txt`

**Current State**:
```
numpy>=1.21.0
pandas>=1.3.0
scikit-learn>=1.0.0
matplotlib>=3.4.0
seaborn>=0.11.0
torch>=1.9.0
torchvision>=0.10.0
pyyaml>=5.4.0
openpyxl>=3.0.0
plotly>=5.0.0
nflows>=0.14.0
```

**Issues Identified**:

1. ⚠️ **MISSING CRITICAL PACKAGE**: `nflows` is required but may not be available via standard pip
   - **Impact**: BLOCKER - NF training will fail
   - **Fix Required**: Add installation instructions or alternative source

2. ⚠️ **VERSION PINNING INCOMPLETE**: Only minimum versions specified
   - **Impact**: MAJOR - Different versions may produce different results
   - **Fix Required**: Pin exact versions or provide `pip freeze` output

3. ⚠️ **MISSING PACKAGES**: README mentions packages not in requirements.txt
   - README says: `pip install numpy pandas torch scikit-learn matplotlib seaborn`
   - Missing: `nflows`, `pyyaml`, `openpyxl`, `plotly`, `torchvision`
   - **Impact**: MAJOR - Installation will be incomplete

**Verification Steps**:
- [ ] Test: `pip install -r environment/requirements.txt` on clean Python 3.8 environment
- [ ] Verify: All packages install without errors
- [ ] Verify: `nflows` package is accessible and installs correctly
- [ ] Check: Package versions match those used in development

### 1.2 R Dependencies

**File**: `environment/R_sessionInfo.txt`

**Current State**: ⚠️ **INCOMPLETE** - Only shows base R packages, no user-installed packages

**Issues Identified**:

1. 🔴 **CRITICAL**: `R_sessionInfo.txt` does not list installed packages
   - **Impact**: BLOCKER - Cannot verify package versions
   - **Fix Required**: Run `sessionInfo()` after loading all packages and save output

2. ⚠️ **VERSION INFORMATION MISSING**: No version numbers for any packages
   - **Impact**: MAJOR - Cannot ensure version compatibility
   - **Fix Required**: Include full `sessionInfo()` output

3. ⚠️ **PACKAGE LIST INCOMPLETE**: README lists packages not verified in sessionInfo
   - README mentions: `rugarch`, `xts`, `dplyr`, `ggplot2`, `quantmod`, `tseries`, `PerformanceAnalytics`, `FinTS`, `openxlsx`, `stringr`, `forecast`, `transport`, `fmsb`, `moments`
   - **Impact**: MAJOR - Missing packages may cause runtime errors

**Packages Actually Used** (from code inspection):
- `xts` (time series)
- `PerformanceAnalytics` (financial analysis)
- `dplyr`, `tidyr` (data manipulation)
- `stringr` (string operations)
- `lubridate` (date handling)
- `parallel`, `doParallel` (parallel processing)
- `openxlsx` (Excel I/O)
- `rugarch` (GARCH fitting - though manual implementation is used)

**Verification Steps**:
- [ ] Generate complete `sessionInfo()` output after loading all required packages
- [ ] Verify: All packages in README install correctly
- [ ] Test: `install.packages()` command from README works
- [ ] Check: Package versions are compatible

### 1.3 Environment Configuration

**File**: `environment/environment.yml`

**Current State**: ⚠️ **INCOMPLETE**

**Issues Identified**:

1. ⚠️ **MISSING PACKAGES**: `environment.yml` does not include all packages from `requirements.txt`
   - Missing: `nflows`, `openpyxl`, `plotly`
   - **Impact**: MAJOR - Conda environment will be incomplete

2. ⚠️ **PYTHON VERSION**: Specifies Python 3.8, but README says "3.8 or higher"
   - **Impact**: MINOR - May cause confusion

**Verification Steps**:
- [ ] Test: `conda env create -f environment/environment.yml`
- [ ] Verify: All packages install correctly
- [ ] Check: Environment matches `requirements.txt`

---

## 2. End-to-End Execution Path

### 2.1 Installation Instructions

**Current State**: ⚠️ **INCOMPLETE AND INCONSISTENT**

**Issues Identified**:

1. ⚠️ **MULTIPLE INCONSISTENT INSTRUCTIONS**:
   - README mentions `run_manual.bat` but file doesn't exist in repo
   - README mentions `scripts/manual/run_manual_optimized.bat` but file doesn't exist
   - README mentions `quick_install.R` and `quick_install_python.py` but they're in `archive/`
   - **Impact**: BLOCKER - Users cannot follow instructions

2. ⚠️ **WINDOWS-SPECIFIC**: All instructions assume Windows
   - Batch files (`.bat`) are Windows-only
   - No Linux/Mac alternatives (README mentions `./run_all.sh` and `make all` but files don't exist)
   - **Impact**: MAJOR - Non-Windows users cannot run pipeline

3. ⚠️ **HARDCODED PATHS**: `run_all.bat` has hardcoded R path
   - `"C:\Program Files\R\R-4.5.1\bin\Rscript.exe"`
   - **Impact**: MAJOR - Will fail if R is installed elsewhere

**Verification Steps**:
- [ ] Test: Follow README installation instructions on clean Windows machine
- [ ] Test: Verify all referenced files exist
- [ ] Test: Check if pipeline works with different R installation paths
- [ ] Create: Linux/Mac alternatives or document Windows-only limitation

### 2.2 Pipeline Execution

**Entry Point**: `run_all.bat`

**Expected Execution Flow**:
1. Clear previous outputs
2. GARCH fitting (`scripts/manual/manual_garch_fitting.R`)
3. NF training (`scripts/manual/manual_nf_training.py`)
4. NF-GARCH simulation (`scripts/simulation_forecasting/simulate_nf_garch_engine.R --engine manual`)
5. Evaluation scripts (multiple)
6. Consolidation (`scripts/core/consolidation.R`)
7. Dashboard generation (`scripts/core/create_final_dashboard.R`)

**Issues Identified**:

1. ⚠️ **INTERACTIVE PROMPT**: `run_all.bat` requires user confirmation
   - Line 31: `set /p confirm="Continue with pipeline? (Y/N): "`
   - **Impact**: MINOR - Prevents fully automated execution
   - **Fix**: Add `--non-interactive` flag or environment variable

2. ⚠️ **ERROR HANDLING**: Some steps continue on error (WARNING messages)
   - **Impact**: MAJOR - Errors may be silently ignored
   - **Fix**: Add strict error checking or document expected behavior

3. ⚠️ **DEPENDENCY ON EXTERNAL DATA**: Pipeline requires `data/processed/raw (FX + EQ).csv`
   - **Impact**: BLOCKER - Data file not in repo (or not documented)
   - **Fix**: Document data source and provide download instructions

**Verification Steps**:
- [ ] Test: Execute `run_all.bat` on clean machine
- [ ] Verify: All intermediate outputs are generated
- [ ] Check: Error handling works correctly
- [ ] Verify: Final results match expected outputs

### 2.3 Data Requirements

**Current State**: ⚠️ **UNCLEAR**

**Issues Identified**:

1. 🔴 **CRITICAL**: Data file location and source not documented
   - Pipeline expects: `data/processed/raw (FX + EQ).csv`
   - **Impact**: BLOCKER - Cannot run pipeline without data
   - **Fix Required**: Document data source, download instructions, or provide sample data

2. ⚠️ **DATA FORMAT**: Expected data format not documented
   - What columns are required?
   - What date format?
   - What asset names?
   - **Impact**: MAJOR - Users may have wrong data format

**Verification Steps**:
- [ ] Document: Data source and download instructions
- [ ] Verify: Data file exists or can be generated
- [ ] Test: Pipeline works with provided/sample data
- [ ] Document: Expected data format and schema

---

## 3. Artifact Verification

### 3.1 Expected Artifacts

**Intermediate Artifacts**:

1. **Fitted GARCH Models**: `outputs/manual/garch_fitting/*.rds`
   - **Generating Script**: `scripts/manual/manual_garch_fitting.R`
   - **Verification**: [ ] Files exist after GARCH fitting step
   - **Schema**: RDS format (R data structure)

2. **Standardized Residuals**: `outputs/manual/residuals_by_model/{model}/{asset}/*.csv`
   - **Generating Script**: `scripts/manual/manual_garch_fitting.R`
   - **Verification**: [ ] Files exist for all model-asset combinations
   - **Schema**: CSV with residual values

3. **Trained NF Models**: `outputs/manual/nf_models/*.pth`
   - **Generating Script**: `scripts/manual/manual_nf_training.py`
   - **Verification**: [ ] Files exist after NF training
   - **Schema**: PyTorch model files

4. **Synthetic Residuals**: `outputs/manual/nf_models/*_residuals_synthetic.csv`
   - **Generating Script**: `scripts/manual/manual_nf_training.py`
   - **Verification**: [ ] Files exist for all model-asset combinations
   - **Schema**: CSV with synthetic residual values

**Final Artifacts**:

1. **Dissertation_Consolidated_Results.xlsx**: Main consolidated results
   - **Generating Script**: `scripts/core/consolidation.R`
   - **Verification**: [ ] File exists and has expected sheets
   - **Schema**: Excel file with multiple sheets (see `OUTPUT_SCHEMAS` in `config.R`)

2. **NF_GARCH_Results_manual.xlsx**: Manual engine results
3. **NF_vs_Standard_GARCH_Comparison.xlsx**: Model comparison
4. **Distributional_Metrics.xlsx**: Distributional analysis
5. **Stylized_Facts.xlsx**: Stylized facts verification
6. **VaR_Backtesting.xlsx**: VaR validation results
7. **Stress_Testing.xlsx**: Stress testing results
8. **Final_Dashboard.xlsx**: Comprehensive Excel dashboard

**Issues Identified**:

1. ⚠️ **ARTIFACT SCHEMA VALIDATION**: No automated validation of output schemas
   - **Impact**: MAJOR - Outputs may have wrong structure
   - **Fix**: Add schema validation in consolidation script

2. ⚠️ **ARTIFACT DOCUMENTATION**: Expected artifacts not fully documented
   - **Impact**: MINOR - Users may not know what to expect
   - **Fix**: Document all expected outputs in README

**Verification Steps**:
- [ ] Test: Run pipeline and verify all artifacts are generated
- [ ] Verify: Artifact schemas match `OUTPUT_SCHEMAS` in `config.R`
- [ ] Check: All artifacts are referenced in documentation
- [ ] Validate: Artifacts can be opened and contain expected data

---

## 4. Blocking Issues Summary

### 🔴 CRITICAL BLOCKERS (Must Fix Before Publication)

1. **Incomplete R Package Information**
   - `R_sessionInfo.txt` does not list installed packages
   - **Fix**: Generate complete `sessionInfo()` output

2. **Missing Data File**
   - Data source and format not documented
   - **Fix**: Document data source or provide sample data

3. **Broken Installation Instructions**
   - README references non-existent files
   - **Fix**: Update README or create missing files

4. **Missing nflows Package Instructions**
   - `nflows` may not install via standard pip
   - **Fix**: Document installation method

### ⚠️ MAJOR ISSUES (Should Fix Before Publication)

1. **Incomplete Version Pinning**
   - Only minimum versions specified
   - **Fix**: Pin exact versions or provide version ranges

2. **Windows-Only Pipeline**
   - No Linux/Mac alternatives
   - **Fix**: Create cross-platform scripts or document limitation

3. **Hardcoded Paths**
   - R path hardcoded in batch files
   - **Fix**: Use environment variables or PATH detection

4. **Inconsistent Package Lists**
   - README, requirements.txt, and environment.yml don't match
   - **Fix**: Synchronize all dependency lists

### ⚠️ MINOR ISSUES (Nice to Have)

1. **Interactive Prompts**
   - Pipeline requires user confirmation
   - **Fix**: Add non-interactive mode

2. **Error Handling**
   - Some errors are warnings that continue execution
   - **Fix**: Document expected behavior or add strict error checking

---

## 5. Reproducibility Checklist

### Pre-Execution
- [ ] R (>=4.0.0) installed and accessible
- [ ] Python (>=3.8) installed and accessible
- [ ] All R packages installed (verify with `sessionInfo()`)
- [ ] All Python packages installed (verify with `pip list`)
- [ ] Data file available (`data/processed/raw (FX + EQ).csv`)
- [ ] Working directory set to repository root

### Execution
- [ ] `run_all.bat` executes without errors
- [ ] All intermediate artifacts generated
- [ ] All final artifacts generated
- [ ] No critical errors in logs

### Post-Execution
- [ ] Final results match expected outputs
- [ ] Results are reproducible (rerun produces same outputs)
- [ ] All artifacts have correct schemas
- [ ] Documentation matches actual outputs

---

## 6. Recommendations

### Immediate Actions (Before Any Publication)

1. **Generate Complete Dependency Lists**
   - Run `sessionInfo()` in R after loading all packages
   - Run `pip freeze > requirements_frozen.txt` in Python
   - Update `R_sessionInfo.txt` and `requirements.txt`

2. **Fix Installation Instructions**
   - Remove references to non-existent files
   - Create missing files or update README
   - Test installation on clean machine

3. **Document Data Requirements**
   - Provide data source and download instructions
   - Or provide sample data file
   - Document expected data format

4. **Create Reproducibility Test**
   - Script that verifies all dependencies
   - Script that validates all outputs
   - Document expected execution time

### Before MDPI Submission

1. **Cross-Platform Support**
   - Create Linux/Mac alternatives or document Windows-only limitation
   - Test on multiple platforms

2. **Version Pinning**
   - Pin exact package versions
   - Provide environment lock files

3. **Automated Validation**
   - Add schema validation for outputs
   - Add integration tests

4. **Complete Documentation**
   - Step-by-step reproduction guide
   - Troubleshooting section
   - Expected outputs documentation

---

**Next Steps**: Proceed to Step 3 (Deep Code Audit)

