# NF-GARCH Software Quality Audit

**Generated**: 2025-01-XX  
**Examiner**: Senior Academic Reviewer  
**Severity Levels**: 🔴 CRITICAL / ⚠️ MAJOR / ⚠️ MINOR

---

## Executive Summary

This audit examines code organization, error handling, determinism, dependency management, and code duplication. **Several issues identified** that affect reproducibility and maintainability.

---

## 1. Code Organization

### 1.1 Naming Conventions

**Status**: ⚠️ **MIXED CONVENTIONS**

**R Code**:
- ✅ Functions: `snake_case` (e.g., `fit_sgarch_manual`, `engine_fit`)
- ✅ Variables: `snake_case` (e.g., `nf_residuals_map`, `model_configs`)
- ✅ Files: `snake_case.R` (e.g., `manual_garch_core.R`)

**Python Code**:
- ✅ Functions: `snake_case` (e.g., `train_optimized_nf`, `set_seed`)
- ✅ Classes: `PascalCase` (e.g., `OptimizedFlow`)
- ✅ Files: `snake_case.py` (e.g., `manual_nf_training.py`)

**Issues**:
- ⚠️ **MINOR**: Some inconsistency in variable naming (e.g., `nf_resid` vs `nf_residuals`)
- ⚠️ **MINOR**: Some functions use abbreviations (e.g., `cfg` instead of `config`)

### 1.2 Directory Structure

**Status**: ✅ **WELL ORGANIZED**

- ✅ Clear separation: `scripts/manual_garch/`, `scripts/evaluation/`, `scripts/core/`
- ✅ Logical grouping of related functionality
- ✅ Archive directory for legacy code

### 1.3 Separation of Concerns

**Status**: ✅ **GOOD**

- ✅ Model fitting separated from evaluation
- ✅ Configuration centralized in `scripts/core/config.R`
- ✅ Utility functions in dedicated modules
- ⚠️ **MINOR**: Some scripts are very long (e.g., `simulate_nf_garch_engine.R` has 871 lines)

---

## 2. Error Handling & Validation

### 2.1 Error Handling Infrastructure

**File**: `scripts/utils/safety_functions.R`

**Status**: ✅ **GOOD INFRASTRUCTURE EXISTS**

**Functions Available**:
- `safe_read_csv()`: Safe file reading with error handling
- `safe_write_csv()`: Safe file writing with directory creation
- `validate_data()`: Data validation with checks for NULL, empty, NA columns
- `check_model_convergence()`: Model convergence verification
- `safe_mean()`, `safe_sd()`: Safe numeric operations

### 2.2 Error Handling Usage

**Status**: ⚠️ **INCONSISTENT**

**Good Examples**:
- `scripts/simulation_forecasting/simulate_nf_garch_engine.R`: Uses `tryCatch()` extensively
- `scripts/manual/manual_garch_fitting.R`: Has error handling for model fitting

**Issues**:
- ⚠️ **MAJOR**: Not all scripts use safety functions consistently
- ⚠️ **MAJOR**: Some errors are caught but only logged as warnings, execution continues
  - Example: `scripts/simulation_forecasting/simulate_nf_garch_engine.R` line 144: `[WARNING] NF-GARCH simulation had issues, continuing...`
  - **Impact**: Errors may be silently ignored

### 2.3 Input Validation

**Status**: ⚠️ **INCOMPLETE**

**Issues**:
- ⚠️ **MAJOR**: Data file existence checks are inconsistent
  - Some scripts check: `if (!file.exists(...))`
  - Others assume files exist
- ⚠️ **MAJOR**: No validation of data format/schema before processing
- ⚠️ **MINOR**: No validation of command-line arguments beyond basic parsing

**Recommendations**:
1. Add data schema validation before processing
2. Validate all input files exist and are readable
3. Add type checking for function arguments

---

## 3. Determinism & Reproducibility

### 3.1 Seed Management

**Status**: ⚠️ **INCOMPLETE**

**Scripts with Seeds**:
- ✅ `scripts/simulation_forecasting/simulate_nf_garch_engine.R`: `set.seed(123)` (line 39)
- ✅ `scripts/model_fitting/extract_residuals.R`: `set.seed(123)` (line 3)
- ✅ `scripts/model_fitting/fit_garch_models.R`: `set.seed(123)` (line 5)
- ✅ `scripts/manual/manual_nf_training.py`: `set_seed(123)` (line 308)
- ✅ `scripts/evaluation/nf_hyperparameter_sensitivity.py`: `torch.manual_seed(123)`, `np.random.seed(123)` (lines 291-292)

**Scripts WITHOUT Seeds**:
- ⚠️ **MAJOR**: Many evaluation scripts don't set seeds
  - `scripts/evaluation/var_backtesting_comprehensive.R`
  - `scripts/evaluation/calculate_stylized_facts.R`
  - `scripts/evaluation/stress_testing_comprehensive.R`
  - `scripts/evaluation/compare_nf_vs_standard_garch.R`
- ⚠️ **MAJOR**: Consolidation scripts don't set seeds
- ⚠️ **MAJOR**: Some utility scripts don't set seeds

**Issues**:
- ⚠️ **MAJOR**: Seed value (123) is hardcoded in multiple places
  - **Impact**: If seed needs to change, must update multiple files
  - **Fix**: Centralize seed in config file
- ⚠️ **MAJOR**: Not all random operations use seeded generators
  - Python: Some operations may use `random` module without seeding
  - R: Some operations may use random number generation without seed

**Recommendations**:
1. Add seed setting to ALL scripts that use randomness
2. Centralize seed value in `scripts/core/config.R`
3. Document which operations are deterministic vs random

### 3.2 Platform Independence

**Status**: 🔴 **CRITICAL - WINDOWS-ONLY**

**Issues**:
- 🔴 **CRITICAL**: Hardcoded Windows paths in `run_all.bat`
  - Line 93: `"C:\Program Files\R\R-4.5.1\bin\Rscript.exe"`
  - **Impact**: Pipeline will fail on Linux/Mac or if R is installed elsewhere
- 🔴 **CRITICAL**: Batch files (`.bat`) are Windows-only
  - No Linux/Mac alternatives (README mentions `./run_all.sh` and `make all` but files don't exist)
- ⚠️ **MAJOR**: Path separators may be inconsistent
  - Some code uses `/` (works on all platforms)
  - Some code may use `\` (Windows-only)

**Recommendations**:
1. Use environment variables or PATH detection for R/Python executables
2. Create Linux/Mac shell scripts or document Windows-only limitation
3. Use `file.path()` in R and `os.path.join()` in Python for path construction

---

## 4. Dependency Management

### 4.1 Python Dependencies

**File**: `environment/requirements.txt`

**Status**: ⚠️ **INCOMPLETE VERSION PINNING**

**Issues**:
- ⚠️ **MAJOR**: Only minimum versions specified (`>=`)
  - **Impact**: Different versions may produce different results
  - **Fix**: Pin exact versions or provide `pip freeze` output
- ⚠️ **MAJOR**: Missing `nflows` installation instructions
  - `nflows` may not be available via standard pip
  - **Fix**: Document installation method

### 4.2 R Dependencies

**File**: `environment/R_sessionInfo.txt`

**Status**: 🔴 **CRITICAL - INCOMPLETE**

**Issues**:
- 🔴 **CRITICAL**: `R_sessionInfo.txt` only shows base R packages
  - **Impact**: Cannot verify package versions
  - **Fix**: Run `sessionInfo()` after loading all packages

**File**: `README.md`

**Issues**:
- ⚠️ **MAJOR**: Package list in README may not match actual usage
  - README lists: `rugarch`, `quantmod`, `xts`, `dplyr`, `ggplot2`, `quantmod`, `tseries`, `PerformanceAnalytics`, `FinTS`, `openxlsx`, `stringr`, `forecast`, `transport`, `fmsb`, `moments`
  - Need to verify all are actually used

### 4.3 Environment Files

**File**: `environment/environment.yml`

**Status**: ⚠️ **INCOMPLETE**

**Issues**:
- ⚠️ **MAJOR**: Missing packages from `requirements.txt`
  - Missing: `nflows`, `openpyxl`, `plotly`
- ⚠️ **MINOR**: Python version specified (3.8) but README says "3.8 or higher"

---

## 5. Code Duplication

### 5.1 Duplicated Code

**Status**: ⚠️ **MODERATE DUPLICATION**

**Examples**:

1. **Standardization Logic** (🔴 CRITICAL - Already identified in Math Audit)
   - Appears in 3+ locations with slight variations
   - **Impact**: Inconsistent behavior, maintenance burden

2. **Data Loading**:
   - Similar data loading code in multiple scripts
   - **Impact**: MINOR - Could be centralized

3. **Model Configuration**:
   - Model configs defined in multiple places
   - **Impact**: MINOR - Some centralization exists in `config.R`

**Recommendations**:
1. Create shared standardization function
2. Centralize data loading utilities
3. Ensure all scripts use centralized config

### 5.2 Dead Code

**Status**: ⚠️ **SOME DEAD CODE**

**Examples**:
- `archive/` directory contains old scripts
  - ✅ **Good**: Archived, not in active use
- `scripts/utils/safety_functions.R` has deprecated functions:
  - `safe_ugarchfit()`: Returns error (line 36)
  - `safe_ugarchforecast()`: Returns error (line 42)
  - **Impact**: MINOR - Deprecated functions should be removed or clearly marked

**Recommendations**:
1. Remove deprecated functions or mark clearly
2. Document why code is archived
3. Consider removing unused imports

---

## 6. Code Quality Issues

### 6.1 Long Functions

**Status**: ⚠️ **SOME LONG FUNCTIONS**

**Examples**:
- `scripts/simulation_forecasting/simulate_nf_garch_engine.R`: 871 lines
  - **Impact**: MINOR - Hard to maintain, but functional
- `scripts/manual/manual_nf_training.py`: `train_optimized_nf()` function is long
  - **Impact**: MINOR - Could be split into smaller functions

### 6.2 Magic Numbers

**Status**: ⚠️ **SOME MAGIC NUMBERS**

**Examples**:
- `var_floor = 1e-12`: Appears in multiple places
  - **Impact**: MINOR - Should be constant
- `0.1` and `1` in standardization checks (line 213, 429)
  - **Impact**: MINOR - Should be named constants
- `0.65` split ratio: Appears in multiple places
  - **Impact**: MINOR - Should be in config

**Recommendations**:
1. Define constants for magic numbers
2. Move to config file where appropriate

### 6.3 Comments & Documentation

**Status**: ⚠️ **MIXED QUALITY**

**Good Examples**:
- `scripts/manual_garch/manual_garch_core.R`: Good mathematical comments
- `scripts/evaluation/var_backtesting_comprehensive.R`: Good function documentation

**Issues**:
- ⚠️ **MINOR**: Some functions lack docstrings
- ⚠️ **MINOR**: "CRITICAL FIX" comments suggest known bugs (should be fixed, not commented)
- ⚠️ **MINOR**: Some comments are outdated or unclear

**Recommendations**:
1. Add docstrings to all functions
2. Remove "CRITICAL FIX" comments after fixing issues
3. Update outdated comments

---

## 7. Summary of Issues

### 🔴 CRITICAL Issues

1. **Platform Dependence** (Section 3.2)
   - Windows-only pipeline, hardcoded paths
   - **Impact**: Cannot run on Linux/Mac
   - **Fix**: Use environment variables, create cross-platform scripts

2. **Incomplete R Package Information** (Section 4.2)
   - `R_sessionInfo.txt` doesn't list installed packages
   - **Impact**: Cannot verify reproducibility
   - **Fix**: Generate complete `sessionInfo()` output

### ⚠️ MAJOR Issues

1. **Incomplete Seed Management** (Section 3.1)
   - Not all scripts set seeds
   - **Impact**: Non-deterministic results
   - **Fix**: Add seeds to all scripts, centralize seed value

2. **Inconsistent Error Handling** (Section 2.2)
   - Some errors are warnings that continue execution
   - **Impact**: Errors may be silently ignored
   - **Fix**: Use consistent error handling, fail fast for critical errors

3. **Incomplete Version Pinning** (Section 4.1)
   - Only minimum versions specified
   - **Impact**: Different versions may produce different results
   - **Fix**: Pin exact versions or provide version ranges

4. **Incomplete Input Validation** (Section 2.3)
   - No schema validation, inconsistent file checks
   - **Impact**: May fail with wrong data format
   - **Fix**: Add comprehensive input validation

### ⚠️ MINOR Issues

1. **Code Duplication** (Section 5.1)
   - Some duplicated code (especially standardization)
   - **Impact**: Maintenance burden
   - **Fix**: Centralize common functions

2. **Magic Numbers** (Section 6.2)
   - Hardcoded values should be constants
   - **Impact**: MINOR - Code clarity
   - **Fix**: Define named constants

3. **Long Functions** (Section 6.1)
   - Some functions are very long
   - **Impact**: MINOR - Maintainability
   - **Fix**: Split into smaller functions

---

## 8. Recommendations

### Immediate Actions (Before Publication)

1. **Fix Platform Dependence**
   - Use environment variables for R/Python paths
   - Create Linux/Mac alternatives or document Windows-only limitation

2. **Complete Seed Management**
   - Add seeds to all scripts
   - Centralize seed in config

3. **Complete Dependency Lists**
   - Generate complete `sessionInfo()` output
   - Pin exact package versions

4. **Improve Error Handling**
   - Use consistent error handling
   - Fail fast for critical errors

### Before MDPI Submission

1. **Add Input Validation**
   - Schema validation for all inputs
   - Comprehensive file existence checks

2. **Reduce Code Duplication**
   - Centralize standardization logic
   - Create shared utilities

3. **Improve Documentation**
   - Add docstrings to all functions
   - Remove "CRITICAL FIX" comments after fixing

---

**Next Steps**: Proceed to Reproducibility Rigor Audit

