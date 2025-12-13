# NF-GARCH Patch List

**Generated**: 2025-01-XX

---

## Files to Edit

### 1. `scripts/utils/utils_nf_garch.R`

**Lines 43-59**: Fix eGARCH E|z| calculation
- Replace `Ezabs <- mean(abs(z_nf), na.rm = TRUE)` with theoretical E|z|
- Use `E_abs_t(nu)` for Student-t distribution

### 2. `scripts/simulation_forecasting/simulate_nf_garch_engine.R`

**Lines 203-215, 360-373, 419-433**: Remove redundant standardization
- Keep standardization in one location only
- Remove "CRITICAL FIX" comments after fixing

### 3. `run_all.bat`

**Line 93**: Use environment variable for R path
- Replace hardcoded path with `%RSCRIPT%` variable
- Add path detection logic

### 4. `scripts/core/config.R`

**Add**: Centralized seed configuration
- Add `REPRODUCIBILITY_SEED = 123`
- Document seed usage

### 5. `environment/R_sessionInfo.txt`

**Replace**: Generate complete sessionInfo output
- Run `sessionInfo()` after loading all packages
- Save complete output

### 6. `environment/requirements.txt`

**Update**: Pin exact versions
- Use `pip freeze > requirements_frozen.txt`
- Or specify version ranges

### 7. `README.md`

**Line 118**: Fix AIC claim
- Remove "4,500x better" (mathematically incorrect)
- Clarify aggregation method
- Verify actual values

**Multiple lines**: Fix installation instructions
- Remove references to non-existent files
- Update file paths

---

## Functions to Create

### 1. `scripts/utils/standardize_residuals.R`

**New function**: `standardize_residuals(z)`
- Single standardization function
- Used by all scripts
- Includes validation

### 2. `scripts/utils/get_e_z.R`

**New function**: `get_e_z(distribution, nu = NULL)`
- Returns theoretical E|z| based on distribution
- Handles Normal and Student-t

### 3. `scripts/utils/path_utils.R`

**New functions**: Cross-platform path utilities
- `get_r_executable()`: Find R executable
- `get_python_executable()`: Find Python executable

---

## Tests to Add

### 1. `tests/test_standardization.R`

**Unit tests**: Standardization function
- Test mean ≈ 0, SD ≈ 1
- Test edge cases (zero variance, NA values)

### 2. `tests/test_egarch_e_z.R`

**Unit tests**: eGARCH E|z| calculation
- Test Normal: E|z| = √(2/π)
- Test Student-t: E|z| = E_abs_t(nu)

### 3. `tests/test_garch_recursions.R`

**Unit tests**: GARCH recursion equations
- Test sGARCH, GJR-GARCH, eGARCH, TGARCH
- Verify against known results

### 4. `tests/test_reproducibility.R`

**Integration tests**: Full pipeline reproducibility
- Test with same seed produces same results
- Test across platforms (if applicable)

---

## Reproducibility Additions

### 1. `Makefile` (NEW)

**Create**: Cross-platform build file
- Targets: `install`, `run`, `test`, `clean`
- Works on Linux/Mac/Windows (with make)

### 2. `run_all.sh` (NEW)

**Create**: Linux/Mac shell script
- Equivalent to `run_all.bat`
- Uses environment variables for paths

### 3. `environment/requirements_frozen.txt` (NEW)

**Create**: Exact package versions
- Generated via `pip freeze`
- Updated after each environment setup

### 4. `REPRODUCIBILITY.md` (NEW)

**Create**: Step-by-step reproduction guide
- Installation instructions
- Data requirements
- Expected outputs
- Troubleshooting

---

## Configuration Updates

### 1. `scripts/core/config.R`

**Add**:
```r
REPRODUCIBILITY_CONFIG <- list(
  seed = 123,
  r_executable = Sys.getenv("RSCRIPT", "Rscript"),
  python_executable = Sys.getenv("PYTHON", "python")
)
```

### 2. `environment/environment.yml`

**Update**: Include all packages from `requirements.txt`
- Add missing: `nflows`, `openpyxl`, `plotly`

---

**Total Files to Edit**: 7  
**New Functions**: 3  
**New Tests**: 4  
**New Files**: 4

