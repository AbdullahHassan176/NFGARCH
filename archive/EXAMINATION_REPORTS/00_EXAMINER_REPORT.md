# NF-GARCH Academic Examination Report

**Generated**: 2025-01-XX  
**Examiner**: Senior Academic Reviewer  
**Purpose**: Master's Dissertation + MDPI Journal Submission

---

## VERDICT

**🔴 MAJOR REVISIONS REQUIRED**

The codebase has **critical mathematical issues**, **reproducibility blockers**, and **questionable result claims** that must be addressed before publication. While the overall structure is sound, several issues could invalidate results or prevent third-party replication.

---

## Top 10 Critical Issues

### 1. 🔴 CRITICAL: Multiple NF Residual Standardization Points

**Severity**: 🔴 **BLOCKER**

**Evidence**:
- `scripts/simulation_forecasting/simulate_nf_garch_engine.R`: Lines 203-215, 360-373, 419-433
- `scripts/utils/utils_nf_garch.R`: Lines 4-10
- "CRITICAL FIX" comments suggest known bug patched, not properly fixed

**Impact**: Results may be invalid due to over-standardization or inconsistent standardization

**Fix**: Standardize once, verify at each step, remove all redundant standardization

---

### 2. 🔴 CRITICAL: eGARCH E|z| Calculation Error

**Severity**: 🔴 **BLOCKER**

**Evidence**:
- `scripts/utils/utils_nf_garch.R` line 124: `Ezabs <- mean(abs(z_nf), na.rm = TRUE)`
- Should use theoretical E|z| based on distribution assumption
- For Normal: E|z| = √(2/π) ≈ 0.798
- For Student-t: Use `E_abs_t(nu)` function (exists but not used)

**Impact**: May bias eGARCH parameter estimates

**Fix**: Use theoretical E|z| based on distribution assumption

---

### 3. 🔴 CRITICAL: Platform Dependence (Windows-Only)

**Severity**: 🔴 **BLOCKER**

**Evidence**:
- `run_all.bat` line 93: `"C:\Program Files\R\R-4.5.1\bin\Rscript.exe"` (hardcoded path)
- No Linux/Mac alternatives (README mentions but files don't exist)

**Impact**: Cannot run on Linux/Mac, prevents third-party replication

**Fix**: Use environment variables, create cross-platform scripts

---

### 4. 🔴 CRITICAL: Incomplete R Package Information

**Severity**: 🔴 **BLOCKER**

**Evidence**:
- `environment/R_sessionInfo.txt` only shows base R packages
- Cannot verify package versions for reproducibility

**Impact**: Cannot ensure reproducibility

**Fix**: Generate complete `sessionInfo()` output after loading all packages

---

### 5. ⚠️ MAJOR: Suspicious AIC Claim

**Severity**: ⚠️ **MAJOR**

**Evidence**:
- README line 118: "NF-GARCH AIC is -34,586 vs Standard GARCH -7.55 (4,500x better)"
- AIC is not a ratio metric, cannot meaningfully compare as "4,500x"
- -34,586 is extremely negative (suspicious)
- Aggregation method unclear

**Impact**: Claim may be incorrect or misleading

**Fix**: Verify AIC calculation, clarify aggregation, correct claim

---

### 6. ⚠️ MAJOR: Incomplete Seed Management

**Severity**: ⚠️ **MAJOR**

**Evidence**:
- Only 6 scripts set seeds (out of 30+ scripts)
- Many evaluation scripts don't set seeds
- Seed value (123) hardcoded in multiple places

**Impact**: Non-deterministic results, cannot reproduce exactly

**Fix**: Add seeds to all scripts, centralize seed in config

---

### 7. ⚠️ MAJOR: Missing Data Documentation

**Severity**: ⚠️ **MAJOR**

**Evidence**:
- Data file `data/processed/raw (FX + EQ).csv` required but source not documented
- No download instructions or sample data provided

**Impact**: Cannot run pipeline without data

**Fix**: Document data source, provide download instructions or sample data

---

### 8. ⚠️ MAJOR: Incomplete Dependency Lists

**Severity**: ⚠️ **MAJOR**

**Evidence**:
- `environment/requirements.txt`: Only minimum versions
- `environment/environment.yml`: Missing packages
- README package list may not match actual usage

**Impact**: Different versions may produce different results

**Fix**: Pin exact versions, synchronize all dependency lists

---

### 9. ⚠️ MAJOR: Forecast vs Simulation Confusion

**Severity**: ⚠️ **MAJOR**

**Evidence**:
- `scripts/simulation_forecasting/simulate_nf_garch_engine.R` lines 447-448:
  - `mse <- mean((asset_returns - fitted_values)^2)`
  - `fitted_values` are simulated returns, not forecasts

**Impact**: Evaluation metrics may not measure what is claimed

**Fix**: Clarify whether this is intended or use proper forecast evaluation

---

### 10. ⚠️ MAJOR: Broken Installation Instructions

**Severity**: ⚠️ **MAJOR**

**Evidence**:
- README mentions `run_manual.bat` but file doesn't exist
- README mentions `scripts/manual/run_manual_optimized.bat` but file doesn't exist
- README mentions `quick_install.R` but it's in `archive/`

**Impact**: Users cannot follow instructions

**Fix**: Update README or create missing files

---

## Key Risks

1. **Results Invalidity**: Multiple standardization points and eGARCH E|z| error could invalidate results
2. **Non-Reproducibility**: Platform dependence, incomplete dependencies, missing data prevent replication
3. **Misleading Claims**: AIC "4,500x better" claim is mathematically incorrect
4. **Non-Determinism**: Incomplete seed management prevents exact reproduction

---

## Recommendations

### Before Any Publication

1. Fix all 🔴 CRITICAL issues
2. Verify all mathematical claims
3. Ensure full reproducibility
4. Document all assumptions and limitations

### Before MDPI Submission

1. Fix all ⚠️ MAJOR issues
2. Add comprehensive tests
3. Complete documentation
4. Verify all claims have code support

---

**See detailed reports in**:
- `01_Repository_Map.md`
- `02_Reproducibility_Dry_Run_Plan.md`
- `03_Mathematical_Correctness_Audit.md`
- `04_Software_Quality_Audit.md`
- `05_Method_Verification_Checklist.md`
- `06_Results_Integrity_Inspection.md`

