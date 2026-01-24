# Code Fixes Applied During Audit

This document describes all code changes made during the comprehensive audit of the synthetic recovery experiment.

## Fix 1: Seed Synchronization

### Issue
Python seed was hardcoded to 123 in `train_nf_synthetic.py`, not synchronized with R seed from `REPRODUCIBILITY_SEED`.

### Files Modified

#### 1. `scripts/experiments/synthetic_recovery/train_nf_synthetic.py`

**Change 1**: Modified function signature to accept seed parameter

**Line 27** (old):
```python
def train_nf_on_residuals(residuals_file, output_model_path, config=None):
```

**Line 27** (new):
```python
def train_nf_on_residuals(residuals_file, output_model_path, config=None, seed=None):
```

**Change 2**: Use provided seed instead of hardcoded value

**Line 56** (old):
```python
    # Set seed for reproducibility
    set_seed(123)
```

**Line 56** (new):
```python
    # Set seed for reproducibility (use provided seed or default to 123)
    if seed is None:
        seed = 123
    set_seed(seed)
```

**Change 3**: Updated main() to accept seed from command line

**Lines 203-210** (old):
```python
def main():
    """Main entry point for command-line usage"""
    if len(sys.argv) < 3:
        print("Usage: python train_nf_synthetic.py <residuals_file> <output_model_path>")
        sys.exit(1)
    
    residuals_file = sys.argv[1]
    output_model_path = sys.argv[2]
    
    if not os.path.exists(residuals_file):
        print(f"ERROR: Residuals file not found: {residuals_file}")
        sys.exit(1)
    
    flow, samples = train_nf_on_residuals(residuals_file, output_model_path)
```

**Lines 203-210** (new):
```python
def main():
    """Main entry point for command-line usage"""
    if len(sys.argv) < 3:
        print("Usage: python train_nf_synthetic.py <residuals_file> <output_model_path> [seed]")
        sys.exit(1)
    
    residuals_file = sys.argv[1]
    output_model_path = sys.argv[2]
    seed = int(sys.argv[3]) if len(sys.argv) > 3 else 123
    
    if not os.path.exists(residuals_file):
        print(f"ERROR: Residuals file not found: {residuals_file}")
        sys.exit(1)
    
    flow, samples = train_nf_on_residuals(residuals_file, output_model_path, seed=seed)
```

#### 2. `scripts/experiments/synthetic_recovery/run_synthetic_recovery.R`

**Change**: Pass seed to Python script

**Lines 174-180** (old):
```r
  # Use normalized paths
  python_cmd <- sprintf('python "%s" "%s" "%s"', 
                         normalizePath(python_script, mustWork = FALSE), 
                         normalizePath(nf_residuals_file, mustWork = FALSE),
                         normalizePath(nf_model_path, mustWork = FALSE))
```

**Lines 174-180** (new):
```r
  # Use normalized paths and pass seed
  current_seed <- if (exists("REPRODUCIBILITY_SEED")) REPRODUCIBILITY_SEED else 123
  python_cmd <- sprintf('python "%s" "%s" "%s" %d', 
                         normalizePath(python_script, mustWork = FALSE), 
                         normalizePath(nf_residuals_file, mustWork = FALSE),
                         normalizePath(nf_model_path, mustWork = FALSE),
                         current_seed)
```

### Impact
- Ensures Python and R use the same seed for full reproducibility
- Allows seed to be changed via `REPRODUCIBILITY_SEED` in `scripts/core/config.R`
- Maintains backward compatibility (defaults to 123 if not provided)

---

## Fix 2: Added Sanity Checks to Evaluation

### Issue
No validation of z_true properties or NF sample scale in evaluation function.

### Files Modified

#### `scripts/experiments/synthetic_recovery/evaluate_recovery.R`

**Change 1**: Load audit validation functions

**After line 5** (added):
```r
# Load audit validation functions
if (file.exists("scripts/experiments/synthetic_recovery/audit_validation.R")) {
  source("scripts/experiments/synthetic_recovery/audit_validation.R")
}
```

**Change 2**: Add validation checks in evaluate_distribution_recovery()

**After line 258** (added):
```r
  # Sanity check: Validate z_true properties
  if (exists("validate_z_true")) {
    cat("  Validating z_true properties...\n")
    z_true_validation <- validate_z_true(z_true, dgp_config$innovation_type, dgp_config$innovation_params)
    if (!z_true_validation$all_checks_passed) {
      warning("z_true validation failed: mean=", z_true_validation$mean_value, 
              ", sd=", z_true_validation$sd_value)
    }
  }
  
  # Load NF samples
  z_nf <- load_nf_samples(nf_model_path, nf_residuals_path)
  
  # Sanity check: Validate NF samples scale
  if (!is.null(z_nf) && length(z_nf) > 10) {
    nf_mean <- mean(z_nf, na.rm = TRUE)
    nf_sd <- sd(z_nf, na.rm = TRUE)
    cat("  NF samples: mean =", round(nf_mean, 6), ", sd =", round(nf_sd, 6), "\n")
    if (abs(nf_mean) > 0.2 || abs(nf_sd - 1) > 0.2) {
      warning("NF samples may not be properly standardized: mean=", nf_mean, ", sd=", nf_sd)
    }
  }
```

### Impact
- Provides runtime validation of z_true properties
- Warns if NF samples deviate from expected scale
- Helps identify issues during experiment execution

---

## Summary

All fixes maintain backward compatibility and do not change the core experiment logic. The fixes improve reproducibility and add validation checks to catch potential issues early.

**Total files modified**: 2
**Total functions modified**: 2
**Lines added**: ~30
**Lines modified**: ~10

