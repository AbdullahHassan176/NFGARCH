# Additional Analysis & Validation Experiments

This folder contains alternative pipelines and validation experiments that are **separate from the main dissertation pipeline** but provide additional robustness checks and methodological validation.

---

## **Contents**

### **Validation Experiments:**

#### 1. **Synthetic Recovery Experiment**
**Purpose:** Validates that NF-GARCH can correctly recover known distributions

**Run:** `run_synthetic_recovery.bat` 
**Script:** `scripts/synthetic_recovery/run_synthetic_recovery.R`

**What it does:**
- Generates synthetic GARCH data with known parameters
- Trains NF on synthetic residuals
- Tests if NF can recover the true distribution
- Validates methodology before applying to real data

**Outputs:** `../outputs/synthetic_recovery/`

---

#### 2. **Audit Experiment**
**Purpose:** Comprehensive audit of the synthetic recovery experiment

**Run:** `run_audit_experiment.bat` 
**Script:** `scripts/synthetic_recovery/run_full_audit.R`

**What it does:**
- Runs synthetic recovery experiment
- Performs validation checks
- Generates detailed audit report
- Verifies all components working correctly

**Outputs:** `../outputs/synthetic_recovery/audit/`

---

### **Alternative Pipelines:**

#### 3. **Chronological Split Pipeline**
**Purpose:** Alternative to main pipeline using pure chronological split (no CV)

**Run:** `run_chronological.bat` 
**Scripts:** `scripts/chronological/`

**What it does:**
- Fits GARCH on 65% training data
- Tests on 35% test data
- Pure chronological split (simpler than TS-CV)
- Useful for comparison with main results

**Outputs:** 
- `../outputs/chronological/`
- `../results/chronological/`

---

#### 4. **Time-Series Cross-Validation Pipeline**
**Purpose:** Alternative pipeline using rolling window TS-CV

**Run:** `run_tscv.bat` 
**Scripts:** `scripts/tscv/`

**What it does:**
- Uses rolling windows for validation
- More robust to temporal instability
- Computationally expensive (4-8 hours)
- Alternative validation approach

**Outputs:**
- `../outputs/tscv/`
- `../results/tscv/`

---

#### 5. **Dual Pipeline Execution**
**Purpose:** Runs both chronological and TS-CV for comprehensive validation

**Run:** `run_both_pipelines.bat`

**What it does:**
- Executes chronological pipeline first
- Then executes TS-CV pipeline
- Generates comparison analysis
- Total time: 8-10 hours

**Outputs:** Both pipeline outputs + comparison analysis

---

## **When to Use These**

### **For Dissertation:**
 **Not required** - Main pipeline (`../run_full_dissertation.bat`) is sufficient

### **For Journal Publication:**
 **Recommended** - Shows robustness across validation methods

### **For Methodology Defense:**
 **Useful** - Synthetic recovery proves NF-GARCH works correctly

### **For Reviewers' Requests:**
 **Ready** - Alternative validation approaches already implemented

---

## **How to Run**

### **From this folder:**

```batch
# Synthetic recovery validation:
run_synthetic_recovery.bat

# Full audit with validation:
run_audit_experiment.bat

# Alternative chronological pipeline:
run_chronological.bat

# Alternative TS-CV pipeline (long runtime):
run_tscv.bat

# Both alternative pipelines:
run_both_pipelines.bat
```

### **Note:** 
All scripts reference the parent repository (`../`) for:
- Data: `../data/`
- Shared scripts: `../scripts/core/`, `../scripts/engines/`, etc.
- Outputs: `../outputs/`, `../results/`

This keeps the main repo clean while maintaining full functionality.

---

## **Outputs**

All outputs are saved to the **parent repository** to maintain centralized results:

**Synthetic Recovery:**
- `../outputs/synthetic_recovery/`
- Audit reports, validation metrics, recovery statistics

**Chronological:**
- `../outputs/chronological/`
- `../results/chronological/`
- Pure chronological split results

**TS-CV:**
- `../outputs/tscv/`
- `../results/tscv/`
- Time-series cross-validation results

---

## **Technical Details**

### **Dependencies:**
- Same as main pipeline (R packages, Python packages)
- See `../environment/requirements.txt`

### **Runtime:**
- Synthetic recovery: ~5-10 minutes
- Audit experiment: ~10-15 minutes
- Chronological pipeline: ~60-90 minutes
- TS-CV pipeline: ~4-8 hours
- Both pipelines: ~8-10 hours

### **Hardware:**
- Same requirements as main pipeline
- 8GB RAM minimum, 16GB recommended
- GPU optional but recommended for TS-CV

---

## 📋 **Relationship to Main Dissertation**

### **Main Dissertation Pipeline:**
Located in parent repo: `../run_full_dissertation.bat`

**Includes:**
1. GARCH model fitting
2. NF training
3. NF-GARCH simulation
4. Comparison analysis
5. GARCH order robustness
6. Dissertation tables and figures

### **This Folder (Additional Analysis):**
**Provides:**
- Methodological validation (synthetic recovery)
- Alternative validation approaches (chronological, TS-CV)
- Robustness checks
- Publication-ready extensions

**Not required for dissertation submission** but strengthens overall research quality.

---

## **Status**

- **All experiments:** Fully functional
- **Scripts:** Self-contained in this folder
- **Outputs:** Saved to parent repo (centralized)
- **Dependencies:** Share main repo utilities
- **Ready to run:** Yes, all batch files updated

---

## **Usage Recommendations**

### **Before Dissertation Submission:**
- Don't need to run these
- Main pipeline results are sufficient

### **After Positive Feedback:**
- Run synthetic recovery for defense preparation
- Shows methodological rigor

### **If Reviewers Ask for Robustness:**
- Run chronological pipeline
- Run TS-CV pipeline
- Shows results hold across validation methods

### **For Journal Publication:**
- Run all analyses
- Include in supplementary materials
- Demonstrates comprehensive validation

---

**Folder Created:** February 2, 2026 
**Purpose:** Keep main repo tidy while preserving validation experiments 
**Status:** All scripts functional and self-contained
