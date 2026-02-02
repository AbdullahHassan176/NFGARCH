# Repository Organization - Clean Structure

**Last Updated:** February 2, 2026  
**Branch:** additional_investigation  
**Status:** ✅ Clean and dissertation-ready

---

## 📁 **Main Repository Structure**

### **Core Dissertation Files:**

```
c:\Experimentation\NFGARCH\
├── data/                           # Datasets
│   └── processed/                  # Combined FX + Equity data
│
├── scripts/                        # Main analysis scripts
│   ├── core/                       # Core utilities and config
│   ├── engines/                    # Engine selector
│   ├── manual/                     # Manual GARCH pipeline (PRIMARY)
│   ├── manual_garch/               # Custom GARCH implementations
│   ├── simulation_forecasting/     # NF-GARCH simulation
│   ├── evaluation/                 # Comparison and metrics
│   ├── experiments/                # Robustness experiments
│   │   └── robustness_garch_order.R
│   └── utils/                      # Utility functions
│
├── results/                        # Dissertation outputs
│   ├── consolidated/               # Excel dashboards
│   ├── dissertation_tables/        # CSV tables for LaTeX
│   ├── figures/                    # PNG figures (Fig-R1 through Fig-R8)
│   └── methodology/                # Methodology documentation
│
├── outputs/                        # Model outputs
│   └── manual/                     # Manual pipeline outputs
│       ├── garch_fitting/          # Fitted GARCH models
│       ├── nf_models/              # Trained NF models
│       └── residuals_by_model/     # Extracted residuals
│
├── overleaf_export/                # LaTeX-ready exports
│   ├── tables/                     # CSV tables
│   └── figures/                    # Figures
│
├── environment/                    # Dependencies
│   ├── requirements.txt            # Python packages
│   └── renv.lock                   # R packages
│
└── Main batch files:
    ├── run_robustness_garch_order.bat  # GARCH order robustness
    └── start_research_dashboard.bat    # Dashboard launcher
```

**Note:** Main dissertation pipeline batch files moved to `archive/deprecated_pipelines/` 
as the manual pipeline (`scripts/manual/`) is the current approach.

---

## 🗂️ **Additional Analysis (Validation Experiments)**

**Location:** `additional_analysis/`

**Purpose:** Optional validation experiments and alternative pipelines, separated to keep main repo tidy.

```
additional_analysis/
├── README.md                       # Comprehensive guide
├── find_r_executable.bat           # Utility
│
├── Validation Experiments:
│   ├── run_synthetic_recovery.bat  # Tests NF on synthetic data
│   └── run_audit_experiment.bat    # Full validation audit
│
├── Alternative Pipelines:
│   ├── run_chronological.bat       # Pure chronological split
│   ├── run_tscv.bat                # Time-series cross-validation
│   └── run_both_pipelines.bat      # Run both for comparison
│
└── scripts/
    ├── synthetic_recovery/         # Synthetic experiment scripts (8 files)
    ├── chronological/              # Chronological pipeline (2 files)
    ├── tscv/                       # TS-CV pipeline (2 files)
    └── *.R config files            # Pipeline configurations
```

**When to use:**
- ❌ Not required for dissertation
- ✅ Useful for methodology defense
- ✅ Recommended for journal publication
- ✅ Shows robustness across validation methods

**Outputs:** All experiments save to parent repo's `outputs/` and `results/` folders

---

## 📚 **Archive Folders**

### **archive/investigation_jan2026/**
**Contains:** January 2026 investigation of NF-GARCH failure mechanisms

**Key files:**
- `README.md` - Investigation overview
- `analyses/SMOKING_GUN_RESULTS.md` - Definitive proof of compatibility hypothesis
- `analyses/KEY_FINDINGS.md` - Complete synthesis
- `analyses/results/*.csv` - Quantitative results (10 CSV files)

**Key finding:** NF learns identical distributions (excess kurt diff = 0.11) but performance differs by 4.8%, proving compatibility > quality.

---

### **archive/deprecated_pipelines/**
**Contains:** Old batch files from previous pipeline architecture

**Files:**
- `run_all.bat` - Old main pipeline
- `run_full_dissertation.bat` - Old full pipeline
- `run_modular.bat` - Modular execution approach

**Status:** Superseded by manual pipeline approach (`scripts/manual/`)

---

### **archive/Manual Scripts/**
**Contains:** Early manual implementation attempts and experiments

**Kept for:** Historical reference and alternative approaches

---

## 🎯 **What to Use for Dissertation**

### **Primary Workflow:**

Currently, the manual pipeline is the primary approach. Run via:

```r
# From R console or Rscript:
source("scripts/manual/manual_garch_fitting.R")      # Fit GARCH models
# Then run Python NF training
# Then:
source("scripts/simulation_forecasting/simulate_nf_garch_engine.R")  # Simulate
source("scripts/evaluation/compare_nf_vs_standard_garch.R")          # Compare
```

**OR** use individual experiment batch files:
```batch
run_robustness_garch_order.bat     # GARCH order robustness (part of dissertation)
```

**For complete pipeline:** See `archive/deprecated_pipelines/run_full_dissertation.bat` 
(may need path updates to work with current structure)

---

### **For Validation (Optional):**

```batch
cd additional_analysis

# Validate methodology on synthetic data:
run_synthetic_recovery.bat

# Alternative validation approach:
run_chronological.bat

# Comprehensive robustness:
run_both_pipelines.bat  (8-10 hours)
```

---

## 📊 **Results Location**

All results centralized in:
- **Tables:** `results/dissertation_tables/*.csv`
- **Figures:** `results/figures/Fig-R*.png`
- **Excel:** `results/consolidated/*.xlsx`
- **Overleaf:** `overleaf_export/` (ready to import)

---

## 🧹 **Repository Quality**

### **Code Cleanliness:**
✅ No AI-generated markers or patterns  
✅ No "cursor" author references  
✅ Professional research code style  
✅ Only legitimate WARNING/NOTE comments  

### **Organization:**
✅ Main pipeline scripts in `scripts/`  
✅ Validation experiments in `additional_analysis/`  
✅ Historical materials in `archive/`  
✅ Results centralized and organized  

### **Git History:**
✅ All commits properly attributed  
✅ Clear, descriptive commit messages  
✅ No cleanup artifacts in history  

---

## 🎓 **Summary**

### **For Dissertation Submission:**
**Use:**
- `scripts/manual/` - Main pipeline
- `results/` - All tables and figures
- `overleaf_export/` - LaTeX imports

**Ignore:**
- `additional_analysis/` - Optional validation
- `archive/` - Historical reference

---

### **For Journal Publication:**
**Use:**
- Everything in main repo
- `additional_analysis/` - Robustness validation
- `archive/investigation_jan2026/` - Detailed failure analysis

---

### **For Future Research:**
**Refer to:**
- `archive/investigation_jan2026/` - Investigation findings
- `archive/investigation_jan2026/_future_research_agenda.md` - Extensions
- `additional_analysis/` - Validation toolkit

---

## ✅ **Current Status**

**Branch:** additional_investigation (8 commits)  
**Working Tree:** Clean  
**Organization:** Professional and tidy  
**Ready for:** Dissertation submission  

---

**All optional/validation experiments organized in `additional_analysis/`**  
**All investigation materials preserved in `archive/investigation_jan2026/`**  
**Main repository clean and focused on core dissertation work**
