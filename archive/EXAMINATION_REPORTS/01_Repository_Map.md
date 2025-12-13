# NF-GARCH Repository Map: Academic Examination

**Generated**: 2025-01-XX  
**Examiner**: Senior Academic Reviewer  
**Purpose**: Master's Dissertation + MDPI Journal Submission Audit

---

## 1. High-Level Directory Structure

```
NFGARCH/
├── data/                          # Data storage
│   ├── raw/                       # Raw price data (fx_equity_prices.csv)
│   └── processed/                 # Processed data (combined_data.csv, raw (FX + EQ).csv)
│
├── outputs/                       # Generated outputs
│   └── manual/                   # Manual engine outputs
│       ├── garch_fitting/        # Fitted GARCH models (.rds files)
│       ├── residuals_by_model/   # Standardized residuals for NF training
│       │   └── {model}/{asset}/  # Organized by model and asset
│       ├── nf_models/            # Trained NF models and synthetic residuals
│       │   ├── *.pth             # PyTorch model files
│       │   └── *_residuals_synthetic.csv  # Generated synthetic residuals
│       └── evaluation/           # Evaluation outputs
│
├── results/                       # Final results and analysis
│   ├── consolidated/              # Consolidated Excel results
│   │   ├── Dissertation_Consolidated_Results.xlsx  # MAIN RESULTS
│   │   ├── NF_GARCH_Results_manual.xlsx
│   │   ├── NF_vs_Standard_GARCH_Comparison.xlsx
│   │   ├── Distributional_Metrics.xlsx
│   │   ├── Stylized_Facts.xlsx
│   │   ├── VaR_Backtesting.xlsx
│   │   └── Stress_Testing.xlsx
│   ├── dashboard_plots/           # Visualization plots
│   ├── diagnostics/              # Diagnostic outputs
│   ├── dissertation_tables/       # Tables for dissertation
│   └── figures/                  # Figures for dissertation
│
├── scripts/                       # All pipeline scripts
│   ├── core/                      # Core utilities
│   │   ├── config.R              # Centralized configuration
│   │   ├── consolidation.R       # Results consolidation
│   │   ├── create_final_dashboard.R
│   │   ├── parallel_execution.R
│   │   └── utils.R
│   │
│   ├── engines/                   # Engine abstraction layer
│   │   └── engine_selector.R     # Unified API for manual engine
│   │
│   ├── manual_garch/             # Manual GARCH implementations
│   │   ├── manual_garch_core.R  # Core GARCH functions (transformations, LL)
│   │   ├── fit_sgarch_manual.R   # sGARCH fitting
│   │   ├── fit_gjr_manual.R     # GJR-GARCH fitting
│   │   ├── fit_egarch_manual.R   # eGARCH fitting
│   │   ├── fit_tgarch_manual.R   # TGARCH fitting
│   │   └── forecast_manual.R    # Forecasting functions
│   │
│   ├── manual/                   # Manual pipeline scripts
│   │   ├── manual_optimized_config.R  # Optimization config
│   │   ├── manual_garch_fitting.R    # GARCH fitting entry point
│   │   ├── manual_nf_training.py     # NF training entry point
│   │   └── verify_manual_math.R     # Mathematical verification
│   │
│   ├── model_fitting/            # Model fitting scripts
│   │   ├── fit_garch_models.R    # GARCH model fitting
│   │   └── extract_residuals.R  # Residual extraction
│   │
│   ├── simulation_forecasting/   # NF-GARCH simulation
│   │   └── simulate_nf_garch_engine.R  # Main NF-GARCH simulation script
│   │
│   ├── evaluation/               # Evaluation and analysis
│   │   ├── var_backtesting_comprehensive.R      # VaR backtesting
│   │   ├── calculate_stylized_facts.R            # Stylized facts
│   │   ├── calculate_distributional_metrics.R   # Distributional metrics
│   │   ├── stress_testing_comprehensive.R         # Stress testing
│   │   ├── compare_nf_vs_standard_garch.R         # Model comparison
│   │   ├── generate_dashboard_visualizations.R     # Visualizations
│   │   └── [20+ other evaluation scripts]
│   │
│   └── utils/                    # Utility functions
│       ├── cli_parser.R          # Command-line argument parsing
│       ├── checkpoint_manager.R  # Pipeline checkpointing
│       ├── safety_functions.R    # Safety and validation functions
│       └── utils_nf_garch.R      # NF-GARCH utility functions
│
├── environment/                   # Environment configuration
│   ├── requirements.txt          # Python dependencies
│   ├── environment.yml           # Conda environment
│   ├── R_sessionInfo.txt        # R package versions
│   └── renv.lock                # R environment lock file
│
├── logs/                          # Pipeline execution logs
├── checkpoints/                   # Pipeline checkpoints (modular pipeline)
├── archive/                       # Archived/legacy code
│
├── run_all.bat                    # Main pipeline entry point (Windows)
├── run_modular.bat                # Modular pipeline with checkpointing
├── README.md                      # User documentation
└── ai.md                          # Project documentation for AI assistants
```

---

## 2. Entry Points

### 2.1 Primary Entry Points

1. **`run_all.bat`** (Windows Batch Script)
   - **Purpose**: Complete end-to-end pipeline execution
   - **Steps**:
     1. Clear previous outputs
     2. GARCH fitting (`scripts/manual/manual_garch_fitting.R`)
     3. NF training (`scripts/manual/manual_nf_training.py`)
     4. NF-GARCH simulation (`scripts/simulation_forecasting/simulate_nf_garch_engine.R`)
     5. Evaluation and comparison scripts
     6. Results consolidation
     7. Dashboard generation
   - **Estimated Time**: 45-90 minutes
   - **Configuration**: Uses manual engine only, optimized assets (6), models (4)

2. **`run_modular.bat`** (Windows Batch Script)
   - **Purpose**: Modular pipeline with checkpointing and resume capability
   - **Features**: Step-by-step execution, checkpointing, detailed logging
   - **Same steps as `run_all.bat`** but with modular control

### 2.2 Script-Level Entry Points

1. **GARCH Fitting**: `scripts/manual/manual_garch_fitting.R`
   - Fits GARCH models to training data
   - Extracts standardized residuals
   - Saves fitted models and residuals

2. **NF Training**: `scripts/manual/manual_nf_training.py`
   - Trains Normalizing Flows on GARCH residuals
   - Generates synthetic residuals
   - Saves trained models and synthetic data

3. **NF-GARCH Simulation**: `scripts/simulation_forecasting/simulate_nf_garch_engine.R`
   - Simulates NF-GARCH models using synthetic residuals
   - Performs time-series cross-validation
   - Evaluates forecasting performance

### 2.3 Evaluation Entry Points

- `scripts/evaluation/var_backtesting_comprehensive.R`
- `scripts/evaluation/calculate_stylized_facts.R`
- `scripts/evaluation/calculate_distributional_metrics.R`
- `scripts/evaluation/stress_testing_comprehensive.R`
- `scripts/evaluation/compare_nf_vs_standard_garch.R`

---

## 3. Data Flow

### 3.1 Complete Pipeline Flow

```
Raw Data (data/raw/fx_equity_prices.csv)
    ↓
[Data Preprocessing]
    ↓
Processed Data (data/processed/raw (FX + EQ).csv)
    ↓
[GARCH Fitting - scripts/manual/manual_garch_fitting.R]
    ├──→ Fitted Models (outputs/manual/garch_fitting/*.rds)
    └──→ Standardized Residuals (outputs/manual/residuals_by_model/{model}/{asset}/*.csv)
    ↓
[NF Training - scripts/manual/manual_nf_training.py]
    ├──→ Trained NF Models (outputs/manual/nf_models/*.pth)
    └──→ Synthetic Residuals (outputs/manual/nf_models/*_residuals_synthetic.csv)
    ↓
[NF-GARCH Simulation - scripts/simulation_forecasting/simulate_nf_garch_engine.R]
    ├──→ Forecasts and Performance Metrics
    └──→ Intermediate Results
    ↓
[Evaluation Scripts - scripts/evaluation/*.R]
    ├──→ VaR Backtesting Results
    ├──→ Stylized Facts Analysis
    ├──→ Distributional Metrics
    ├──→ Stress Testing Results
    └──→ Model Comparisons
    ↓
[Consolidation - scripts/core/consolidation.R]
    ↓
Final Results (results/consolidated/*.xlsx)
    ├──→ Dissertation_Consolidated_Results.xlsx
    ├──→ NF_GARCH_Results_manual.xlsx
    ├──→ NF_vs_Standard_GARCH_Comparison.xlsx
    └──→ [Other consolidated results]
```

### 3.2 Data Splits

- **Chronological Split**: 65% training, 35% testing (strictly chronological)
- **Time-Series Cross-Validation**: Sliding window approach
  - Window size: 500 observations
  - Step size: 150 observations (optimized)
  - Forecast horizon: 20 observations
  - Max windows: 3-4 (for speed)

---

## 4. Key Components

### 4.1 GARCH Models Implemented

1. **sGARCH_norm**: Standard GARCH with normal distribution
2. **sGARCH_sstd**: Standard GARCH with skewed Student-t distribution
3. **eGARCH**: Exponential GARCH with asymmetric effects
4. **gjrGARCH**: Glosten-Jagannathan-Runkle GARCH with leverage effects
5. **TGARCH**: Threshold GARCH with regime-dependent behavior

**Implementation**: Manual implementation from scratch (no rugarch simulation)

### 4.2 Normalizing Flows

- **Architecture**: Masked Autoregressive Flow (MAF)
- **Implementation**: `nflows` library (PyTorch)
- **Configuration**:
  - Layers: 4 (optimized from 5)
  - Hidden features: 64 (optimized from 128)
  - Epochs: 75 (optimized from 100)
  - Batch size: 512
  - Learning rate: 0.001

### 4.3 Assets

**FX (6 assets)**:
- EURUSD, GBPUSD, GBPCNY, USDZAR, GBPZAR, EURZAR

**Equity (6 assets)**:
- NVDA, MSFT, PG, CAT, WMT, AMZN, X (United States Steel)

**Total**: 12 assets (though optimized pipeline uses 6)

---

## 5. Pipeline Dependencies

### 5.1 R Dependencies

**Core Packages**:
- `rugarch`: GARCH model fitting (though manual implementation is used)
- `xts`: Time series objects
- `dplyr`, `tidyr`: Data manipulation
- `openxlsx`: Excel file I/O
- `PerformanceAnalytics`: Financial analysis
- `FinTS`: Financial time series utilities

**Statistical Packages**:
- `tseries`: Time series analysis
- `forecast`: Forecasting utilities
- `lmtest`: Linear model tests
- `moments`: Statistical moments

**Version Info**: See `environment/R_sessionInfo.txt`

### 5.2 Python Dependencies

**Core Packages**:
- `torch` (>=1.9.0): PyTorch for deep learning
- `nflows` (>=0.14.0): Normalizing flows implementation
- `numpy` (>=1.21.0): Numerical computing
- `pandas` (>=1.3.0): Data manipulation
- `scikit-learn` (>=1.0.0): Machine learning utilities

**Visualization**:
- `matplotlib` (>=3.4.0)
- `seaborn` (>=0.11.0)
- `plotly` (>=5.0.0)

**Version Info**: See `environment/requirements.txt`

### 5.3 Engine Configuration

- **Standard GARCH Engine**: `manual` (only option)
- **NF-GARCH Engine**: `manual` (only option)
- **Engine Selector**: `scripts/engines/engine_selector.R` provides unified API

---

## 6. Configuration Management

### 6.1 Central Configuration

- **File**: `scripts/core/config.R`
- **Contains**:
  - Model configurations (GARCH_MODELS, NF_GARCH_MODELS)
  - Asset lists (ASSETS, ALL_ASSETS)
  - Output schemas (OUTPUT_SCHEMAS)
  - Simulation parameters (SIMULATION_PARAMS)
  - Time-series CV parameters (TSCV_OPTIMIZATION)

### 6.2 Manual Pipeline Configuration

- **File**: `scripts/manual/manual_optimized_config.R`
- **Contains**:
  - Optimized asset lists (6 assets)
  - Optimized model lists (4 models)
  - CV parameters (3 folds, max 3 windows)
  - NF training parameters (75 epochs, etc.)

---

## 7. Output Artifacts

### 7.1 Intermediate Artifacts

1. **Fitted GARCH Models**: `outputs/manual/garch_fitting/*.rds`
2. **Standardized Residuals**: `outputs/manual/residuals_by_model/{model}/{asset}/*.csv`
3. **Trained NF Models**: `outputs/manual/nf_models/*.pth`
4. **Synthetic Residuals**: `outputs/manual/nf_models/*_residuals_synthetic.csv`

### 7.2 Final Results

1. **Dissertation_Consolidated_Results.xlsx**: Main consolidated results
2. **NF_GARCH_Results_manual.xlsx**: Manual engine results
3. **NF_vs_Standard_GARCH_Comparison.xlsx**: Model comparison
4. **Distributional_Metrics.xlsx**: Distributional analysis
5. **Stylized_Facts.xlsx**: Stylized facts verification
6. **VaR_Backtesting.xlsx**: VaR validation results
7. **Stress_Testing.xlsx**: Stress testing results
8. **Final_Dashboard.xlsx**: Comprehensive Excel dashboard

---

## 8. Critical Files for Audit

### 8.1 Mathematical Correctness
- `scripts/manual_garch/manual_garch_core.R` (GARCH recursions, transformations)
- `scripts/manual/manual_nf_training.py` (NF training, standardization)
- `scripts/simulation_forecasting/simulate_nf_garch_engine.R` (residual injection)
- `scripts/evaluation/var_backtesting_comprehensive.R` (statistical tests)
- `scripts/utils/utils_nf_garch.R` (NF-GARCH simulation utilities)

### 8.2 Reproducibility
- `run_all.bat` (pipeline execution)
- `scripts/core/config.R` (configuration)
- `environment/requirements.txt` (Python dependencies)
- `environment/R_sessionInfo.txt` (R dependencies)

### 8.3 Documentation
- `README.md` (user documentation)
- `ai.md` (project documentation)
- Function docstrings across all scripts

---

## 9. Notes for Examination

### 9.1 Potential Issues to Investigate

1. **Multiple Standardization Points**: NF residual standardization appears in multiple locations
   - `scripts/simulation_forecasting/simulate_nf_garch_engine.R` (lines 203-215, 360-373, 419-433)
   - `scripts/utils/utils_nf_garch.R` (`.standardize_nf` function)
   - Need to verify consistency

2. **Seed Management**: Verify seed (123) is set consistently across all scripts

3. **Data Leakage**: Verify chronological splits and time-series CV don't leak future data

4. **Configuration Consistency**: Verify asset lists and model configs are consistent across scripts

### 9.2 Missing Components to Verify

1. Unit tests for mathematical functions
2. Integration tests for pipeline
3. Reproducibility documentation (exact steps to reproduce)
4. Version pinning completeness

---

**Next Steps**: Proceed to Step 2 (Reproducibility Dry-Run Plan)

