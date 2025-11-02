# Manual Engine Quick Reference Card

## 🚀 One-Liner Execution (Full Pipeline)

```bash
# Windows PowerShell
cd C:\Github\Financial-SDG-GARCH
.\scripts\manual\run_manual_optimized.bat
```

## 📝 Step-by-Step Execution

### Phase 1: Math Verification (5 min)
```r
source("scripts/manual/verify_manual_math.R")
```

### Phase 2: GARCH Fitting (20-30 min)
```r
setwd("C:/Github/Financial-SDG-GARCH")
source("scripts/manual/manual_optimized_config.R")
source("scripts/manual/manual_garch_fitting.R")
```

### Phase 3: NF Training (20-30 min)
```bash
python scripts/manual/manual_nf_training.py
```

### Phase 4: NF-GARCH Simulation (15-20 min)
```r
Rscript scripts/simulation_forecasting/simulate_nf_garch_engine.R --engine manual
```

## 🔍 Key Verification Points

### After GARCH Fitting:
- ✓ Check convergence rate > 90%
- ✓ Verify standardized residuals: mean ≈ 0, var ≈ 1
- ✓ Check parameter constraints satisfied

### After NF Training:
- ✓ Verify synthetic residuals preserve distribution (KS test)
- ✓ Check Wasserstein distance < 0.3

### After Simulation:
- ✓ Verify results file exists: `results/consolidated/NF_GARCH_Results_manual.xlsx`
- ✓ Check MSE and MAE are positive and reasonable
- ✓ Verify AIC/BIC are finite

## 📊 Expected Output Files

```
outputs/manual/
├── garch_fitting/
│   └── model_summary.csv           # Convergence status
├── residuals_by_model/
│   ├── sGARCH/                     # Residuals by model
│   ├── eGARCH/
│   └── TGARCH/
└── nf_models/
    ├── *_synthetic_residuals.csv   # Synthetic residuals
    └── */nf_model.pth              # Trained models

results/consolidated/
└── NF_GARCH_Results_manual.xlsx    # Final results
```

## 🎯 Optimized Settings

- **Assets**: 6 (NVDA, MSFT, AMZN, EURUSD, GBPUSD, USDZAR)
- **Models**: 3 (sGARCH, eGARCH, TGARCH)
- **CV Folds**: 3 (optimized)
- **Expected Time**: 45-90 minutes

## 📚 Full Documentation

See `scripts/manual/MANUAL_EXECUTION_GUIDE.md` for complete walkthrough with mathematical verification.

