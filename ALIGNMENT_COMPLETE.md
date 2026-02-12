# Pipeline Alignment Complete

## All Three Pipelines Now Use SAME Scripts

### Main Pipeline (`run_full_dissertation.bat` → `run_all.bat`)
```
Scripts: scripts/manual/manual_garch_fitting.R
         scripts/manual/manual_nf_training.py
Config:  scripts/core/config.R (Student-t models)
```

### Chronological Pipeline (`run_both_pipelines.bat` → `run_chronological.bat`)
```
Scripts: scripts/manual/manual_garch_fitting.R  ✅ FIXED
         scripts/manual/manual_nf_training.py   ✅ FIXED
Config:  scripts/core/config.R (Student-t models)
```

### TS-CV Pipeline (`run_both_pipelines.bat` → `run_tscv.bat`)
```
Scripts: scripts/manual/manual_garch_fitting.R  ✅ FIXED NOW
         scripts/manual/manual_nf_training.py   ✅ FIXED NOW
Config:  scripts/core/config.R (Student-t models)
```

## Shared Configuration

All three pipelines now use:
- **Same GARCH models**: sGARCH_std, eGARCH_std, gjrGARCH_std, TGARCH_std
- **Same distribution**: Student-t (std)
- **Same NF training**: 4 layers, 64 hidden, 75 epochs
- **Same assets**: 6 assets (NVDA, MSFT, AMZN, EURUSD, GBPUSD, USDZAR)
- **Same scripts**: scripts/manual/ (the working implementation)
- **Same simulation paths**: 100

## ONLY Difference: Data Splitting

- **Main**: CV-based splitting (3 windows)
- **Chronological**: 65/35 train/test split (NO CV)
- **TS-CV**: Rolling window cross-validation (3 windows)

This is EXACTLY what you requested - same methodology, only data splitting differs!

## Expected Result

All three pipelines should now show **NF-GARCH winning**, because they're all using:
- The working manual scripts
- Student-t distribution
- Fair comparison framework
