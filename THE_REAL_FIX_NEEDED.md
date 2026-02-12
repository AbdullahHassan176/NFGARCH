# The Real Fix - Use Same Scripts, Different Data Splits

## Current Problem

**Main Pipeline (WORKS):**
```
Batch: run_all.bat
Scripts: scripts/manual/manual_garch_fitting.R
         scripts/manual/manual_nf_training.py
Config: scripts/core/config.R (restored to Feb 2 version)
Result: NF-GARCH WINS
```

**Chronological/TSCV (DON'T WORK):**
```
Batch: run_chronological.bat, run_tscv.bat
Scripts: additional_analysis/scripts/chronological/fit_garch_chronological.R
         additional_analysis/scripts/tscv/fit_garch_tscv.R
Result: Standard GARCH WINS (opposite result!)
```

## The Real Issue

**DIFFERENT SCRIPTS = DIFFERENT IMPLEMENTATIONS = DIFFERENT RESULTS**

The user is 100% right: We should use THE SAME `scripts/manual/` methodology, just with different data splitting.

## The Solution

### Option 1: Modify Manual Scripts to Support Split Modes
Add a parameter to `manual_garch_fitting.R` to support:
- `--split=cv` (default, current behavior)
- `--split=chronological` (65/35 train/test)
- `--split=tscv` (rolling windows)

### Option 2: Make Chronological/TSCV Call Manual Scripts
Change `run_chronological.bat` and `run_tscv.bat` to:
1. Call `scripts/manual/manual_garch_fitting.R` (not additional_analysis/)
2. Call `scripts/manual/manual_nf_training.py` (not additional_analysis/)
3. Just pass a config parameter for split mode

### Option 3: Copy Working Manual Scripts
Copy the ENTIRE `scripts/manual/` methodology to chronological/TSCV, then ONLY change the data splitting lines.

## Recommendation

**Option 2 is cleanest**: Use the exact same scripts, just pass different split configs.

The manual scripts already use `cv_config`, so we can:
1. Create `chrono_config.R` and `tscv_config.R` with split parameters
2. Modify `run_chronological.bat` to set `SPLIT_MODE=chronological` environment variable
3. Modify `manual_garch_fitting.R` to check `SPLIT_MODE` and use different splitting logic
4. Everything else stays the same!

## Next Steps

1. ✅ Config restored to working version (Feb 2)
2. ⏳ Modify manual scripts to support split modes
3. ⏳ Update chronological/TSCV batch scripts to use manual scripts
4. ⏳ Rerun and verify NF-GARCH wins on all pipelines
