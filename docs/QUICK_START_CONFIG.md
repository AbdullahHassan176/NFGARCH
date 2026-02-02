# Quick Start: Configuration Guide

## Single Change to Switch Modes

**Want to run FULL comprehensive analysis **

1. Open: `scripts/core/config.R`
2. Find line 59 (search for `PIPELINE_MODE`)
3. Change from:
 ```r
 PIPELINE_MODE <- "optimized"
 ```
 to:
 ```r
 PIPELINE_MODE <- "full"
 ```
4. Save and run: `run_all.bat`

**That's it!** Everything else updates automatically.

---

## What Changes

| Setting | OPTIMIZED (Default) | FULL (Robustness) |
|---------|---------------------|-------------------|
| **Assets** | 6 | **13** (+117%) |
| **CV Windows** | 3 | **8-10** (+167%) |
| **NF Layers** | 4 | **8** (+100%) |
| **NF Hidden** | 64 | **256** (+300%) |
| **Epochs** | 75 | **150** (+100%) |
| **Runtime** | 1-2 hours | **4-8 hours** |

---

## Examples

### For Dissertation Main Results (Current Setup)
```r
# In scripts/core/config.R line 59:
PIPELINE_MODE <- "optimized"
```
- Fast iteration
- Representative sample
- Sufficient for main findings
- ⏱ 60-120 minutes

### For Robustness & Appendix
```r
# In scripts/core/config.R line 59:
PIPELINE_MODE <- "full"
```
- All 13 assets
- Deep 8-layer NF architecture
- Comprehensive CV validation
- Publication-ready robustness
- ⏱ 4-8 hours

---

## Files Modified

 **`scripts/core/config.R`** - Master config (single source of truth)
 **`scripts/manual/manual_optimized_config.R`** - Wrapper (backward compatible)
 **`CONFIGURATION_GUIDE.md`** - Complete documentation

All existing scripts work without changes! They automatically use the master config.

---

## Verify Current Mode

Run this in R:
```r
source("scripts/core/config.R")
print_config_summary()
```

Output shows:
- Current mode
- Active assets
- NF architecture
- CV parameters

---

## Full Documentation

See **`CONFIGURATION_GUIDE.md`** for complete details on:
- All configuration options
- Resource requirements
- Troubleshooting
- Academic reporting guidelines
