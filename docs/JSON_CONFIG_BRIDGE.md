# JSON Config Bridge: R ↔ Python Integration

## Overview

The JSON Config Bridge ensures that Python NF training scripts use the same configuration as R scripts, maintaining consistency across the entire pipeline based on `PIPELINE_MODE`.

## How It Works

```
┌─────────────────────────────────────┐
│ scripts/core/config.R               │
│ - PIPELINE_MODE = "optimized"/"full"│
│ - NF_CONFIG (dynamic)               │
│ - ASSETS (dynamic)                  │
│ - REPRODUCIBILITY_SEED              │
└──────────────┬──────────────────────┘
               │ export_config_to_json()
               ↓
┌─────────────────────────────────────┐
│ scripts/core/nf_config.json         │
│ (Auto-generated)                    │
└──────────────┬──────────────────────┘
               │
               ├─→ R scripts read directly
               │
               └─→ Python scripts: load_nf_config()
                   └─→ manual_nf_training.py
```

## Configuration Flow

### 1. R Config Sources

When `scripts/core/config.R` is sourced:
- It sets `PIPELINE_MODE` (optimized or full)
- Configures `NF_CONFIG` based on mode
- **Automatically exports** to `scripts/core/nf_config.json`

### 2. Python Scripts Load

When `manual_nf_training.py` runs:
- Calls `load_nf_config()` function
- Reads `scripts/core/nf_config.json`
- Uses exact same parameters as R

### 3. Benefits

✅ **Single Source of Truth**: Change one place (`scripts/core/config.R`)
✅ **Mode Awareness**: Python automatically adapts to optimized/full mode
✅ **No Duplication**: Eliminates hardcoded values in Python
✅ **Consistency**: R and Python always in sync
✅ **Reproducibility**: Both use same seed from config

## What Gets Exported

The JSON config contains:

```json
{
  "pipeline_mode": "optimized",
  "reproducibility_seed": 123,
  "assets": {
    "fx": ["EURUSD", "GBPUSD", "USDZAR"],
    "equity": ["NVDA", "MSFT", "AMZN"],
    "all_assets": [...]
  },
  "nf_config": {
    "epochs": 75,
    "batch_size": 512,
    "learning_rate": 0.001,
    "num_layers": 4,
    "hidden_features": 64,
    "early_stopping": true,
    "patience": 15,
    ...
  }
}
```

## Mode Comparison

| Parameter | Optimized Mode | Full Mode |
|-----------|---------------|-----------|
| Epochs | 75 | 150 |
| Batch Size | 512 | 256 |
| Num Layers | 4 | 8 |
| Hidden Features | 64 | 256 |
| Learning Rate | 0.001 | 0.0005 |
| Patience | 15 | 25 |

## Usage

### Switching Modes

Edit `scripts/core/config.R` (line 59):

```r
# Change this line:
PIPELINE_MODE <- "optimized"  # or "full"
```

That's it! The next time any script sources config.R:
1. JSON is auto-generated with new mode settings
2. Python scripts automatically use new settings

### Manual JSON Export (Optional)

If you need to regenerate the JSON manually:

```r
source("scripts/core/config.R")
export_config_to_json()
```

## Verification

### Check Current Mode

**In R:**
```r
source("scripts/core/config.R")
print(PIPELINE_MODE)
print(NF_CONFIG$num_layers)
```

**In Python:**
```python
import json
with open("scripts/core/nf_config.json") as f:
    config = json.load(f)
print(f"Mode: {config['pipeline_mode']}")
print(f"Layers: {config['nf_config']['num_layers']}")
```

### Ensure Sync

Both should show the same values!

## Troubleshooting

### ❌ Python shows "Config file not found"

**Cause:** `scripts/core/nf_config.json` doesn't exist

**Fix:** Source the R config first:
```r
source("scripts/core/config.R")
```

Or run any R script that sources config.R (e.g., `manual_garch_fitting.R`)

### ❌ Python values don't match R

**Cause:** Old JSON file from previous mode

**Fix:** Re-source config.R or manually export:
```r
source("scripts/core/config.R")
export_config_to_json()
```

### ❌ JSON file is missing parameters

**Cause:** You're using an older version of config.R

**Fix:** Ensure you have the latest version with `export_config_to_json()` function

## Files Modified

1. **`scripts/core/config.R`**
   - Added `export_config_to_json()` function
   - Auto-exports on source

2. **`scripts/manual/manual_nf_training.py`**
   - Added `load_nf_config()` function
   - Removed hardcoded `MANUAL_NF_CONFIG`
   - Now reads from JSON

3. **`scripts/core/nf_config.json`** (auto-generated)
   - Created by R config
   - Read by Python scripts

## Integration with Pipeline

### run_all.bat
```batch
# Step 1: GARCH Fitting (sources config.R)
Rscript scripts\manual\manual_garch_fitting.R
  └─→ Sources config.R
  └─→ JSON exported automatically

# Step 2: NF Training (reads JSON)
python scripts\manual\manual_nf_training.py
  └─→ Loads nf_config.json
  └─→ Uses same settings as R!
```

### No Changes Needed to .bat Files!

The pipeline scripts already work because:
- R scripts source config.R first (generates JSON)
- Python scripts run after (read JSON)
- Everything automatic! 🎉

## Best Practices

1. **Always edit config.R first** - Never hardcode values in Python
2. **Let auto-export work** - JSON updates automatically when config.R sources
3. **Check mode before runs** - Use `print_config_summary()` in R
4. **Verify Python sees changes** - Check startup message in Python output
5. **Commit JSON to git** - Helps track which mode was used for results

## Academic Rigour

This integration ensures:

✅ **Reproducibility**: Both languages use same seed
✅ **Consistency**: No config drift between R and Python  
✅ **Traceability**: JSON file shows exact config used
✅ **Auditability**: Can verify R and Python used same settings
✅ **Version Control**: JSON in git tracks config history

Perfect for dissertation defense: "Show me your code proves R and Python used the same parameters..."
→ Point to JSON config bridge! 🎓
