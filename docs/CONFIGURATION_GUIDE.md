# NF-GARCH Pipeline Configuration Guide

## Quick Start: Switching Between Modes

**To change from OPTIMIZED to FULL mode:**

1. Open `scripts/core/config.R`
2. Change line 30 from:
 ```r
 PIPELINE_MODE <- "optimized"
 ```
 to:
 ```r
 PIPELINE_MODE <- "full"
 ```
3. Save and re-run: `run_all.bat` or `run_full_dissertation.bat`

That's it! All scripts automatically adapt.

---

## Mode Comparison

| Feature | OPTIMIZED Mode | FULL Mode |
|---------|----------------|-----------|
| **Assets** | 6 (3 FX + 3 Equity) | 13 (6 FX + 7 Equity) |
| **FX Pairs** | EURUSD, GBPUSD, USDZAR | +GBPCNY, GBPZAR, EURZAR |
| **Equities** | NVDA, MSFT, AMZN | +X, PG, CAT, WMT |
| **CV Windows** | 3 (max) | 8-10 (unlimited) |
| **CV Step Size** | 15% (larger steps) | 5% (smaller steps) |
| **Forecast Horizon** | 20 steps | 40 steps |
| **NF Layers** | 4 | 8 (2x deeper!) |
| **NF Hidden Features** | 64 | 256 (4x wider!) |
| **NF Epochs** | 75 | 150 (2x more training) |
| **Early Stopping Patience** | 15 | 25 |
| **Batch Size** | 512 | 256 (better gradients) |
| **Learning Rate** | 0.001 | 0.0005 (more stable) |
| **Runtime** | 60-120 minutes | 4-8 hours |
| **RAM Required** | 8GB | 16GB recommended |
| **GPU** | Optional | Highly recommended |

---

## What Changes Automatically

When you switch `PIPELINE_MODE`, the following automatically update:

### 1. **Asset Selection**
- **Scripts affected:** All GARCH fitting, NF training, evaluation scripts
- **Change:** From 6 representative assets to complete 13-asset dataset
- **Impact:** More comprehensive cross-sectional coverage

### 2. **Cross-Validation**
- **Scripts affected:** `manual_garch_fitting.R`, TSCV modules
- **Changes:**
 - Window count: 3 → 8-10 windows
 - Step size: 15% → 5% (more overlapping validation)
 - Forecast horizon: 20 → 40 steps
- **Impact:** More robust model selection, better out-of-sample validation

### 3. **NF Architecture**
- **Scripts affected:** `manual_nf_training.py`
- **Changes:**
 - Layers: 4 → 8 (deeper network)
 - Hidden features: 64 → 256 (4x capacity)
 - Epochs: 75 → 150 (more training)
 - Additional features: dropout, batch norm, residual connections
- **Impact:** Better capacity to learn complex distributions

### 4. **Training Hyperparameters**
- **Learning rate:** 0.001 → 0.0005 (more stable)
- **Batch size:** 512 → 256 (better gradient estimates)
- **Patience:** 15 → 25 epochs (more time to converge)
- **Validation frequency:** Every 5 → Every 3 epochs
- **Impact:** Better convergence for deeper networks

---

## Advanced Full Mode Features

When running in FULL mode, you also get:

### **Enhanced NF Architecture:**
```python
# Automatically enabled in FULL mode:
dropout = 0.1 # Regularization
batch_norm = TRUE # Batch normalization
residual_connections = TRUE # Skip connections
gradient_clipping = 1.0 # Gradient stability
weight_decay = 1e-5 # L2 regularization
lr_scheduler = "cosine" # Cosine annealing
warmup_epochs = 10 # Learning rate warmup
```

### **Resource Requirements:**
- **GPU Memory:** 4-8GB for NF training
- **System RAM:** 16GB recommended
- **Disk Space:** ~10GB for intermediate results
- **CPU Cores:** 8 cores for parallel CV

---

## Use Cases

### When to Use OPTIMIZED Mode:
 **Development & Testing**
- Iterating on code changes
- Debugging pipeline issues
- Quick sanity checks

 **Dissertation Main Results**
- Main empirical findings
- Core performance comparisons
- Standard tables and figures

 **Limited Resources**
- Running on laptop
- No GPU available
- Time constraints

### When to Use FULL Mode:
 **Robustness Checks**
- Sensitivity to asset selection
- Cross-sectional stability
- Comprehensive validation

 **Appendix & Supplementary**
- Additional asset coverage
- Deep architecture comparison
- Extended training results

 **Publication Requirements**
- Journal submission
- Comprehensive robustness
- Peer review preparation

---

## Configuration File Structure

```
scripts/core/config.R ← MASTER CONFIG (edit this!)
│
├─ PIPELINE_MODE ← Change this line (line 30)
├─ OPTIMIZED_ASSETS ← 6 assets
├─ FULL_ASSETS ← 13 assets
├─ TSCV_OPTIMIZED ← Fast CV
├─ TSCV_FULL ← Comprehensive CV
├─ NF_OPTIMIZED ← Shallow network
├─ NF_FULL ← Deep network
└─ Helper functions ← Auto-select based on mode

scripts/manual/manual_optimized_config.R ← Wrapper (backward compatible)
```

---

## Functions to Use in Scripts

All scripts should use these functions (they automatically respect PIPELINE_MODE):

```r
# Load the config
source("scripts/core/config.R")

# Get assets
assets <- get_pipeline_assets() # All assets
fx <- get_fx_assets() # FX only
equity <- get_equity_assets() # Equity only

# Get configurations
nf_config <- get_nf_config() # NF parameters
cv_config <- get_cv_config() # CV parameters

# Print current setup
print_config_summary() # Shows active mode
```

---

## Verification Checklist

After switching modes, verify:

- [ ] `print_config_summary()` shows correct mode
- [ ] Asset count matches expectation (6 vs 13)
- [ ] NF config shows correct architecture (4/64 vs 8/256)
- [ ] CV config shows correct windows (3 vs unlimited)
- [ ] Output directories are cleared if needed
- [ ] Sufficient RAM/GPU for selected mode

---

## Troubleshooting

### "Out of memory" in FULL mode
- **GPU:** Reduce `batch_size` in `NF_FULL` config
- **RAM:** Reduce `parallel_cores` in `TSCV_FULL` config
- **Disk:** Clear `outputs/` directory

### FULL mode too slow
- **Compromise:** Set `max_windows = 5` in `TSCV_FULL`
- **Partial:** Run subset of assets first
- **GPU:** Use CUDA-capable GPU (10x speedup)

### Results differ between modes
- **Expected:** Different assets/architecture = different results
- **Both valid:** OPTIMIZED for main findings, FULL for robustness
- **Report both:** Main text (optimized) + Appendix (full)

---

## Academic Rigour Notes

### Why Two Modes 

**OPTIMIZED Mode:**
- Sufficient for establishing main empirical findings
- Representative sample across asset classes
- Computational efficiency enables iteration
- Standard practice for dissertation main results

**FULL Mode:**
- Demonstrates robustness to asset selection
- Shows results hold across comprehensive dataset
- Deep architecture tests capacity limits
- Publication-ready comprehensive analysis

### Reporting:

**In Dissertation:**
- **Main Text:** Use OPTIMIZED results (clearly documented)
- **Appendix:** Include FULL results as robustness
- **Note:** "Results robust to full asset coverage (Appendix X)"

**Key Principle:**
> "Optimized configuration provides sufficient statistical power for main findings while enabling efficient research iteration. Full configuration validates robustness."

---

## Questions 

**Where is mode set **
→ `scripts/core/config.R`, line 30

**What changes automatically **
→ Assets, CV windows, NF architecture, training params

**Do I need to change anything else **
→ No! All scripts auto-adapt via helper functions

**Can I mix modes **
→ No, mode applies globally. Use one mode per run.

**Which mode for dissertation **
→ OPTIMIZED for main results, FULL for appendix

**Which mode for publication **
→ Both - show consistency across modes
