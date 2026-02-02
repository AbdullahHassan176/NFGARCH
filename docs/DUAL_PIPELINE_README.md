# Dual Pipeline System for NF-GARCH Validation

## Overview

This implementation provides **two independent validation pipelines** for robust NF-GARCH evaluation:

1. **Chronological Split Pipeline** (`run_chronological.bat`)
   - Pure 65/35 chronological split
   - Single train/test division
   - ~2 hours execution time

2. **Time-Series Cross-Validation Pipeline** (`run_tscv.bat`)
   - Rolling window TS CV with 5 folds
   - Multiple temporal windows
   - ~6-8 hours execution time

3. **Combined Execution** (`run_both_pipelines.bat`)
   - Runs both pipelines sequentially
   - Generates comparative analysis
   - ~8-10 hours total execution time

## Why Two Pipelines?

### Academic Benefits
- **Robustness Validation**: Results consistent across methods = strong evidence
- **Temporal Stability**: TS CV reveals performance across market regimes
- **Publication Quality**: Addresses reviewer concerns about overfitting
- **Methodological Rigor**: Demonstrates thorough validation strategy

### Research Contributions
1. Compare validation strategies for volatility models
2. Assess temporal stability of NF-GARCH
3. Identify when chronological splits are sufficient
4. Provide recommendations for practitioners

## Directory Structure

```
outputs/
  chronological/           # Chronological split outputs
    garch_fitting/
    residuals_by_model/
    nf_models/
    evaluation/
  
  tscv/                    # TS CV outputs
    garch_fitting/
      window_1/
      window_2/
      ...
    residuals_by_model/
      window_1/
      window_2/
      ...
    nf_models/
      window_1/
      window_2/
      ...
    evaluation/

results/
  chronological/           # Chronological results
    consolidated/
    dissertation_tables/
    figures/
    diagnostics/
  
  tscv/                    # TS CV results
    consolidated/
    dissertation_tables/
    figures/
    diagnostics/
  
  comparison/              # Meta-analysis
    Chronological_vs_TSCV_Analysis.xlsx
```

## Data Flow

### Chronological Pipeline
```
Raw Data (100%)
    ↓
65/35 Split (train/test)
    ↓
GARCH Fit (65% only)
    ↓
Extract Residuals (65%)
    ↓
Train NF (100% of residuals, NO validation)
    ↓
Simulate (test on 35%)
    ↓
Evaluate
```

**Key Feature**: NO validation split in NF training to avoid data leakage

### TS CV Pipeline
```
Raw Data (100%)
    ↓
Calculate CV Windows (5 folds)
    ↓
For Each Window:
    ├─ GARCH Fit (window training set)
    ├─ Extract Residuals (window)
    ├─ Train NF (window residuals)
    └─ Simulate (window test set)
    ↓
Aggregate Results
    ↓
Evaluate
```

**Key Feature**: Each window is independent with its own NF model

## Usage

### Option 1: Run Chronological Only
```batch
run_chronological.bat
```
- Fast validation (~2 hours)
- Good for iterative development
- Single point estimate

### Option 2: Run TS CV Only
```batch
run_tscv.bat
```
- Comprehensive temporal analysis (~6-8 hours)
- Multiple performance estimates
- Robustness assessment

### Option 3: Run Both (Recommended for Dissertation)
```batch
run_both_pipelines.bat
```
- Complete validation (~8-10 hours)
- Publication-ready results
- Comparative analysis included

## Configuration Files

### Split-Specific Configs
- `scripts/config/chrono_split_config.R` - Chronological configuration
- `scripts/config/tscv_split_config.R` - TS CV configuration

### Key Parameters
```r
# Chronological
TRAIN_RATIO <- 0.65
TEST_RATIO <- 0.35
NF_VALIDATION_SPLIT <- 0.0  # No validation

# TS CV
TSCV_CONFIG <- list(
  n_folds = 5,
  window_size = 0.65,
  step_size = 0.1,
  max_windows = NULL
)
```

## Pipeline Scripts

### Chronological Pipeline
1. `scripts/chronological/fit_garch_chronological.R` - GARCH fitting
2. `scripts/chronological/train_nf_chronological.py` - NF training
3. `scripts/simulation_forecasting/simulate_nf_garch_engine.R --split chronological`
4. `scripts/evaluation/*` (with `--split chronological`)

### TS CV Pipeline
1. `scripts/tscv/fit_garch_tscv.R` - GARCH fitting (per window)
2. `scripts/tscv/train_nf_tscv.py` - NF training (per window)
3. `scripts/simulation_forecasting/simulate_nf_garch_engine.R --split tscv`
4. `scripts/evaluation/*` (with `--split tscv`)

## Validation Checklist

### Chronological Pipeline
- [ ] GARCH uses only first 65% of data
- [ ] Residuals extracted from 65% training set only
- [ ] NF trains on 100% of training residuals (no validation split)
- [ ] Simulations evaluate on 35% test set
- [ ] No test data leakage

### TS CV Pipeline
- [ ] Each window maintains proper train/test split
- [ ] No overlap violates temporal ordering
- [ ] Residuals per window are independent
- [ ] NF models are window-specific
- [ ] Aggregated results represent all windows

### Comparison Analysis
- [ ] Both pipelines completed successfully
- [ ] Results loaded and compared
- [ ] Temporal stability analyzed
- [ ] Method consistency evaluated
- [ ] Winners identified and compared

## Expected Results

### If Both Methods Agree
- **Strong validation** of NF-GARCH approach
- Emphasize robustness in dissertation
- Recommend model for practical use
- Claim generalizability across time periods

### If Methods Disagree
- **Important finding** about temporal instability
- Discuss regime-dependent performance
- Investigate market conditions
- Provide nuanced recommendations

### Temporal Stability Metrics
- Low CV (<0.1): High stability
- Medium CV (0.1-0.3): Moderate stability
- High CV (>0.3): Significant temporal variation

## Troubleshooting

### Chronological Pipeline Fails
1. Check data file exists: `data/processed/raw (FX + EQ).csv`
2. Verify R packages installed
3. Check Python dependencies (torch, nflows)
4. Review logs in `outputs/chronological/`

### TS CV Pipeline Fails
1. Ensure chronological pipeline works first
2. Check available memory (TS CV is memory-intensive)
3. Consider reducing `max_windows` in config
4. Review per-window logs

### Comparison Script Fails
1. Verify both pipelines completed
2. Check result files exist:
   - `results/chronological/consolidated/NF_GARCH_Results_chronological.xlsx`
   - `results/tscv/consolidated/NF_GARCH_Results_tscv.xlsx`
3. Review comparison script output

## Dissertation Integration

### Chapter 3: Methodology
**Section 3.5: Validation Strategy**
- Describe dual validation approach
- Justify both methods
- Explain data split rationale

### Chapter 4: Results
**Section 4.6: Validation Strategy Comparison**
- Present results from both pipelines
- Compare performance metrics
- Analyze temporal stability

### Chapter 5: Discussion
**Section 5.4: Model Generalization**
- Discuss implications of findings
- Address temporal stability
- Provide practical recommendations

## Performance Estimates

### Chronological Pipeline
- GARCH Fitting: ~30 minutes
- NF Training: ~20 minutes
- Simulation: ~15 minutes
- Evaluation: ~30 minutes
- **Total: ~2 hours**

### TS CV Pipeline  
- GARCH Fitting: ~2 hours (5 windows)
- NF Training: ~1.5 hours (5 windows)
- Simulation: ~1 hour
- Evaluation: ~1.5 hours
- **Total: ~6-8 hours**

## Citation

If you use this dual pipeline approach in your research, please cite:

```
[Your dissertation citation]
Title: [Your title]
Method: Dual validation using chronological split and time-series cross-validation
Implementation: Independent pipelines with consistent data splitting
```

## Support

For issues or questions:
1. Check this README
2. Review plan file: `.cursor/plans/dual_pipeline_setup_*.plan.md`
3. Examine configuration files in `scripts/config/`
4. Review batch file comments

## Version History

- v1.0 (2026-02-02): Initial dual pipeline implementation
  - Chronological split pipeline
  - TS CV pipeline
  - Comparative analysis
  - Complete documentation

---

**Remember**: The dual pipeline approach significantly strengthens your dissertation by demonstrating methodological rigor and providing robust validation of your NF-GARCH methodology.
