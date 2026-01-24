# Comparison to Empirical Test Set Residuals

## What This Does

This new analysis provides **STRONGER evidence** for distributional realism by comparing model-generated residuals to **actual empirical test set residuals** (what actually happened in the test data), rather than just comparing NF-generated residuals to Standard GARCH residuals.

## Why This Is Stronger Evidence

### Previous Comparison (Weak Evidence)
- **Compared**: NF-generated residuals vs Standard GARCH residuals (both from training set)
- **Shows**: Whether NF can replicate Standard GARCH residual distributions
- **Problem**: Doesn't show if either model matches **real empirical data**
- **Interpretation**: Lower KS/Wasserstein = NF replicates Standard GARCH, but doesn't prove improvement over empirical reality

### New Comparison (Strong Evidence)
- **Compared**: 
  1. NF-generated residuals vs **Empirical test set residuals**
  2. Standard GARCH residuals vs **Empirical test set residuals**
- **Shows**: Which model better matches what **actually happened** in the test set
- **Benefit**: Directly tests distributional realism against empirical data
- **Interpretation**: Lower KS/Wasserstein = Better match to empirical test data

## How It Works

1. **Train/Test Split**: 65% training, 35% test (chronological)
2. **Fit GARCH**: Fit model on training set
3. **Forecast Volatility**: Get volatility forecasts for test set
4. **Calculate Empirical Test Residuals**: 
   ```
   empirical_test_residual = (test_return - forecast_mean) / forecast_sigma
   ```
   This is what **actually happened** in the test set, standardized by the model's volatility forecast.

5. **Compare**:
   - Standard GARCH residuals (from training) vs Empirical test residuals
   - NF-generated residuals vs Empirical test residuals
6. **Metrics**: KS distance, Wasserstein distance, Skewness difference, Kurtosis difference

## What Results Mean

### If NF Wins (Lower Distance to Empirical)
- **KS/Wasserstein**: NF-generated residuals better match empirical test data
- **Skewness/Kurtosis**: NF better captures empirical distribution shape
- **Interpretation**: NF improves distributional realism over Standard GARCH

### If Standard Wins
- Standard GARCH residuals already match empirical test data well
- NF doesn't improve (or may worsen) distributional realism
- **Interpretation**: NF may be overfitting to training set, not generalizing to test set

## Expected Outcomes

Based on your synthetic recovery experiment:
- **Wasserstein**: NF may win (NF showed better Wasserstein in synthetic experiment)
- **KS**: Standard may win (Student-t GARCH had better KS in synthetic experiment)
- **Skewness**: Standard likely wins (NF failed to capture skewness in synthetic experiment)
- **Kurtosis**: Standard likely wins (NF failed to capture kurtosis in synthetic experiment)

## How to Use Results

1. **If NF wins on multiple metrics**: Strong evidence for distributional improvement
2. **If mixed results**: Report nuanced findings (NF improves some aspects, not others)
3. **If Standard wins**: Acknowledge limitations, focus on methodological contribution

## Files Generated

- `outputs/evaluation/comparison_to_empirical_test_residuals.xlsx`
  - Sheet 1: Full results (all model-asset combinations)
  - Sheet 2: Summary by model
  - Sheet 3: Summary by asset

## Running the Analysis

```bash
# Windows
run_empirical_comparison.bat

# Or directly
Rscript scripts/evaluation/compare_to_empirical_test_residuals.R
```

## Integration with Dissertation

This analysis can:
1. **Replace or supplement** the current distributional metrics section
2. **Strengthen** the "distributional realism" claim with direct empirical evidence
3. **Address reviewer concerns** about whether improvements are real or just replication
4. **Provide clearer interpretation**: "NF better matches empirical test data" vs "NF replicates Standard GARCH"

## Next Steps

After running:
1. Review win rates and average distances
2. Check if results align with synthetic recovery experiment
3. If NF wins, this strengthens your distributional realism claim
4. If Standard wins, reframe as methodological contribution with limitations
5. Update dissertation with these stronger results
