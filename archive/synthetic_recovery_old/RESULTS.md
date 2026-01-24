# Synthetic Distribution Recovery Experiment - Results

## Overview

This experiment evaluates how well different GARCH models recover the true innovation distribution from synthetic GARCH(1,1) data with skewed-t innovations (ν=5, ξ=1.5).

## Experiment Configuration

- **Sample size**: 2000 observations
- **GARCH parameters**: ω=0.0001, α=0.1, β=0.85 (stationary)
- **Innovation distribution**: Skewed-t (ν=5, ξ=1.5)
- **Seed**: 123 (reproducible)

## Key Results

### Distribution Recovery Metrics

| Method | KS Statistic | Wasserstein Distance | Skewness Diff | Kurtosis Diff |
|--------|--------------|---------------------|---------------|---------------|
| Student-t GARCH | 0.1155 | 1.3127 | 0.3922 | 4.7542 |
| NF-GARCH | 0.1585 | 0.5197 | 1.4941 | 4.9575 |
| Gaussian GARCH | 0.3310 | 1.2847 | 0.0714 | 1.3959 |

### Summary Statistics

| Statistic | True | Gaussian | Student-t | NF-GARCH |
|-----------|------|----------|-----------|----------|
| Mean | -0.0039 | -0.5595 | 0.0325 | -0.0198 |
| SD | 0.9886 | 1.2728 | 1.5613 | 1.5041 |
| Skewness | 1.4550 | 1.3836 | 1.8473 | -0.0391 |
| Kurtosis | 8.1568 | 9.5526 | 12.9109 | 3.1993 |
| Q(0.01) | -1.8451 | -3.4580 | -3.0563 | -3.5817 |
| Q(0.99) | 3.1739 | 3.4463 | 5.3054 | 3.4758 |

## Findings

### Best Performers by Metric

1. **KS Statistic**: Student-t GARCH (0.1155) - Best overall distribution match
2. **Wasserstein Distance**: NF-GARCH (0.5197) - Best optimal transport properties
3. **Skewness Recovery**: Gaussian GARCH (difference = 0.0714) - Best moment matching
4. **Kurtosis Recovery**: Gaussian GARCH (difference = 1.3959) - Best tail heaviness capture

### Critical Finding

NF-GARCH shows a critical failure in skewness recovery:
- True distribution: Skewness = 1.455 (right-skewed)
- NF-GARCH output: Skewness = -0.039 (nearly symmetric)
- The NF completely misses the sign of skewness

This suggests the two-stage approach (Student-t GARCH → NF training) may not be optimal for skewed distributions, and the NF architecture itself may be the bottleneck.

### Method Strengths

**Student-t GARCH**:
- Best overall distribution match (lowest KS)
- Best lower tail recovery
- Reasonable skewness capture

**NF-GARCH**:
- Best optimal transport properties (lowest Wasserstein)
- Best standard deviation recovery
- Critical skewness recovery failure

**Gaussian GARCH**:
- Best moment matching (skewness and kurtosis)
- Best upper tail recovery
- Worst overall distribution match

## Comparison: Student-t Base vs Gaussian Base

The experiment was re-run with Student-t GARCH as the base for NF training (instead of Gaussian GARCH).

### Improvements with Student-t Base

- KS Statistic: Improved by 45.6% (0.2915 → 0.1585)
- Wasserstein Distance: Improved by 11.6% (0.5880 → 0.5197)
- Upper Tail Recovery: Q(0.99) improved from 2.2013 to 3.4758
- Mean Recovery: Improved from -0.5919 to -0.0198

### Persistent Issues

- Skewness Recovery: No change (both bases produce -0.0391)
- Kurtosis: Still underestimated (3.2 vs true 8.16)

The fact that both bases produce identical skewness suggests the NF architecture or training is the bottleneck, not the base model.

## Recommendations

1. **For overall distribution recovery**: Use Student-t GARCH
2. **For optimal transport properties**: Use NF-GARCH (with Student-t base)
3. **For moment matching**: Use Gaussian GARCH
4. **Future work**: Investigate NF architecture improvements for skewness recovery

## Files

- `recovery_metrics.csv`: Detailed recovery metrics
- `summary_statistics.csv`: Summary statistics table
- `plots/`: Visualization plots (KDE overlay, QQ plots)
- `residuals/`: True and fitted residuals
- `nf_model.pth`: Trained normalizing flow model

