# Synthetic Distribution Recovery Experiment - Detailed Analysis

## Executive Summary

This experiment evaluated how well three competing GARCH approaches recover the **true innovation distribution** from synthetic GARCH(1,1) data with **skewed-t innovations** (ν=5, ξ=1.5).

### Key Findings

1. **Student-t GARCH performs best overall** in terms of KS statistic (0.1155), indicating the best overall distribution match
2. **NF-GARCH performs best** in terms of Wasserstein distance (0.5880), indicating better optimal transport properties
3. **Gaussian GARCH performs best** in terms of moment recovery (skewness and kurtosis differences)
4. **NF-GARCH shows concerning skewness recovery** - completely misses the sign of skewness

## Detailed Metric Analysis

### 1. Kolmogorov-Smirnov (KS) Statistic

**Best: Student-t GARCH (0.1155)**
- Measures maximum difference between CDFs
- Student-t GARCH achieves the best overall distribution match
- NF-GARCH (0.2915) is intermediate
- Gaussian GARCH (0.3310) performs worst

**Interpretation**: Student-t GARCH best captures the overall shape of the true distribution.

### 2. Wasserstein Distance

**Best: NF-GARCH (0.5880)**
- Measures optimal transport cost between distributions
- NF-GARCH significantly outperforms both parametric models
- Gaussian GARCH (1.2847) and Student-t GARCH (1.3127) are similar

**Interpretation**: NF-GARCH better captures the "transport" properties of the distribution, suggesting better tail behavior recovery.

### 3. Skewness Recovery

**Best: Gaussian GARCH (difference = 0.0714)**
- True skewness: 1.455
- Gaussian GARCH: 1.384 (difference: 0.071) ✓
- Student-t GARCH: 1.847 (difference: 0.392)
- **NF-GARCH: -0.039 (difference: 1.494)** ✗ **MAJOR ISSUE**

**Critical Finding**: NF-GARCH completely misses the sign of skewness! The true distribution is right-skewed (1.455), but NF-GARCH produces a nearly symmetric distribution (-0.039).

**Possible Causes**:
- NF training may be overfitting to the Gaussian GARCH residuals (which are already misspecified)
- The two-stage approach may propagate errors from the base GARCH fit
- NF architecture may need more capacity or different training

### 4. Kurtosis Recovery

**Best: Gaussian GARCH (difference = 1.396)**
- True kurtosis: 8.157
- Gaussian GARCH: 9.553 (difference: 1.396) ✓
- NF-GARCH: 3.199 (difference: 4.958) ✗ **Underestimates**
- Student-t GARCH: 12.911 (difference: 4.754) ✗ **Overestimates**

**Finding**: Gaussian GARCH best captures tail heaviness, though all methods struggle with the high kurtosis (8.16).

### 5. Tail Quantile Recovery

**Lower Tail (Q0.01)**:
- True: -1.845
- Gaussian GARCH: -3.458 (difference: 1.613) ✗
- Student-t GARCH: -3.056 (difference: 1.211) ✓ Best
- NF-GARCH: -3.438 (difference: 1.593) ✗

**Upper Tail (Q0.99)**:
- True: 3.174
- Gaussian GARCH: 3.446 (difference: 0.272) ✓ Best
- NF-GARCH: 2.201 (difference: 0.973) ✗
- Student-t GARCH: 5.305 (difference: 2.131) ✗

**Finding**: Mixed results - Student-t best for lower tail, Gaussian best for upper tail.

## Summary Statistics Comparison

| Statistic | True | Gaussian | Student-t | NF-GARCH | Best |
|-----------|------|----------|-----------|----------|------|
| Mean | -0.004 | -0.560 | 0.033 | -0.592 | Student-t ✓ |
| SD | 0.989 | 1.273 | 1.561 | 1.202 | NF-GARCH ✓ |
| Skewness | 1.455 | 1.384 | 1.847 | -0.039 | Gaussian ✓ |
| Kurtosis | 8.157 | 9.553 | 12.911 | 3.199 | NF-GARCH ✓ |
| Q(0.01) | -1.845 | -3.458 | -3.056 | -3.438 | Student-t ✓ |
| Q(0.99) | 3.174 | 3.446 | 5.305 | 2.201 | Gaussian ✓ |

## Method Rankings

### Overall Performance (Weighted Average)

1. **Student-t GARCH**: Best KS statistic, good lower tail, reasonable skewness
2. **Gaussian GARCH**: Best moment recovery, good upper tail, but poor overall distribution match
3. **NF-GARCH**: Best Wasserstein distance, but **critical failure in skewness recovery**

## Critical Issues Identified

### 1. NF-GARCH Skewness Failure

**Problem**: NF-GARCH produces a symmetric distribution when the true distribution is highly skewed (skewness = 1.455).

**Root Cause Analysis**:
- The two-stage approach fits Gaussian GARCH first, which may not capture skewness well
- NF training on misspecified residuals may learn the wrong distribution
- The NF architecture may need skewness-aware components

**Recommendations**:
- Consider joint training of GARCH + NF (not just two-stage)
- Use Student-t GARCH as base instead of Gaussian GARCH
- Add skewness-aware transforms to the NF architecture
- Increase NF capacity or training epochs

### 2. Kurtosis Underestimation

**Problem**: NF-GARCH severely underestimates kurtosis (3.20 vs true 8.16).

**Possible Causes**:
- NF may be learning a smoother distribution than the true heavy-tailed one
- Training on standardized residuals may lose tail information

**Recommendations**:
- Use longer training or more complex NF architecture
- Consider tail-focused loss functions

## Strengths of Each Method

### Gaussian GARCH
- ✓ Best moment recovery (skewness, kurtosis)
- ✓ Best upper tail quantile recovery
- ✗ Worst overall distribution match (KS statistic)
- ✗ Poor lower tail recovery

### Student-t GARCH
- ✓ Best overall distribution match (KS statistic)
- ✓ Best lower tail recovery
- ✓ Reasonable skewness capture
- ✗ Overestimates kurtosis
- ✗ Poor upper tail recovery

### NF-GARCH
- ✓ Best Wasserstein distance (optimal transport)
- ✓ Best standard deviation recovery
- ✓ Best kurtosis recovery (though still underestimates)
- ✗ **Critical: Completely misses skewness sign**
- ✗ Poor tail quantile recovery

## Conclusions

1. **For overall distribution recovery**: Student-t GARCH performs best (lowest KS statistic)

2. **For optimal transport properties**: NF-GARCH performs best (lowest Wasserstein distance)

3. **For moment matching**: Gaussian GARCH performs best (skewness and kurtosis)

4. **Critical finding**: NF-GARCH's two-stage approach has a **fundamental flaw** in skewness recovery, producing symmetric distributions when the true distribution is skewed.

5. **Recommendation**: The two-stage NF-GARCH approach needs improvement:
   - Use Student-t GARCH as base instead of Gaussian
   - Consider joint training
   - Add skewness-aware components to NF architecture

## Next Steps

1. **Re-run with Student-t GARCH as base** for NF training (instead of Gaussian)
2. **Investigate NF architecture** - may need more layers or different transforms
3. **Consider joint training** approach (if available in repo)
4. **Test with different innovation distributions** to see if issue persists
5. **Analyze QQ plots** to identify specific regions where NF fails

## Files for Further Investigation

- `plots/kde_overlay.png`: Visual comparison of all distributions
- `plots/qq_nf.png`: QQ plot showing where NF deviates from true
- `residuals/z_hat_gaussian.csv`: Base residuals used for NF training
- `nf_model.pth`: Trained NF model (can be inspected)

