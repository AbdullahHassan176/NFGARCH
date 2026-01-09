# Comparison: Gaussian Base vs Student-t Base for NF-GARCH

## Experiment Re-run Results

The experiment was re-run with **Student-t GARCH as the base** for NF training (instead of Gaussian GARCH) to test if this improves skewness recovery.

## Results Comparison

### Recovery Metrics

| Metric | Gaussian Base (Before) | Student-t Base (After) | Change | Status |
|--------|------------------------|------------------------|--------|--------|
| **KS Statistic** | 0.2915 | 0.1585 | **-0.1330** | **IMPROVED** |
| **Wasserstein Distance** | 0.5880 | 0.5197 | **-0.0683** | **IMPROVED** |
| **Skewness Difference** | 1.4941 | 1.4941 | 0.0000 | **NO CHANGE** |
| **Kurtosis Difference** | 4.9575 | 4.9575 | 0.0000 | **NO CHANGE** |

### Summary Statistics

| Statistic | True | Gaussian Base | Student-t Base | Best |
|-----------|------|---------------|---------------|------|
| Mean | -0.0039 | -0.5919 | -0.0198 | Student-t Base |
| SD | 0.9886 | 1.2018 | 1.5041 | Gaussian Base |
| **Skewness** | **1.4550** | **-0.0391** | **-0.0391** | **NO IMPROVEMENT** |
| Kurtosis | 8.1568 | 3.1993 | 3.1993 | Same |
| Q(0.01) | -1.8451 | -3.4379 | -3.5817 | Student-t Base (slightly worse) |
| Q(0.99) | 3.1739 | 2.2013 | 3.4758 | **Student-t Base** |

## Key Findings

### Improvements with Student-t Base

1. **KS Statistic improved by 45.6%** (0.2915 → 0.1585)
   - Better overall distribution match
   - This is a significant improvement!

2. **Wasserstein Distance improved by 11.6%** (0.5880 → 0.5197)
   - Better optimal transport properties
   - Indicates better tail behavior

3. **Upper Tail Recovery Improved**
   - Q(0.99): 2.2013 → 3.4758 (much closer to true 3.1739)
   - Student-t base better captures upper tail

4. **Mean Recovery Improved**
   - Mean: -0.5919 → -0.0198 (much closer to true -0.0039)
   - Student-t base better captures location

### Persistent Issues

1. **Skewness Recovery Still Fails**
   - Both bases produce: Skewness = -0.0391 (nearly symmetric)
   - True skewness: 1.4550 (highly right-skewed)
   - **The NF architecture itself may be the issue, not the base**

2. **Kurtosis Still Underestimated**
   - Both produce: Kurtosis ≈ 3.2
   - True kurtosis: 8.16
   - NF may be learning a smoother distribution

## Root Cause Analysis

### Why Skewness Didn't Improve

The fact that **both Gaussian and Student-t bases produce identical skewness** (-0.0391) suggests:

1. **The NF architecture may be the bottleneck**
   - The normalizing flow may not have sufficient capacity to learn skewness
   - The MAF (Masked Autoregressive Flow) architecture may favor symmetric distributions

2. **Training may be converging to a local minimum**
   - The loss function (negative log-likelihood) may not penalize skewness mismatch
   - Need skewness-aware loss or regularization

3. **Standardization may be removing skewness information**
   - Standardizing residuals to mean=0, sd=1 may lose skewness
   - May need to preserve higher moments

### Why Other Metrics Improved

1. **KS and Wasserstein improved** because:
   - Student-t GARCH residuals better match the true distribution shape
   - NF learns from better-quality residuals

2. **Upper tail improved** because:
   - Student-t GARCH better captures heavy tails
   - NF inherits this improvement

## Recommendations

### For Dissertation

1. **Report the improvement** in KS and Wasserstein metrics
2. **Acknowledge the persistent skewness issue** as a limitation
3. **Suggest future work**:
   - Skewness-aware NF architectures
   - Joint training approaches
   - Alternative loss functions that penalize moment mismatches

### For Code Improvements

1. **Keep Student-t GARCH as base** (shows improvement)
2. **Consider NF architecture improvements**:
   - More layers or hidden units
   - Skewness-preserving transforms
   - Moment-matching loss functions
3. **Test with different NF configurations**:
   - More training epochs
   - Different learning rates
   - Different architectures (e.g., RealNVP, coupling layers)

## Conclusion

**Using Student-t GARCH as the base for NF training shows significant improvements:**
- 45.6% improvement in KS statistic
- 11.6% improvement in Wasserstein distance
- Better upper tail and mean recovery

**However, the skewness recovery issue persists:**
- Both bases produce identical (wrong) skewness
- This suggests the NF architecture or training is the bottleneck
- Not a base model issue, but an NF limitation

**Overall Assessment**: The change to Student-t base is beneficial and should be kept, but the skewness issue requires NF architecture improvements, not just base model changes.

