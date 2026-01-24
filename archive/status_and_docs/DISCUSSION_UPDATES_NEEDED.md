# Discussion Chapter Updates Needed

Based on the new empirical comparison results, the following updates are needed in your Discussion chapter:

## 1. Section: "Distributional Realism and the Behaviour of Residuals"

### Current Issues:
- Says "NF-GARCH residuals align more closely with empirical quantiles" but doesn't specify this is compared to **empirical test set residuals** (the stronger evidence)
- Doesn't mention the skewness limitation
- Doesn't reference the new empirical comparison results

### Required Updates:

**Replace the first paragraph** with the updated version in `updated_discussion_distributional.tex` which:
- Clarifies that improvements are measured against **empirical test set residuals** (stronger evidence)
- Cites the 66.7% win rate on Wasserstein distance
- Mentions the 10.2% reduction in average Wasserstein distance
- Notes that improvements are particularly strong for equity assets

**Add a new paragraph** (after the unconditional distribution limitation) that:
- Explicitly discusses NF's skewness limitation
- References Table 1 showing near-zero skewness (0.004-0.018) vs Standard GARCH (-1.418 to 0.458)
- Notes that while NF achieves 66.7% win rate in skewness matching, the improvement is modest
- Acknowledges NF's weakness in kurtosis matching (44.4% win rate)

**Key points to include:**
1. NF's strength: Superior Wasserstein distance (66.7% win rate, 10.2% reduction)
2. NF's weakness: Near-zero skewness in generated innovations despite training on skewed data
3. Modest improvement: 66.7% win rate in skewness matching but still weak overall
4. Kurtosis limitation: Standard GARCH better (55.6% win rate for Standard)

## 2. Section: "Key Limitations"

### Add New Limitation:

Add a new limitation paragraph (after the unconditional distribution limitation) using the text in `updated_limitations_skewness.tex` that:
- Explicitly states NF's inability to capture skewness
- Provides evidence: near-zero skewness (0.004-0.018) vs training data (-1.418 to 0.458)
- Notes the modest improvement in empirical comparison (66.7% win rate but still weak)
- Suggests this may limit effectiveness for assets with pronounced asymmetry

## 3. Section: "Conclusion"

### Minor Update:

The conclusion mentions "distributional realism" but could be more specific:
- Could reference the 66.7% win rate on Wasserstein distance
- Could acknowledge the skewness limitation as a constraint

## Summary of Changes:

1. **Distributional Realism Section**: 
   - Update to reference empirical test set residuals (stronger evidence)
   - Add explicit discussion of skewness limitation
   - Add discussion of kurtosis limitation

2. **Limitations Section**:
   - Add new limitation paragraph on skewness capture

3. **Conclusion**:
   - Minor update to be more specific about distributional improvements and limitations

## Files Created:

- `updated_discussion_distributional.tex`: Complete updated "Distributional Realism" section
- `updated_limitations_skewness.tex`: New limitation paragraph on skewness

These can be directly inserted into your Discussion chapter.
