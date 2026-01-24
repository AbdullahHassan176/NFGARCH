# Reassessed Review: Based on Actual Outputs and Code

## Understanding Wilcoxon Tests and Win Rates

### What is a Wilcoxon Signed-Rank Test?

**Definition**: A non-parametric paired test that compares two related samples (NF-GARCH vs Standard GARCH on the same assets/models). It tests whether the median difference is significantly different from zero.

**Your Code** (line 293 in `compare_nf_vs_standard_garch.R`):
```r
wilcoxon_test <- wilcox.test(mse_nf, mse_standard, paired = TRUE, alternative = "less")
```

**Interpretation**:
- `paired = TRUE`: Compares NF vs Standard on the same assets/models (correct)
- `alternative = "less"`: Tests if NF MSE < Standard MSE (one-sided)
- **p=0.50**: Cannot reject null hypothesis that median difference = 0
- **This means**: No statistically significant evidence that NF-GARCH has lower MSE

**Is This Appropriate?**
- ✅ **YES** for comparing two methods on the same data
- ⚠️ **BUT**: For time series forecasts, **Diebold-Mariano (DM) test is more appropriate** because it accounts for forecast error serial correlation

### What is Win Rate?

**Definition**: Simple count of how many times NF-GARCH has lower MSE than Standard GARCH.

**Your Code** (lines 237-253):
```r
nf_better = nf_mse < std_mse
win_rate = mean(nf_better) * 100
```

**Interpretation**:
- 33% win rate = NF wins in 1/3 of comparisons
- This is **descriptive**, not **statistical**
- No account for magnitude of differences or statistical significance

**Is This Appropriate?**
- ✅ **YES** as a descriptive summary
- ❌ **NO** as the primary evidence (needs statistical test)
- ⚠️ **Problem**: 33% sounds like "failure" but could be meaningful if wins are large and losses are small

---

## Alternative Metrics Available in Your Codebase

### 1. Diebold-Mariano (DM) Test
**Location**: `archive/Manual Scripts/R - NFGARCH Main Training/4. NFGARCH - Train and Compare Forecasted Data using NFGARCH.R` (lines 751-781)

**Why Better for Forecasts**:
- Accounts for forecast error serial correlation
- Standard in forecast evaluation literature
- Tests if one forecast is significantly better than another

**Your Code**:
```r
dm.test(garch_vals, nf_vals, alternative = "greater")  # GARCH worse than NF
```

**Recommendation**: **USE THIS INSTEAD OF WILCOXON** for forecast comparisons. It's more appropriate for time series.

### 2. Distributional Metrics (Already Used)
- KS distance: ✅ Used
- Wasserstein distance: ✅ Used
- Tail index: ✅ Used
- **Missing**: Statistical tests for these (paired t-test or Wilcoxon on KS/Wasserstein)

### 3. Model Confidence Set (MCS)
**Not in your code**: Could be added to identify set of "best" models rather than binary win/loss

---

## Critical Findings from Your Outputs

### Finding 1: NF Skewness/Kurtosis Capture Issues
**Source**: `outputs/synthetic_recovery/AUDIT_FIXES.md` (lines 213-218)

**Key Evidence**:
- NF fails to capture skewness (diff = 1.94)
- NF fails to capture kurtosis (diff = 12.07)
- **Skewness sign match rate: 40%** (worse than 50% random)
- NF outputs near-normal (skew~0, kurt~3) despite training on skewed heavy-tailed data

**Implication for Dissertation**:
- This is a **MAJOR LIMITATION** that should be discussed
- Explains why distributional improvements are modest
- Suggests MAF architecture may have bias toward symmetry
- **This is NOT in your dissertation** - should be added to Limitations

### Finding 2: Scale Drift in NF Samples
**Source**: `outputs/synthetic_recovery/AUDIT_FIXES.md` (line 22)

**Key Evidence**:
- NF samples have SD≈1.56 (should be ~1.0 for standardized residuals)
- Student-t GARCH also shows SD≈1.55 (variance normalization issue)

**Implication**:
- May affect forecasting if scale is wrong
- Should be checked in main results
- **Not mentioned in dissertation**

### Finding 3: Win Rate Calculation Method
**Your Code Analysis**:
- Win rate calculated per Model-Asset pair (line 238-239)
- Takes first value if multiple (line 240-241)
- **Potential Issue**: If you have multiple CV folds per asset, only first is used

**Question**: Do you have multiple CV folds? If yes, win rate should aggregate across all folds, not just first.

---

## Reassessed Issues

### Issue 1: Wilcoxon p=0.50 - Is This Actually a Problem?

**Original Assessment**: "Critical contradiction - claims improvement but test shows no difference"

**Reassessed**:
- **p=0.50 is NOT a problem** - it's a valid finding: "no statistically significant difference"
- **The problem is LANGUAGE**: Saying "improves" when test shows "no significant difference"
- **Solution**: Change language to "directionally positive but not statistically significant"

**Alternative**: Use Diebold-Mariano test instead (more appropriate for forecasts)

### Issue 2: Win Rate 33% - Is This Actually a Problem?

**Original Assessment**: "Majority failure - loses 67% of the time"

**Reassessed**:
- **33% win rate is descriptive, not statistical**
- **Missing**: Magnitude of wins vs losses
- **Missing**: Statistical significance of individual wins
- **Better approach**: Report win rate WITH magnitude (e.g., "NF wins 33% of cases, with average improvement of X% when it wins, and average loss of Y% when it loses")

**Question**: Can you calculate magnitude-weighted win rate? This would be more informative.

### Issue 3: Are Distributional Improvements Real?

**Original Assessment**: "No statistical tests - unsubstantiated claims"

**Reassessed**:
- **YES, tests are missing** - should add paired tests on KS/Wasserstein
- **BUT**: Your synthetic recovery experiment shows NF has trouble capturing skewness/kurtosis
- **This suggests**: Distributional improvements may be limited to certain aspects (e.g., overall shape via Wasserstein) but not higher moments

**Recommendation**: 
1. Add statistical tests for KS/Wasserstein
2. Acknowledge NF's skewness/kurtosis limitations from synthetic experiment
3. Discuss why Wasserstein improves but skewness/kurtosis don't

### Issue 4: Missing Results

**What Should Be Included But Isn't**:

1. **Synthetic Recovery Experiment Results**:
   - NF skewness/kurtosis capture failure (40% sign match)
   - NF scale drift (SD≈1.56)
   - These are validation experiments that inform main results

2. **Diebold-Mariano Test Results**:
   - More appropriate than Wilcoxon for forecasts
   - Already coded in archive, just needs to be run on main data

3. **Magnitude-Weighted Win Rates**:
   - Not just "wins 33%" but "wins 33% with average improvement of X%"
   - Would show if wins are large and losses are small

4. **Distributional Metric Statistical Tests**:
   - Paired t-test or Wilcoxon on KS/Wasserstein distances
   - Currently only descriptive statistics reported

---

## Revised Recommendations

### Priority 1: Language Alignment (Still Critical)

**Problem**: Claims "improves" when tests show "no significant difference"

**Fix**: 
- Abstract: "shows directionally positive but non-significant improvements (Wilcoxon p=0.50)"
- Results: "NF-GARCH achieves lower MSE in 33% of cases, though differences are not statistically significant"
- Conclusion: Align with evidence

### Priority 2: Add Diebold-Mariano Test (Better Than Wilcoxon)

**Why**:
- More appropriate for forecast comparison
- Accounts for serial correlation
- Standard in forecast evaluation literature

**Action**:
- Run DM test on your forecast errors
- Report DM test results alongside (or instead of) Wilcoxon
- If DM also shows non-significance, this strengthens your finding

### Priority 3: Acknowledge NF Limitations from Synthetic Experiment

**What to Add**:
- In Limitations section: "Synthetic recovery experiments indicate that NF struggles to capture skewness (40% sign match rate) and kurtosis, despite improving overall distributional shape (Wasserstein distance). This suggests that distributional improvements are primarily in overall shape rather than higher moments."

**Why Important**:
- Shows you've validated the method
- Explains why improvements are modest
- Demonstrates scientific rigor

### Priority 4: Enhance Win Rate Reporting

**Current**: "33% win rate"

**Better**:
- "NF-GARCH achieves lower MSE in 33% of comparisons (6 of 18 model-asset pairs)"
- "When NF-GARCH wins, average improvement is X%; when it loses, average deterioration is Y%"
- "Win rate is higher for equities (X%) than FX (Y%)"

### Priority 5: Add Statistical Tests for Distributional Metrics

**Current**: Only descriptive statistics (mean KS, mean Wasserstein)

**Add**:
- Paired t-test or Wilcoxon on KS distances
- Report p-values
- If significant, strengthens distributional claim
- If not significant, acknowledge limitation

---

## Can You Skip Wilcoxon/Win Rates?

### Option 1: Replace with Diebold-Mariano
- ✅ More appropriate for forecasts
- ✅ Standard in literature
- ✅ Accounts for serial correlation
- **Action**: Run DM test, report instead of (or alongside) Wilcoxon

### Option 2: Keep Both but Reframe
- ✅ Wilcoxon: Tests if median difference ≠ 0 (general comparison)
- ✅ DM: Tests if forecast accuracy differs (forecast-specific)
- **Action**: Report both, explain why both are used

### Option 3: Focus on Distributional Metrics
- ✅ Your main contribution is distributional realism, not forecasting
- ✅ Emphasize KS/Wasserstein improvements (with tests)
- ✅ De-emphasize forecasting (acknowledge modest/non-significant)
- **Action**: Reframe thesis as "distributional realism" rather than "forecasting improvement"

---

## What Your Results Actually Show (Reassessed)

### Forecasting
- **Directionally positive** but **not statistically significant** (Wilcoxon p=0.50)
- **Context-dependent**: Better for equities, worse for FX
- **33% win rate**: Descriptive summary, needs magnitude weighting

### Distributional Realism
- **Wasserstein distance improves**: Likely real (but needs statistical test)
- **KS distance improves**: Likely real (but needs statistical test)
- **Skewness/kurtosis**: NF struggles (from synthetic experiment - not in dissertation)

### Risk Calibration
- **Both models pass VaR tests**: Comparable performance
- **No significant difference**: Correctly stated

---

## Final Verdict on Original Issues

### Red Flag 1: Statistical Significance Contradiction
- **Status**: **REAL ISSUE** but less severe than originally thought
- **Reason**: p=0.50 is a valid finding ("no significant difference"), not a contradiction
- **Fix**: Language alignment, not test replacement
- **Alternative**: Add DM test for robustness

### Red Flag 2: Win Rate 33%
- **Status**: **PARTIALLY REAL ISSUE**
- **Reason**: 33% is descriptive, not statistical. Need magnitude weighting.
- **Fix**: Enhance reporting with magnitude of wins/losses
- **Alternative**: Could skip if focusing on distributional metrics

### Red Flag 3: Abstract Overclaims
- **Status**: **REAL ISSUE** - unchanged
- **Fix**: Align language with evidence

### Red Flag 4: Distributional Improvements Without Tests
- **Status**: **REAL ISSUE** - unchanged
- **Fix**: Add statistical tests

### Red Flag 5: Missing NF Limitations
- **Status**: **REAL ISSUE** - actually worse than originally thought
- **Reason**: Synthetic experiment shows major skewness/kurtosis limitations
- **Fix**: Add to Limitations section

---

## Recommended Action Plan

1. **Immediate (Text fixes - 30 min)**:
   - Fix Abstract language
   - Fix Conclusion language
   - Add win rate magnitude (if available)
   - Add NF limitations from synthetic experiment

2. **Short-term (Analysis - 2-3 hours)**:
   - Run Diebold-Mariano test on forecast errors
   - Run statistical tests on KS/Wasserstein distances
   - Calculate magnitude-weighted win rates

3. **Medium-term (If time permits)**:
   - Add synthetic experiment results to Limitations
   - Discuss why NF improves Wasserstein but not skewness/kurtosis
   - Consider reframing thesis around distributional realism rather than forecasting

---

## Key Insight: Your Thesis Might Be Stronger Than It Appears

**If you reframe**:
- **Current framing**: "NF-GARCH improves forecasting" (weak evidence)
- **Better framing**: "NF-GARCH improves distributional realism while maintaining comparable forecasting" (strong evidence)

**Why this works**:
- Distributional improvements are more consistent
- Forecasting improvements are modest but directionally positive
- Risk calibration is maintained
- This is still a valuable contribution

**The problem**: Your Abstract/Conclusion emphasize forecasting, but your strongest evidence is distributional.
