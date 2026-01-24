# Direct Answers to Your Questions

## 1. What Even Is Wilcoxon Test?

**Answer**: A non-parametric paired test comparing NF-GARCH vs Standard GARCH on the same assets/models.

**What p=0.50 Means**:
- Cannot reject null hypothesis that median difference = 0
- This means: **No statistically significant evidence** that NF-GARCH has lower MSE
- **This is a VALID finding**, not an error
- **The problem**: Your language says "improves" but test says "no significant difference"

**Is It Appropriate?**
- ✅ Yes for comparing two methods on same data
- ⚠️ But **Diebold-Mariano (DM) test is MORE appropriate** for forecast comparison (accounts for serial correlation)

**Your Code Shows**: DM test exists in archive but not used in main analysis

---

## 2. What Even Is Win Rate?

**Answer**: Simple count of how many times NF-GARCH has lower MSE than Standard GARCH.

**33% Win Rate Means**:
- NF wins in 1/3 of comparisons (6 of 18 model-asset pairs)
- NF loses in 2/3 of comparisons
- **This is descriptive, not statistical**

**Is It Useful?**
- ✅ Yes as a summary statistic
- ❌ No as primary evidence (needs statistical test)
- ⚠️ **Missing**: Magnitude of wins vs losses (are wins large and losses small?)

**Your Code**: Calculates win rate per Model-Asset pair, but may only use first value if multiple CV folds exist

---

## 3. Do I Even Need This?

### Option A: Keep But Improve
- ✅ Keep Wilcoxon but add Diebold-Mariano (more appropriate for forecasts)
- ✅ Keep win rate but add magnitude weighting
- ✅ Add statistical tests for distributional metrics

### Option B: Replace Wilcoxon with DM
- ✅ DM test is more appropriate for forecasts
- ✅ Already coded in your archive
- ✅ Standard in forecast evaluation literature

### Option C: De-emphasize Forecasting, Emphasize Distributional
- ✅ Your strongest evidence is distributional realism (Wasserstein, KS improvements)
- ✅ Forecasting improvements are modest/non-significant
- ✅ Reframe thesis: "NF-GARCH improves distributional realism while maintaining comparable forecasting"

**Recommendation**: **Option A or C** - Keep tests but improve reporting, OR reframe thesis

---

## 4. Can I Use Alternative Metrics?

**YES - You Already Have Better Metrics Available:**

### Diebold-Mariano Test (Better Than Wilcoxon)
- **Location**: `archive/Manual Scripts/R - NFGARCH Main Training/4. NFGARCH - Train and Compare Forecasted Data using NFGARCH.R`
- **Why Better**: Accounts for forecast error serial correlation
- **Action**: Run on your main forecast errors, report instead of (or alongside) Wilcoxon

### Magnitude-Weighted Win Rate (Better Than Simple Win Rate)
- **Current**: "33% win rate"
- **Better**: "33% win rate; when NF wins, average improvement is X%; when it loses, average deterioration is Y%"
- **Action**: Calculate from your MSE differences

### Statistical Tests for Distributional Metrics
- **Current**: Only descriptive (mean KS, mean Wasserstein)
- **Better**: Paired t-test or Wilcoxon on KS/Wasserstein distances
- **Action**: Run tests, report p-values

---

## 5. What Results Should Have Been Included But Weren't?

### Missing Result 1: Synthetic Recovery Experiment Findings
**Source**: `outputs/synthetic_recovery/AUDIT_FIXES.md`

**Key Findings NOT in Dissertation**:
- NF fails to capture skewness (40% sign match rate, worse than random)
- NF fails to capture kurtosis (diff = 12.07)
- NF outputs near-normal (skew~0, kurt~3) despite training on skewed data
- NF scale drift (SD≈1.56 instead of ~1.0)

**Why Important**: 
- Validates your method
- Explains why distributional improvements are modest
- Shows NF limitations

**Where to Add**: Limitations section (Section 6.2)

### Missing Result 2: Diebold-Mariano Test
**Source**: Archive code shows DM test exists but not used

**Why Important**:
- More appropriate than Wilcoxon for forecasts
- Standard in forecast evaluation literature

**Where to Add**: Results Section 4.3 (alongside or instead of Wilcoxon)

### Missing Result 3: Statistical Tests for Distributional Metrics
**Current**: Only descriptive statistics

**Missing**: 
- Paired t-test or Wilcoxon on KS distances
- Paired t-test or Wilcoxon on Wasserstein distances
- P-values to confirm if improvements are significant

**Where to Add**: Results Section 4.4 (after Table 4.6, 4.7)

### Missing Result 4: Magnitude-Weighted Win Rates
**Current**: "33% win rate"

**Missing**:
- Average improvement when NF wins
- Average deterioration when NF loses
- Breakdown by asset class (equity vs FX)

**Where to Add**: Results Section 4.3 (after Table 4.6)

---

## 6. Is Anything Explained Incorrectly?

### Issue 1: "Improves" Language
**Location**: Abstract, Results, Discussion, Conclusion

**Problem**: Says "improves" but Wilcoxon p=0.50 shows "no significant difference"

**Fix**: Change to "shows directionally positive but non-significant improvements"

### Issue 2: Win Rate Interpretation
**Location**: Results Section 4.3

**Problem**: "33% win rate" presented neutrally, but means NF loses 67% of the time

**Fix**: Add interpretation: "indicating context-dependent benefits rather than universal superiority"

### Issue 3: Distributional Improvements
**Location**: Results Section 4.4

**Problem**: Claims "lower KS/Wasserstein" without statistical tests

**Fix**: Add paired tests, report p-values

### Issue 4: NF Limitations
**Location**: Limitations Section 6.2

**Problem**: Doesn't mention NF's skewness/kurtosis capture failure from synthetic experiment

**Fix**: Add: "Synthetic recovery experiments indicate NF struggles to capture skewness (40% sign match) and kurtosis, despite improving overall distributional shape"

---

## 7. Reassessed View of Issues

### Original Assessment: "Critical Issues"
**Reassessed**: Some are less severe, some are more severe

### Less Severe Than Originally Thought:

1. **Wilcoxon p=0.50**
   - **Original**: "Critical contradiction"
   - **Reassessed**: Valid finding ("no significant difference"), just needs language alignment
   - **Severity**: Medium (language fix, not methodological error)

2. **Win Rate 33%**
   - **Original**: "Majority failure"
   - **Reassessed**: Descriptive statistic, needs magnitude weighting
   - **Severity**: Medium (enhance reporting, not remove)

### More Severe Than Originally Thought:

1. **Missing NF Limitations**
   - **Original**: "Unconditional flow limitation not linked"
   - **Reassessed**: Synthetic experiment shows major skewness/kurtosis limitations NOT mentioned
   - **Severity**: High (major limitation not discussed)

2. **Distributional Tests Missing**
   - **Original**: "No statistical tests"
   - **Reassessed**: Confirmed - should add tests
   - **Severity**: Medium (easy to add)

### Unchanged:

1. **Abstract Overclaims**: Still critical
2. **Conclusion Misalignment**: Still critical

---

## Recommended Actions (Prioritized)

### Immediate (30 min - Text Only):
1. Fix Abstract language (align with Wilcoxon p=0.50)
2. Fix Conclusion language
3. Add win rate interpretation ("context-dependent")
4. Add NF limitations from synthetic experiment to Limitations section

### Short-term (2-3 hours - Analysis):
1. Run Diebold-Mariano test on forecast errors
2. Run statistical tests on KS/Wasserstein distances
3. Calculate magnitude-weighted win rates

### Optional (If Time Permits):
1. Reframe thesis around distributional realism rather than forecasting
2. Add synthetic experiment results to Methodology or Results
3. Discuss why NF improves Wasserstein but not skewness/kurtosis

---

## Bottom Line

**Your Methods Are Fine**:
- Wilcoxon is appropriate (though DM is better)
- Win rate is useful (but needs enhancement)
- Distributional metrics are good (but need tests)

**Your Language Needs Fixing**:
- "Improves" → "shows directionally positive but non-significant improvements"
- "33% win rate" → "33% win rate, indicating context-dependent benefits"

**Your Results Are Missing**:
- Synthetic experiment limitations (skewness/kurtosis failure)
- Statistical tests for distributional metrics
- Diebold-Mariano test (better than Wilcoxon for forecasts)

**Your Thesis Might Be Stronger If Reframed**:
- Current: "NF-GARCH improves forecasting" (weak evidence)
- Better: "NF-GARCH improves distributional realism while maintaining comparable forecasting" (strong evidence)
