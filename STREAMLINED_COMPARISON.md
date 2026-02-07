# Streamlined Fair Comparison - 4 Models with Student-t

## Rationale

Reduced from 8 models to 4 models using **Student-t distribution only**.

### Why Student-t Only?

1. **Standard in Financial Econometrics**: Student-t is the baseline for modeling financial returns
2. **Realistic Fat Tails**: Captures the heavy-tailed nature of financial data
3. **Fair Baseline**: Both NF-GARCH and Standard GARCH start from appropriate distributional assumption
4. **Faster Execution**: Half the models = ~50% faster pipeline
5. **Cleaner Results**: Focus on core comparison without distribution sensitivity analysis

### The 4 Models

```
1. sGARCH_std   - Standard GARCH(1,1) with Student-t
2. eGARCH_std   - Exponential GARCH (Nelson 1991) with Student-t  
3. gjrGARCH_std - GJR-GARCH with leverage (Glosten et al. 1993) with Student-t
4. TGARCH_std   - Threshold GARCH (Zakoian 1994) with Student-t
```

### Comparison Framework

For each of the 4 models:

**Training Phase:**
1. Fit GARCH model with Student-t distribution
2. Extract standardized residuals from Student-t fit
3. Train NF on these residuals (learns deviations from Student-t)

**Testing Phase:**
1. **NF-GARCH**: Use GARCH dynamics + NF-generated innovations
2. **Standard GARCH**: Use SAME GARCH dynamics + Student-t parametric innovations

**What We're Testing:**
- Does NF's learned innovation distribution improve over parametric Student-t?
- Holding GARCH specification constant (sGARCH/eGARCH/gjrGARCH/TGARCH)
- Starting from the same realistic baseline (Student-t)

## Expected Runtime

- **Old setup (8 models)**: ~90 minutes
- **New setup (4 models)**: ~45-60 minutes

## Normal Distribution Models?

Removed for main analysis. Can be added back for:
- Sensitivity analysis (Appendix)
- Testing "Does NF help more when base distribution is wrong?"

But for core dissertation results, Student-t is sufficient.
