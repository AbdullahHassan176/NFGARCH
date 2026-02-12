# Student-t Configuration Applied

## Updated Config

All GARCH models now use **Student-t (std)** distribution:

```r
GARCH_MODELS:
1. sGARCH_std   - Standard GARCH with Student-t
2. eGARCH_std   - Exponential GARCH with Student-t  
3. gjrGARCH_std - GJR-GARCH (leverage) with Student-t
4. TGARCH_std   - Threshold GARCH with Student-t
```

## Why Student-t?

- **Fat tails**: Captures heavy-tailed nature of financial returns
- **Realistic**: Standard in financial econometrics
- **Better fit**: More appropriate baseline than Normal for returns

## What This Means

Both NF-GARCH and Standard GARCH now:
1. Start from Student-t residuals (fair comparison)
2. Use the same GARCH dynamics
3. Only differ in innovation distribution (parametric Student-t vs NF-learned)

This is the proper way to test if NF adds value over parametric distributions.
