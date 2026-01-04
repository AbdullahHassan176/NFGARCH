# GARCH Order Robustness Experiment - Results Summary

## Experiment Status: ✅ COMPLETED

The experiment ran successfully and generated results comparing classical GARCH vs NF-GARCH across different GARCH orders.

## Key Findings

### Order Selection Results

The experiment tested GARCH orders (1,1), (2,1), (1,2), and (2,2) and selected the best by BIC:

- **Most assets selected (1,1)**: Standard GARCH(1,1) was optimal for most cases
- **Higher orders selected in some cases**:
  - AMZN sGARCH: (1,2) selected
  - NVDA eGARCH: (2,1) selected  
  - EURUSD eGARCH: (2,1) selected
  - GBPUSD eGARCH: (2,1) selected
  - USDZAR eGARCH: (2,2) selected
  - AMZN gjrGARCH: (2,1) selected
  - NVDA gjrGARCH: (2,1) selected

### Performance Comparison

**Classical vs NF-GARCH Performance:**

1. **NF-GARCH generally performs worse** (positive delta_MSE, delta_MAE) in most cases
   - This suggests that when allowing higher-order GARCH, the classical models become more competitive
   - However, this could also be due to:
     - Numerical instability in some NF-GARCH simulations (especially gjrGARCH with higher orders)
     - Missing NF residuals for some eGARCH models
     - Simulation method differences (forecasting vs simulation)

2. **Notable exceptions:**
   - Some models show very large deltas (numerical instability)
   - Some eGARCH models missing NF residuals (MSFT, AMZN, USDZAR)

### Issues Identified

1. **Numerical Instability**: 
   - gjrGARCH models with higher orders (2,1) show extreme values
   - Examples: NVDA gjrGARCH (2,1) has MSE > 1e+80
   - AMZN gjrGARCH (2,1) has MSE > 1e+56
   - These should be filtered out or investigated

2. **Missing NF Residuals**:
   - eGARCH models for MSFT, AMZN, USDZAR don't have NF residuals
   - These appear as NA in results

3. **Simulation Method**:
   - Classical uses `ugarchforecast` (point forecasts)
   - NF-GARCH uses `ugarchpath` with NF innovations (simulation)
   - This is a methodological difference that should be noted

## Results Files

1. **CSV**: `garch_order_robustness_results.csv` - Full detailed results
2. **Excel**: `garch_order_robustness_results.xlsx` - Same data in Excel
3. **LaTeX**: `garch_order_robustness_table.tex` - Summary table for dissertation

## Recommendations

1. **Filter extreme values**: Remove or flag results with numerical instability (MSE > 1e10)

2. **Investigate missing NF residuals**: 
   - Check if eGARCH NF models were trained
   - Consider using alternative NF residual sources

3. **Methodological note**: 
   - The comparison uses forecasting (classical) vs simulation (NF-GARCH)
   - Consider using same method for both (either both forecast or both simulate)

4. **Interpretation**:
   - The results suggest that when allowing higher-order GARCH, classical models become more competitive
   - However, numerical issues and missing data limit definitive conclusions
   - Further investigation needed for robust interpretation

## Next Steps

1. Filter out numerically unstable results
2. Investigate missing NF residuals for eGARCH
3. Consider methodological alignment (forecast vs simulate)
4. Re-run with filtered/cleaned data if needed
5. Create cleaned summary table for dissertation

## Files Generated

- `garch_order_robustness_results.csv` - Full results (37 rows)
- `garch_order_robustness_results.xlsx` - Excel format
- `garch_order_robustness_table.tex` - LaTeX summary table

