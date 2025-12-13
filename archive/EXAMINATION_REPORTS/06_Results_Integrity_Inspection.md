# Results Integrity Inspection

**Generated**: 2025-01-XX

---

## 1. Code vs Claims Comparison

### Claim: "NF-GARCH AIC is -34,586 vs Standard GARCH -7.55 (4,500x better)"

**Status**: 🔴 **CRITICAL - SUSPICIOUS CLAIM**

**Issues**:
1. **AIC Calculation**: AIC = -2·LL + 2·k
   - Very negative AIC suggests very high log-likelihood
   - -34,586 is extremely negative (suspicious)
2. **"4,500x better"**: AIC is not a ratio metric
   - Cannot meaningfully compare AIC values as ratios
   - Should compare differences, not ratios
3. **Aggregation Unclear**: 
   - Is this per-asset? Aggregated? Best case?
   - Need to verify in code

**Verification Needed**:
- Check actual AIC values in results files
- Verify calculation method
- Clarify aggregation method

---

## 2. Metric Consistency

**Status**: ⚠️ **INCONSISTENCIES FOUND**

**Issues**:
1. **MSE/MAE Definitions**: 
   - Some scripts compare forecasts vs actuals
   - Others compare simulations vs actuals
   - Need clarification

2. **AIC Aggregation**:
   - Some scripts average AIC across assets
   - Others sum AIC
   - Inconsistent methods

---

## 3. Code Support for Claims

**Status**: ⚠️ **PARTIAL SUPPORT**

**Missing**:
- Diebold-Mariano test (mentioned but not found)
- Some statistical tests may not be implemented
- Some claims may not have code support

---

**Recommendations**:
1. Verify all README claims have code support
2. Clarify metric definitions
3. Document aggregation methods

