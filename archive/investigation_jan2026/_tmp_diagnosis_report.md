# Root Cause Analysis: Equity vs FX Discrepancy

## Key Findings

### 1. Volatility Characteristics
- **Equity volatility**: 2.46% daily (3.2x higher than FX)
  - NVDA: 3.16% (highest)
  - AMZN: 2.44%
  - MSFT: 1.78%
- **FX volatility**: 0.77% daily
  - USDZAR: 1.08%
  - GBPUSD: 0.64%
  - EURUSD: 0.59%

### 2. Problematic Models (Standard GARCH)

| Model | Asset | MSE | Status |
|-------|-------|-----|--------|
| eGARCH | AMZN | 265,545 | 🔴 CATASTROPHIC |
| sGARCH_sstd | NVDA | 20.3 | 🔴 VERY BAD |
| sGARCH_norm | MSFT | 8.1 | 🔴 BAD |
| sGARCH_norm | EURUSD | 7.4 | 🟡 BORDERLINE |

### 3. Working Models (Standard GARCH)

| Model | Asset Range | MSE Range | Status |
|-------|-------------|-----------|--------|
| gjrGARCH | All | 0.00003 - 0.0011 | ✅ EXCELLENT |
| TGARCH | All | 0.018 - 0.088 | ✅ GOOD |
| sGARCH_sstd | FX (excl. 1) | 0.027 - 0.34 | ✅ REASONABLE |

## Root Cause

### Current Sigma Bounds Are Too Permissive

**Current Implementation** (`manual_garch_core.R`):
```r
# TGARCH: sigma capped at 10
sigma_next <- pmax(pmin(sigma_next, 10), safe_sqrt(var_floor))

# eGARCH: log_sigma2 clipped to [-20, 20]
log_sigma2_next <- pmax(pmin(log_sigma2_next, 20), -20)
# This allows sigma up to exp(10) = 22,026!
```

**Problem**: For 1581-step forecasts with high volatility:
1. **Equity volatility ~2.5%** means reasonable sigma ~ 0.025
2. **Cap of 10** (1000% volatility) is 400x too high!
3. Over 1581 steps, even small persistence causes exponential growth
4. **sGARCH** (no asymmetry) grows fastest
5. **eGARCH** (log-space) explodes even faster

### Why TGARCH/gjrGARCH Work

- Threshold effects naturally dampen volatility growth
- Asymmetric response prevents runaway dynamics
- These models are self-stabilizing

## Proposed Solution

### Asset-Class Specific Bounds

```r
# Determine asset class from historical volatility
historical_vol <- sd(returns_historical)

# Set adaptive bounds (3x to 5x historical volatility)
if (historical_vol > 0.015) {  # Equity
  sigma_max <- 0.15  # 15% daily vol (extreme but possible)
  sigma_min <- 1e-4
} else {  # FX  
  sigma_max <- 0.03  # 3% daily vol (crisis level)
  sigma_min <- 1e-5
}

# Apply bounds
sigma_next <- pmax(pmin(sigma_next, sigma_max), sigma_min)

# For eGARCH, adjust log bounds accordingly
log_sigma2_max <- log(sigma_max^2)  # ~= -3.8 for equity
log_sigma2_min <- log(sigma_min^2)  # ~= -18
```

### Expected Impact

| Metric | Before Fix | After Fix |
|--------|-----------|-----------|
| eGARCH AMZN MSE | 265,545 | ~0.5-1.0 |
| sGARCH equity MSE | 0.6-20.3 | ~0.5-2.0 |
| sGARCH FX MSE | 0.04-7.4 | ~0.04-0.5 |
| Overall win rate | 83-100% | 70-85% |

## Why This Will Work

1. **Preserves Normal Dynamics**: Bounds allow 3-5x historical vol (covers 99.7% of scenarios)
2. **Prevents Explosion**: Caps runaway growth in long-horizon forecasts
3. **Asset-Specific**: Equity gets higher bounds than FX (realistic)
4. **Doesn't Affect TGARCH/gjrGARCH**: These already work, bounds are redundant
5. **Fixes sGARCH/eGARCH**: These need external bounds to prevent explosion
