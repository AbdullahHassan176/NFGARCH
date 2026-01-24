# Return Forecast Evaluation Methodology

## Overview

This document describes the updated return forecast evaluation methodology for NF-GARCH models. The approach uses multiple simulation paths to generate point forecasts and evaluate both point forecast accuracy and density forecast quality.

## Key Changes

### Previous Approach (Problematic)
- Generated a single simulation path
- Compared one simulated path to one actual path
- Highly random and unstable results
- Did not properly test forecast accuracy

### New Approach (Proper)
- Generates multiple simulation paths (default: 1000)
- Calculates point forecast as mean across paths (expected return)
- Evaluates point forecast accuracy (MSE, MAE)
- Evaluates density forecast quality (predictive log-likelihood)
- More stable and interpretable results

## Methodology

### 1. Multiple Path Generation

For each forecast horizon, we generate `n_paths` (default: 1000) independent simulation paths:

```r
# For each path i = 1, ..., n_paths:
# 1. Sample residuals (with replacement if needed)
# 2. Generate simulation path using engine_path()
# 3. Store returns and sigma for this path
```

### 2. Point Forecast Calculation

The point forecast (expected return) is the mean across all paths:

```r
point_forecast[t] = mean(sim_returns[t, 1:n_paths])
```

This represents the expected return at time `t` under the model's predictive distribution.

### 3. Evaluation Metrics

**Point Forecast Metrics:**
- **MSE**: Mean squared error between point forecast and actual returns
- **MAE**: Mean absolute error between point forecast and actual returns

**Density Forecast Metrics:**
- **Predictive Log-Likelihood**: Log-likelihood of actual returns under the predictive distribution (estimated from simulation paths)

### 4. Standard GARCH Comparison

For fair comparison, standard GARCH models are also evaluated using the same approach:
- Generate multiple paths using standard GARCH residuals
- Calculate point forecasts as mean across paths
- Evaluate using same metrics

## Implementation

### Key Functions

**`generate_multiple_paths()`**
- Generates multiple simulation paths
- Returns point forecasts, all paths, and volatility forecasts

**`evaluate_return_forecasts()`**
- Main evaluation function
- Generates paths and calculates all metrics
- Returns MSE, MAE, log-likelihood, and number of valid paths

**`calculate_predictive_loglik()`**
- Calculates predictive log-likelihood using kernel density estimation
- Estimates density from simulation paths
- Evaluates likelihood of actual returns

### Usage

```r
# Load utilities
source("scripts/utils/return_forecast_evaluation.R")

# Evaluate forecasts
eval_result <- evaluate_return_forecasts(
  fit = fit,
  nf_residuals = nf_resid,
  actual_returns = test_returns,
  horizon = length(test_returns),
  model_type = "sGARCH",
  submodel = NULL,
  engine = "manual",
  n_paths = 1000
)

# Access results
mse <- eval_result$mse
mae <- eval_result$mae
loglik <- eval_result$loglik
```

## Updated Files

1. **`scripts/utils/return_forecast_evaluation.R`** (NEW)
   - Helper functions for return forecast evaluation

2. **`scripts/simulation_forecasting/simulate_nf_garch_engine.R`**
   - Updated to use `evaluate_return_forecasts()`
   - Now generates multiple paths and calculates point forecasts

3. **`scripts/evaluation/stress_testing_comprehensive.R`**
   - Updated to use new evaluation approach
   - Both NF-GARCH and standard GARCH use same method

## Interpretation

### Point Forecast Metrics
- **Lower MSE/MAE** = Better return forecast accuracy
- These metrics test whether the model's expected return predictions are accurate

### Density Forecast Metrics
- **Higher log-likelihood** = Better predictive distribution
- Tests whether the model's full predictive distribution (not just mean) matches reality

### Comparison to Standard GARCH
- Both models evaluated using same methodology
- Fair comparison of return forecasting ability
- NF-GARCH improvements indicate better innovation distribution helps return forecasting

## Research Implications

### Positioning
- **Focus**: Return forecasting (innovative time series modeling)
- **Contribution**: NF-GARCH improves return forecasts through flexible innovation distributions
- **Application**: Trading strategies, portfolio optimization, not just risk management

### Results Interpretation
- Results now reflect return forecast accuracy, not just simulation quality
- More stable and interpretable than single-path comparison
- Aligns with "innovative time series modeling" positioning

## Next Steps

1. **Rerun all experiments** with new evaluation approach
2. **Update dissertation** to reflect return forecasting focus
3. **Regenerate tables** with new MSE/MAE values
4. **Update interpretation** in results and discussion sections
