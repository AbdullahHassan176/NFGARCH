# Fixes Applied to Simulation Pipeline

## Issue 1: "missing value where TRUE/FALSE needed" Error

**Problem**: Error occurred in TS CV results flattening when checking data.frames with NA values.

**Fix**: Added robust validation in `fx_list_with_asset` and `eq_list_with_asset` lapply functions:
- Check if df is NULL
- Check if df is a data.frame
- Check if df contains NA values (skip if so)
- Check if df has rows > 0

**Location**: Lines ~681-688 and ~708-715

## Issue 2: Chronological Split Using Wrong Data

**Problem**: `fit_nf_garch` was fitting and evaluating on the full return series instead of splitting into train/test.

**Fix**: 
1. Modified `fit_nf_garch` function signature to accept separate `train_returns` and `test_returns`
2. Fit model on training data
3. Evaluate forecasts on test data
4. Updated function calls to pass `fx_train_returns[[asset]]` and `fx_test_returns[[asset]]` (and equity equivalents)

**Location**: 
- Function definition: Lines ~417-466
- Function calls: Lines ~544 and ~576

## Summary

Both fixes ensure:
- Proper train/test split for chronological analysis
- Robust handling of edge cases (NULL, NA, empty data.frames)
- Correct evaluation on test data only

The simulation should now generate results successfully.
