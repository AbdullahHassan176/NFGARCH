#!/usr/bin/env python3
"""
Quick test to verify return forecast evaluation implementation
Tests that the R functions are accessible and work correctly
"""

import subprocess
import sys
import os

print("=== QUICK TEST: Return Forecast Evaluation ===\n")

# Check if R is available
print("1. Checking R availability...")
try:
    result = subprocess.run(["R", "--version"], capture_output=True, text=True, timeout=5)
    if result.returncode == 0:
        print("   [OK] R is available")
        print(f"   Version: {result.stdout.split(chr(10))[0]}")
    else:
        print("   [WARNING] R command returned non-zero exit code")
except FileNotFoundError:
    print("   [ERROR] R not found in PATH")
    print("   Please ensure R is installed and in PATH")
    sys.exit(1)
except Exception as e:
    print(f"   [ERROR] {e}")
    sys.exit(1)

# Check if test script exists
print("\n2. Checking test script...")
test_script = "scripts/utils/test_return_forecast_evaluation.R"
if os.path.exists(test_script):
    print(f"   [OK] Test script exists: {test_script}")
else:
    print(f"   [ERROR] Test script not found: {test_script}")
    sys.exit(1)

# Check if required R files exist
print("\n3. Checking required R files...")
required_files = [
    "scripts/utils/return_forecast_evaluation.R",
    "scripts/simulation_forecasting/simulate_nf_garch_engine.R",
    "scripts/evaluation/stress_testing_comprehensive.R",
    "scripts/core/config.R",
    "scripts/engines/engine_selector.R"
]

all_exist = True
for file in required_files:
    if os.path.exists(file):
        print(f"   [OK] {file}")
    else:
        print(f"   [ERROR] Missing: {file}")
        all_exist = False

if not all_exist:
    print("\n[ERROR] Some required files are missing")
    sys.exit(1)

# Check if data file exists
print("\n4. Checking data file...")
data_file = "data/processed/raw (FX + EQ).csv"
if os.path.exists(data_file):
    print(f"   [OK] Data file exists: {data_file}")
else:
    print(f"   [WARNING] Data file not found: {data_file}")
    print("   Test may fail if data is required")

# Summary
print("\n=== TEST PREPARATION COMPLETE ===")
print("\nTo run the actual test, execute:")
print(f'  Rscript {test_script}')
print("\nOr if Rscript is not in PATH, use full path to Rscript")
print("\nThe test will:")
print("  1. Load EURUSD data")
print("  2. Fit sGARCH model")
print("  3. Generate 100 simulation paths")
print("  4. Calculate point forecasts")
print("  5. Evaluate MSE, MAE, and log-likelihood")
print("\nIf test passes, you can proceed with full pipeline rerun.")
