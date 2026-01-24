#!/usr/bin/env python3
"""
Calculate Distributional Metrics for GARCH Models
Calculates KS distance, Wasserstein distance, Tail index, Skewness, Kurtosis
for both Standard and NF residuals

ARCHIVED: Unused. The pipeline uses scripts/evaluation/calculate_distributional_metrics.R instead.
"""

import pandas as pd
import numpy as np
from scipy import stats
from scipy.stats import kstest
from pathlib import Path
import warnings
warnings.filterwarnings('ignore')

def calculate_ks_distance(actual, predicted):
    """Calculate Kolmogorov-Smirnov distance"""
    try:
        # Sort both arrays
        sorted_actual = np.sort(actual)
        sorted_pred = np.sort(predicted)
        
        # Create empirical CDFs
        n_actual = len(actual)
        n_pred = len(predicted)
        
        # Combine and sort all unique values
        all_values = np.unique(np.concatenate([sorted_actual, sorted_pred]))
        
        # Calculate CDFs at each point
        cdf_actual = np.array([np.mean(sorted_actual <= val) for val in all_values])
        cdf_pred = np.array([np.mean(sorted_pred <= val) for val in all_values])
        
        # KS statistic is maximum difference
        ks_stat = np.max(np.abs(cdf_actual - cdf_pred))
        return ks_stat
    except:
        return np.nan

def calculate_wasserstein_distance(actual, predicted):
    """Calculate Wasserstein-1 distance"""
    try:
        sorted_actual = np.sort(actual)
        sorted_pred = np.sort(predicted)
        n = len(sorted_actual)
        m = len(sorted_pred)
        
        min_len = min(n, m)
        if min_len == 0:
            return np.nan
        
        sorted_actual = np.sort(sorted_actual)[:min_len]
        sorted_pred = np.sort(sorted_pred)[:min_len]
        
        wd = np.mean(np.abs(sorted_actual - sorted_pred))
        return wd
    except:
        return np.nan

def calculate_tail_index(data, k=None):
    """Calculate tail index using Hill estimator"""
    try:
        abs_data = np.abs(data)
        sorted_data = np.sort(abs_data)[::-1]  # Descending order
        
        if k is None:
            k = max(int(len(sorted_data) * 0.1), 5)
        k = min(k, len(sorted_data) - 1)
        
        if k < 2:
            return np.nan
        
        log_threshold = np.log(sorted_data[k])
        log_ratios = np.log(sorted_data[:k]) - log_threshold
        
        hill_estimator = np.mean(log_ratios)
        if hill_estimator <= 0:
            return np.nan
        
        tail_index = 1 / hill_estimator
        return tail_index
    except:
        return np.nan

def calculate_skewness(data):
    """Calculate skewness"""
    try:
        mean_val = np.mean(data)
        std_val = np.std(data)
        if std_val == 0 or np.isnan(std_val):
            return np.nan
        skew = np.mean(((data - mean_val) / std_val) ** 3)
        return skew
    except:
        return np.nan

def calculate_kurtosis(data):
    """Calculate excess kurtosis"""
    try:
        mean_val = np.mean(data)
        std_val = np.std(data)
        if std_val == 0 or np.isnan(std_val):
            return np.nan
        kurt = np.mean(((data - mean_val) / std_val) ** 4) - 3
        return kurt
    except:
        return np.nan

# Main calculation
print("=== CALCULATING DISTRIBUTIONAL METRICS ===\n")

residuals_dir = Path("outputs/manual/residuals_by_model")
nf_residuals_dir = Path("outputs/manual/nf_models")

models = ["sGARCH", "eGARCH", "TGARCH", "gjrGARCH"]
assets = ["EURUSD", "GBPUSD", "USDZAR", "NVDA", "MSFT", "AMZN"]

results = []

for model_name in models:
    for asset_name in assets:
        # Standard residuals
        std_file = residuals_dir / model_name / f"{asset_name}_Manual_Optimized_residuals.csv"
        # NF residuals
        nf_file = nf_residuals_dir / f"{model_name}_{asset_name}_synthetic_residuals.csv"
        
        metrics = {
            "Model": model_name,
            "Asset": asset_name,
            "KS_distance": np.nan,
            "Wasserstein_distance": np.nan,
            "Tail_index_Std": np.nan,
            "Skewness_Std": np.nan,
            "Kurtosis_Std": np.nan,
            "Tail_index_NF": np.nan,
            "Skewness_NF": np.nan,
            "Kurtosis_NF": np.nan
        }
        
        # Load standard residuals
        std_residuals = None
        if std_file.exists():
            try:
                std_data = pd.read_csv(std_file, header=None)
                std_residuals = std_data.iloc[:, 0].values
                # Skip header if string
                if isinstance(std_residuals[0], str):
                    std_residuals = std_residuals[1:]
                std_residuals = pd.to_numeric(std_residuals, errors='coerce')
                std_residuals = std_residuals[~np.isnan(std_residuals)]
                
                if len(std_residuals) > 10:
                    std_residuals_std = (std_residuals - np.mean(std_residuals)) / np.std(std_residuals)
                    metrics["Tail_index_Std"] = calculate_tail_index(std_residuals_std)
                    metrics["Skewness_Std"] = calculate_skewness(std_residuals_std)
                    metrics["Kurtosis_Std"] = calculate_kurtosis(std_residuals_std)
            except Exception as e:
                print(f"  [WARNING] Error loading standard residuals for {model_name}-{asset_name}: {e}")
        
        # Load NF residuals
        nf_residuals = None
        if nf_file.exists():
            try:
                nf_data = pd.read_csv(nf_file, header=None)
                nf_residuals = nf_data.iloc[:, 0].values
                # Skip header if string
                if isinstance(nf_residuals[0], str):
                    nf_residuals = nf_residuals[1:]
                nf_residuals = pd.to_numeric(nf_residuals, errors='coerce')
                nf_residuals = nf_residuals[~np.isnan(nf_residuals)]
                
                if len(nf_residuals) > 10:
                    nf_residuals_std = (nf_residuals - np.mean(nf_residuals)) / np.std(nf_residuals)
                    metrics["Tail_index_NF"] = calculate_tail_index(nf_residuals_std)
                    metrics["Skewness_NF"] = calculate_skewness(nf_residuals_std)
                    metrics["Kurtosis_NF"] = calculate_kurtosis(nf_residuals_std)
            except Exception as e:
                print(f"  [WARNING] Error loading NF residuals for {model_name}-{asset_name}: {e}")
        
        # Compare if both available
        if std_residuals is not None and nf_residuals is not None and len(std_residuals) > 10 and len(nf_residuals) > 10:
            try:
                std_residuals_std = (std_residuals - np.mean(std_residuals)) / np.std(std_residuals)
                nf_residuals_std = (nf_residuals - np.mean(nf_residuals)) / np.std(nf_residuals)
                
                metrics["KS_distance"] = calculate_ks_distance(std_residuals_std, nf_residuals_std)
                metrics["Wasserstein_distance"] = calculate_wasserstein_distance(std_residuals_std, nf_residuals_std)
            except Exception as e:
                print(f"  [WARNING] Error calculating distances for {model_name}-{asset_name}: {e}")
        
        results.append(metrics)

# Create DataFrame
df = pd.DataFrame(results)

# Summary by model
summary = df.groupby("Model").agg({
    "KS_distance": ["mean", "median"],
    "Wasserstein_distance": ["mean", "median"],
    "Tail_index_Std": "mean",
    "Skewness_Std": "mean",
    "Kurtosis_Std": "mean",
    "Tail_index_NF": "mean",
    "Skewness_NF": "mean",
    "Kurtosis_NF": "mean"
}).reset_index()

# Flatten column names
summary.columns = ["Model", "Mean_KS", "Median_KS", "Mean_Wasserstein", "Median_Wasserstein",
                   "Mean_Tail_index_Std", "Mean_Skewness_Std", "Mean_Kurtosis_Std",
                   "Mean_Tail_index_NF", "Mean_Skewness_NF", "Mean_Kurtosis_NF"]

print("\n=== SUMMARY STATISTICS BY MODEL ===\n")
print(summary.to_string(index=False))

# Save results
output_dir = Path("results/dissertation_tables")
output_dir.mkdir(parents=True, exist_ok=True)

# Save detailed results
df.to_csv(output_dir / "distributional_metrics_detailed.csv", index=False)

# Save summary
summary.to_csv(output_dir / "distributional_metrics_by_model.csv", index=False)

print(f"\n[OK] Results saved to {output_dir}/")
print("=== DISTRIBUTIONAL METRICS CALCULATION COMPLETE ===\n")
