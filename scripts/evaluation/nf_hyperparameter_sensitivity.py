"""
Normalizing Flow Hyperparameter Sensitivity Analysis
Performs sensitivity analysis by varying key hyperparameters one at a time
and assessing impact on model performance.
"""

import os
import numpy as np
import torch
import torch.nn as nn
from torch.utils.data import DataLoader, TensorDataset
import pandas as pd
from glob import glob
from scipy.stats import ks_2samp, wasserstein_distance
from nflows.distributions import StandardNormal
from nflows.transforms import CompositeTransform, MaskedAffineAutoregressiveTransform
from nflows.flows import Flow
import time
import json
from pathlib import Path

# =============================================================================
# BASE CONFIGURATION (Current settings)
# =============================================================================

BASE_CONFIG = {
    "epochs": 75,
    "batch_size": 512,
    "learning_rate": 0.001,
    "num_layers": 4,
    "hidden_features": 64,
    "validation_split": 0.2,
    "early_stopping": True,
    "patience": 15,
    "min_delta": 1e-4
}

# =============================================================================
# SENSITIVITY ANALYSIS PARAMETERS
# =============================================================================

# Parameters to test (vary one at a time)
SENSITIVITY_PARAMS = {
    "num_layers": [3, 4, 5, 6],  # Current: 4
    "hidden_features": [32, 64, 128],  # Current: 64
    "learning_rate": [0.0005, 0.001, 0.002],  # Current: 0.001
    "batch_size": [256, 512, 1024]  # Current: 512
}

# Sample a subset of residual files for testing (to save time)
# Use 2-3 representative files: one FX, one equity, one from different model
TEST_FILES = [
    "outputs/manual/residuals_by_model/eGARCH/EURUSD_Manual_Optimized_residuals.csv",
    "outputs/manual/residuals_by_model/sGARCH/NVDA_Manual_Optimized_residuals.csv",
    "outputs/manual/residuals_by_model/TGARCH/GBPUSD_Manual_Optimized_residuals.csv"
]

# =============================================================================
# NF MODEL CLASS
# =============================================================================

class OptimizedFlow(nn.Module):
    """Normalizing Flow model"""
    
    def __init__(self, num_layers=4, hidden_features=64):
        super().__init__()
        
        transforms = []
        for _ in range(num_layers):
            transforms.append(
                MaskedAffineAutoregressiveTransform(
                    features=1, 
                    hidden_features=hidden_features
                )
            )
        
        self.transform = CompositeTransform(transforms)
        self.base_dist = StandardNormal([1])
        self.flow = Flow(self.transform, self.base_dist)
    
    def forward(self, x):
        return self.flow.log_prob(x)
    
    def sample(self, n_samples):
        return self.flow.sample(n_samples)
    
    def parameters(self):
        return self.flow.parameters()

# =============================================================================
# TRAINING FUNCTION
# =============================================================================

def train_nf_sensitivity(file_path, config, model_key):
    """
    Train NF model with given configuration and return metrics
    """
    # Load residuals
    residuals = pd.read_csv(file_path).values.astype(np.float32)
    residuals = residuals[~np.isnan(residuals)].flatten().reshape(-1, 1)
    
    if len(residuals) < 100:
        return None
    
    # Split data for validation
    n_train = int(len(residuals) * (1 - config["validation_split"]))
    train_residuals = residuals[:n_train]
    val_residuals = residuals[n_train:]
    
    # Create datasets
    train_dataset = TensorDataset(torch.tensor(train_residuals, dtype=torch.float32))
    val_dataset = TensorDataset(torch.tensor(val_residuals, dtype=torch.float32))
    
    # Create data loaders
    train_loader = DataLoader(train_dataset, batch_size=config["batch_size"], shuffle=True)
    val_loader = DataLoader(val_dataset, batch_size=config["batch_size"], shuffle=False)
    
    # Create model
    flow = OptimizedFlow(
        num_layers=config["num_layers"],
        hidden_features=config["hidden_features"]
    )
    
    # Setup optimizer
    optimizer = torch.optim.Adam(flow.parameters(), lr=config["learning_rate"])
    
    # Training loop
    train_loss_history = []
    val_loss_history = []
    best_val_loss = float('inf')
    patience_counter = 0
    
    start_time = time.time()
    
    for epoch in range(config["epochs"]):
        # Training phase
        flow.train()
        total_train_loss = 0.0
        num_batches = 0
        
        for batch in train_loader:
            x = batch[0]
            loss = -flow(x).mean()
            
            if torch.isnan(loss):
                return None
            
            optimizer.zero_grad()
            loss.backward()
            torch.nn.utils.clip_grad_norm_(flow.parameters(), max_norm=1.0)
            optimizer.step()
            
            total_train_loss += loss.item()
            num_batches += 1
        
        avg_train_loss = total_train_loss / num_batches
        train_loss_history.append(avg_train_loss)
        
        # Validation phase (every 5 epochs)
        if (epoch + 1) % 5 == 0:
            flow.eval()
            val_loss = 0.0
            val_batches = 0
            
            with torch.no_grad():
                for batch in val_loader:
                    x = batch[0]
                    loss = -flow(x).mean()
                    val_loss += loss.item()
                    val_batches += 1
            
            avg_val_loss = val_loss / val_batches
            val_loss_history.append(avg_val_loss)
            
            # Early stopping check
            if config["early_stopping"]:
                if avg_val_loss < best_val_loss - config["min_delta"]:
                    best_val_loss = avg_val_loss
                    patience_counter = 0
                else:
                    patience_counter += 1
                
                if patience_counter >= config["patience"]:
                    break
        else:
            val_loss_history.append(None)
    
    training_time = time.time() - start_time
    
    # Generate samples for evaluation
    flow.eval()
    with torch.no_grad():
        samples = flow.sample(len(residuals)).numpy()
    
    # Calculate evaluation metrics
    ks_stat, ks_pvalue = ks_2samp(residuals.flatten(), samples.flatten())
    wass_dist = wasserstein_distance(residuals.flatten(), samples.flatten())
    
    # Calculate overfitting indicator (gap between train and validation loss)
    final_train_loss = train_loss_history[-1]
    final_val_loss = val_loss_history[-1] if val_loss_history[-1] is not None else best_val_loss
    overfitting_gap = final_train_loss - final_val_loss if final_val_loss is not None else None
    
    return {
        'final_train_loss': final_train_loss,
        'final_val_loss': final_val_loss,
        'best_val_loss': best_val_loss,
        'ks_statistic': ks_stat,
        'ks_pvalue': ks_pvalue,
        'wasserstein_distance': wass_dist,
        'training_time': training_time,
        'overfitting_gap': overfitting_gap,
        'epochs_trained': len(train_loss_history)
    }

# =============================================================================
# SENSITIVITY ANALYSIS
# =============================================================================

def run_sensitivity_analysis():
    """
    Run sensitivity analysis by varying each hyperparameter
    """
    print("=== HYPERPARAMETER SENSITIVITY ANALYSIS ===\n")
    
    # Check which test files exist
    available_files = [f for f in TEST_FILES if os.path.exists(f)]
    
    if len(available_files) == 0:
        print("ERROR: No test residual files found. Please run GARCH fitting first.")
        return None
    
    print(f"Using {len(available_files)} test files for sensitivity analysis\n")
    
    all_results = []
    
    # For each parameter to test
    for param_name, param_values in SENSITIVITY_PARAMS.items():
        print(f"\nTesting parameter: {param_name}")
        print(f"Values to test: {param_values}")
        print(f"Base value: {BASE_CONFIG[param_name]}")
        
        # For each test file
        for file_path in available_files:
            # Extract model and asset info
            path_parts = file_path.split(os.sep)
            model_name = path_parts[-2]
            asset_name = path_parts[-1].replace("_Manual_Optimized_residuals.csv", "")
            model_key = f"{model_name}_{asset_name}"
            
            print(f"  Testing: {model_key}")
            
            # Test each parameter value
            for param_value in param_values:
                # Create config with this parameter value
                test_config = BASE_CONFIG.copy()
                test_config[param_name] = param_value
                
                print(f"    {param_name}={param_value}...", end=" ", flush=True)
                
                # Train and evaluate
                metrics = train_nf_sensitivity(file_path, test_config, model_key)
                
                if metrics is not None:
                    result = {
                        'Parameter': param_name,
                        'Parameter_Value': param_value,
                        'Model': model_name,
                        'Asset': asset_name,
                        'Model_Key': model_key,
                        **metrics
                    }
                    all_results.append(result)
                    print(f"✓ (val_loss={metrics['final_val_loss']:.4f})")
                else:
                    print("✗ (failed)")
    
    # Convert to DataFrame
    if len(all_results) > 0:
        results_df = pd.DataFrame(all_results)
        return results_df
    else:
        return None

# =============================================================================
# MAIN EXECUTION
# =============================================================================

if __name__ == "__main__":
    # Set seed for reproducibility
    torch.manual_seed(123)
    np.random.seed(123)
    
    # Run sensitivity analysis
    results_df = run_sensitivity_analysis()
    
    if results_df is not None and len(results_df) > 0:
        # Create summary statistics
        summary_by_param = results_df.groupby(['Parameter', 'Parameter_Value']).agg({
            'final_val_loss': ['mean', 'std', 'min', 'max'],
            'ks_statistic': ['mean', 'std'],
            'wasserstein_distance': ['mean', 'std'],
            'training_time': ['mean', 'std'],
            'overfitting_gap': ['mean', 'std']
        }).reset_index()
        
        summary_by_param.columns = ['Parameter', 'Parameter_Value', 
                                    'Mean_Val_Loss', 'Std_Val_Loss', 'Min_Val_Loss', 'Max_Val_Loss',
                                    'Mean_KS', 'Std_KS',
                                    'Mean_Wasserstein', 'Std_Wasserstein',
                                    'Mean_Training_Time', 'Std_Training_Time',
                                    'Mean_Overfitting_Gap', 'Std_Overfitting_Gap']
        
        # Find best configuration for each parameter
        best_configs = []
        for param_name in SENSITIVITY_PARAMS.keys():
            param_results = results_df[results_df['Parameter'] == param_name]
            if len(param_results) > 0:
                # Best is minimum validation loss
                best_idx = param_results['final_val_loss'].idxmin()
                best_configs.append(param_results.loc[best_idx])
        
        best_configs_df = pd.DataFrame(best_configs)
        
        # Overfitting analysis
        overfitting_analysis = results_df[results_df['overfitting_gap'].notna()].copy()
        overfitting_analysis['overfitting_severity'] = pd.cut(
            overfitting_analysis['overfitting_gap'],
            bins=[-np.inf, -0.1, 0.1, np.inf],
            labels=['Underfitting', 'Good Fit', 'Overfitting']
        )
        
        overfitting_summary = overfitting_analysis.groupby(['Parameter', 'Parameter_Value', 'overfitting_severity']).size().reset_index(name='Count')
        
        # Save results
        output_dir = Path("results/consolidated")
        output_dir.mkdir(parents=True, exist_ok=True)
        
        output_file = output_dir / "Methodology_Hyperparameter_Sensitivity.xlsx"
        
        with pd.ExcelWriter(output_file, engine='openpyxl') as writer:
            results_df.to_excel(writer, sheet_name='Sensitivity_Results', index=False)
            summary_by_param.to_excel(writer, sheet_name='Summary_Statistics', index=False)
            best_configs_df.to_excel(writer, sheet_name='Best_Configurations', index=False)
            overfitting_summary.to_excel(writer, sheet_name='Overfitting_Analysis', index=False)
        
        print(f"\n=== SENSITIVITY ANALYSIS COMPLETE ===")
        print(f"Results saved to: {output_file}")
        print(f"Total configurations tested: {len(results_df)}")
        print(f"Parameters tested: {len(SENSITIVITY_PARAMS)}")
        print(f"Test files used: {len([f for f in TEST_FILES if os.path.exists(f)])}")
    else:
        print("\nERROR: No results generated. Check residual file paths and dependencies.")

