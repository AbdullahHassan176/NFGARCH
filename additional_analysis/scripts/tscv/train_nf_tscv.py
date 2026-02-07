"""
Time-Series Cross-Validation NF Training
Trains separate NF models for each CV window
Each window gets its own NF model trained on that window's residuals
"""

import os
import numpy as np
import torch
import torch.nn as nn
from torch.utils.data import DataLoader, TensorDataset
import matplotlib.pyplot as plt
import pandas as pd
from glob import glob
from scipy.stats import ks_2samp, wasserstein_distance
from nflows.distributions import StandardNormal
from nflows.transforms import CompositeTransform, MaskedAffineAutoregressiveTransform
from nflows.flows import Flow
import time
import gc
from pathlib import Path
import re

# =============================================================================
# TS CV NF CONFIGURATION
# =============================================================================

# TS CV NF training parameters
TSCV_NF_CONFIG = {
    # Training parameters
    "epochs": 75,
    "batch_size": 512,
    "learning_rate": 0.001,
    
    # NO validation split - each window is already a validation fold
    "validation_split": 0.0,
    "early_stopping": False,
    
    # Model architecture
    "num_layers": 4,
    "hidden_features": 64,
    
    # Memory optimization
    "clear_cache": True,
    
    # Output
    "save_models": True,
    "output_dir": "outputs/tscv/nf_models"
}

# Performance monitoring
PERFORMANCE_CONFIG = {
    "enable_timing": True,
    "progress_frequency": 10,
    "memory_monitoring": False,
    "log_file": "outputs/tscv/nf_training.log"
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

def set_seed(seed=123):
    """Set random seeds for reproducibility"""
    torch.manual_seed(seed)
    np.random.seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed(seed)
        torch.cuda.manual_seed_all(seed)

def clear_memory():
    """Clear GPU and CPU memory"""
    if torch.cuda.is_available():
        torch.cuda.empty_cache()
    gc.collect()

def print_tscv_nf_summary():
    """Print configuration summary"""
    print("=== TS CV NF TRAINING CONFIGURATION ===")
    print(f"Epochs: {TSCV_NF_CONFIG['epochs']}")
    print(f"Batch size: {TSCV_NF_CONFIG['batch_size']}")
    print(f"Model layers: {TSCV_NF_CONFIG['num_layers']}")
    print(f"Hidden features: {TSCV_NF_CONFIG['hidden_features']}")
    print(f"Validation split: {TSCV_NF_CONFIG['validation_split']} (Window-based CV)")
    print(f"Early stopping: {TSCV_NF_CONFIG['early_stopping']}")
    print(f"Output dir: {TSCV_NF_CONFIG['output_dir']}")
    print("=======================================")

def extract_window_id(filepath):
    """Extract window ID from filename"""
    match = re.search(r'window(\d+)', filepath)
    if match:
        return int(match.group(1))
    return None

# =============================================================================
# OPTIMIZED NF MODEL CLASS
# =============================================================================

class OptimizedFlow(nn.Module):
    """Optimized Normalizing Flow"""
    
    def __init__(self, num_layers=4, hidden_features=64):
        super().__init__()
        
        # Create transforms
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
# TRAINING FUNCTION (PER WINDOW)
# =============================================================================

def train_tscv_nf(file_path, model_key, window_id, output_dir, config):
    """
    Train Normalizing Flow on a single CV window's residuals
    """
    print(f"\nTraining NF for {model_key} - Window {window_id}...")
    
    # Load residuals
    residuals = pd.read_csv(file_path).values.astype(np.float32)
    residuals = residuals[~np.isnan(residuals)].flatten().reshape(-1, 1)
    
    print(f"  Loaded {len(residuals)} residuals from window {window_id}")
    print(f"  Training on 100% of window residuals (no additional validation)")
    
    # Use ALL window residuals for training
    train_residuals = residuals
    
    # Create dataset
    train_dataset = TensorDataset(torch.tensor(train_residuals, dtype=torch.float32))
    
    # Create data loader
    train_loader = DataLoader(train_dataset, batch_size=config["batch_size"], shuffle=True)
    
    # Create model
    flow = OptimizedFlow(
        num_layers=config["num_layers"],
        hidden_features=config["hidden_features"]
    )
    
    # Setup optimizer
    optimizer = torch.optim.Adam(flow.parameters(), lr=config["learning_rate"])
    
    # Training loop
    loss_history = []
    best_loss = float('inf')
    
    start_time = time.time()
    
    for epoch in range(config["epochs"]):
        # Training phase
        flow.train()
        total_loss = 0.0
        num_batches = 0
        
        for batch in train_loader:
            x = batch[0]
            
            # Forward pass
            loss = -flow(x).mean()
            
            # Check for NaN
            if torch.isnan(loss):
                print(f"[ERROR] NaN loss at epoch {epoch+1} for {model_key} window {window_id}")
                return None, [], residuals, None
            
            # Backward pass
            optimizer.zero_grad()
            loss.backward()
            
            # Gradient clipping
            torch.nn.utils.clip_grad_norm_(flow.parameters(), max_norm=1.0)
            
            optimizer.step()
            
            total_loss += loss.item()
            num_batches += 1
        
        avg_train_loss = total_loss / num_batches
        loss_history.append(avg_train_loss)
        
        # Track best loss
        if avg_train_loss < best_loss:
            best_loss = avg_train_loss
        
        # Progress reporting (less frequent for CV windows)
        if (epoch + 1) % (PERFORMANCE_CONFIG["progress_frequency"] * 2) == 0 or epoch == 0:
            elapsed_time = time.time() - start_time
            print(
                f"    [W{window_id}] Epoch {epoch+1}/{config['epochs']}: "
                f"Loss = {avg_train_loss:.4f}, Time = {elapsed_time:.1f}s"
            )
        
        # Memory management
        if config["clear_cache"] and (epoch + 1) % 10 == 0:
            clear_memory()
    
    # Save model and results
    if config["save_models"]:
        # Create window-specific directory
        window_dir = Path(output_dir) / f"window_{window_id}" / model_key
        window_dir.mkdir(parents=True, exist_ok=True)
        
        # Save model state
        torch.save(flow.state_dict(), window_dir / "nf_model.pth")
        
        # Save training history
        history_df = pd.DataFrame({
            'epoch': range(1, len(loss_history) + 1),
            'train_loss': loss_history
        })
        history_df.to_csv(window_dir / "training_history.csv", index=False)
    
    # Generate samples
    flow.eval()
    with torch.no_grad():
        samples = flow.sample(len(residuals)).numpy()
    
    # Verify NF samples are standardized (should be naturally if trained correctly)
    samples_mean = samples.mean()
    samples_std = samples.std()
    
    print(f"    NF sample statistics: mean={samples_mean:.6f}, std={samples_std:.6f}")
    
    # Check if samples are properly standardized
    if abs(samples_mean) > 0.1 or abs(samples_std - 1) > 0.15:
        print(f"    WARNING: NF samples NOT standardized! This indicates NF training issue.")
        print(f"      Expected: mean~0, std~1")
        print(f"      Got: mean={samples_mean:.4f}, std={samples_std:.4f}")
        print(f"    Recommendation: Check training residuals and NF architecture")
        # Don't force standardization - let it fail so we know there's an issue
    elif abs(samples_mean) > 0.05 or abs(samples_std - 1) > 0.05:
        print(f"    Note: Slight deviation from perfect standardization (acceptable)")
    else:
        print(f"    [OK] NF samples properly standardized")
    
    # Calculate evaluation metrics
    ks_stat, ks_pvalue = ks_2samp(residuals.flatten(), samples.flatten())
    wass_dist = wasserstein_distance(residuals.flatten(), samples.flatten())
    
    print(f"    [OK] Window {window_id} completed: Loss = {loss_history[-1]:.4f}, "
          f"KS = {ks_stat:.4f}, Wass = {wass_dist:.4f}")
    
    return flow, loss_history, residuals, samples

# =============================================================================
# MAIN TRAINING PIPELINE
# =============================================================================

def main():
    """Main training pipeline for TS CV"""
    
    # Set reproducibility
    set_seed(123)
    
    # Print configuration summary
    print_tscv_nf_summary()
    
    # Create output directory
    output_dir = Path(TSCV_NF_CONFIG["output_dir"])
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Discover residual files from TS CV GARCH fitting
    residuals_base_dir = "outputs/tscv/residuals_by_model"
    
    if not os.path.exists(residuals_base_dir):
        print(f"[ERROR] Residuals directory not found: {residuals_base_dir}")
        print("Please run fit_garch_tscv.R first to generate residuals")
        return
    
    # Find all window directories
    window_dirs = glob(os.path.join(residuals_base_dir, "window_*"))
    
    if not window_dirs:
        print("[ERROR] No window directories found in", residuals_base_dir)
        print("Please run fit_garch_tscv.R first to generate residuals")
        return
    
    # Extract window IDs and sort
    window_ids = sorted([int(os.path.basename(d).replace("window_", "")) for d in window_dirs])
    
    print(f"\nFound {len(window_ids)} CV windows: {window_ids}")
    
    # Training results storage
    training_results = {}
    all_samples = {}
    
    # Train NF for each window
    start_time = time.time()
    
    for window_id in window_ids:
        print(f"\n{'='*60}")
        print(f"PROCESSING WINDOW {window_id}")
        print(f"{'='*60}")
        
        # Find all residual files for this window
        window_resid_dir = os.path.join(residuals_base_dir, f"window_{window_id}")
        residual_files = glob(os.path.join(window_resid_dir, "*", f"*_TSCV_window{window_id}_residuals.csv"))
        
        print(f"Found {len(residual_files)} residual files for window {window_id}")
        
        for file_path in residual_files:
            # Extract model and asset info
            path_parts = file_path.split(os.sep)
            model_name = path_parts[-2]  # model name directory
            filename = path_parts[-1]
            asset_name = filename.replace(f"_TSCV_window{window_id}_residuals.csv", "")
            model_key = f"{model_name}_{asset_name}"
            
            try:
                # Train NF for this window
                flow, loss_history, residuals, samples = train_tscv_nf(
                    file_path, model_key, window_id, str(output_dir), TSCV_NF_CONFIG
                )
                
                if flow is not None:
                    result_key = f"{model_key}_window_{window_id}"
                    training_results[result_key] = {
                        'flow': flow,
                        'loss_history': loss_history,
                        'residuals': residuals,
                        'samples': samples,
                        'window_id': window_id
                    }
                    all_samples[result_key] = samples
                    
                    # Save samples
                    window_dir = output_dir / f"window_{window_id}"
                    window_dir.mkdir(parents=True, exist_ok=True)
                    samples_df = pd.DataFrame({'synthetic_residuals': samples.flatten()})
                    samples_file = window_dir / f"{model_key}_synthetic_residuals.csv"
                    samples_df.to_csv(samples_file, index=False)
                
            except Exception as e:
                print(f"[ERROR] Error training {model_key} window {window_id}: {str(e)}")
                continue
            
            # Memory management
            clear_memory()
    
    # Generate summary report
    end_time = time.time()
    execution_time = end_time - start_time
    
    print(f"\n{'='*60}")
    print("TS CV NF TRAINING SUMMARY")
    print(f"{'='*60}")
    print(f"Total execution time: {execution_time:.2f} seconds ({execution_time/60:.2f} minutes)")
    print(f"CV Windows processed: {len(window_ids)}")
    print(f"Models trained successfully: {len(training_results)}")
    print(f"Success rate: {len(training_results)/(len(window_ids)*len(window_dirs))*100:.1f}%")
    print(f"Validation strategy: Window-based TS CV (no additional validation split)")
    
    # Per-window summary
    print(f"\nPer-window summary:")
    for window_id in window_ids:
        window_models = [k for k in training_results.keys() if f"_window_{window_id}" in k]
        print(f"  Window {window_id}: {len(window_models)} models trained")
    
    # Save comprehensive results
    results_summary = {
        'execution_time': execution_time,
        'windows_processed': len(window_ids),
        'window_ids': window_ids,
        'models_trained': len(training_results),
        'config': TSCV_NF_CONFIG
    }
    
    import json
    with open(output_dir / "training_summary.json", 'w') as f:
        json.dump(results_summary, f, indent=2)
    
    print(f"\n[OK] TS CV NF training completed.")
    print(f"Results saved to: {output_dir}")
    print(f"Generated {len(all_samples)} synthetic residual files")
    print(f"{'='*60}")
    
    return training_results, all_samples

# =============================================================================
# EXECUTION
# =============================================================================

if __name__ == "__main__":
    # Run main training pipeline
    training_results, all_samples = main()
