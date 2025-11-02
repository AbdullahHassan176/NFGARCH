"""
Manual Optimized Normalizing Flow Training
Implements optimizations for manual execution in Jupyter Notebook
- Reduced epochs (25% reduction)
- Increased batch size for better GPU utilization
- Early stopping and validation
- Memory optimization
"""

import os
import numpy as np
import torch
import torch.nn as nn
from torch.utils.data import DataLoader, TensorDataset
import matplotlib.pyplot as plt
import pandas as pd
from glob import glob
from scipy.stats import ks_2samp, wasserstein_distance, skew, kurtosis
from nflows.distributions import StandardNormal
from nflows.transforms import CompositeTransform, MaskedAffineAutoregressiveTransform
from nflows.flows import Flow
import time
import gc
from pathlib import Path

# =============================================================================
# OPTIMIZATION CONFIGURATION
# =============================================================================

# Manual optimization parameters
MANUAL_NF_CONFIG = {
    # Training parameters (25% reduction in epochs)
    "epochs": 75,                    # Reduced from 100
    "batch_size": 512,              # Increased from 256 for better GPU utilization
    "learning_rate": 0.001,
    
    # Early stopping
    "early_stopping": True,
    "patience": 15,                  # Stop if no improvement for 15 epochs
    "min_delta": 1e-4,
    
    # Validation
    "validation_split": 0.2,
    "validation_frequency": 5,       # Validate every 5 epochs
    
    # Model architecture (reduced complexity)
    "num_layers": 4,                 # Reduced from 5 (20% reduction)
    "hidden_features": 64,           # Reduced from 128 (50% reduction)
    
    # Memory optimization
    "gradient_checkpointing": True,
    "mixed_precision": True,
    "clear_cache": True,
    
    # Output
    "save_models": True,
    "save_plots": True,
    "output_dir": "outputs/manual/nf_models"
}

# Performance monitoring
PERFORMANCE_CONFIG = {
    "enable_timing": True,
    "progress_frequency": 10,        # Update every 10 epochs
    "memory_monitoring": True,
    "log_file": "outputs/manual/nf_training.log"
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

def set_seed(seed=123):
    """Set random seeds for reproducibility (matching R seed)"""
    torch.manual_seed(seed)
    np.random.seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed(seed)

def clear_memory():
    """Clear GPU and CPU memory"""
    if torch.cuda.is_available():
        torch.cuda.empty_cache()
    gc.collect()

def monitor_memory():
    """Monitor memory usage"""
    if PERFORMANCE_CONFIG["memory_monitoring"]:
        if torch.cuda.is_available():
            gpu_memory = torch.cuda.memory_allocated() / 1024**3  # GB
            print(f"GPU Memory: {gpu_memory:.2f} GB")
        
        import psutil
        cpu_memory = psutil.virtual_memory().used / 1024**3  # GB
        print(f"CPU Memory: {cpu_memory:.2f} GB")

def print_optimization_summary():
    """Print optimization summary"""
    print("=== MANUAL NF TRAINING OPTIMIZATION ===")
    print(f"Epochs: {MANUAL_NF_CONFIG['epochs']} (25% reduction from 100)")
    print(f"Batch size: {MANUAL_NF_CONFIG['batch_size']} (increased for GPU utilization)")
    print(f"Model layers: {MANUAL_NF_CONFIG['num_layers']} (20% reduction from 5)")
    print(f"Hidden features: {MANUAL_NF_CONFIG['hidden_features']} (50% reduction from 128)")
    print(f"Early stopping: {MANUAL_NF_CONFIG['early_stopping']}")
    print(f"Memory optimization: {MANUAL_NF_CONFIG['clear_cache']}")
    print("======================================")

# =============================================================================
# OPTIMIZED NF MODEL CLASS
# =============================================================================

class OptimizedFlow(nn.Module):
    """Optimized Normalizing Flow with reduced complexity"""
    
    def __init__(self, num_layers=4, hidden_features=64):
        super().__init__()
        
        # Create transforms with reduced complexity
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
# OPTIMIZED TRAINING FUNCTION
# =============================================================================

def train_optimized_nf(file_path, model_key, output_dir="outputs/manual/nf_models", 
                      config=MANUAL_NF_CONFIG):
    """
    Train Normalizing Flow with optimizations for manual execution
    """
    print(f"\nTraining optimized NF for {model_key}...")
    
    # Load residuals
    residuals = pd.read_csv(file_path).values.astype(np.float32)
    residuals = residuals[~np.isnan(residuals)].flatten().reshape(-1, 1)
    
    # Split data for validation
    n_train = int(len(residuals) * (1 - config["validation_split"]))
    train_residuals = residuals[:n_train]
    val_residuals = residuals[n_train:]
    
    # Create datasets
    train_dataset = TensorDataset(torch.tensor(train_residuals, dtype=torch.float32))
    val_dataset = TensorDataset(torch.tensor(val_residuals, dtype=torch.float32))
    
    # Create data loaders with optimized batch size
    train_loader = DataLoader(train_dataset, batch_size=config["batch_size"], shuffle=True)
    val_loader = DataLoader(val_dataset, batch_size=config["batch_size"], shuffle=False)
    
    # Create optimized model
    flow = OptimizedFlow(
        num_layers=config["num_layers"],
        hidden_features=config["hidden_features"]
    )
    
    # Setup optimizer
    optimizer = torch.optim.Adam(flow.parameters(), lr=config["learning_rate"])
    
    # Training loop with optimizations
    loss_history = []
    val_loss_history = []
    best_val_loss = float('inf')
    patience_counter = 0
    
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
                print(f"[ERROR] NaN loss encountered at epoch {epoch+1} for {model_key}")
                return None, [], residuals
            
            # Backward pass
            optimizer.zero_grad()
            loss.backward()
            
            # Gradient clipping for stability
            torch.nn.utils.clip_grad_norm_(flow.parameters(), max_norm=1.0)
            
            optimizer.step()
            
            total_loss += loss.item()
            num_batches += 1
        
        avg_train_loss = total_loss / num_batches
        loss_history.append(avg_train_loss)
        
        # Validation phase (every validation_frequency epochs)
        if config["validation_frequency"] > 0 and (epoch + 1) % config["validation_frequency"] == 0:
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
                    print(f"Early stopping at epoch {epoch+1} for {model_key}")
                    break
        else:
            val_loss_history.append(None)
        
        # Progress reporting
        if (epoch + 1) % PERFORMANCE_CONFIG["progress_frequency"] == 0 or epoch == 0:
            elapsed_time = time.time() - start_time
            last_val = val_loss_history[-1] if len(val_loss_history) > 0 else None
            val_str = f"{last_val:.4f}" if isinstance(last_val, (float, int)) else "N/A"
            print(
                f"[{model_key}] Epoch {epoch+1}/{config['epochs']}: "
                f"Train Loss = {avg_train_loss:.4f}, "
                f"Val Loss = {val_str}, "
                f"Time = {elapsed_time:.1f}s"
            )
            
            # Memory monitoring
            if PERFORMANCE_CONFIG["memory_monitoring"]:
                monitor_memory()
        
        # Memory management
        if config["clear_cache"] and (epoch + 1) % 10 == 0:
            clear_memory()
    
    # Save model and results
    if config["save_models"]:
        model_dir = Path(output_dir) / model_key
        model_dir.mkdir(parents=True, exist_ok=True)
        
        # Save model state
        torch.save(flow.state_dict(), model_dir / "nf_model.pth")
        
        # Save training history
        history_df = pd.DataFrame({
            'epoch': range(1, len(loss_history) + 1),
            'train_loss': loss_history,
            'val_loss': val_loss_history
        })
        history_df.to_csv(model_dir / "training_history.csv", index=False)
    
    # Generate samples
    flow.eval()
    with torch.no_grad():
        samples = flow.sample(len(residuals)).numpy()
    
    # Calculate evaluation metrics
    ks_stat, ks_pvalue = ks_2samp(residuals.flatten(), samples.flatten())
    wass_dist = wasserstein_distance(residuals.flatten(), samples.flatten())
    
    print(f"[OK] {model_key} training completed:")
    print(f"   Final train loss: {loss_history[-1]:.4f}")
    print(f"   KS statistic: {ks_stat:.4f} (p-value: {ks_pvalue:.4f})")
    print(f"   Wasserstein distance: {wass_dist:.4f}")
    
    return flow, loss_history, residuals, samples

# =============================================================================
# MAIN TRAINING PIPELINE
# =============================================================================

def main():
    """Main training pipeline with optimizations"""
    
    # Set reproducibility (matching R seed 123 for consistency)
    set_seed(123)
    
    # Print optimization summary
    print_optimization_summary()
    
    # Create output directory
    output_dir = Path(MANUAL_NF_CONFIG["output_dir"])
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Discover residual files (using manual optimized residuals)
    residuals_dir = "outputs/manual/residuals_by_model"
    residual_files = glob(os.path.join(residuals_dir, "*", "*.csv"))
    
    if not residual_files:
        print("[ERROR] No residual files found in", residuals_dir)
        print("Please run manual_garch_fitting.R first to generate residuals")
        return
    
    print(f"Found {len(residual_files)} residual files")
    
    # Training results storage
    training_results = {}
    all_samples = {}
    
    # Train NF on each model's residuals
    start_time = time.time()
    
    for file_path in residual_files:
        # Extract model and asset info from file path
        path_parts = file_path.split(os.sep)
        model_name = path_parts[-2]  # residuals_by_model/model_name/asset.csv
        asset_name = path_parts[-1].replace("_Manual_Optimized_residuals.csv", "")
        model_key = f"{model_name}_{asset_name}"
        
        try:
            # Train optimized NF
            flow, loss_history, residuals, samples = train_optimized_nf(
                file_path, model_key, str(output_dir), MANUAL_NF_CONFIG
            )
            
            if flow is not None:
                training_results[model_key] = {
                    'flow': flow,
                    'loss_history': loss_history,
                    'residuals': residuals,
                    'samples': samples
                }
                all_samples[model_key] = samples
                
                # Save samples
                samples_df = pd.DataFrame({'synthetic_residuals': samples.flatten()})
                samples_file = output_dir / f"{model_key}_synthetic_residuals.csv"
                samples_df.to_csv(samples_file, index=False)
            
        except Exception as e:
            print(f"[ERROR] Error training {model_key}: {str(e)}")
            continue
        
        # Memory management
        clear_memory()
    
    # Generate summary report
    end_time = time.time()
    execution_time = end_time - start_time
    
    print(f"\n=== TRAINING SUMMARY ===")
    print(f"Total execution time: {execution_time:.2f} seconds ({execution_time/60:.2f} minutes)")
    print(f"Models trained successfully: {len(training_results)}")
    print(f"Total residual files processed: {len(residual_files)}")
    print(f"Success rate: {len(training_results)/len(residual_files)*100:.1f}%")
    
    # Save comprehensive results
    results_summary = {
        'execution_time': execution_time,
        'models_trained': len(training_results),
        'total_files': len(residual_files),
        'success_rate': len(training_results)/len(residual_files),
        'config': MANUAL_NF_CONFIG
    }
    
    import json
    with open(output_dir / "training_summary.json", 'w') as f:
        json.dump(results_summary, f, indent=2)
    
    print(f"\n[OK] Manual optimized NF training completed!")
    print(f"Results saved to: {output_dir}")
    print(f"Generated {len(all_samples)} synthetic residual files")
    
    return training_results, all_samples

# =============================================================================
# JUPYTER NOTEBOOK EXECUTION
# =============================================================================

if __name__ == "__main__":
    # Run main training pipeline
    training_results, all_samples = main()
    
    # For Jupyter notebook, also provide individual functions
    print("\n=== JUPYTER NOTEBOOK FUNCTIONS AVAILABLE ===")
    print("• train_optimized_nf(file_path, model_key) - Train single NF model")
    print("• set_seed(seed) - Set random seeds")
    print("• clear_memory() - Clear GPU/CPU memory")
    print("• monitor_memory() - Monitor memory usage")
    print("• print_optimization_summary() - Show optimization settings")
    print("=============================================")
