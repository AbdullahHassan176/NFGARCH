"""
NF Training Wrapper for Synthetic Recovery Experiment
Trains a normalizing flow on standardized GARCH residuals
"""

import sys
import os
import numpy as np
import pandas as pd
import torch
import torch.nn as nn
from torch.utils.data import DataLoader, TensorDataset
from pathlib import Path

# Add parent directories to path to import NF training utilities
script_dir = os.path.dirname(os.path.abspath(__file__))
repo_root = os.path.abspath(os.path.join(script_dir, '..', '..', '..'))
sys.path.insert(0, repo_root)
sys.path.insert(0, os.path.join(repo_root, 'scripts', 'manual'))

# Import NF training utilities
try:
    from manual_nf_training import OptimizedFlow, set_seed, MANUAL_NF_CONFIG
except ImportError:
    # Fallback: try absolute import
    import importlib.util
    nf_training_path = os.path.join(repo_root, 'scripts', 'manual', 'manual_nf_training.py')
    spec = importlib.util.spec_from_file_location("manual_nf_training", nf_training_path)
    manual_nf = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(manual_nf)
    OptimizedFlow = manual_nf.OptimizedFlow
    set_seed = manual_nf.set_seed
    MANUAL_NF_CONFIG = manual_nf.MANUAL_NF_CONFIG

def train_nf_on_residuals(residuals_file, output_model_path, config=None):
    """
    Train normalizing flow on GARCH residuals
    
    Args:
        residuals_file: Path to CSV file with residuals
        output_model_path: Path to save trained NF model
        config: Training configuration (uses MANUAL_NF_CONFIG if None)
    
    Returns:
        flow: Trained flow model
        samples: Generated samples from the flow
    """
    if config is None:
        config = MANUAL_NF_CONFIG.copy()
        # Use lighter config for synthetic experiment
        config["epochs"] = 50  # Reduced for speed
        config["num_layers"] = 4
        config["hidden_features"] = 64
    
    # Set seed for reproducibility
    set_seed(123)
    
    # Load residuals
    print(f"Loading residuals from: {residuals_file}")
    residuals_df = pd.read_csv(residuals_file)
    
    # Extract residuals (handle different column names)
    if "residual" in residuals_df.columns:
        residuals = residuals_df["residual"].values
    elif "z_hat_for_nf" in residuals_df.columns:
        residuals = residuals_df["z_hat_for_nf"].values
    else:
        residuals = residuals_df.iloc[:, 0].values
    
    residuals = residuals.astype(np.float32)
    residuals = residuals[~np.isnan(residuals)].flatten().reshape(-1, 1)
    
    print(f"  Loaded {len(residuals)} residuals")
    print(f"  Residual stats: mean = {np.mean(residuals):.6f}, std = {np.std(residuals):.6f}")
    
    # Split for validation
    n_train = int(len(residuals) * (1 - config["validation_split"]))
    train_residuals = residuals[:n_train]
    val_residuals = residuals[n_train:]
    
    # Create datasets
    train_dataset = TensorDataset(torch.tensor(train_residuals, dtype=torch.float32))
    val_dataset = TensorDataset(torch.tensor(val_residuals, dtype=torch.float32))
    
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
    loss_history = []
    best_val_loss = float('inf')
    patience_counter = 0
    
    print(f"Training NF model ({config['epochs']} epochs)...")
    
    for epoch in range(config["epochs"]):
        # Training
        flow.train()
        total_loss = 0.0
        num_batches = 0
        
        for batch in train_loader:
            x = batch[0]
            loss = -flow(x).mean()
            
            if torch.isnan(loss):
                print(f"ERROR: NaN loss at epoch {epoch+1}")
                return None, None
            
            optimizer.zero_grad()
            loss.backward()
            torch.nn.utils.clip_grad_norm_(flow.parameters(), max_norm=1.0)
            optimizer.step()
            
            total_loss += loss.item()
            num_batches += 1
        
        avg_train_loss = total_loss / num_batches
        loss_history.append(avg_train_loss)
        
        # Validation
        if (epoch + 1) % config["validation_frequency"] == 0:
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
            
            # Early stopping
            if config["early_stopping"]:
                if avg_val_loss < best_val_loss - config["min_delta"]:
                    best_val_loss = avg_val_loss
                    patience_counter = 0
                else:
                    patience_counter += 1
                
                if patience_counter >= config["patience"]:
                    print(f"Early stopping at epoch {epoch+1}")
                    break
        
        if (epoch + 1) % 10 == 0:
            print(f"  Epoch {epoch+1}/{config['epochs']}: Train Loss = {avg_train_loss:.4f}")
    
    # Save model
    output_dir = Path(output_model_path).parent
    output_dir.mkdir(parents=True, exist_ok=True)
    
    torch.save(flow.state_dict(), output_model_path)
    print(f"Model saved to: {output_model_path}")
    
    # Generate samples for evaluation
    flow.eval()
    with torch.no_grad():
        n_samples = len(residuals)
        samples = flow.sample(n_samples).numpy()
    
    # Save samples
    samples_file = output_model_path.replace(".pth", "_samples.csv")
    pd.DataFrame({"z_nf": samples.flatten()}).to_csv(samples_file, index=False)
    print(f"Samples saved to: {samples_file}")
    
    return flow, samples

def main():
    """Main entry point for command-line usage"""
    if len(sys.argv) < 3:
        print("Usage: python train_nf_synthetic.py <residuals_file> <output_model_path>")
        sys.exit(1)
    
    residuals_file = sys.argv[1]
    output_model_path = sys.argv[2]
    
    if not os.path.exists(residuals_file):
        print(f"ERROR: Residuals file not found: {residuals_file}")
        sys.exit(1)
    
    flow, samples = train_nf_on_residuals(residuals_file, output_model_path)
    
    if flow is None:
        print("ERROR: NF training failed")
        sys.exit(1)
    
    print("NF training completed successfully")

if __name__ == "__main__":
    main()

