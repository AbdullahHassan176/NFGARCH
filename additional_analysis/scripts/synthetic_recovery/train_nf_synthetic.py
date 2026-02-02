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
import matplotlib
matplotlib.use('Agg')  # Non-interactive backend
import matplotlib.pyplot as plt
from nflows.distributions import StandardNormal

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

def train_nf_on_residuals(residuals_file, output_model_path, config=None, seed=None, alt_config=False):
    """
    Train normalizing flow on GARCH residuals
    
    Args:
        residuals_file: Path to CSV file with residuals
        output_model_path: Path to save trained NF model
        config: Training configuration (uses MANUAL_NF_CONFIG if None)
        seed: Random seed (defaults to 123 if not provided)
        alt_config: If True, use alternative config (layers=8, hidden=128) for architecture test
    
    Returns:
        flow: Trained flow model
        samples: Generated samples from the flow
    """
    if config is None:
        config = MANUAL_NF_CONFIG.copy()
        if alt_config:
            # D) Alternative config for architecture stability test
            config["epochs"] = 50
            config["num_layers"] = 8
            config["hidden_features"] = 128
            print("Using alternative NF config: layers=8, hidden=128")
        else:
            # Use lighter config for synthetic experiment
            config["epochs"] = 50  # Reduced for speed
            config["num_layers"] = 4
            config["hidden_features"] = 64
    
    # Set seed for reproducibility (use provided seed or default to 123)
    if seed is None:
        seed = 123
    
    # Enhanced seed synchronization: set all RNGs
    import random
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    
    # CUDA deterministic settings
    if torch.cuda.is_available():
        torch.cuda.manual_seed(seed)
        torch.cuda.manual_seed_all(seed)
        torch.backends.cudnn.deterministic = True
        torch.backends.cudnn.benchmark = False
    
    # Also call the set_seed function for consistency
    set_seed(seed)
    
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
    
    # C) NF MODEL VERIFICATION: Sample before training
    flow.eval()
    with torch.no_grad():
        n_samples_before = min(1000, len(residuals))
        samples_before = flow.sample(n_samples_before).numpy()
    
    samples_before_file = output_model_path.replace(".pth", "_samples_before_training.csv")
    pd.DataFrame({"z_nf": samples_before.flatten()}).to_csv(samples_before_file, index=False)
    print(f"Pre-training samples saved to: {samples_before_file}")
    
    # Setup optimizer
    optimizer = torch.optim.Adam(flow.parameters(), lr=config["learning_rate"])
    
    # Training loop
    loss_history = []
    val_loss_history = []
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
            val_loss_history.append(avg_val_loss)
            
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
    
    # C) SAVE TRAINING LOSS HISTORY
    loss_df = pd.DataFrame({
        'epoch': range(1, len(loss_history) + 1),
        'train_loss': loss_history,
        'val_loss': val_loss_history + [np.nan] * (len(loss_history) - len(val_loss_history))
    })
    loss_file = output_model_path.replace(".pth", "_training_loss.csv")
    loss_df.to_csv(loss_file, index=False)
    print(f"Training loss history saved to: {loss_file}")
    
    # Plot loss curve
    plt.figure(figsize=(10, 6))
    plt.plot(loss_df['epoch'], loss_df['train_loss'], label='Train Loss', marker='o', markersize=3)
    if not loss_df['val_loss'].isna().all():
        val_epochs = loss_df['epoch'][~loss_df['val_loss'].isna()]
        val_losses = loss_df['val_loss'][~loss_df['val_loss'].isna()]
        plt.plot(val_epochs, val_losses, label='Val Loss', marker='s', markersize=3)
    plt.xlabel('Epoch')
    plt.ylabel('Negative Log-Likelihood')
    plt.title('NF Training Loss History')
    plt.legend()
    plt.grid(True, alpha=0.3)
    loss_plot_file = output_model_path.replace(".pth", "_training_loss.png")
    plt.savefig(loss_plot_file, dpi=150, bbox_inches='tight')
    plt.close()
    print(f"Training loss plot saved to: {loss_plot_file}")
    
    # C) LOG-LIKELIHOOD COMPARISON: Flow vs Base
    flow.eval()
    base_dist = StandardNormal([1])
    train_tensor = torch.tensor(train_residuals, dtype=torch.float32)
    
    with torch.no_grad():
        log_p_flow = flow(train_tensor).mean().item()
        log_p_base = base_dist.log_prob(train_tensor).mean().item()
    
    delta = log_p_flow - log_p_base
    margin = 0.1
    passes_check = delta > margin
    
    ll_comparison = pd.DataFrame({
        'seed': [seed],
        'log_p_flow': [log_p_flow],
        'log_p_base': [log_p_base],
        'delta': [delta],
        'passes_check': [passes_check],
        'margin': [margin]
    })
    ll_file = output_model_path.replace(".pth", "_ll_comparison.csv")
    ll_comparison.to_csv(ll_file, index=False)
    print(f"Log-likelihood comparison saved to: {ll_file}")
    print(f"  log_p_flow = {log_p_flow:.4f}, log_p_base = {log_p_base:.4f}, delta = {delta:.4f}")
    if passes_check:
        print(f"  [OK] Flow log-likelihood > base + margin ({margin})")
    else:
        print(f"  [WARNING] Flow log-likelihood not sufficiently better than base")
    
    # Generate samples for evaluation
    flow.eval()
    with torch.no_grad():
        n_samples = len(residuals)
        samples = flow.sample(n_samples).numpy()
    
    # Save samples
    samples_file = output_model_path.replace(".pth", "_samples.csv")
    pd.DataFrame({"z_nf": samples.flatten()}).to_csv(samples_file, index=False)
    print(f"Samples saved to: {samples_file}")
    
    # C) VERIFY MODEL CHANGED: Compare before/after samples
    from scipy.stats import ks_2samp
    ks_stat_before_after = ks_2samp(samples_before.flatten(), samples.flatten()[:len(samples_before)])[0]
    print(f"Model verification: KS(before, after) = {ks_stat_before_after:.4f}")
    if ks_stat_before_after < 0.1:
        print(f"  [WARNING] Model may not have changed significantly (KS < 0.1)")
    
    return flow, samples

def main():
    """Main entry point for command-line usage"""
    if len(sys.argv) < 3:
        print("Usage: python train_nf_synthetic.py <residuals_file> <output_model_path> [seed] [--alt-config]")
        sys.exit(1)
    
    residuals_file = sys.argv[1]
    output_model_path = sys.argv[2]
    seed = int(sys.argv[3]) if len(sys.argv) > 3 and sys.argv[3] != '--alt-config' else 123
    alt_config = '--alt-config' in sys.argv
    
    if not os.path.exists(residuals_file):
        print(f"ERROR: Residuals file not found: {residuals_file}")
        sys.exit(1)
    
    flow, samples = train_nf_on_residuals(residuals_file, output_model_path, seed=seed, alt_config=alt_config)
    
    if flow is None:
        print("ERROR: NF training failed")
        sys.exit(1)
    
    print("NF training completed successfully")

if __name__ == "__main__":
    main()

