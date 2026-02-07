import pandas as pd
import numpy as np

print("="*80)
print("NF-GARCH PERFORMANCE AFTER sGARCH OPTIMIZER FIX")
print("="*80)

# Load combined results
df = pd.read_excel('results/chronological/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx', 
                   sheet_name='Combined_Results')

# Separate NF and Standard
nf_df = df[df['Source'] == 'NF_GARCH'].copy()
std_df = df[df['Source'] == 'Standard'].copy()

# For sGARCH_norm specifically (the one we fixed)
print("\n" + "="*80)
print("sGARCH (Normal) - THE MODEL WE FIXED")
print("="*80)

sgarch_nf = nf_df[(nf_df['Model'] == 'sGARCH') & (nf_df['Distribution'] == 'norm')]
sgarch_std = std_df[(std_df['Model'] == 'sGARCH') & (std_df['Distribution'] == 'norm')]

print("\nAsset-by-Asset sGARCH Comparison:")
print(f"{'Asset':<10} {'Standard MSE':<15} {'NF-GARCH MSE':<15} {'Improvement':<12}")
print("-"*52)

for asset in sgarch_nf['Asset'].values:
    nf_mse = sgarch_nf[sgarch_nf['Asset'] == asset]['MSE'].values[0]
    std_mse = sgarch_std[sgarch_std['Asset'] == asset]['MSE'].values[0]
    improvement = ((nf_mse / std_mse) - 1) * 100
    
    winner = "NF" if nf_mse < std_mse else "Std"
    print(f"{asset:<10} {std_mse:<15.6f} {nf_mse:<15.6f} {improvement:+6.1f}% [{winner}]")

sgarch_nf_mean = sgarch_nf['MSE'].mean()
sgarch_std_mean = sgarch_std['MSE'].mean()
sgarch_improvement = ((sgarch_nf_mean / sgarch_std_mean) - 1) * 100

print("-"*52)
print(f"{'MEAN':<10} {sgarch_std_mean:<15.6f} {sgarch_nf_mean:<15.6f} {sgarch_improvement:+6.1f}%")

print("\n" + "="*80)
print("ALL MODELS (where both Standard and NF exist)")
print("="*80)

# Match models
results = []
for _, nf_row in nf_df.iterrows():
    model = nf_row['Model']
    dist = nf_row['Distribution']
    asset = nf_row['Asset']
    
    std_match = std_df[(std_df['Model'] == model) & 
                       (std_df['Distribution'] == dist) & 
                       (std_df['Asset'] == asset)]
    
    if len(std_match) > 0:
        std_mse = std_match['MSE'].values[0]
        nf_mse = nf_row['MSE']
        
        results.append({
            'Model': model,
            'Asset': asset,
            'Standard_MSE': std_mse,
            'NF_MSE': nf_mse,
            'Improvement_Pct': ((nf_mse / std_mse) - 1) * 100
        })

comp_df = pd.DataFrame(results)

print(f"\nTotal matched comparisons: {len(comp_df)}")
print(f"NF-GARCH wins: {len(comp_df[comp_df['NF_MSE'] < comp_df['Standard_MSE']])} ({len(comp_df[comp_df['NF_MSE'] < comp_df['Standard_MSE']])/len(comp_df)*100:.0f}%)")

print("\n" + "="*80)
print("BY MODEL TYPE")
print("="*80)

for model in comp_df['Model'].unique():
    subset = comp_df[comp_df['Model'] == model]
    std_mean = subset['Standard_MSE'].mean()
    nf_mean = subset['NF_MSE'].mean()
    diff = ((nf_mean / std_mean) - 1) * 100
    nf_wins = len(subset[subset['NF_MSE'] < subset['Standard_MSE']])
    
    print(f"\n{model}:")
    print(f"  Standard:    {std_mean:.6f}")
    print(f"  NF-GARCH:    {nf_mean:.6f}")
    print(f"  Difference:  {diff:+.1f}%")
    print(f"  NF wins:     {nf_wins}/{len(subset)} assets")

print("\n" + "="*80)
print("OVERALL VERDICT")
print("="*80)

overall_std = comp_df['Standard_MSE'].mean()
overall_nf = comp_df['NF_MSE'].mean()
overall_diff = ((overall_nf / overall_std) - 1) * 100

print(f"\nStandard GARCH Mean MSE: {overall_std:.6f}")
print(f"NF-GARCH Mean MSE:       {overall_nf:.6f}")
print(f"Difference:              {overall_diff:+.1f}%")
print(f"\nWinner: {'NF-GARCH' if overall_diff < 0 else 'Standard GARCH'}")

if abs(overall_diff) < 1.0:
    print("\n*** PERFORMANCE IS ESSENTIALLY TIED (< 1% difference) ***")
