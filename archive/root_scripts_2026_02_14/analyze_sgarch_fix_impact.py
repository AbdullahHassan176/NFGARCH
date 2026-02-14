import pandas as pd

print("="*80)
print("IMPACT OF sGARCH OPTIMIZER FIX")
print("="*80)

# Load comparison
df = pd.read_excel('results/chronological/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx', 
                   sheet_name='Model_Comparison')

print("\n=== Model-by-Model MSE Comparison ===\n")
print(df[['Model', 'Asset', 'Standard_MSE', 'NF_MSE', 'Diff_Pct']].to_string(index=False))

print("\n" + "="*80)
print("BY MODEL TYPE")
print("="*80)

for model in df['Model'].unique():
    subset = df[df['Model'] == model]
    std_mean = subset['Standard_MSE'].mean()
    nf_mean = subset['NF_MSE'].mean()
    diff_pct = ((nf_mean / std_mean) - 1) * 100
    
    print(f"\n{model}:")
    print(f"  Standard MSE: {std_mean:.6f}")
    print(f"  NF-GARCH MSE: {nf_mean:.6f}")
    print(f"  Difference: {diff_pct:+.1f}%")
    print(f"  Winner: {'NF-GARCH' if diff_pct < 0 else 'Standard GARCH'}")

print("\n" + "="*80)
print("OVERALL")
print("="*80)

std_overall = df['Standard_MSE'].mean()
nf_overall = df['NF_MSE'].mean()
diff_overall = ((nf_overall / std_overall) - 1) * 100

print(f"\nStandard GARCH: {std_overall:.6f}")
print(f"NF-GARCH:       {nf_overall:.6f}")
print(f"Difference:     {diff_overall:+.1f}%")
print(f"Winner:         {'NF-GARCH' if diff_overall < 0 else 'Standard GARCH'}")

# Win counts
nf_wins = len(df[df['NF_MSE'] < df['Standard_MSE']])
total = len(df)
print(f"\nNF-GARCH wins on: {nf_wins}/{total} assets ({nf_wins/total*100:.0f}%)")
