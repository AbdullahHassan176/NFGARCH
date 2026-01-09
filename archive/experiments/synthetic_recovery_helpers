import pandas as pd

df = pd.read_csv('recovery_metrics.csv')
summary = pd.read_csv('summary_statistics.csv')

print('=== UPDATED RECOVERY METRICS ===\n')
print(df.to_string(index=False))

print('\n\n=== IMPROVEMENT SUMMARY ===\n')
print('BEFORE (Gaussian base):')
print('  KS: 0.2915')
print('  Wasserstein: 0.5880')
print('  Skew Diff: 1.4941')

print('\nAFTER (Student-t base):')
nf_row = df[df['method'] == 'NF_GARCH'].iloc[0]
print(f'  KS: {nf_row["ks_stat"]:.4f} (improved by {((0.2915 - nf_row["ks_stat"]) / 0.2915 * 100):.1f}%)')
print(f'  Wasserstein: {nf_row["wasserstein"]:.4f} (improved by {((0.5880 - nf_row["wasserstein"]) / 0.5880 * 100):.1f}%)')
print(f'  Skew Diff: {nf_row["skewness_diff"]:.4f} (no change)')

print('\n\n=== SUMMARY STATISTICS ===\n')
print(summary.to_string(index=False))

nf_summary = summary[summary['Method'] == 'NF-GARCH'].iloc[0]
true_summary = summary[summary['Method'] == 'True'].iloc[0]

print('\n\n=== NF-GARCH vs TRUE ===\n')
print(f'Skewness: NF={nf_summary["Skewness"]:.4f}, True={true_summary["Skewness"]:.4f}, Diff={abs(nf_summary["Skewness"] - true_summary["Skewness"]):.4f}')
print(f'Kurtosis: NF={nf_summary["Kurtosis"]:.4f}, True={true_summary["Kurtosis"]:.4f}, Diff={abs(nf_summary["Kurtosis"] - true_summary["Kurtosis"]):.4f}')

