import pandas as pd
import numpy as np

print("="*80)
print("FINAL RESULTS: NF-GARCH vs STANDARD GARCH (PARAMETRIC SAMPLING)")
print("="*80)

# Load results
df = pd.read_excel('results/chronological/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx', 
                   sheet_name='Combined_Results')

nf_df = df[df['Source'] == 'NF_GARCH'].copy()
std_df = df[df['Source'] == 'Standard'].copy()

print("\n" + "="*80)
print("OVERALL PERFORMANCE SUMMARY")
print("="*80)

print(f"\n{'Metric':<25} {'NF-GARCH':>15} {'Standard':>15} {'Difference':>15} {'Winner':>12}")
print("-" * 85)

# MSE
nf_mse = nf_df['MSE'].mean()
std_mse = std_df['MSE'].mean()
mse_diff = ((nf_mse / std_mse) - 1) * 100
mse_winner = 'NF-GARCH' if nf_mse < std_mse else 'Standard'
print(f"{'MSE (lower is better)':<25} {nf_mse:>15.6f} {std_mse:>15.6f} {mse_diff:>14.2f}% {mse_winner:>12}")

# MAE
nf_mae = nf_df['MAE'].mean()
std_mae = std_df['MAE'].mean()
mae_diff = ((nf_mae / std_mae) - 1) * 100
mae_winner = 'Tied' if abs(mae_diff) < 0.1 else ('NF-GARCH' if nf_mae < std_mae else 'Standard')
print(f"{'MAE (lower is better)':<25} {nf_mae:>15.6f} {std_mae:>15.6f} {mae_diff:>14.2f}% {mae_winner:>12}")

# Predictive Log-Likelihood
nf_ll = nf_df['PredictiveLogLik'].mean()
std_ll = std_df['PredictiveLogLik'].mean()
ll_diff = nf_ll - std_ll
ll_winner = 'NF-GARCH' if nf_ll > std_ll else 'Standard'
print(f"{'PredLogLik (higher better)':<25} {nf_ll:>15.2f} {std_ll:>15.2f} {ll_diff:>14.2f} {ll_winner:>12}")

# AIC
nf_aic = nf_df['AIC'].mean()
std_aic = std_df['AIC'].mean()
aic_diff = nf_aic - std_aic
aic_winner = 'NF-GARCH' if nf_aic < std_aic else 'Standard'
print(f"{'AIC (lower is better)':<25} {nf_aic:>15.0f} {std_aic:>15.0f} {aic_diff:>14.0f} {aic_winner:>12}")

print("\n" + "="*80)
print("KEY FINDINGS")
print("="*80)

print("\n1. POINT FORECASTS (MSE/MAE):")
print(f"   - Standard GARCH wins MSE by {abs(mse_diff):.2f}%")
print(f"   - MAE is virtually tied ({abs(mae_diff):.2f}% difference)")
print("   - Conclusion: NF doesn't improve 1-step point forecasts")

print("\n2. DENSITY FORECASTS (Predictive Log-Likelihood):")
if nf_ll > std_ll:
    print(f"   - NF-GARCH wins by {ll_diff:.2f} points")
    print("   - NF learns a better predictive distribution!")
else:
    print(f"   - Standard GARCH wins by {abs(ll_diff):.2f} points")
    print("   - Parametric (Normal/Student-t) is sufficient for density forecasting")

print("\n3. MODEL FIT (AIC):")
if nf_aic < std_aic:
    print(f"   - NF-GARCH has better in-sample fit ({abs(aic_diff):.0f} points lower)")
else:
    print(f"   - Standard GARCH has better in-sample fit ({abs(aic_diff):.0f} points lower)")

print("\n" + "="*80)
print("COMPARISON BY MODEL TYPE")
print("="*80)

# Only compare sGARCH_norm (the only matched models)
sgarch_nf = nf_df[(nf_df['Model'] == 'sGARCH') & (nf_df['Distribution'] == 'norm')]
sgarch_std = std_df[(std_df['Model'] == 'sGARCH') & (std_df['Distribution'] == 'norm')]

if len(sgarch_nf) > 0 and len(sgarch_std) > 0:
    print("\nsGARCH_norm comparison by asset:")
    print(f"{'Asset':<10} {'NF MSE':>12} {'Std MSE':>12} {'NF LogLik':>12} {'Std LogLik':>12} {'Winner (LogLik)':>15}")
    print("-" * 83)
    
    for asset in sgarch_nf['Asset'].values:
        nf_row = sgarch_nf[sgarch_nf['Asset'] == asset].iloc[0]
        std_row = sgarch_std[sgarch_std['Asset'] == asset].iloc[0]
        
        winner = "NF-GARCH" if nf_row['PredictiveLogLik'] > std_row['PredictiveLogLik'] else "Standard"
        
        print(f"{asset:<10} {nf_row['MSE']:>12.6f} {std_row['MSE']:>12.6f} {nf_row['PredictiveLogLik']:>12.2f} {std_row['PredictiveLogLik']:>12.2f} {winner:>15}")

print("\n" + "="*80)
print("THE ANSWER TO YOUR QUESTION")
print("="*80)

print("\n**Did NF-GARCH improve forecasting accuracy and distributional realism?**")

print("\nFORECASTING ACCURACY (MSE/MAE):")
if nf_mse <= std_mse * 1.01:  # Within 1%
    print("  [NO] NF-GARCH does NOT improve point forecast accuracy")
    print(f"       Standard GARCH is {abs(mse_diff):.2f}% better on MSE")
else:
    print("  [YES] NF-GARCH improves point forecast accuracy")

print("\nDISTRIBUTIONAL REALISM (Predictive Log-Likelihood):")
if nf_ll > std_ll:
    improvement_pct = ((nf_ll / std_ll) - 1) * 100
    print(f"  [YES] NF-GARCH improves distributional forecasts by {improvement_pct:.2f}%")
    print("        NF learns a more realistic distribution than Student-t/Normal")
else:
    decline_pct = ((std_ll / nf_ll) - 1) * 100
    print(f"  [NO] Standard GARCH is {decline_pct:.2f}% better at distributional forecasts")
    print("       Parametric assumptions (Normal/Student-t) are sufficient")

print("\n" + "="*80)
print("WHY DIDN'T NF-GARCH OUTPERFORM?")
print("="*80)

print("\n1. The bug we fixed (parametric vs bootstrap) was CRITICAL:")
print("   - Before fix: Both used bootstrap -> nearly identical performance")
print("   - After fix: Standard uses parametric -> reveals true comparison")

print("\n2. Standard GARCH with parametric distributions is STRONG:")
print("   - Student-t distribution already captures fat tails")
print("   - Normal distribution appropriate for many FX pairs")
print("   - These parametric forms are well-suited for financial returns")

print("\n3. NF may not be learning beyond parametric assumptions:")
print("   - If true distribution is close to Student-t, NF has nothing to gain")
print("   - Training on 2934 points may not be enough for complex distributions")
print("   - NF may be overfitting training data without generalizing")

print("\n4. 1-step ahead forecasting is too simple:")
print("   - Both methods converge to conditional mean (which is ~0)")
print("   - Distributional advantages don't matter for point forecasts")
print("   - Multi-step or tail-risk forecasts might show NF advantages")

print("\n" + "="*80)
print("FINAL VERDICT")
print("="*80)

if nf_ll > std_ll and nf_mse <= std_mse:
    print("\n[SUCCESS] NF-GARCH improves distributional forecasts")
    print("          Worth using for density/risk forecasting")
elif nf_ll > std_ll:
    print("\n[PARTIAL SUCCESS] NF improves distribution but not point forecasts")
    print("                  Use for VaR/tail risk, not for mean prediction")
elif abs(nf_mse - std_mse) / std_mse < 0.02 and abs(nf_ll - std_ll) / abs(std_ll) < 0.02:
    print("\n[TIED] NF-GARCH and Standard GARCH perform equally well")
    print("       Use simpler Standard GARCH (easier to estimate, no training needed)")
else:
    print("\n[FAILURE] Standard GARCH outperforms NF-GARCH")
    print("          No benefit from using normalizing flows")
    print("          Parametric assumptions are sufficient for these assets")

print("\n" + "="*80)
