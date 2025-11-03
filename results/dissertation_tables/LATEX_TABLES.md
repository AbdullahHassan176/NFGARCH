# LaTeX Tables for Dissertation Chapter 4

All tables extracted from results files. Use these values directly in your LaTeX document.

## Table 1: Stylised Facts by Asset Class

**Source**: `stylized_facts_summary.csv`

```latex
\begin{table}[H]
\centering
\caption{Stylised Facts Summary by Asset Class}
\label{tab:stylized-facts}
\begin{tabular}{lrrrr}
\toprule
Asset Class & Volatility Clustering & Leverage Effect & Gain/Loss Asymmetry & Skewness \\
\midrule
Equity & 1.817 & 0.026 & 0.975 & 0.013 \\
FX     & 2.174 & -0.008 & 0.987 & -0.168 \\
\bottomrule
\end{tabular}
\end{table}
```

## Table 2: Baseline GARCH Model Performance

**Source**: `baseline_garch_performance.csv`

```latex
\begin{table}[H]
\centering
\caption{Baseline GARCH Model Performance (Chronological Split)}
\label{tab:baseline-performance}
\begin{tabular}{lrrrrrr}
\toprule
Model & N Assets & Mean MSE & Mean MAE & Mean AIC & Mean BIC & Mean LogLik \\
\midrule
TGARCH   & 6  & 0.000353 & 0.011279 & -25490.14 & -25451.65 & 12751.07 \\
gjrGARCH & 6  & 0.000411 & 0.012823 & -27474.07 & -27435.57 & 13743.03 \\
sGARCH   & 12 & 0.000468 & 0.013524 & -25189.37 & -25160.50 & 12599.18 \\
eGARCH   & 1  & 0.719    & 0.092    & 31837.77  & 31876.26  & -15912.88 \\
\bottomrule
\end{tabular}
\end{table}
```

## Table 3: Overall NF-GARCH vs Standard GARCH Comparison

**Source**: `nf_vs_standard_overall.csv`

```latex
\begin{table}[H]
\centering
\caption{Overall NF-GARCH vs Standard GARCH Performance}
\label{tab:nf-vs-standard-overall}
\begin{tabular}{lrrrrrrrr}
\toprule
Source & N Obs & Mean MSE & Median MSE & Mean MAE & Median MAE & Mean AIC & Mean BIC \\
\midrule
NF\_GARCH & 25 & 0.029 & 0.000319 & 0.0160 & 0.0122 & -23528.80 & -23494.92 \\
Standard  & 19 & 3.502 & 0.000191 & 0.0952 & 0.0117 & -23419.17 & -23382.71 \\
\bottomrule
\end{tabular}
\end{table}
```

**Note**: The mean MSE values differ significantly due to outliers. Use median values for comparison.

## Table 4: NF vs Standard by Model

**Source**: `nf_vs_standard_by_model.csv`

**Check the CSV file directly** for model-by-model breakdown with MSE/MAE/AIC improvements.

## Table 5: NF-GARCH Win Rate

**Source**: `nf_win_rate.csv`

```latex
\begin{table}[H]
\centering
\caption{NF-GARCH Win Rate by Model}
\label{tab:nf-win-rate}
\begin{tabular}{lrrr}
\toprule
Model & Total Comparisons & NF Wins & Win Rate (\%) \\
\midrule
TGARCH   & 6 & 2 & 33.3 \\
eGARCH   & 1 & 1 & 100.0 \\
gjrGARCH & 6 & 2 & 33.3 \\
sGARCH   & 6 & 2 & 33.3 \\
\bottomrule
\end{tabular}
\end{table}
```

## Table 6: Wilcoxon Signed-Rank Test Results

**Source**: `wilcoxon_test_results.csv`

```latex
\begin{table}[H]
\centering
\caption{Wilcoxon Signed-Rank Test: Statistical Significance of NF-GARCH Performance}
\label{tab:wilcoxon-test}
\begin{tabular}{llrrlr}
\toprule
Model & Test Type & Statistic & P-value & Significant & Alternative \\
\midrule
sGARCH   & MSE (NF < Standard) & 10 & 0.50 & No  & less \\
TGARCH   & MSE (NF < Standard) & 10 & 0.50 & No  & less \\
gjrGARCH & MSE (NF < Standard) & 10 & 0.50 & No  & less \\
\bottomrule
\end{tabular}
\end{table}
```

**Interpretation**: None of the models show statistically significant improvement (all p-values = 0.50, not significant at α = 0.05).

## Table 7: Distributional Metrics by Model

**Source**: `distributional_metrics_by_model.csv`

```latex
\begin{table}[H]
\centering
\caption{Distributional Metrics: NF vs Standard Residuals}
\label{tab:distributional-metrics}
\begin{tabular}{lrrrrrrr}
\toprule
Model & Mean KS & Mean Wasserstein & Mean Tail Index & Mean Skewness & Mean Kurtosis \\
\midrule
TGARCH   & 0.095 & 0.216 & 2.590 & 0.425 & 27.86 \\
eGARCH  & 0.143 & 0.299 & 2.404 & -0.011 & 79.01 \\
gjrGARCH & 0.074 & 0.147 & 3.106 & 0.199 & 13.02 \\
sGARCH  & 0.072 & 0.160 & 2.975 & 0.458 & 15.52 \\
\bottomrule
\end{tabular}
\end{table}
```

## Table 8: VaR Backtesting Results

**Source**: `var_backtesting_by_model.csv`

```latex
\begin{table}[H]
\centering
\caption{VaR Backtesting Results by Model and Confidence Level}
\label{tab:var-backtesting}
\begin{tabular}{lrrrrrr}
\toprule
Model & Conf Level & N Assets & Observed Rate & Expected Rate & Kupiec p-value & Christoffersen p-value \\
\midrule
TGARCH   & 0.95 & 6 & 0.0506 & 0.05 & 1.00 & 1.00 \\
TGARCH   & 0.99 & 6 & 0.0101 & 0.01 & 1.00 & 1.00 \\
eGARCH  & 0.95 & 6 & 0.0506 & 0.05 & 1.00 & 1.00 \\
eGARCH  & 0.99 & 6 & 0.0101 & 0.01 & 1.00 & 1.00 \\
gjrGARCH & 0.95 & 6 & 0.0506 & 0.05 & 1.00 & 1.00 \\
gjrGARCH & 0.99 & 6 & 0.0101 & 0.01 & 1.00 & 1.00 \\
sGARCH  & 0.95 & 6 & 0.0506 & 0.05 & 1.00 & 1.00 \\
sGARCH  & 0.99 & 6 & 0.0101 & 0.01 & 1.00 & 1.00 \\
\bottomrule
\end{tabular}
\end{table}
```

## Table 9: Crisis Forecast Performance (GFC vs COVID)

**Source**: `crisis_forecast_performance.csv`

```latex
\begin{table}[H]
\centering
\caption{Forecast Performance During Historical Crises: GFC 2008 vs COVID-19 2020}
\label{tab:crisis-forecast}
\begin{tabular}{llrrrrr}
\toprule
Crisis & Model & N & NF MSE & Standard MSE & MSE Improvement (\%) \\
\midrule
GFC 2008  & TGARCH   & 6 & 0.00152 & 0.00165 & 5.80 \\
GFC 2008  & gjrGARCH & 6 & 0.00159 & 0.00167 & 1.47 \\
GFC 2008  & sGARCH   & 6 & 0.00161 & 0.00167 & 4.47 \\
COVID 2020 & TGARCH   & 6 & 0.00137 & 0.00149 & 8.07 \\
COVID 2020 & gjrGARCH & 6 & 0.00131 & 0.00128 & -4.39 \\
COVID 2020 & sGARCH   & 6 & 0.00151 & 0.00141 & -22.48 \\
\bottomrule
\end{tabular}
\end{table}
```

## Figures Reference

All figures are located in `results/figures/`:

1. `Fig-R1_stylisedfacts_acf_pacf.png` - ACF/PACF of squared returns
2. `Fig-R2_hist_qq_equity.png` - Residual histogram + QQ (equity)
3. `Fig-R3_hist_qq_fx.png` - Residual histogram + QQ (FX)
4. `Fig-R4_nf_vs_garch_resqq_equity.png` - NF vs GARCH QQ (equity)
5. `Fig-R5_nf_vs_garch_resqq_fx.png` - NF vs GARCH QQ (FX)
6. `Fig-R7_stress_gfc_vs_covid.png` - Crisis-period error comparison
7. `Fig-R8_nf_winrate_bars.png` - NF win rate bar chart

## Notes

1. **Mean vs Median**: For MSE/MAE comparisons, use median values when mean values are skewed by outliers (e.g., eGARCH).
2. **Statistical Significance**: None of the Wilcoxon tests show significant differences (all p = 0.50).
3. **Win Rates**: NF-GARCH shows mixed results - best for eGARCH (100%) but only 33% for other models.
4. **Crisis Performance**: NF-GARCH shows improvement during GFC but mixed results during COVID.

