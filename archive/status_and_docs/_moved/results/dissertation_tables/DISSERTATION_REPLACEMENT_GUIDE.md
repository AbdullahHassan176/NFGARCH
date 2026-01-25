# Section-by-Section: What to Replace in the Dissertation

For each table below, **delete only the `\begin{tabularx}...\end{tabularx}` block** (or `\begin{tabular}...\end{tabular}` where noted) and **insert** the corresponding `\input{\tablesdir ...}` line. **Keep** `\begin{table}[H]`, `\centering`, `\caption{...}`, `\label{...}`, `\small`, `\setlength{\tabcolsep}{...}`, and any `\footnotetext{...}` that comes after the tabular.

Your preamble already has:
```latex
\newcommand{\tablesdir}{results/dissertation_tables/}
```
For **Overleaf**, use `\newcommand{\tablesdir}{tables/}` and put the `.tex` files in a `tables/` folder.

---

## 1. **Table: Stylized Facts by Asset Class**  
**Section:** Experiments and Results → *Stylized Facts of Return Series*  
**Label:** `tab:stylized-facts`

**Remove this block:**
```latex
\begin{tabularx}{\linewidth}{l *{4}{>{\raggedleft\arraybackslash}X}}
\toprule
Asset Class & Volatility Clustering & Leverage Effect & Gain/Loss Asymmetry & Skewness \\
\midrule
Equity & 1.817 & 0.026 & 0.975 & 0.013 \\
FX     & 2.174 & -0.008 & 0.987 & -0.168 \\
\bottomrule
\end{tabularx}
```

**Insert instead:**
```latex
\input{\tablesdir stylized_facts_summary.tex}
```

**Keep:** `\begin{table}[H]`, `\centering`, `\caption{Stylized Facts by Asset Class}`, `\label{tab:stylized-facts}`, `\small`, `\setlength{\tabcolsep}{5pt}`.

---

## 2. **Table: Baseline GARCH Model Performance**  
**Section:** Experiments and Results → *Baseline GARCH Performance*  
**Label:** `tab:baseline-performance`

**Remove this block:**
```latex
\begin{tabularx}{\linewidth}{l *{6}{>{\raggedleft\arraybackslash}X}}
\toprule
Model & N Assets & Mean MSE & Mean MAE & Mean AIC & Mean BIC & Mean LogLik \\
\midrule
TGARCH   & 6  & 0.000353 & 0.011279 & -25490.14 & -25451.65 & 12751.07 \\
gjrGARCH & 6  & 0.000411 & 0.012823 & -27474.07 & -27435.57 & 13743.03 \\
sGARCH   & 12 & 0.000468 & 0.013524 & -25189.37 & -25160.50 & 12599.18 \\
eGARCH   & 1  & 0.719    & 0.092    & 31837.77  & 31876.26  & -15912.88 \\
\bottomrule
\end{tabularx}
```

**Insert instead:**
```latex
\input{\tablesdir baseline_garch_performance.tex}
```

**Keep:** `\begin{table}[H]`, `\centering`, `\caption{Baseline GARCH Model Performance (Chronological Split)}`, `\label{tab:baseline-performance}`, `\small`, `\setlength{\tabcolsep}{4.5pt}`.

---

## 3. **Table: Overall NF-GARCH vs standard GARCH**  
**Section:** Experiments and Results → *NF-GARCH Forecasting Performance*  
**Label:** `tab:nf-vs-standard-overall`

**Remove this block:**
```latex
\begin{tabularx}{\linewidth}{l *{7}{>{\raggedleft\arraybackslash}X}}
\toprule
Source & N Obs & Mean MSE & Median MSE & Mean MAE & Median MAE & Mean AIC & Mean BIC \\
\midrule
NF\_GARCH & 25 & 0.029  & 0.000319 & 0.0160 & 0.0122 & -23528 & -23494 \\
Standard  & 19 & 3.502  & 0.000191 & 0.0952 & 0.0117 & -23419 & -23382 \\
\bottomrule
\end{tabularx}
```

**Insert instead:**
```latex
\input{\tablesdir nf_vs_standard_overall.tex}
```

**Keep:** `\begin{table}[H]`, `\centering`, `\caption{Overall comparison of NF-GARCH and standard GARCH models...}`, `\label{tab:nf-vs-standard-overall}`, `\small`, `\setlength{\tabcolsep}{4.5pt}`.

---

## 4. **Table: NF-GARCH vs standard by model**  
**Section:** Experiments and Results → *NF-GARCH Forecasting Performance*  
**Label:** `tab:nf-vs-standard-by-model`

**Remove this block:**
```latex
\begin{tabularx}{\linewidth}{l *{5}{>{\raggedleft\arraybackslash}X}}
\toprule
Model & N & NF MSE & Standard MSE & MSE Improvement (\%) & MAE Improvement (\%) \\
\midrule
TGARCH   & 6 & 0.000353 & 0.000560 & 37.0 & 17.9 \\
gjrGARCH & 6 & 0.000411 & 0.000597 & 31.2 & 11.6 \\
eGARCH   & 1 & 0.719000 & 66.522000 & 98.9 & 94.0 \\
\bottomrule
\end{tabularx}
```

**Insert instead:**
```latex
\input{\tablesdir nf_vs_standard_by_model.tex}
```

**Keep:** `\begin{table}[H]`, `\centering`, `\caption{Comparison of NF-GARCH and standard GARCH forecasting performance disaggregated by GARCH specification...}`, `\label{tab:nf-vs-standard-by-model}`, `\small`, `\setlength{\tabcolsep}{4.5pt}`.

---

## 5. **Table: Performance by Asset Class (Median Values)**  
**Section:** Experiments and Results → *NF-GARCH Forecasting Performance*  
**Label:** `tab:nf_assetclass`

**Remove this block:**
```latex
\begin{tabularx}{\linewidth}{l l *{4}{>{\raggedleft\arraybackslash}X}}
\toprule
Asset Class & Source & N Assets & Median MSE & Median MAE & Median AIC \\
\midrule
Equity & Standard  & 3 & 0.001249 & 0.023260 & -21048.57 \\
Equity & NF\_GARCH & 3 & 0.000773 & 0.019640 & -20521.23 \\
FX     & Standard  & 3 & 0.000033 & 0.004516 & -32326.25 \\
FX     & NF\_GARCH & 3 & 0.000053 & 0.005301 & -31509.84 \\
\bottomrule
\end{tabularx}
```

**Insert instead:**
```latex
\input{\tablesdir nf_performance_by_asset_class.tex}
```

**Keep:** `\begin{table}[H]`, `\centering`, `\caption{Performance Summary by Asset Class (Median Values)}`, `\label{tab:nf_assetclass}`, `\small`, `\setlength{\tabcolsep}{5pt}`.

---

## 6. **Table: Win-rate comparison**  
**Section:** Experiments and Results → *NF-GARCH Forecasting Performance*  
**Label:** `tab:nf-win-rate`

**Remove this block:**
```latex
\begin{tabularx}{\linewidth}{l *{3}{>{\centering\arraybackslash}X}}
\toprule
Model & Total Comparisons & NF Wins & Win Rate (\%) \\
\midrule
TGARCH   & 6 & 2 & 33.3 \\
eGARCH   & 1 & 1 & 100.0 \\
gjrGARCH & 6 & 2 & 33.3 \\
sGARCH   & 6 & 2 & 33.3 \\
\bottomrule
\end{tabularx}
```

**Insert instead:**
```latex
\input{\tablesdir nf_win_rate.tex}
```

**Keep:** `\begin{table}[H]`, `\centering`, `\caption{Win-rate comparison of NF-GARCH versus standard GARCH models by GARCH specification...}`, `\label{tab:nf-win-rate}`, `\small`, `\setlength{\tabcolsep}{6pt}`.

---

## 7. **Table: Wilcoxon Signed-Rank Tests**  
**Section:** Experiments and Results → *NF-GARCH Forecasting Performance*  
**Label:** `tab:wilcoxon-tests`

**Remove this block:**
```latex
\begin{tabularx}{\linewidth}{l l *{4}{>{\centering\arraybackslash}X}}
\toprule
Model & Metric & Statistic & P-value & Significant & Alternative \\
\midrule
sGARCH   & MSE (NF < Standard) & 10 & 0.50 & No & less \\
TGARCH   & MSE (NF < Standard) & 10 & 0.50 & No & less \\
gjrGARCH & MSE (NF < Standard) & 10 & 0.50 & No & less \\
\bottomrule
\end{tabularx}
```

**Insert instead:**
```latex
\input{\tablesdir wilcoxon_test_results.tex}
```

**Keep:** `\begin{table}[H]`, `\centering`, `\caption{Wilcoxon Signed-Rank Tests for NF-GARCH vs Standard GARCH}`, `\label{tab:wilcoxon-tests}`, `\small`, `\setlength{\tabcolsep}{5pt}`.

---

## 8. **Table: Distributional metrics (NF vs Standard GARCH)**  
**Section:** Experiments and Results → *Distributional Realism and Stylized-Fact Diagnostics* → *Replication of Standard GARCH Residual Distributions*  
**Label:** `tab:distributional-metrics`

**Remove this block:**
```latex
\begin{tabularx}{\linewidth}{l *{8}{>{\raggedleft\arraybackslash}X}}
\toprule
Model & \multicolumn{2}{c}{KS Distance} & \multicolumn{2}{c}{Wasserstein} & \multicolumn{2}{c}{Tail Index} & \multicolumn{2}{c}{Skewness} \\
\cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7} \cmidrule(lr){8-9}
& Std & NF & Std & NF & Std & NF & Std & NF \\
\midrule
TGARCH   & 0.094 & -- & 0.214 & -- & 2.581 & 4.788 & 0.425 & 0.017 \\
eGARCH   & 0.269 & -- & 0.502 & -- & 1.560 & 4.501 & -1.418 & 0.004 \\
gjrGARCH & 0.074 & -- & 0.149 & -- & 3.135 & 4.739 & 0.199 & 0.004 \\
sGARCH   & 0.073 & -- & 0.159 & -- & 2.976 & 4.650 & 0.458 & 0.018 \\
\bottomrule
\end{tabularx}
\footnotetext{KS Distance and Wasserstein Distance are single values measuring...}
```

**Insert instead:**
```latex
\input{\tablesdir distributional_metrics_by_model.tex}
\footnotetext{KS Distance and Wasserstein Distance are single values measuring the difference between Standard GARCH residuals and NF-generated innovations (shown in the ``Std'' column). Tail Index and Skewness are properties of each distribution separately, reported for both Standard GARCH residuals (Std) and NF-generated innovations (NF). Note that NF-generated innovations show near-zero skewness (0.004-0.018) compared to Standard GARCH residuals, suggesting NF may struggle to capture asymmetry despite training on skewed data.}
```

**Keep:** `\begin{table}[H]`, `\centering`, `\caption{Comparison of NF-generated innovations to Standard GARCH residuals...}`, `\label{tab:distributional-metrics}`, `\small`, `\setlength{\tabcolsep}{4pt}`.

**Note:** The generated `.tex` does not include `\footnotetext`. Re-insert the `\footnotetext{...}` as above (or your preferred wording) after `\input{...}`.

---

## 9. **Table: VaR Backtesting Results**  
**Section:** Experiments and Results → *Risk Calibration: VaR Backtesting*  
**Label:** `tab:var-backtesting`

**Remove this block:**
```latex
\begin{tabularx}{\linewidth}{l l *{5}{>{\raggedleft\arraybackslash}X}}
\toprule
Model & Conf Level & N Assets & Observed Rate & Expected Rate & Kupiec p-value & Christoffersen p-value \\
\midrule
TGARCH   & 0.95 & 6 & 0.0506 & 0.05 & 1.00 & 1.00 \\
... (all 8 rows)
sGARCH   & 0.99 & 6 & 0.0101 & 0.01 & 1.00 & 1.00 \\
\bottomrule
\end{tabularx}
```

**Insert instead:**
```latex
\input{\tablesdir var_backtesting_by_model.tex}
```

**Keep:** `\begin{table}[H]`, `\centering`, `\caption{VaR Backtesting Results by Model and Confidence Level}`, `\label{tab:var-backtesting}`, `\small`, `\setlength{\tabcolsep}{4.5pt}`.

---

## 10. **Table: Forecast performance during crises (GFC vs COVID-19)**  
**Section:** Experiments and Results → *Stress Testing and Robustness*  
**Label:** `tab:crisis-forecast`

**Remove this block:**
```latex
\begin{tabularx}{\linewidth}{l l r *{3}{>{\raggedleft\arraybackslash}X}}
\toprule
Crisis & Model & N & NF MSE & Standard MSE & MSE Improvement (\%) \\
\midrule
GFC 2008  & TGARCH   & 6 & 0.00152 & 0.00165 & 5.80 \\
...
COVID 2020 & sGARCH  & 6 & 0.00151 & 0.00141 & -22.48 \\
\bottomrule
\end{tabularx}
```

**Insert instead:**
```latex
\input{\tablesdir crisis_forecast_performance.tex}
```

**Keep:** `\begin{table}[H]`, `\centering`, `\caption{Forecast Performance During Historical Crises: GFC 2008 vs COVID-19 2020}`, `\label{tab:crisis-forecast}`, `\small`, `\setlength{\tabcolsep}{4.5pt}`.

---

## Tables to leave as-is (no `\input`)

- **tab:empirical-comparison-summary** (4.9)  
- **tab:empirical-comparison-detailed** (4.10)  
- **tab:dist_summary** (4.11)  
- **tab:forecast-crisis-model** (4.14)  

These are not yet produced by `extract_dissertation_tables.R`. Keep their current `\begin{tabularx}...\end{tabularx}` (or `\begin{tabular}...\end{tabular}`) blocks.

---

## Optional: GARCH order selection (Table 4.15)

**Label in text:** `tab:garch_order_selection`  
**Source:** `outputs/robust_garch_order/garch_order_robustness_table.tex` (also copied to `overleaf_export/tables/` as `garch_order_robustness_table.tex`).

That file contains the **entire** `\begin{table}[h]...\end{table}` (including `\caption` and `\label{tab:garch_order_robustness}`). To use it:

1. **Replace** the full `\begin{table}[h]` … `\end{table}` block for the GARCH order selection table with:
   ```latex
   \input{\tablesdir garch_order_robustness_table.tex}
   ```
2. Ensure `garch_order_robustness_table.tex` is in `\tablesdir` (e.g. copy from `outputs/robust_garch_order/` into `results/dissertation_tables/`, or rely on `overleaf_export/tables/` when building for Overleaf).
3. The included file uses `\label{tab:garch_order_robustness}`. If you cite this table, change `\ref{tab:garch_order_selection}` to `\ref{tab:garch_order_robustness}` (or edit the robustness script to emit `\label{tab:garch_order_selection}`).

---

## Summary checklist

| # | Label | Replace tabular with |
|---|-------|----------------------|
| 1 | `tab:stylized-facts` | `\input{\tablesdir stylized_facts_summary.tex}` |
| 2 | `tab:baseline-performance` | `\input{\tablesdir baseline_garch_performance.tex}` |
| 3 | `tab:nf-vs-standard-overall` | `\input{\tablesdir nf_vs_standard_overall.tex}` |
| 4 | `tab:nf-vs-standard-by-model` | `\input{\tablesdir nf_vs_standard_by_model.tex}` |
| 5 | `tab:nf_assetclass` | `\input{\tablesdir nf_performance_by_asset_class.tex}` |
| 6 | `tab:nf-win-rate` | `\input{\tablesdir nf_win_rate.tex}` |
| 7 | `tab:wilcoxon-tests` | `\input{\tablesdir wilcoxon_test_results.tex}` |
| 8 | `tab:distributional-metrics` | `\input{\tablesdir distributional_metrics_by_model.tex}` + `\footnotetext{...}` |
| 9 | `tab:var-backtesting` | `\input{\tablesdir var_backtesting_by_model.tex}` |
| 10 | `tab:crisis-forecast` | `\input{\tablesdir crisis_forecast_performance.tex}` |

After re-running `extract_dissertation_tables.R` (or the full pipeline), recompile the dissertation; the tables will reflect the latest results.
