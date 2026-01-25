# Dissertation Table `\input{}` Reference

Use these `\input{}` calls in your main dissertation `.tex` to pull generated table bodies from `results/dissertation_tables/`. Each generated `.tex` contains **only** the `\begin{tabularx}...\end{tabularx}` block. Wrap with `\begin{table}[H]`, `\centering`, `\caption{...}`, `\label{...}` in your main file.

## Required in preamble

```latex
\newcommand{\tablesdir}{results/dissertation_tables/}
% Or for Overleaf when tables are in project root: \newcommand{\tablesdir}{tables/}
```

## Substitutions (replace the tabular block only)

| Label | Caption (keep as is) | Replace tabular with |
|-------|----------------------|----------------------|
| `tab:stylized-facts` | Stylized Facts by Asset Class | `\input{\tablesdir stylized_facts_summary.tex}` |
| `tab:baseline-performance` | Baseline GARCH Model Performance (Chronological Split) | `\input{\tablesdir baseline_garch_performance.tex}` |
| `tab:nf-vs-standard-overall` | Overall comparison of NF-GARCH and standard GARCH… | `\input{\tablesdir nf_vs_standard_overall.tex}` |
| `tab:nf-vs-standard-by-model` | Comparison… by GARCH specification | `\input{\tablesdir nf_vs_standard_by_model.tex}` |
| `tab:nf_assetclass` | Performance Summary by Asset Class (Median Values) | `\input{\tablesdir nf_performance_by_asset_class.tex}` |
| `tab:nf-win-rate` | Win-rate comparison… | `\input{\tablesdir nf_win_rate.tex}` |
| `tab:wilcoxon-tests` | Wilcoxon Signed-Rank Tests… | `\input{\tablesdir wilcoxon_test_results.tex}` |
| `tab:distributional-metrics` | Comparison of NF-generated innovations to Standard GARCH residuals | `\input{\tablesdir distributional_metrics_by_model.tex}` |
| `tab:var-backtesting` | VaR Backtesting Results by Model and Confidence Level | `\input{\tablesdir var_backtesting_by_model.tex}` |
| `tab:crisis-forecast` | Forecast Performance During Historical Crises | `\input{\tablesdir crisis_forecast_performance.tex}` |

## Example (Table 4.1)

**Before (hardcoded):**
```latex
\begin{table}[H]
\centering
\caption{Stylized Facts by Asset Class}
\label{tab:stylized-facts}
\small
\setlength{\tabcolsep}{5pt}
\begin{tabularx}{\linewidth}{l *{4}{>{\raggedleft\arraybackslash}X}}
\toprule
Asset Class & Volatility Clustering & ... \\
\midrule
Equity & 1.817 & ... \\
...
\end{tabularx}
\end{table}
```

**After:**
```latex
\begin{table}[H]
\centering
\caption{Stylized Facts by Asset Class}
\label{tab:stylized-facts}
\small
\setlength{\tabcolsep}{5pt}
\input{\tablesdir stylized_facts_summary.tex}
\end{table}
```

## Overleaf

- `run_full_dissertation.bat` (or `run_full_dissertation.bat /OverleafOnly`) copies `results/dissertation_tables/*.tex` (and `*.*`) to `overleaf_export/tables/`.
- In Overleaf, place those files in a `tables/` folder and set:  
  `\newcommand{\tablesdir}{tables/}`  
  then use `\input{\tablesdir stylized_facts_summary.tex}` etc.

## Regenerating

Run the pipeline (or at least `scripts/evaluation/extract_dissertation_tables.R`) to refresh CSVs and `.tex` in `results/dissertation_tables/`. Recompile the dissertation; no manual table edits needed.

## Tables still manual

- **tab:empirical-comparison-summary**, **tab:empirical-comparison-detailed**, **tab:dist_summary** (4.9–4.11): from distributional comparison to empirical test residuals; add export in `extract_dissertation_tables.R` or `calculate_distributional_metrics.R` if that data is written to CSV/Excel.
- **tab:forecast-crisis-model** (4.14): asset-averaged crisis forecast; can be derived from `crisis_forecast_performance` with different grouping if needed.
- **tab:garch_order_selection** (4.15): produced by `scripts/experiments/robustness_garch_order.R` → `outputs/robust_garch_order/garch_order_robustness_table.tex`; that script already emits `.tex`; copy to `overleaf_export/tables/` is in `run_full_dissertation.bat` Step 4.
