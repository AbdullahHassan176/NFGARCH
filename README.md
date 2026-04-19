# NF-GARCH

Two-stage GARCH with Normalizing Flow innovations. R and Python pipeline.

**For reviewers:** Reproduction, pipeline mode, and MDPI Risks Reviewer 3 add-ons are in **`docs/REPRODUCTION_FOR_REVIEWERS.md`**. Use `PIPELINE_MODE = "full"` in `scripts/core/config.R` for 13-asset (dissertation) results; use `"optimized"` for the 6-asset article subset.

**Requirements:** R 4.x, Python 3.8+. R: `run_chronological.bat /InstallRPackages` (or `Rscript environment/install_r_packages.R` from repo root) populates `environment/R_library`. Python: `pip install -r environment/requirements.txt`

**Run from repo root:**
- `run_both_pipelines.bat` — chronological then TS-CV
- `run_both_pipelines.bat /WithReviewer3` — same, but chronological leg also runs the Reviewer 3 supplement (multi-seed MAF + RealNVP under `outputs/reviewer3/`)
- `run_chronological.bat` — full 22-step chronological pipeline (exports `nf_config.json` from R before NF training so the main MAF run matches `scripts/core/config.R`)
- `run_chronological.bat /Reviewer3` — only the Reviewer 3 supplement (expects `outputs/manual/` from a prior GARCH fit)
- `run_chronological.bat /WithReviewer3` — full chronological pipeline, then Reviewer 3 supplement
- `run_tscv.bat` — time-series cross-validation pipeline only
- `start_research_dashboard.bat` — open HTML/Excel dashboard
- `run_robustness_garch_order.bat` — GARCH order robustness experiment

Superseded helper batches live under **`archive/reviewer3_mdpi_risks/`** (reference only).

### Minimal checklist — main MAF replication + MDPI Reviewer 3

From repo root, in order (details: **`docs/REPRODUCTION_FOR_REVIEWERS.md`**):

1. **R packages (once per machine or after R upgrade)**  
   `run_chronological.bat /InstallRPackages`

2. **Python dependencies (once)**  
   `pip install -r environment/requirements.txt`

3. **Edit `scripts/core/config.R`** — set `PIPELINE_MODE` (`"optimized"` = 6-asset article, `"full"` = dissertation). For the paper baseline keep **`NF_OPTIMIZED$flow_family <- "maf"`** and **`REPRODUCIBILITY_SEED`** (default `123`).

4. **Main chronological pipeline (MAF; exports `nf_config.json` then NF training)**  
   `run_chronological.bat`

5. **Reviewer 3 supplement (MAF + RealNVP, seeds 123/456/789, then R eval per run)** — after step 4, or combine 4+5 with **`/WithReviewer3`**:  
   `run_chronological.bat /Reviewer3`  
   One-shot main + supplement: **`run_chronological.bat /WithReviewer3`**.  
   To add TS-CV after that: **`run_both_pipelines.bat /WithReviewer3`** (chronological leg includes Reviewer 3, then `run_tscv.bat`).

**After a bad or old run:** see **What to rerun (minimal)** in **`docs/REPRODUCTION_FOR_REVIEWERS.md`**. Multiseed MAF-only tables: `python scripts/manual/run_reviewer3_full_chain.py --runs maf_seed123 maf_seed456 maf_seed789`. RealNVP-only: same with `realnvp_seed*`. Those scripts re-export **`nf_config.json`** for the next main chronological run unless you pass **`--skip-restore-nf-config`**.

Config: `scripts/core/config.R` (PIPELINE_MODE: `"full"` = 13 assets for dissertation, `"optimized"` = 6 assets for the article subset). Results: `results/`, `results/dissertation_tables/`, `overleaf_export/`. License: MIT.

---

## Data

The pipeline reads price/return data from **`data/processed/raw (FX + EQ).csv`** (row names = dates, columns = asset symbols). Raw data are not in the repo (see `.gitignore`). For full (13-asset) mode, assets are: **6 FX** — EURUSD, GBPUSD, GBPCNY, USDZAR, GBPZAR, EURZAR; **7 equity** — X, NVDA, MSFT, PG, CAT, WMT, AMZN (defined in `scripts/core/config.R`). Provide or create the processed CSV in `data/processed/` before running the pipeline; the dissertation uses the same asset set and date range as produced by your data preparation.

---

## Reproducing dissertation tables and figures

To regenerate all artefacts cited in the thesis (tables are hardcoded in the .tex; these steps recreate the source tables and figures):

1. **Set pipeline mode**  
   In `scripts/core/config.R`, set `PIPELINE_MODE` to the mode used for the submitted dissertation (e.g. `"full"` for 13 assets).

2. **Run the main pipeline**  
   From repo root: `run_both_pipelines.bat` (chronological then TS-CV).  
   For stress and crisis outputs (e.g. crisis forecast table source), also run: `run_stress_then_extract.bat`.

3. **Export dissertation tables**  
   Run `Rscript scripts/evaluation/extract_dissertation_tables.R` to produce all dissertation table sources in `results/dissertation_tables/` (including detailed NF vs Standard, crisis forecast, Wilcoxon, win rate, VaR, distributional, baseline). For stress/crisis only, `run_stress_then_extract.bat` runs stress then this extract. Optionally run `Rscript scripts/evaluation/export_dissertation_tables_disaggregated.R` for per-asset CSVs/TeX. Update the hardcoded tables in the thesis .tex from these outputs when refreshing.

4. **Generate figures**  
   Run `Rscript scripts/evaluation/generate_report_figures.R`. Figures are written to `results/figures/` (e.g. `Fig-R1_stylisedfacts_acf_pacf.png`, `Fig-R2_hist_qq_equity.png`, `Fig-R3_hist_qq_fx.png`, `Fig-R7_stress_gfc_vs_covid.png`). Either use these paths when compiling the thesis or copy them into the dissertation folder.

**Dissertation table sources (for verification or updating the .tex):** After the pipeline and extract, the following files in `results/dissertation_tables/` back the thesis tables. Use them to refresh or audit the hardcoded tables.

| Thesis table | Source file(s) |
|--------------|----------------|
| Baseline (tab:baseline-performance) | From `Final_Dashboard.xlsx` → baseline in extract |
| Wilcoxon (tab:wilcoxon-tests) | `wilcoxon_test_results.csv`, `.tex` |
| **Detailed NF vs Standard (Table 4.5, tab:detailed-nf-vs-standard)** | `detailed_nf_vs_standard.csv`, `detailed_nf_vs_standard.tex` |
| By model (tab:nf-vs-standard-by-model) | `nf_vs_standard_by_model.csv`, `.tex` |
| Asset class (tab:nf_assetclass) | `nf_performance_by_asset_class.csv`, `.tex` |
| Win rate (tab:nf-win-rate) | `nf_win_rate.csv`, `.tex` |
| Distributional (tab:distributional-metrics) | `distributional_metrics_by_model.csv`, `.tex` (from `Distributional_Metrics.xlsx`) |
| VaR (tab:var-backtesting) | `var_backtesting_by_model.csv`, `.tex` |
| Crisis forecast (tab:crisis-forecast) | `crisis_forecast_performance.csv`, `.tex` |
| GARCH order (tab:garch_order_selection) | From robustness run; see `run_robustness_garch_order.bat` |
| Stylised facts | `stylized_facts_summary.csv`, `.tex` (if extracted) |


