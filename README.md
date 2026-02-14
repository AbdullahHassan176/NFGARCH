# NF-GARCH

Two-stage GARCH with Normalizing Flow innovations. R and Python pipeline.

**Requirements:** R 4.x, Python 3.8+. R packages: xts, rugarch, PerformanceAnalytics, dplyr, openxlsx. Python: `pip install -r environment/requirements.txt`

**Run from repo root:**
- `run_both_pipelines.bat` — main pipeline (chronological then TS-CV)
- `run_chronological.bat` — chronological 65/35 pipeline only
- `run_tscv.bat` — time-series cross-validation pipeline only
- `start_research_dashboard.bat` — open HTML/Excel dashboard
- `run_robustness_garch_order.bat` — GARCH order robustness experiment

Config: `scripts/core/config.R` (PIPELINE_MODE: optimized or full). Results: `results/`, `overleaf_export/`. License: MIT.
