# NF-GARCH — AI / maintainer notes

**Purpose:** Two-stage GARCH with normalizing-flow innovations; R evaluation + Python NF training.

**Layout:** `scripts/core/config.R` (`PIPELINE_MODE`: `"full"` vs `"optimized"`). Main run: `run_chronological.bat` (exports `nf_config.json` before NF training) → `results/chronological` + `outputs/manual` NF. Reviewer 3: `run_chronological.bat /Reviewer3` or `/WithReviewer3` → `outputs/reviewer3/` + `scripts/manual/run_reviewer3_*.py`. Legacy root `.bat` stubs: `archive/reviewer3_mdpi_risks/`.

**R packages:** `run_chronological.bat /InstallRPackages` or `Rscript environment/install_r_packages.R` → `environment/R_library`.

**Python:** `pip install -r environment/requirements.txt`. NF: `scripts/manual/manual_nf_training.py`; multi-seed grid: `scripts/manual/run_reviewer3_robustness.py`.

**Reviewer3:** Same `NF_OPTIMIZED` (75/15). `run_reviewer3_robustness.py` and **`run_reviewer3_full_chain.py`** both re-export `nf_config.json` from `config.R` when done (`--skip-restore-nf-config` on full_chain to opt out). **Main baseline:** `run_chronological.bat` runs `export_nf_config_from_r.R` before Step 3; `maf_seed<REPRODUCIBILITY_SEED>` under `outputs/reviewer3/` matches that baseline when residuals are unchanged.

**Reproduction for reviewers:** command checklist in `README.md` (minimal steps) and `docs/REPRODUCTION_FOR_REVIEWERS.md`.

**Conventions:** Match existing script style; avoid drive-by refactors. NF synthetic residuals need not be exactly N(0,1) if diagnostics are acceptable—do not change re-standardization unless requested.
