# Reproduction (reviewers)

## Minimal checklist (commands only)

Run from the **repository root** after placing data under `data/processed/` (see main README).

| Step | Command | Purpose |
|------|---------|---------|
| 1 | `run_chronological.bat /InstallRPackages` | Install R packages into `environment/R_library` (repeat after R upgrades). |
| 2 | `pip install -r environment/requirements.txt` | Python dependencies (once per venv). |
| 3 | Edit `scripts/core/config.R` | `PIPELINE_MODE`, `REPRODUCIBILITY_SEED`, keep **`NF_OPTIMIZED$flow_family <- "maf"`** for the main baseline. |
| 4 | `run_chronological.bat` | Full chronological pipeline; **must** succeed at NF config export → main **MAF** NF training matches `config.R`. |
| 5 | `run_chronological.bat /Reviewer3` | MAF + RealNVP, multi-seed NF + R evaluation under `outputs/reviewer3/`. Requires `outputs/manual/` from step 4. |

**One-shot main + Reviewer 3:** `run_chronological.bat /WithReviewer3`  
**Add TS-CV after that:** `run_both_pipelines.bat /WithReviewer3`

---

## What to rerun (minimal)

Use this if a previous run used a **stale `nf_config.json`** (e.g. RealNVP/wrong seed in main Step 3), or if methodology steps 10–11 failed to find residuals **before** the `evaluation_split_config.R` fix.

### Must rerun — trustworthy MAF main baseline

- Run **`run_chronological.bat`** end-to-end (cleanest: full batch from Step 1).
- *Alternatively:* rerun from **Step 2** (GARCH) onward, or **Step 3** onward only if you accept skipping a fresh GARCH fit and trust existing `outputs/manual/` — most users prefer a **full** or **Step 2+** rerun for consistency.
- After the current pipeline fixes, **Step 3** uses **MAF** and **`REPRODUCIBILITY_SEED`** from `scripts/core/config.R`, and **steps 10–11** read residuals from **`outputs/manual/`**.

### Optional — after a good main run

- **`run_chronological.bat /Reviewer3`** — retrains **MAF + RealNVP** (seeds 123/456/789) and runs the **R evaluation chain** per folder under `outputs/reviewer3/`. The robustness script **restores `nf_config.json`** from `config.R` when it finishes so the next main run stays MAF-aligned.

### RealNVP tables only

If NF training for RealNVP seeds is already done and you only need consolidated Excel:

```text
python scripts/manual/run_reviewer3_full_chain.py --runs realnvp_seed123 realnvp_seed456 realnvp_seed789
```

(Run from repo root; same Python env as NF training.)

### Multiseed MAF tables only (no main chronological)

```text
python scripts/manual/run_reviewer3_full_chain.py --runs maf_seed123 maf_seed456 maf_seed789
```

### Refresh `full_chain_summary.csv` without re-running R

If parallel jobs overwrote the summary or it is stale, rebuild from existing `results/consolidated/*.xlsx`:

```text
python scripts/manual/run_reviewer3_full_chain.py --runs maf_seed123 maf_seed456 maf_seed789 realnvp_seed123 realnvp_seed456 realnvp_seed789 --summary-only
```

List **every** run id you want in the table; the CSV is **rewritten** for that list only (not merged with old rows). Exit codes are **inferred**: `0` if the expected workbook for that step exists, `1` if not. This mode does **not** re-export `nf_config.json` (use a normal full-chain run or `Rscript scripts/utils/export_nf_config_from_r.R` if needed).

### `nf_config.json` after Reviewer3-only work

`run_reviewer3_full_chain.py` **re-exports** `nf_config.json` from `config.R` when it finishes (MAF + paper seed), unless you pass **`--skip-restore-nf-config`**. `run_reviewer3_robustness.py` does the same after NF training. If you ever skip those scripts, run:

```text
Rscript scripts/utils/export_nf_config_from_r.R "%CD%"
```

from the repo root before **`run_chronological.bat`** Step 3.

---

## Prerequisites

- R 4.x, Python 3.8+, data CSV under `data/processed/` as in the main README.
- Steps 1–2 in the table above (or `Rscript environment/install_r_packages.R` instead of the batch for step 1).

## Configuration

Edit **`scripts/core/config.R`**: `PIPELINE_MODE <- "optimized"` (6 assets, article) or `"full"` (13 assets, dissertation). `REPRODUCIBILITY_SEED` and `NF_OPTIMIZED` (including `flow_family = "maf"` for the primary run) apply to the main chronological NF step once `nf_config.json` is exported (done automatically in `run_chronological.bat` after GARCH fitting).

## Main chronological replication (MAF baseline)

The batch file **always** runs `scripts/utils/export_nf_config_from_r.R` before Python NF training so `nf_config.json` matches `scripts/core/config.R` (**`flow_family`**, **`REPRODUCIBILITY_SEED`**, **`NF_OPTIMIZED`**). If export fails, the batch **stops** (it no longer continues with a stale JSON).

**Baseline for “same as original main”:** `PIPELINE_MODE` and data unchanged, **`REPRODUCIBILITY_SEED`** in `config.R` (default **123**), **`NF_OPTIMIZED$flow_family <- "maf"`**. Then:

```bat
run_chronological.bat
```

**Do not treat a past run as the baseline** if the log showed `export_config_to_json failed` and Step 3 logged **RealNVP** or a seed other than `config.R` — that used leftover `nf_config.json`.

**Reviewer 3 vs main:** `outputs/reviewer3/maf_seed<seed>/` with the **same** seed as `REPRODUCIBILITY_SEED` is the multi-seed MAF counterpart (same code path as main, isolated output root). RealNVP + other seeds are **additional** robustness runs, not the replication baseline.

Outputs: `results/chronological/`, GARCH + NF under `outputs/manual/`, and legacy `outputs/chronological/` scaffolding as before.

## MDPI Risks — Reviewer 3 (second flow + multi-seed)

After a successful manual GARCH fit (`outputs/manual/residuals_by_model/`), run the supplement:

```bat
run_chronological.bat /Reviewer3
```

Or run the full main pipeline and the supplement in one go:

```bat
run_chronological.bat /WithReviewer3
```

Or chronological + TS-CV wrapper:

```bat
run_both_pipelines.bat /WithReviewer3
```

This trains **MAF and RealNVP** with seeds **123, 456, 789** into `outputs/reviewer3/<run_id>/`, then runs the same R evaluation chain as the main chronological steps 4–11 per run. Details and archived legacy batch stubs: **`archive/reviewer3_mdpi_risks/README.txt`**.

## Faster NF grid (non–main settings)

```text
python scripts/manual/run_reviewer3_robustness.py --quick
```
