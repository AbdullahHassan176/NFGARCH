#!/usr/bin/env python3
"""
Reviewer 3 robustness: MAF and RealNVP, each with multiple seeds (default 123,456,789).

Prerequisites (once):
  - PIPELINE_MODE <- "optimized" in scripts/core/config.R (6 article assets)
  - Rscript scripts/manual/manual_garch_fitting.R   # writes outputs/manual/residuals_by_model

Then run from repo root (default = same NF settings as main chronological / scripts/core/config.R
NF_OPTIMIZED: 75 epochs, patience 15, validation_split 0.2, for each seed and both flow families):
  python scripts/manual/run_reviewer3_robustness.py

Faster iteration only (40 epochs, patience 8 — not main-aligned):
  python scripts/manual/run_reviewer3_robustness.py --quick

MAF-only refresh (seed 123), same main settings:
  python scripts/manual/run_reviewer3_robustness.py --maf-seeds 123 --skip-realnvp

Outputs under outputs/reviewer3/:
  maf_seed123/, ..., realnvp_seed123/, ...
  robustness_nf_runs_summary.csv

Full downstream chain: run_chronological.bat /Reviewer3 runs this script then run_reviewer3_full_chain.py, or invoke python scripts/manual/run_reviewer3_full_chain.py directly.
"""

from __future__ import annotations

import argparse
import csv
import glob
import json
import os
import shutil
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
NF_CONFIG_PATH = REPO_ROOT / "scripts" / "core" / "nf_config.json"
TRAIN_SCRIPT = REPO_ROOT / "scripts" / "manual" / "manual_nf_training.py"
OUT_BASE = REPO_ROOT / "outputs" / "reviewer3"

OPTIMIZED_ASSETS = {
    "fx": ["EURUSD", "GBPUSD", "USDZAR"],
    "equity": ["NVDA", "MSFT", "AMZN"],
    "all_assets": ["EURUSD", "GBPUSD", "USDZAR", "NVDA", "MSFT", "AMZN"],
}


def nf_config_optimized(*, quick: bool, seed: int, flow_family: str) -> dict:
    """Match scripts/core/config.R NF_OPTIMIZED when quick=False; quick=True uses shorter training."""
    epochs = 40 if quick else 75
    patience = 8 if quick else 15
    return {
        "pipeline_mode": "optimized",
        "reproducibility_seed": seed,
        "assets": OPTIMIZED_ASSETS,
        "nf_config": {
            "epochs": epochs,
            "batch_size": 512,
            "learning_rate": 0.001,
            "early_stopping": True,
            "patience": patience,
            "min_delta": 1e-4,
            "validation_split": 0.2,
            "validation_frequency": 5,
            "num_layers": 4,
            "hidden_features": 64,
            "flow_family": flow_family,
            "realnvp_blocks_per_layer": 2,
            "gradient_checkpointing": True,
            "mixed_precision": True,
            "clear_cache": True,
            "dropout": None,
            "batch_norm": None,
            "residual_connections": None,
            "gradient_clipping": None,
            "weight_decay": None,
            "lr_scheduler": None,
            "warmup_epochs": None,
        },
        "output_paths": {
            "nf_models": "outputs/manual/nf_models",
            "residuals": "outputs/manual/residuals_by_model",
            "garch_fitting": "outputs/manual/garch_fitting",
        },
    }


def write_nf_config(cfg: dict) -> None:
    NF_CONFIG_PATH.parent.mkdir(parents=True, exist_ok=True)
    def _strip_null(d):
        if isinstance(d, dict):
            return {k: _strip_null(v) for k, v in d.items() if v is not None}
        if isinstance(d, list):
            return [_strip_null(x) for x in d]
        return d

    with open(NF_CONFIG_PATH, "w", encoding="utf-8") as f:
        json.dump(_strip_null(cfg), f, indent=2)


def find_rscript() -> str | None:
    for name in ("Rscript.exe", "Rscript"):
        p = shutil.which(name)
        if p:
            return p
    for base in (r"C:\Program Files\R", r"C:\Program Files (x86)\R"):
        if os.path.isdir(base):
            found = sorted(glob.glob(os.path.join(base, "R-*", "bin", "Rscript.exe")), reverse=True)
            if found:
                return found[0]
    return None


def restore_main_nf_config_json() -> None:
    """Reset scripts/core/nf_config.json from config.R so the next main run is MAF + paper seed."""
    export_r = REPO_ROOT / "scripts" / "utils" / "export_nf_config_from_r.R"
    if not export_r.is_file():
        return
    rscript = find_rscript()
    if not rscript:
        print("[WARNING] Rscript not found; re-run: Rscript scripts/utils/export_nf_config_from_r.R <repo_root>")
        return
    p = subprocess.run([rscript, str(export_r), str(REPO_ROOT)], cwd=str(REPO_ROOT))
    if p.returncode != 0:
        print("[WARNING] Could not re-export nf_config.json from config.R after Reviewer3.")
    else:
        print("[OK] Restored scripts/core/nf_config.json from config.R (main MAF baseline).")


def run_one_training(run_id: str, cfg: dict) -> None:
    write_nf_config(cfg)
    out_root = OUT_BASE / run_id
    out_root.mkdir(parents=True, exist_ok=True)
    env = os.environ.copy()
    env["NF_OUTPUT_ROOT"] = str(out_root)
    env["NF_RESIDUALS_DIR"] = str(REPO_ROOT / "outputs" / "manual" / "residuals_by_model")
    cmd = [sys.executable, str(TRAIN_SCRIPT)]
    print("\n===", run_id, "===\n", cmd, "NF_OUTPUT_ROOT=", out_root, sep="")
    r = subprocess.run(cmd, cwd=str(REPO_ROOT), env=env)
    if r.returncode != 0:
        raise SystemExit(f"NF training failed for {run_id} (exit {r.returncode})")


def aggregate_summaries(run_ids: list[str]) -> Path:
    rows = []
    for rid in run_ids:
        p = OUT_BASE / rid / "training_summary.json"
        if not p.exists():
            rows.append({"run_id": rid, "error": "missing training_summary.json"})
            continue
        with open(p, encoding="utf-8") as f:
            s = json.load(f)
        c = s.get("config") or {}
        rows.append(
            {
                "run_id": rid,
                "flow_family": s.get("flow_family", c.get("flow_family")),
                "seed": s.get("reproducibility_seed", c.get("reproducibility_seed")),
                "models_trained": s.get("models_trained"),
                "total_files": s.get("total_files"),
                "success_rate": s.get("success_rate"),
                "execution_time_s": s.get("execution_time"),
            }
        )
    out_csv = OUT_BASE / "robustness_nf_runs_summary.csv"
    with open(out_csv, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()) if rows else [])
        w.writeheader()
        w.writerows(rows)
    return out_csv


def main() -> None:
    ap = argparse.ArgumentParser(
        description="Reviewer 3: MAF + RealNVP multi-seed NF training (defaults match main NF_OPTIMIZED)."
    )
    ap.add_argument(
        "--quick",
        action="store_true",
        help="Use 40 epochs and patience 8 instead of main settings (75 / 15).",
    )
    ap.add_argument(
        "--no-quick",
        action="store_true",
        help=argparse.SUPPRESS,
    )
    ap.add_argument(
        "--maf-seeds",
        default="123,456,789",
        help="Comma-separated seeds for MAF (default: 123,456,789).",
    )
    ap.add_argument(
        "--realnvp-seeds",
        default="123,456,789",
        help="Comma-separated seeds for RealNVP (default: same as MAF).",
    )
    ap.add_argument("--skip-maf", action="store_true", help="Only run RealNVP seeds.")
    ap.add_argument("--skip-realnvp", action="store_true", help="Only run MAF seeds.")
    ap.add_argument("--dry-run", action="store_true", help="Print plan and exit.")
    args = ap.parse_args()
    if args.no_quick and args.quick:
        print("[ERROR] Do not pass both --quick and --no-quick.")
        sys.exit(1)
    if args.no_quick:
        print(
            "[NOTE] --no-quick is deprecated: main pipeline NF settings (75 epochs, patience 15) are the default."
        )
    quick = args.quick
    maf_seeds = [int(x.strip()) for x in args.maf_seeds.split(",") if x.strip()]
    realnvp_seeds = [int(x.strip()) for x in args.realnvp_seeds.split(",") if x.strip()]

    res_dir = REPO_ROOT / "outputs" / "manual" / "residuals_by_model"
    if not res_dir.is_dir():
        print(f"[ERROR] Missing {res_dir}\nRun: Rscript scripts/manual/manual_garch_fitting.R")
        sys.exit(1)

    n_models_article = 4  # GARCH_MODELS in scripts/core/config.R (std specs)
    n_exp = n_models_article * len(OPTIMIZED_ASSETS["all_assets"])
    allowed = set(OPTIMIZED_ASSETS["all_assets"])
    n_have = 0
    for fp in res_dir.glob("*/*_Manual_Optimized_residuals.csv"):
        asset = fp.name.replace("_Manual_Optimized_residuals.csv", "")
        if asset in allowed:
            n_have += 1
    if n_have < n_exp:
        print(
            f"[WARNING] Article grid expects {n_exp} residual files ({n_models_article} models × "
            f"{len(OPTIMIZED_ASSETS['all_assets'])} assets); found {n_have} for those tickers."
        )
        print("           Re-run: Rscript scripts/manual/manual_garch_fitting.R")
        print("           (Residual export now runs for every asset×model; no TSCV gate.)")

    runs: list[tuple[str, dict]] = []
    if not args.skip_maf:
        for s in maf_seeds:
            runs.append((f"maf_seed{s}", nf_config_optimized(quick=quick, seed=s, flow_family="maf")))
    if not args.skip_realnvp:
        for s in realnvp_seeds:
            runs.append(
                (f"realnvp_seed{s}", nf_config_optimized(quick=quick, seed=s, flow_family="realnvp"))
            )

    if not runs:
        print("[ERROR] No runs scheduled (both --skip-maf and --skip-realnvp?)")
        sys.exit(1)

    if args.dry_run:
        print("Would run:", [r[0] for r in runs])
        print("NF training:", "QUICK (40 epochs, patience 8)" if quick else "MAIN-ALIGNED (75 epochs, patience 15, NF_OPTIMIZED)")
        return

    print(
        "NF training mode:",
        "QUICK (40 epochs, patience 8)" if quick else "MAIN-ALIGNED — NF_OPTIMIZED (75 epochs, patience 15)",
    )

    if NF_CONFIG_PATH.exists():
        bak = NF_CONFIG_PATH.with_suffix(".json.bak_reviewer3")
        shutil.copy2(NF_CONFIG_PATH, bak)
        print("Backed up", NF_CONFIG_PATH.name, "->", bak.name)

    OUT_BASE.mkdir(parents=True, exist_ok=True)
    for run_id, cfg in runs:
        run_one_training(run_id, cfg)

    csv_path = aggregate_summaries([r[0] for r in runs])
    print("\n[OK] Wrote", csv_path)
    restore_main_nf_config_json()


if __name__ == "__main__":
    main()
