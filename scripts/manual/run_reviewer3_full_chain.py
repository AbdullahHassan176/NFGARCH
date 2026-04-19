#!/usr/bin/env python3
"""
Full replication chain per Reviewer-3 folder: simulation + evaluation (chronological split).

Prerequisites:
  - outputs/reviewer3/<run_id>/ with *_synthetic_residuals.csv (from run_reviewer3_robustness.py)
  - outputs/manual/garch_fitting and residuals_by_model (same PIPELINE_MODE as NF training)

Usage (repo root):
  python scripts/manual/run_reviewer3_full_chain.py
  python scripts/manual/run_reviewer3_full_chain.py --runs maf_seed123 realnvp_seed789
  python scripts/manual/run_reviewer3_full_chain.py --steps simulate compare
  (Also invoked from run_chronological.bat /Reviewer3 and /WithReviewer3.)

Environment (set automatically per run):
  REVIEWER3_RUN_ROOT, NF_RESIDUALS_ONLY_DIR, REVIEWER3_REPRODUCIBILITY_SEED, REVIEWER3_MANUAL_OUTPUT_BASE

After a successful run (unless --skip-restore-nf-config), re-exports scripts/core/nf_config.json from
scripts/core/config.R so run_chronological.bat Step 3 is not stuck on RealNVP from Reviewer3.

Outputs per run under: outputs/reviewer3/<run_id>/results/consolidated/*.xlsx
Summary: outputs/reviewer3/full_chain_summary.csv

Rebuild summary CSV from existing Excel only (no R rerun):
  python scripts/manual/run_reviewer3_full_chain.py --runs ... --summary-only
"""

from __future__ import annotations

import argparse
import csv
import glob
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
REVIEWER3_BASE = REPO_ROOT / "outputs" / "reviewer3"
MANUAL_OUT = REPO_ROOT / "outputs" / "manual"

# (name, r_args) — chronological; evaluation_split_config.R reads REVIEWER3_RUN_ROOT
# Expected workbook per step (under <run>/results/consolidated/) for --summary-only
STEP_OUTPUT_FILES: dict[str, list[str]] = {
    "simulate": ["NF_GARCH_Results_chronological.xlsx"],
    "compare": ["NF_vs_Standard_GARCH_Comparison.xlsx"],
    "distributional": ["Distributional_Metrics.xlsx"],
    "stylized_facts": ["Stylized_Facts.xlsx"],
    "var": ["VaR_Backtesting.xlsx"],
    "stress": ["Stress_Testing.xlsx"],
    "resid_stationarity": ["Methodology_Residual_Stationarity.xlsx"],
    "cond_hetero": ["Methodology_Conditional_Heterogeneity.xlsx"],
}

R_STEPS = [
    ("simulate", ["scripts/simulation_forecasting/simulate_nf_garch_engine.R", "--split", "chronological"]),
    ("compare", ["scripts/evaluation/compare_nf_vs_standard_garch.R", "--split", "chronological"]),
    ("distributional", ["scripts/evaluation/calculate_distributional_metrics.R", "--split", "chronological"]),
    ("stylized_facts", ["scripts/evaluation/calculate_stylized_facts.R", "--split", "chronological"]),
    ("var", ["scripts/evaluation/var_backtesting_comprehensive.R", "--split", "chronological"]),
    ("stress", ["scripts/evaluation/stress_testing_comprehensive.R", "--split", "chronological"]),
    ("resid_stationarity", ["scripts/evaluation/test_residual_stationarity.R", "--split", "chronological"]),
    ("cond_hetero", ["scripts/evaluation/test_conditional_heterogeneity.R", "--split", "chronological"]),
]


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


def parse_seed(run_id: str) -> int:
    m = re.search(r"_seed(\d+)$", run_id)
    if not m:
        raise ValueError(f"Cannot parse seed from run_id: {run_id}")
    return int(m.group(1))


def discover_runs() -> list[str]:
    ids: list[str] = []
    if not REVIEWER3_BASE.is_dir():
        return ids
    for p in REVIEWER3_BASE.iterdir():
        if not p.is_dir():
            continue
        if not re.match(r"(maf|realnvp)_seed\d+$", p.name):
            continue
        if (p / "training_summary.json").exists():
            ids.append(p.name)

    return sorted(set(ids), key=lambda rid: (0 if rid.startswith("maf_") else 1, parse_seed(rid)))


def env_for_run(run_dir: Path) -> dict[str, str]:
    e = os.environ.copy()
    rs = str(run_dir.resolve())
    mo = str(MANUAL_OUT.resolve())
    seed = parse_seed(run_dir.name)
    e["REVIEWER3_RUN_ROOT"] = rs
    e["NF_RESIDUALS_ONLY_DIR"] = rs
    e["REVIEWER3_REPRODUCIBILITY_SEED"] = str(seed)
    e["REVIEWER3_MANUAL_OUTPUT_BASE"] = mo
    r_lib = REPO_ROOT / "environment" / "R_library"
    if r_lib.is_dir() and any(r_lib.iterdir()):
        r_lib_s = str(r_lib.resolve())
        prev = e.get("R_LIBS", "").strip()
        e["R_LIBS"] = r_lib_s + (os.pathsep + prev if prev else "")
    return e


def normalize_exit_code(code: int | None) -> int:
    """Map unsigned Windows exit values (e.g. -1 as 4294967295) to signed for logs/CSV."""
    if code is None:
        return 1
    if code > 2**31 - 1:
        return int(code - 2**32)
    return int(code)


def run_rscript(rscript: str, args: list[str], env: dict[str, str]) -> int:
    cmd = [rscript] + args
    print(" ", " ".join(cmd))
    proc = subprocess.run(cmd, cwd=str(REPO_ROOT), env=env)
    return normalize_exit_code(proc.returncode)


def row_from_disk(run_dir: Path, run_id: str, seed: str, step_names: list[str]) -> dict[str, str]:
    cons = run_dir / "results" / "consolidated"
    row: dict[str, str] = {"run_id": run_id, "seed": seed}
    for name in step_names:
        files = STEP_OUTPUT_FILES.get(name, [])
        ok = bool(files) and all((cons / fn).is_file() for fn in files)
        row[name] = "0" if ok else "1"
    return row


def restore_nf_config_from_r() -> None:
    """Reset scripts/core/nf_config.json from config.R so a later run_chronological Step 3 is MAF + paper seed."""
    export_r = REPO_ROOT / "scripts" / "utils" / "export_nf_config_from_r.R"
    if not export_r.is_file():
        return
    rscript = find_rscript()
    if not rscript:
        print("[WARNING] Rscript not found; before main chronological, run:")
        print('  Rscript scripts/utils/export_nf_config_from_r.R "%CD%"')
        return
    proc = subprocess.run([rscript, str(export_r), str(REPO_ROOT)], cwd=str(REPO_ROOT))
    if proc.returncode != 0:
        print("[WARNING] export_nf_config_from_r.R failed; fix before run_chronological.bat Step 3.")
    else:
        print("[OK] Restored scripts/core/nf_config.json from scripts/core/config.R (main MAF baseline).")


def main() -> None:
    ap = argparse.ArgumentParser(description="Full R chain per outputs/reviewer3/<run_id>.")
    ap.add_argument(
        "--runs",
        nargs="*",
        default=None,
        help="Run ids (default: auto-discover maf_seed*/realnvp_seed* with training_summary.json).",
    )
    ap.add_argument(
        "--steps",
        nargs="*",
        default=[s[0] for s in R_STEPS],
        help=f"Subset of steps. Available: {', '.join(s[0] for s in R_STEPS)}",
    )
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument(
        "--skip-restore-nf-config",
        action="store_true",
        help="Do not re-export nf_config.json from R after this script (default: restore for next main run).",
    )
    ap.add_argument(
        "--summary-only",
        action="store_true",
        help="Only write full_chain_summary.csv from consolidated/*.xlsx on disk (no R steps).",
    )
    args = ap.parse_args()

    runs = args.runs if args.runs else discover_runs()
    if not runs:
        print("[ERROR] No runs found under", REVIEWER3_BASE)
        sys.exit(1)

    step_set = set(args.steps)
    unknown = step_set - {s[0] for s in R_STEPS}
    if unknown:
        print("[ERROR] Unknown steps:", unknown)
        sys.exit(1)
    steps = [(n, a) for n, a in R_STEPS if n in step_set]
    step_names = [n for n, _ in steps]

    rscript = find_rscript()
    if not rscript and not args.dry_run and not args.summary_only:
        print("[ERROR] Rscript not found. Install R or add Rscript to PATH.")
        sys.exit(1)

    if not MANUAL_OUT.joinpath("garch_fitting", "model_summary.csv").exists():
        print("[WARNING] Missing outputs/manual/garch_fitting/model_summary.csv — run manual_garch_fitting.R first.")

    summary_rows: list[dict[str, str]] = []

    for run_id in runs:
        run_dir = REVIEWER3_BASE / run_id
        if not run_dir.is_dir():
            print("[SKIP] missing dir:", run_dir)
            continue
        synth = list(run_dir.glob("*_synthetic_residuals.csv"))
        if not synth:
            print("[SKIP] no synthetic residuals in", run_id)
            continue

        env = env_for_run(run_dir)
        print("\n==========", run_id, "seed=", env["REVIEWER3_REPRODUCIBILITY_SEED"], "==========")

        if args.dry_run:
            for name, rargs in steps:
                print("  [dry-run]", name, rargs)
            continue

        if args.summary_only:
            row = row_from_disk(run_dir, run_id, env["REVIEWER3_REPRODUCIBILITY_SEED"], step_names)
            summary_rows.append(row)
            print("  [summary-only]", run_id, {k: row[k] for k in step_names})
            continue

        row = {"run_id": run_id, "seed": env["REVIEWER3_REPRODUCIBILITY_SEED"]}
        for name, rargs in steps:
            code = run_rscript(rscript, rargs, env)
            row[name] = str(code)
            if code != 0:
                print(f"  [FAIL] {name} exit {code}")
            else:
                print(f"  [OK] {name}")
        summary_rows.append(row)

    if args.dry_run:
        return

    out_csv = REVIEWER3_BASE / "full_chain_summary.csv"
    fieldnames = ["run_id", "seed"] + [n for n, _ in R_STEPS if n in step_set]
    with open(out_csv, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
        w.writeheader()
        w.writerows(summary_rows)
    print("\n[OK] Wrote", out_csv)

    if not args.skip_restore_nf_config and not args.summary_only:
        print("\nRestoring nf_config.json for any later run_chronological.bat (Step 3 = MAF per config.R)...")
        restore_nf_config_from_r()


if __name__ == "__main__":
    main()
