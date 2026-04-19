MDPI Risks — Reviewer 3 (Wilson) workflow archive
==================================================

The reviewer-facing entry points live on the main batch files (no separate root .bat clutter):

  run_chronological.bat /InstallRPackages   — R packages into environment\R_library
  run_chronological.bat                     — full 22-step chronological pipeline; main article replication uses MAF from scripts/core/config.R (nf_config.json exported before NF training)
  run_chronological.bat /Reviewer3          — only the supplement: MAF+RealNVP × seeds 123,456,789 under outputs\reviewer3\, then the same R evaluation chain per run
  run_chronological.bat /WithReviewer3      — chronological pipeline, then the supplement above
  run_both_pipelines.bat /WithReviewer3     — chronological (+ supplement), then TS-CV

Python implementations (kept under scripts\manual\): run_reviewer3_robustness.py, run_reviewer3_full_chain.py

Superseded root batch files were moved here for reference (same behavior as the integrated flags).
