One-off / debug scripts and output formerly in repo root. Not used by the pipeline.
- analyze_fixed_comparison_results.py, analyze_sgarch_fix_impact.py: ad-hoc comparison analysis
- check_chrono.py, check_final_results.py, check_results.py: result-file checks
- compare_with_fix.py: comparison after a fix
- diagnose_egarch.R: eGARCH residual extraction diagnostic
- egarch_debug.txt: R package load / debug output
- FINAL_RESULTS_WITH_PARAMETRIC_SAMPLING.py: parametric sampling results script
- test_egarch_direct.R: eGARCH direct test/diagnostic.
- run_all.bat, run_full_dissertation.bat: old main/full pipeline entry points (superseded by run_both_pipelines.bat).
- run_chronological_from_nf.bat: rerun chronological pipeline from Step 3 (NF) only, using existing outputs\manual\residuals_by_model. Used once after psutil/rugarch fix; main pipeline is run_both_pipelines.bat.
Moved 2026-02-14.
