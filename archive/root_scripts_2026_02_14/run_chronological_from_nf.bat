@echo off
setlocal enabledelayedexpansion
REM Rerun chronological pipeline from Step 3 (NF training) only.
REM Use after fixing deps (psutil, rugarch) when GARCH residuals already exist in outputs/manual.
REM Requires: outputs\manual\residuals_by_model\* populated by a previous Step 2 run.

cd /d "%~dp0"
set "REPO_ROOT=%CD%"
if exist "%REPO_ROOT%\environment\R_library" set "R_LIBS=%REPO_ROOT%\environment\R_library"
call scripts\utils\find_r_executable.bat
if %errorlevel% neq 0 exit /b 1
if not defined RSCRIPT exit /b 1

REM Steps 3-22 only; requires outputs\manual\residuals_by_model from prior run.

echo STEP 3: NF TRAINING
python scripts\manual\manual_nf_training.py
if %errorlevel% neq 0 (echo [ERROR] NF training failed & exit /b 1)

echo STEP 4: NF-GARCH SIMULATION
"%RSCRIPT%" scripts\simulation_forecasting\simulate_nf_garch_engine.R --split chronological
echo STEP 5: COMPARISON
"%RSCRIPT%" scripts\evaluation\compare_nf_vs_standard_garch.R --split chronological
echo STEP 6: DISTRIBUTIONAL METRICS
"%RSCRIPT%" scripts\evaluation\calculate_distributional_metrics.R --split chronological
echo STEP 7: STYLIZED FACTS
"%RSCRIPT%" scripts\evaluation\calculate_stylized_facts.R --split chronological
echo STEP 8: VaR BACKTESTING
"%RSCRIPT%" scripts\evaluation\var_backtesting_comprehensive.R --split chronological
echo STEP 9: STRESS TESTING
"%RSCRIPT%" scripts\evaluation\stress_testing_comprehensive.R --split chronological
echo STEP 10: STATIONARITY
"%RSCRIPT%" scripts\evaluation\test_residual_stationarity.R --split chronological
echo STEP 11: HETEROGENEITY
"%RSCRIPT%" scripts\evaluation\test_conditional_heterogeneity.R --split chronological
echo STEP 12: VERIFY
"%RSCRIPT%" scripts\evaluation\verify_all_results.R --split chronological
echo STEP 13: CONSOLIDATE
"%RSCRIPT%" -e "source('scripts/core/consolidation.R'); consolidate_all_results('results/chronological')"
echo STEP 14: HYPERPARAMETER SUMMARY
"%RSCRIPT%" scripts\evaluation\create_hyperparameter_summary.R --split chronological
echo STEP 15: METHODOLOGY DOC
"%RSCRIPT%" scripts\evaluation\create_methodology_consolidated.R --split chronological
echo STEP 16: DASHBOARD
"%RSCRIPT%" scripts\core\create_final_dashboard.R --split chronological
echo STEP 17: HTML DASHBOARD
"%RSCRIPT%" scripts\evaluation\generate_dashboard_visualizations.R --split chronological
echo STEP 18: DISSERTATION TABLES
"%RSCRIPT%" scripts\evaluation\extract_dissertation_tables.R --split chronological
echo STEP 19: REPORT FIGURES
"%RSCRIPT%" scripts\evaluation\generate_report_figures.R --split chronological
echo STEP 20: GARCH ORDER ROBUSTNESS
"%RSCRIPT%" scripts\experiments\robustness_garch_order.R --split chronological
echo STEP 21: COMPLETE ANALYSIS
"%RSCRIPT%" scripts\complete_analysis.R --split chronological
echo STEP 22: OVERLEAF EXPORT
if not exist "overleaf_export\chronological\tables" mkdir "overleaf_export\chronological\tables"
if not exist "overleaf_export\chronological\figures" mkdir "overleaf_export\chronological\figures"
if exist "results\chronological\dissertation_tables" copy /Y "results\chronological\dissertation_tables\*.*" "overleaf_export\chronological\tables\" >nul 2>&1
if exist "results\chronological\figures" copy /Y "results\chronological\figures\*.*" "overleaf_export\chronological\figures\" >nul 2>&1

echo.
echo [OK] Chronological pipeline (Steps 3-22) completed.
exit /b 0
