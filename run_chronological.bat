@echo off
setlocal enabledelayedexpansion
REM =============================================================================
REM Comprehensive Chronological Split Pipeline (65/35)
REM Pure chronological split without CV for model selection
REM Includes full NF-GARCH analysis and dissertation output generation
REM =============================================================================

cd /d "%~dp0"

if /i "%~1"=="/OverleafOnly" (
  set OLEAF=1
  echo ========================================
  echo OVERLEAF EXPORT ONLY (CHRONOLOGICAL)
  echo ========================================
  echo Refreshing overleaf_export from existing chronological results...
  echo.
  goto :overleaf_export
)

echo ========================================
echo COMPREHENSIVE CHRONOLOGICAL PIPELINE
echo ========================================
echo.
echo Data Splitting Strategy: Pure 65/35 chronological (NO CV)
echo.
echo This will:
echo  1. Clear previous chronological outputs
echo  2. GARCH fitting with chronological split (65/35)
echo  3. NF training on chronological residuals
echo  4. NF-GARCH simulation
echo  5. NF vs Standard GARCH comparison
echo  6. Distributional metrics
echo  7. Stylized facts
echo  8. VaR backtesting
echo  9. Stress testing
echo  10. Residual stationarity tests
echo  11. Conditional heterogeneity tests
echo  12. Verify results
echo  13. Consolidate results
echo  14. Hyperparameter sensitivity summary
echo  15. Methodology consolidated documentation
echo  16. Final dashboard
echo  17. HTML dashboard visualizations
echo  18. Dissertation tables
echo  19. Report figures
echo  20. GARCH order robustness
echo  21. Complete analysis
echo  22. Overleaf export
echo.
echo Expected time: 90-180 minutes (FULLY COMPREHENSIVE)
echo ========================================
echo.

REM --- Resolve Rscript ---
call scripts\utils\find_r_executable.bat
if %errorlevel% neq 0 (
  echo [ERROR] Rscript not found. Install R or set RSCRIPT.
  pause
  exit /b 1
)
if not defined RSCRIPT (
  echo [ERROR] RSCRIPT not set after find_r_executable.
  pause
  exit /b 1
)
echo Using Rscript: %RSCRIPT%
echo.

REM When called with /Y, skip confirmation
if /i not "%~1"=="/Y" (
  set /p confirm="Run full chronological pipeline? (Y/N): "
  if /i not "!confirm!"=="Y" (
    echo Cancelled.
    pause
    exit /b 0
  )
)
echo.

REM =============================================================================
REM STEP 1: CLEAR PREVIOUS OUTPUTS
REM =============================================================================
echo ========================================
echo STEP 1: CLEARING CHRONOLOGICAL OUTPUTS
echo ========================================
echo.

if exist "outputs\chronological" (
    echo Clearing outputs\chronological...
    rd /s /q "outputs\chronological" 2>nul
)

REM Recreate directory structure
if not exist "outputs\chronological" mkdir "outputs\chronological"
if not exist "outputs\chronological\garch_fitting" mkdir "outputs\chronological\garch_fitting"
if not exist "outputs\chronological\residuals_by_model" mkdir "outputs\chronological\residuals_by_model"
if not exist "outputs\chronological\nf_models" mkdir "outputs\chronological\nf_models"
if not exist "outputs\chronological\evaluation" mkdir "outputs\chronological\evaluation"
if not exist "results\chronological" mkdir "results\chronological"
if not exist "results\chronological\dissertation_tables" mkdir "results\chronological\dissertation_tables"
if not exist "results\chronological\figures" mkdir "results\chronological\figures"

echo [OK] Chronological outputs cleared and directories created
echo.

REM =============================================================================
REM STEP 2: GARCH FITTING (CHRONOLOGICAL SPLIT)
REM =============================================================================
echo ========================================
echo STEP 2: GARCH FITTING (CHRONOLOGICAL)
echo ========================================
echo.
echo Running chronological GARCH fitting (65/35 split, no CV)...
echo.

"%RSCRIPT%" additional_analysis\scripts\chronological\fit_garch_chronological.R
if %errorlevel% neq 0 (
    echo [ERROR] Chronological GARCH fitting failed
    pause
    exit /b 1
) else (
    echo [OK] Chronological GARCH fitting completed
)
echo.

REM =============================================================================
REM STEP 3: NF TRAINING (ON CHRONOLOGICAL RESIDUALS)
REM =============================================================================
echo ========================================
echo STEP 3: NF TRAINING (CHRONOLOGICAL)
echo ========================================
echo.
echo Training NF models on chronological residuals...
echo.

python additional_analysis\scripts\chronological\train_nf_chronological.py
if %errorlevel% neq 0 (
    echo [ERROR] NF training failed
    pause
    exit /b 1
) else (
    echo [OK] NF training completed
)
echo.

REM =============================================================================
REM STEP 4: NF-GARCH SIMULATION
REM =============================================================================
echo ========================================
echo STEP 4: NF-GARCH SIMULATION (CHRONOLOGICAL)
echo ========================================
echo.
echo Running NF-GARCH simulation with chronological models...
echo.

"%RSCRIPT%" scripts\simulation_forecasting\simulate_nf_garch_engine.R --split chronological
if %errorlevel% neq 0 (
    echo [WARNING] NF-GARCH simulation had issues, continuing...
) else (
    echo [OK] NF-GARCH simulation completed
)
echo.

REM =============================================================================
REM STEP 5: COMPARE NF-GARCH vs STANDARD GARCH
REM =============================================================================
echo ========================================
echo STEP 5: NF-GARCH vs STANDARD COMPARISON
echo ========================================
echo.

"%RSCRIPT%" scripts\evaluation\compare_nf_vs_standard_garch.R --engine chronological
if %errorlevel% neq 0 (
    echo [WARNING] Comparison analysis had issues, continuing...
) else (
    echo [OK] Comparison analysis completed
)
echo.

REM =============================================================================
REM STEP 6: DISTRIBUTIONAL METRICS
REM =============================================================================
echo ========================================
echo STEP 6: DISTRIBUTIONAL METRICS
echo ========================================
echo.

"%RSCRIPT%" scripts\evaluation\calculate_distributional_metrics.R --split chronological
if %errorlevel% neq 0 (
    echo [WARNING] Distributional metrics had issues, continuing...
) else (
    echo [OK] Distributional metrics calculated
)
echo.

REM =============================================================================
REM STEP 7: STYLIZED FACTS
REM =============================================================================
echo ========================================
echo STEP 7: STYLIZED FACTS
echo ========================================
echo.

"%RSCRIPT%" scripts\evaluation\calculate_stylized_facts.R --engine chronological
if %errorlevel% neq 0 (
    echo [WARNING] Stylized facts had issues, continuing...
) else (
    echo [OK] Stylized facts calculated
)
echo.

REM =============================================================================
REM STEP 8: VaR BACKTESTING
REM =============================================================================
echo ========================================
echo STEP 8: VaR BACKTESTING
echo ========================================
echo.

"%RSCRIPT%" scripts\evaluation\var_backtesting_comprehensive.R --split chronological
if %errorlevel% neq 0 (
    echo [WARNING] VaR backtesting had issues, continuing...
) else (
    echo [OK] VaR backtesting completed
)
echo.

REM =============================================================================
REM STEP 9: STRESS TESTING
REM =============================================================================
echo ========================================
echo STEP 9: STRESS TESTING
echo ========================================
echo.

"%RSCRIPT%" scripts\evaluation\stress_testing_comprehensive.R --engine chronological
if %errorlevel% neq 0 (
    echo [WARNING] Stress testing had issues, continuing...
) else (
    echo [OK] Stress testing completed
)
echo.

REM =============================================================================
REM STEP 10: RESIDUAL STATIONARITY TESTS
REM =============================================================================
echo ========================================
echo STEP 10: RESIDUAL STATIONARITY TESTS
echo ========================================
echo.

"%RSCRIPT%" scripts\evaluation\test_residual_stationarity.R --split chronological
if %errorlevel% neq 0 (
    echo [WARNING] Stationarity tests had issues, continuing...
) else (
    echo [OK] Stationarity tests completed
)
echo.

REM =============================================================================
REM STEP 11: CONDITIONAL HETEROGENEITY TESTS
REM =============================================================================
echo ========================================
echo STEP 11: CONDITIONAL HETEROGENEITY TESTS
echo ========================================
echo.

"%RSCRIPT%" scripts\evaluation\test_conditional_heterogeneity.R --engine chronological
if %errorlevel% neq 0 (
    echo [WARNING] Conditional heterogeneity tests had issues, continuing...
) else (
    echo [OK] Conditional heterogeneity tests completed
)
echo.

REM =============================================================================
REM STEP 12: VERIFY RESULTS
REM =============================================================================
echo ========================================
echo STEP 12: VERIFYING RESULTS
echo ========================================
echo.

"%RSCRIPT%" scripts\evaluation\verify_all_results.R --split chronological
if %errorlevel% neq 0 (
    echo [WARNING] Verification had issues, continuing...
) else (
    echo [OK] Results verification completed
)
echo.

REM =============================================================================
REM STEP 13: CONSOLIDATE RESULTS
REM =============================================================================
echo ========================================
echo STEP 13: CONSOLIDATING RESULTS
echo ========================================
echo.

"%RSCRIPT%" -e "source('scripts/core/consolidation.R'); consolidate_all_results('results/chronological', engine='chronological')"
if %errorlevel% neq 0 (
    echo [WARNING] Consolidation had issues, continuing...
) else (
    echo [OK] Results consolidated
)
echo.

REM =============================================================================
REM STEP 14: HYPERPARAMETER SENSITIVITY SUMMARY
REM =============================================================================
echo ========================================
echo STEP 14: HYPERPARAMETER SENSITIVITY SUMMARY
echo ========================================
echo.

"%RSCRIPT%" scripts\evaluation\create_hyperparameter_summary.R --split chronological
if %errorlevel% neq 0 (
    echo [WARNING] Hyperparameter summary had issues, continuing...
) else (
    echo [OK] Hyperparameter summary created
)
echo.

REM =============================================================================
REM STEP 15: METHODOLOGY CONSOLIDATED DOCUMENTATION
REM =============================================================================
echo ========================================
echo STEP 15: METHODOLOGY CONSOLIDATED DOCUMENTATION
echo ========================================
echo.

"%RSCRIPT%" scripts\evaluation\create_methodology_consolidated.R --split chronological
if %errorlevel% neq 0 (
    echo [WARNING] Methodology consolidation had issues, continuing...
) else (
    echo [OK] Methodology consolidated documentation created
)
echo.

REM =============================================================================
REM STEP 16: FINAL DASHBOARD
REM =============================================================================
echo ========================================
echo STEP 16: CREATING FINAL DASHBOARD
echo ========================================
echo.

"%RSCRIPT%" scripts\core\create_final_dashboard.R --split chronological
if %errorlevel% neq 0 (
    echo [WARNING] Dashboard creation had issues, continuing...
) else (
    echo [OK] Final Excel dashboard created
)
echo.

REM =============================================================================
REM STEP 17: HTML DASHBOARD VISUALIZATIONS
REM =============================================================================
echo ========================================
echo STEP 17: HTML DASHBOARD VISUALIZATIONS
echo ========================================
echo.

"%RSCRIPT%" scripts\evaluation\generate_dashboard_visualizations.R --split chronological
if %errorlevel% neq 0 (
    echo [WARNING] HTML dashboard had issues, continuing...
) else (
    echo [OK] HTML dashboard visualizations generated
)
echo.

REM =============================================================================
REM STEP 18: DISSERTATION TABLES
REM =============================================================================
echo ========================================
echo STEP 18: DISSERTATION TABLES
echo ========================================
echo.

"%RSCRIPT%" scripts\evaluation\extract_dissertation_tables.R --engine chronological
if %errorlevel% neq 0 (
    echo [WARNING] Dissertation tables had issues, continuing...
) else (
    echo [OK] Dissertation tables generated
)
echo.

REM =============================================================================
REM STEP 19: REPORT FIGURES
REM =============================================================================
echo ========================================
echo STEP 19: REPORT FIGURES
echo ========================================
echo.

"%RSCRIPT%" scripts\evaluation\generate_report_figures.R --split chronological
if %errorlevel% neq 0 (
    echo [WARNING] Report figures had issues, continuing...
) else (
    echo [OK] Report figures generated
)
echo.

REM =============================================================================
REM STEP 20: GARCH ORDER ROBUSTNESS
REM =============================================================================
echo ========================================
echo STEP 20: GARCH ORDER ROBUSTNESS
echo ========================================
echo.

"%RSCRIPT%" scripts\experiments\robustness_garch_order.R --split chronological
if %errorlevel% neq 0 (
    echo [WARNING] GARCH order robustness had issues, continuing...
) else (
    echo [OK] GARCH order robustness completed
)
echo.

REM =============================================================================
REM STEP 21: COMPLETE ANALYSIS
REM =============================================================================
echo ========================================
echo STEP 21: COMPLETE ANALYSIS
echo ========================================
echo.

"%RSCRIPT%" scripts\complete_analysis.R --split chronological
if %errorlevel% neq 0 (
    echo [WARNING] Complete analysis had issues, continuing...
) else (
    echo [OK] Complete analysis finished
)
echo.

REM =============================================================================
REM STEP 22: OVERLEAF EXPORT
REM =============================================================================
:overleaf_export
echo ========================================
echo STEP 15: OVERLEAF EXPORT (CHRONOLOGICAL)
echo ========================================
echo.

if not exist "overleaf_export" mkdir "overleaf_export"
if not exist "overleaf_export\chronological" mkdir "overleaf_export\chronological"
if not exist "overleaf_export\chronological\tables" mkdir "overleaf_export\chronological\tables"
if not exist "overleaf_export\chronological\figures" mkdir "overleaf_export\chronological\figures"

REM Copy dissertation tables
if exist "results\chronological\dissertation_tables" (
  copy /Y "results\chronological\dissertation_tables\*.*" "overleaf_export\chronological\tables\" >nul 2>&1
  echo   Copied chronological dissertation tables
)

REM Copy figures
if exist "results\chronological\figures" (
  copy /Y "results\chronological\figures\*.*" "overleaf_export\chronological\figures\" >nul 2>&1
  echo   Copied chronological figures
)

REM Create import instructions
(
  echo NF-GARCH Chronological Pipeline - Overleaf Export
  echo ================================================
  echo.
  echo Data Splitting: Pure 65/35 chronological split ^(NO CV^)
  echo.
  echo Tables: overleaf_export\chronological\tables\
  echo Figures: overleaf_export\chronological\figures\
  echo.
  echo This export uses a pure chronological split approach for validation.
  echo Compare with the main pipeline results to assess robustness.
) > "overleaf_export\chronological\README.txt"

echo   Created overleaf_export\chronological\README.txt
echo.

REM =============================================================================
REM SUMMARY
REM =============================================================================
echo ========================================
if defined OLEAF (echo CHRONOLOGICAL OVERLEAF EXPORT COMPLETED) else (echo COMPREHENSIVE CHRONOLOGICAL PIPELINE COMPLETED)
echo ========================================
echo.
echo Data Splitting: Pure 65/35 chronological ^(NO CV^)
echo.
echo Results saved to:
echo   - results\chronological\NF_GARCH_Results_chronological.xlsx
echo   - results\chronological\NF_vs_Standard_GARCH_Comparison.xlsx
echo   - results\chronological\Distributional_Metrics.xlsx
echo   - results\chronological\Stylized_Facts.xlsx
echo   - results\chronological\VaR_Backtesting.xlsx
echo   - results\chronological\Stress_Testing.xlsx
echo   - results\chronological\Final_Dashboard.xlsx (Excel dashboard)
echo   - results\chronological\Methodology_Residual_Stationarity.xlsx
echo   - results\chronological\Methodology_Conditional_Heterogeneity.xlsx
echo   - results\chronological\Methodology_Consolidated.xlsx
echo   - results\chronological\dashboard_visualizations.html
echo   - results\chronological\dashboard_plots\ (visualization plots)
echo   - results\chronological\dissertation_tables\ (LaTeX tables)
echo   - results\chronological\figures\ (Dissertation figures)
echo   - outputs\chronological\ (raw outputs)
echo.
echo Overleaf Export:
echo   - overleaf_export\chronological\tables\
echo   - overleaf_export\chronological\figures\
echo.
echo This COMPREHENSIVE pipeline provides an alternative validation using pure
echo chronological data splitting without cross-validation for model selection.
echo It includes ALL evaluation, methodology validation, and dissertation outputs.
echo.
echo ========================================
pause
