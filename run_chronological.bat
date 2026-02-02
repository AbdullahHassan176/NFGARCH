@echo off
setlocal enabledelayedexpansion
REM =============================================================================
REM Comprehensive Chronological Split Pipeline (65/35)
REM Pure chronological split without CV for model selection
REM Includes full NF-GARCH analysis and dissertation output generation
REM =============================================================================

cd /d "%~dp0"

REM Initialize logging
for /f "tokens=2-4 delims=/ " %%a in ('date /t') do (set mydate=%%c%%a%%b)
for /f "tokens=1-3 delims=:. " %%a in ('echo %time%') do (set mytime=%%a%%b%%c)
set mytime=%mytime: =0%
set LOG_FILE=logs\chronological_pipeline_%mydate%_%mytime%.log
if not exist "logs" mkdir "logs"

REM Initialize timing array
set STEP_COUNT=0

REM Note: Logging will start with first STEP (helper functions defined at end of file)

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

REM DEBUG: Show parameter
echo [DEBUG] Parameter 1: [%~1]
echo.

REM Handle special flags
if /i "%~1"=="/OverleafOnly" (
  set OLEAF=1
  echo ========================================
  echo OVERLEAF EXPORT ONLY (CHRONOLOGICAL)
  echo ========================================
  echo Refreshing overleaf_export from existing chronological results...
  echo.
  goto :overleaf_export
)

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
call :START_STEP "STEP 1: CLEARING CHRONOLOGICAL OUTPUTS"
call :LOG "=========================================="
call :LOG "COMPREHENSIVE CHRONOLOGICAL PIPELINE"
call :LOG "=========================================="
call :LOG "Pipeline Start Time: %date% %time%"
call :LOG "Working Directory: %cd%"
call :LOG ""
call :LOG "Clearing previous outputs and recreating directory structure"

if exist "outputs\chronological" (
    call :LOG "  - Removing outputs\chronological\"
    echo Clearing outputs\chronological...
    rd /s /q "outputs\chronological" 2>nul
)

REM Recreate directory structure
call :LOG "  - Creating outputs\chronological\ directory structure"
if not exist "outputs\chronological" mkdir "outputs\chronological"
if not exist "outputs\chronological\garch_fitting" mkdir "outputs\chronological\garch_fitting"
if not exist "outputs\chronological\residuals_by_model" mkdir "outputs\chronological\residuals_by_model"
if not exist "outputs\chronological\nf_models" mkdir "outputs\chronological\nf_models"
if not exist "outputs\chronological\evaluation" mkdir "outputs\chronological\evaluation"

call :LOG "  - Creating results\chronological\ directory structure"
if not exist "results\chronological" mkdir "results\chronological"
if not exist "results\chronological\dissertation_tables" mkdir "results\chronological\dissertation_tables"
if not exist "results\chronological\figures" mkdir "results\chronological\figures"

echo [OK] Chronological outputs cleared and directories created
call :LOG "[OK] Directories cleared and recreated"
call :END_STEP
echo.

REM =============================================================================
REM STEP 2: GARCH FITTING (CHRONOLOGICAL SPLIT)
REM =============================================================================
call :START_STEP "STEP 2: GARCH FITTING (CHRONOLOGICAL)"
call :LOG "Running chronological GARCH fitting with 65/35 split (no CV)"
call :LOG "Script: additional_analysis\scripts\chronological\fit_garch_chronological.R"
echo.
echo Running chronological GARCH fitting (65/35 split, no CV)...
echo.

"%RSCRIPT%" additional_analysis\scripts\chronological\fit_garch_chronological.R
if %errorlevel% neq 0 (
    call :LOG "[ERROR] Chronological GARCH fitting failed with exit code %errorlevel%"
    echo [ERROR] Chronological GARCH fitting failed
    call :END_STEP
    pause
    exit /b 1
) else (
    call :LOG "[OK] Chronological GARCH fitting completed successfully"
    echo [OK] Chronological GARCH fitting completed
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 3: NF TRAINING (ON CHRONOLOGICAL RESIDUALS)
REM =============================================================================
call :START_STEP "STEP 3: NF TRAINING (CHRONOLOGICAL)"
call :LOG "Training NF models on chronological residuals"
call :LOG "Script: additional_analysis\scripts\chronological\train_nf_chronological.py"
echo.
echo Training NF models on chronological residuals...
echo.

python additional_analysis\scripts\chronological\train_nf_chronological.py
if %errorlevel% neq 0 (
    call :LOG "[ERROR] NF training failed with exit code %errorlevel%"
    echo [ERROR] NF training failed
    call :END_STEP
    pause
    exit /b 1
) else (
    call :LOG "[OK] NF training completed successfully"
    echo [OK] NF training completed
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 4: NF-GARCH SIMULATION
REM =============================================================================
call :START_STEP "STEP 4: NF-GARCH SIMULATION (CHRONOLOGICAL)"
call :LOG "Running NF-GARCH simulation with chronological models"
call :LOG "Script: scripts\simulation_forecasting\simulate_nf_garch_engine.R --split chronological"
echo.
echo Running NF-GARCH simulation with chronological models...
echo.

"%RSCRIPT%" scripts\simulation_forecasting\simulate_nf_garch_engine.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] NF-GARCH simulation had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] NF-GARCH simulation had issues, continuing...
) else (
    call :LOG "[OK] NF-GARCH simulation completed successfully"
    echo [OK] NF-GARCH simulation completed
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 5: COMPARE NF-GARCH vs STANDARD GARCH
REM =============================================================================
call :START_STEP "STEP 5: NF-GARCH vs STANDARD COMPARISON"
call :LOG "Comparing NF-GARCH vs Standard GARCH performance"
call :LOG "Script: scripts\evaluation\compare_nf_vs_standard_garch.R --split chronological"
echo.

"%RSCRIPT%" scripts\evaluation\compare_nf_vs_standard_garch.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] Comparison analysis had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] Comparison analysis had issues, continuing...
) else (
    call :LOG "[OK] Comparison analysis completed successfully"
    echo [OK] Comparison analysis completed
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 6: DISTRIBUTIONAL METRICS
REM =============================================================================
call :START_STEP "STEP 6: DISTRIBUTIONAL METRICS"
call :LOG "Calculating distributional metrics (KS, Wasserstein, tail indices)"
call :LOG "Script: scripts\evaluation\calculate_distributional_metrics.R --split chronological"
echo.

"%RSCRIPT%" scripts\evaluation\calculate_distributional_metrics.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] Distributional metrics had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] Distributional metrics had issues, continuing...
) else (
    call :LOG "[OK] Distributional metrics calculated successfully"
    echo [OK] Distributional metrics calculated
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 7: STYLIZED FACTS
REM =============================================================================
call :START_STEP "STEP 7: STYLIZED FACTS"
call :LOG "Calculating stylized facts (volatility clustering, leverage effects)"
call :LOG "Script: scripts\evaluation\calculate_stylized_facts.R --split chronological"
echo.

"%RSCRIPT%" scripts\evaluation\calculate_stylized_facts.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] Stylized facts had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] Stylized facts had issues, continuing...
) else (
    call :LOG "[OK] Stylized facts calculated successfully"
    echo [OK] Stylized facts calculated
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 8: VaR BACKTESTING
REM =============================================================================
call :START_STEP "STEP 8: VaR BACKTESTING"
call :LOG "Running VaR backtesting (Kupiec, Christoffersen tests)"
call :LOG "Script: scripts\evaluation\var_backtesting_comprehensive.R --split chronological"
echo.

"%RSCRIPT%" scripts\evaluation\var_backtesting_comprehensive.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] VaR backtesting had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] VaR backtesting had issues, continuing...
) else (
    call :LOG "[OK] VaR backtesting completed successfully"
    echo [OK] VaR backtesting completed
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 9: STRESS TESTING
REM =============================================================================
call :START_STEP "STEP 9: STRESS TESTING"
call :LOG "Running stress tests (historical crises, hypothetical shocks)"
call :LOG "Script: scripts\evaluation\stress_testing_comprehensive.R --split chronological"
echo.

"%RSCRIPT%" scripts\evaluation\stress_testing_comprehensive.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] Stress testing had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] Stress testing had issues, continuing...
) else (
    call :LOG "[OK] Stress testing completed successfully"
    echo [OK] Stress testing completed
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 10: RESIDUAL STATIONARITY TESTS
REM =============================================================================
call :START_STEP "STEP 10: RESIDUAL STATIONARITY TESTS"
call :LOG "Testing GARCH residuals for stationarity (ADF, KPSS, Ljung-Box, ARCH)"
call :LOG "Script: scripts\evaluation\test_residual_stationarity.R --split chronological"
echo.

"%RSCRIPT%" scripts\evaluation\test_residual_stationarity.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] Stationarity tests had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] Stationarity tests had issues, continuing...
) else (
    call :LOG "[OK] Stationarity tests completed successfully"
    echo [OK] Stationarity tests completed
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 11: CONDITIONAL HETEROGENEITY TESTS
REM =============================================================================
call :START_STEP "STEP 11: CONDITIONAL HETEROGENEITY TESTS"
call :LOG "Testing for conditional heterogeneity in GARCH residuals"
call :LOG "Script: scripts\evaluation\test_conditional_heterogeneity.R --split chronological"
echo.

"%RSCRIPT%" scripts\evaluation\test_conditional_heterogeneity.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] Conditional heterogeneity tests had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] Conditional heterogeneity tests had issues, continuing...
) else (
    call :LOG "[OK] Conditional heterogeneity tests completed successfully"
    echo [OK] Conditional heterogeneity tests completed
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 12: VERIFY RESULTS
REM =============================================================================
call :START_STEP "STEP 12: VERIFYING RESULTS"
call :LOG "Verifying all results for consistency and completeness"
call :LOG "Script: scripts\evaluation\verify_all_results.R --split chronological"
echo.

"%RSCRIPT%" scripts\evaluation\verify_all_results.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] Verification had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] Verification had issues, continuing...
) else (
    call :LOG "[OK] Results verification completed successfully"
    echo [OK] Results verification completed
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 13: CONSOLIDATE RESULTS
REM =============================================================================
call :START_STEP "STEP 13: CONSOLIDATING RESULTS"
call :LOG "Consolidating all results into unified format"
call :LOG "Command: consolidate_all_results('results/chronological', split='chronological')"
echo.

"%RSCRIPT%" -e "source('scripts/core/consolidation.R'); consolidate_all_results('results/chronological', split='chronological')"
if %errorlevel% neq 0 (
    call :LOG "[WARNING] Consolidation had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] Consolidation had issues, continuing...
) else (
    call :LOG "[OK] Results consolidated successfully"
    echo [OK] Results consolidated
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 14: HYPERPARAMETER SENSITIVITY SUMMARY
REM =============================================================================
call :START_STEP "STEP 14: HYPERPARAMETER SENSITIVITY SUMMARY"
call :LOG "Creating hyperparameter selection methodology documentation"
call :LOG "Script: scripts\evaluation\create_hyperparameter_summary.R --split chronological"
echo.

"%RSCRIPT%" scripts\evaluation\create_hyperparameter_summary.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] Hyperparameter summary had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] Hyperparameter summary had issues, continuing...
) else (
    call :LOG "[OK] Hyperparameter summary created successfully"
    echo [OK] Hyperparameter summary created
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 15: METHODOLOGY CONSOLIDATED DOCUMENTATION
REM =============================================================================
call :START_STEP "STEP 15: METHODOLOGY CONSOLIDATED DOCUMENTATION"
call :LOG "Consolidating methodology validation results"
call :LOG "Script: scripts\evaluation\create_methodology_consolidated.R --split chronological"
echo.

"%RSCRIPT%" scripts\evaluation\create_methodology_consolidated.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] Methodology consolidation had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] Methodology consolidation had issues, continuing...
) else (
    call :LOG "[OK] Methodology consolidated documentation created successfully"
    echo [OK] Methodology consolidated documentation created
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 16: FINAL DASHBOARD
REM =============================================================================
call :START_STEP "STEP 16: CREATING FINAL DASHBOARD"
call :LOG "Creating comprehensive Excel dashboard with all metrics"
call :LOG "Script: scripts\core\create_final_dashboard.R --split chronological"
echo.

"%RSCRIPT%" scripts\core\create_final_dashboard.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] Dashboard creation had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] Dashboard creation had issues, continuing...
) else (
    call :LOG "[OK] Final Excel dashboard created successfully"
    echo [OK] Final Excel dashboard created
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 17: HTML DASHBOARD VISUALIZATIONS
REM =============================================================================
call :START_STEP "STEP 17: HTML DASHBOARD VISUALIZATIONS"
call :LOG "Generating interactive HTML dashboard and visualization plots"
call :LOG "Script: scripts\evaluation\generate_dashboard_visualizations.R --split chronological"
echo.

"%RSCRIPT%" scripts\evaluation\generate_dashboard_visualizations.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] HTML dashboard had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] HTML dashboard had issues, continuing...
) else (
    call :LOG "[OK] HTML dashboard visualizations generated successfully"
    echo [OK] HTML dashboard visualizations generated
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 18: DISSERTATION TABLES
REM =============================================================================
call :START_STEP "STEP 18: DISSERTATION TABLES"
call :LOG "Generating LaTeX tables for dissertation"
call :LOG "Script: scripts\evaluation\extract_dissertation_tables.R --split chronological"
echo.

"%RSCRIPT%" scripts\evaluation\extract_dissertation_tables.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] Dissertation tables had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] Dissertation tables had issues, continuing...
) else (
    call :LOG "[OK] Dissertation tables generated successfully"
    echo [OK] Dissertation tables generated
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 19: REPORT FIGURES
REM =============================================================================
call :START_STEP "STEP 19: REPORT FIGURES"
call :LOG "Generating dissertation report figures (publication-ready PNGs)"
call :LOG "Script: scripts\evaluation\generate_report_figures.R --split chronological"
echo.

"%RSCRIPT%" scripts\evaluation\generate_report_figures.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] Report figures had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] Report figures had issues, continuing...
) else (
    call :LOG "[OK] Report figures generated successfully"
    echo [OK] Report figures generated
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 20: GARCH ORDER ROBUSTNESS
REM =============================================================================
call :START_STEP "STEP 20: GARCH ORDER ROBUSTNESS"
call :LOG "Running GARCH order robustness analysis"
call :LOG "Script: scripts\experiments\robustness_garch_order.R --split chronological"
echo.

"%RSCRIPT%" scripts\experiments\robustness_garch_order.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] GARCH order robustness had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] GARCH order robustness had issues, continuing...
) else (
    call :LOG "[OK] GARCH order robustness completed successfully"
    echo [OK] GARCH order robustness completed
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 21: COMPLETE ANALYSIS
REM =============================================================================
call :START_STEP "STEP 21: COMPLETE ANALYSIS"
call :LOG "Running complete analysis summary"
call :LOG "Script: scripts\complete_analysis.R --split chronological"
echo.

"%RSCRIPT%" scripts\complete_analysis.R --split chronological
if %errorlevel% neq 0 (
    call :LOG "[WARNING] Complete analysis had issues with exit code %errorlevel%, continuing..."
    echo [WARNING] Complete analysis had issues, continuing...
) else (
    call :LOG "[OK] Complete analysis finished successfully"
    echo [OK] Complete analysis finished
)
call :END_STEP
echo.

REM =============================================================================
REM STEP 22: OVERLEAF EXPORT
REM =============================================================================
:overleaf_export
call :START_STEP "STEP 22: OVERLEAF EXPORT (CHRONOLOGICAL)"
call :LOG "Exporting tables and figures to Overleaf-ready format"
echo ========================================
echo STEP 22: OVERLEAF EXPORT (CHRONOLOGICAL)
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
  call :LOG "Overleaf export files copied"
echo.
call :LOG "Overleaf export completed"
call :END_STEP

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

REM Generate timing summary
call :GENERATE_SUMMARY

pause
goto :EOF

REM =============================================================================
REM HELPER FUNCTIONS FOR LOGGING AND TIMING
REM =============================================================================

:LOG
REM Log message to both console and file
echo %~1 >> "%LOG_FILE%"
goto :EOF

:START_STEP
REM Start timing a step
set /a STEP_COUNT+=1
set STEP_NAME[%STEP_COUNT%]=%~1
echo ========================================
echo %~1
echo ========================================
call :LOG "=========================================="
call :LOG "%~1"
call :LOG "=========================================="
call :LOG "Step Start Time: %time%"
set STEP_START[%STEP_COUNT%]=%time%
goto :EOF

:END_STEP
REM End timing a step
set STEP_END[%STEP_COUNT%]=%time%
call :CALCULATE_DURATION !STEP_START[%STEP_COUNT%]! !STEP_END[%STEP_COUNT%]!
set STEP_DURATION[%STEP_COUNT%]=!DURATION!
call :LOG "Step End Time: %time%"
call :LOG "Step Duration: !DURATION!"
call :LOG ""
goto :EOF

:CALCULATE_DURATION
REM Calculate duration between two times
set start_time=%~1
set end_time=%~2

REM Parse start time
for /f "tokens=1-4 delims=:.," %%a in ("%start_time%") do (
    set /a start_h=%%a
    set /a start_m=%%b
    set /a start_s=%%c
    set /a start_ms=%%d
)

REM Parse end time
for /f "tokens=1-4 delims=:.," %%a in ("%end_time%") do (
    set /a end_h=%%a
    set /a end_m=%%b
    set /a end_s=%%c
    set /a end_ms=%%d
)

REM Convert to total seconds
set /a start_total=(start_h*3600)+(start_m*60)+start_s
set /a end_total=(end_h*3600)+(end_m*60)+end_s

REM Calculate difference
set /a diff_seconds=end_total-start_total

REM Handle day rollover
if !diff_seconds! lss 0 set /a diff_seconds+=86400

REM Convert back to hours, minutes, seconds
set /a hours=diff_seconds/3600
set /a remainder=diff_seconds%%3600
set /a minutes=remainder/60
set /a seconds=remainder%%60

REM Format duration
if !hours! gtr 0 (
    set DURATION=!hours!h !minutes!m !seconds!s
) else if !minutes! gtr 0 (
    set DURATION=!minutes!m !seconds!s
) else (
    set DURATION=!seconds!s
)
goto :EOF

:GENERATE_SUMMARY
REM Generate timing summary
call :LOG ""
call :LOG "=========================================="
call :LOG "PIPELINE TIMING SUMMARY"
call :LOG "=========================================="
echo.
echo ========================================
echo PIPELINE TIMING SUMMARY
echo ========================================
for /l %%i in (1,1,%STEP_COUNT%) do (
    echo %%i. !STEP_NAME[%%i]!: !STEP_DURATION[%%i]!
    call :LOG "%%i. !STEP_NAME[%%i]!: !STEP_DURATION[%%i]!"
)
call :LOG "=========================================="
call :LOG "Pipeline End Time: %date% %time%"
call :LOG "Log file saved to: %LOG_FILE%"
echo.
echo Log file saved to: %LOG_FILE%
goto :EOF