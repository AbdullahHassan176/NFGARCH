@echo off
setlocal enabledelayedexpansion
REM Run Both Comprehensive Validation Pipelines - Wrapper
REM Executes chronological and TS CV pipelines sequentially
REM Both pipelines now include FULL NF training, evaluation, and dissertation outputs

echo ==========================================
echo DUAL COMPREHENSIVE VALIDATION PIPELINE
echo ==========================================
echo.
echo This will run BOTH fully comprehensive validation pipelines:
echo.
echo 1. CHRONOLOGICAL SPLIT (65/35) - 90-180 minutes
echo    - Pure chronological validation
echo    - Full NF-GARCH training, simulation, and evaluation
echo    - All methodology validation tests
echo    - Complete dissertation outputs and Overleaf export
echo.
echo 2. TIME-SERIES CROSS-VALIDATION - 120-240 minutes
echo    - Rolling window TS-CV validation
echo    - Full NF-GARCH training, simulation, and evaluation
echo    - All methodology validation tests
echo    - Complete dissertation outputs and Overleaf export
echo.
echo TOTAL EXPECTED TIME: 3.5-7 hours (FULLY COMPREHENSIVE)
echo ==========================================
echo.
echo Both pipelines now include:
echo  - GARCH fitting with respective splitting strategy
echo  - NF training on residuals
echo  - NF-GARCH simulation
echo  - Complete evaluation (distributional, stylized facts, VaR, stress)
echo  - Methodology validation (stationarity, heterogeneity)
echo  - GARCH order robustness
echo  - Final dashboards (Excel + HTML)
echo  - Dissertation tables and figures
echo  - Overleaf export
echo.
echo These are ALTERNATIVE validation approaches to the main pipeline.
echo They provide robustness checks using different data splitting strategies.
echo ==========================================
echo.
echo Starting pipelines automatically...
echo.
echo Starting pipeline 1 of 2...
echo.
echo [TRACE] Current directory: %cd%
echo [TRACE] About to call %~dp0run_chronological.bat

call "%~dp0run_chronological.bat"

echo [TRACE] Returned from run_chronological.bat with errorlevel=%errorlevel%

if %errorlevel% neq 0 (
    echo.
    echo [ERROR] Chronological pipeline failed!
    pause
    exit /b 1
)

echo.
echo Starting pipeline 2 of 2...
echo.
echo [TRACE] About to call %~dp0run_tscv.bat

call "%~dp0run_tscv.bat"

echo [TRACE] Returned from run_tscv.bat with errorlevel=%errorlevel%

if %errorlevel% neq 0 (
    echo.
    echo [WARNING] TS CV pipeline had issues
)

echo.
echo ==========================================
echo DUAL COMPREHENSIVE PIPELINE COMPLETED
echo ==========================================
echo.
echo Both pipelines have completed successfully!
echo.
echo CHRONOLOGICAL PIPELINE RESULTS:
echo   - results\chronological\ (all outputs, dashboards, tables, figures)
echo   - overleaf_export\chronological\
echo.
echo TS-CV PIPELINE RESULTS:
echo   - results\tscv\ (all outputs, dashboards, tables, figures)
echo   - overleaf_export\tscv\
echo.
echo Each pipeline includes:
echo   - Full NF-GARCH training and simulation results
echo   - Complete evaluation metrics and comparisons
echo   - Methodology validation tests
echo   - GARCH order robustness analysis
echo   - Final dashboards (Excel + HTML with visualizations)
echo   - Dissertation tables (LaTeX/CSV format)
echo   - Dissertation figures (publication-ready PNGs)
echo   - Overleaf-ready export packages
echo.
echo These are COMPREHENSIVE alternative validation approaches that can
echo serve as standalone analysis pipelines or robustness checks.
echo.
echo Compare results across pipelines to assess:
echo   - Model stability across different splitting strategies
echo   - Validation approach sensitivity
echo   - Robustness of NF-GARCH improvements
echo.
echo ==========================================
pause
