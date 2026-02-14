@echo off
REM Time-Series Cross-Validation Pipeline - Entry point
REM Runs the FULL TS-CV pipeline from repo root (GARCH + NF training + simulation + evaluation + reports)

echo ==========================================
echo TIME-SERIES CROSS-VALIDATION PIPELINE
echo ==========================================
echo Running FULL pipeline from repo root...
echo   - Step 1:  Clear TS-CV outputs
echo   - Step 2:  GARCH fitting (rolling TS-CV)
echo   - Step 3:  NF training (per window)
echo   - Step 4:  NF-GARCH simulation
echo   - Steps 5-22: Comparison, distributional metrics, VaR, dashboard, tables, Overleaf export
echo ==========================================
echo.

cd /d "%~dp0"
REM Go to repo root and run the comprehensive TS-CV batch
cd ..
call run_tscv.bat %*
if %errorlevel% neq 0 (
    echo [ERROR] TS-CV pipeline returned an error
    pause
    exit /b 1
)
exit /b 0
