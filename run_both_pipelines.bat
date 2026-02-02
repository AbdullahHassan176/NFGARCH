@echo off
REM Run Both Validation Pipelines - Wrapper
REM Executes chronological and TS CV pipelines sequentially

echo ==========================================
echo DUAL VALIDATION PIPELINE EXECUTION
echo ==========================================
echo.
echo This will run BOTH validation pipelines:
echo.
echo 1. CHRONOLOGICAL SPLIT (65/35) - ~2 hours
echo 2. TIME-SERIES CROSS-VALIDATION - ~6-8 hours
echo.
echo TOTAL EXPECTED TIME: 8-10 hours
echo ==========================================
echo.

set /p confirm="Run BOTH pipelines? (Y/N): "
if /i not "%confirm%"=="Y" (
    echo Cancelled.
    exit /b 0
)

echo.
echo Starting pipeline 1 of 2...
echo.

call run_chronological.bat /Y

if %errorlevel% neq 0 (
    echo.
    echo [ERROR] Chronological pipeline failed!
    pause
    exit /b 1
)

echo.
echo Starting pipeline 2 of 2...
echo.

call run_tscv.bat /Y

if %errorlevel% neq 0 (
    echo.
    echo [WARNING] TS CV pipeline had issues
)

echo.
echo ==========================================
echo DUAL PIPELINE EXECUTION COMPLETED
echo ==========================================
echo.
echo Results available in:
echo   - outputs\chronological\
echo   - outputs\tscv\
echo   - results\chronological\
echo   - results\tscv\
echo.
echo Note: These are alternative validation approaches.
echo Main dissertation results are in outputs\manual\
echo.
pause
