@echo off
REM Time-Series Cross-Validation Pipeline
REM Alternative validation using rolling TS-CV windows

echo ==========================================
echo TIME-SERIES CROSS-VALIDATION PIPELINE
echo ==========================================
echo Alternative validation approach
echo ==========================================
echo.

cd /d "%~dp0"

REM Find Rscript
call scripts\utils\find_r_executable.bat
if %errorlevel% neq 0 (
    echo [ERROR] Failed to find Rscript executable
    pause
    exit /b 1
)

echo Running TS-CV pipeline...
echo.

REM Run TS-CV fit
"%RSCRIPT%" additional_analysis\scripts\tscv\fit_garch_tscv.R

if %errorlevel% neq 0 (
    echo [ERROR] TS-CV GARCH fitting failed
    pause
    exit /b 1
)

echo.
echo === TS-CV Pipeline Complete ===
echo.
echo Results saved to:
echo   - outputs\tscv\
echo   - results\tscv\
echo.
echo Note: This is an alternative validation approach.
echo Main dissertation results are in outputs\manual\
echo.
pause
