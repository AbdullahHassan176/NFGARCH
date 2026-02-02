@echo off
REM Time-Series Cross-Validation Pipeline - Wrapper
REM Runs from additional_analysis folder but uses parent repo infrastructure

echo ==========================================
echo TIME-SERIES CROSS-VALIDATION PIPELINE
echo ==========================================
echo Running from: additional_analysis\
echo Main repo: ..\
echo ==========================================
echo.

cd /d "%~dp0"

REM Find Rscript
call find_r_executable.bat
if %errorlevel% neq 0 (
    echo [ERROR] Failed to find Rscript executable
    pause
    exit /b 1
)

REM Change to parent repo to run
cd ..

echo Running TS-CV pipeline from main repo...
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
pause
