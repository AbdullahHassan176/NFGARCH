@echo off
REM Chronological Split Pipeline (65/35) - Wrapper
REM Runs from additional_analysis folder but uses parent repo infrastructure

echo ==========================================
echo CHRONOLOGICAL SPLIT PIPELINE (65/35)
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

echo Running chronological pipeline from main repo...
echo.

REM Run chronological fit
"%RSCRIPT%" additional_analysis\scripts\chronological\fit_garch_chronological.R

if %errorlevel% neq 0 (
    echo [ERROR] Chronological GARCH fitting failed
    pause
    exit /b 1
)

echo.
echo === Chronological Pipeline Complete ===
echo.
echo Results saved to:
echo   - outputs\chronological\
echo   - results\chronological\
echo.
pause
