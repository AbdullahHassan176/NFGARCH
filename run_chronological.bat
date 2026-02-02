@echo off
REM Chronological Split Pipeline (65/35)
REM Alternative validation using pure chronological split (no CV)

echo ==========================================
echo CHRONOLOGICAL SPLIT PIPELINE (65/35)
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

echo Running chronological pipeline...
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
echo Note: This is an alternative validation approach.
echo Main dissertation results are in outputs\manual\
echo.
pause
