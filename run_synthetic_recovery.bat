@echo off
REM Synthetic Distribution Recovery Experiment Runner
REM Runs the complete experiment with one command

echo === Synthetic Distribution Recovery Experiment ===
echo.

cd /d "%~dp0"

REM Find Rscript executable
call scripts\utils\find_r_executable.bat
if %errorlevel% neq 0 (
    echo [ERROR] Failed to find Rscript executable
    pause
    exit /b 1
)

REM Run the main experiment script
"%RSCRIPT%" scripts\experiments\synthetic_recovery\run_synthetic_recovery.R

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo ERROR: Experiment failed with exit code %ERRORLEVEL%
    pause
    exit /b %ERRORLEVEL%
)

echo.
echo === Experiment Complete ===
echo Results saved to: outputs\synthetic_recovery\
echo.
pause

