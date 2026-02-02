@echo off
REM Run Synthetic Recovery Experiment Audit
REM Runs experiment with fixes and generates audit report

echo === Synthetic Recovery Experiment Audit ===
echo.

cd /d "%~dp0"

REM Find Rscript executable
call scripts\utils\find_r_executable.bat
if %errorlevel% neq 0 (
    echo [ERROR] Failed to find Rscript executable
    pause
    exit /b 1
)

REM Run the full audit
"%RSCRIPT%" scripts\experiments\synthetic_recovery\run_full_audit.R

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo ERROR: Audit failed with exit code %ERRORLEVEL%
    pause
    exit /b %ERRORLEVEL%
)

echo.
echo === Audit Complete ===
pause

