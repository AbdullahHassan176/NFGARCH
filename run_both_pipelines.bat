@echo off
setlocal enabledelayedexpansion
REM Runs chronological then TS-CV pipeline.
REM Pass /WithReviewer3 as first argument to run chronological main pipeline plus MDPI Risks Reviewer 3 supplement (multi-seed MAF+RealNVP).

if /I "%~1"=="/WithReviewer3" (
    call "%~dp0run_chronological.bat" /WithReviewer3
) else (
    call "%~dp0run_chronological.bat"
)

if %errorlevel% neq 0 (
    echo.
    echo [ERROR] Chronological pipeline failed!
    pause
    exit /b 1
)

call "%~dp0run_tscv.bat"
if %errorlevel% neq 0 echo [WARNING] TS-CV pipeline had issues.
echo Done. Results: results\chronological\, results\tscv\, overleaf_export\
pause
