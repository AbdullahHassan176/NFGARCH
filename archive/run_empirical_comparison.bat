@echo off
REM Run comparison to empirical test set residuals (archived)
REM Run from project root: archive\run_empirical_comparison.bat

cd /d "%~dp0\.."

echo ========================================
echo Comparing to Empirical Test Residuals
echo ========================================
echo.
echo This script compares NF-GARCH and Standard GARCH residuals
echo to ACTUAL empirical test set residuals (not just to each other).
echo.

call scripts\utils\find_r_executable.bat
if %errorlevel% neq 0 (
    echo [ERROR] Failed to find Rscript executable
    pause
    exit /b 1
)

"%RSCRIPT%" archive\scripts_tests\compare_to_empirical_test_residuals.R

if %ERRORLEVEL% EQU 0 (
    echo.
    echo Analysis completed. Results: outputs/evaluation/comparison_to_empirical_test_residuals.xlsx
) else (
    echo ERROR: Analysis failed
    exit /b %ERRORLEVEL%
)
pause
