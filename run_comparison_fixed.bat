@echo off
setlocal enabledelayedexpansion

cd /d "%~dp0"

echo ========================================================================
echo Running Fixed Standard GARCH Comparison (Parametric Sampling)
echo ========================================================================
echo.

REM Find R executable using the same method as main pipeline
call scripts\utils\find_r_executable.bat
if %errorlevel% neq 0 (
  echo [ERROR] Rscript not found
  exit /b 1
)

echo Using R: %RSCRIPT%
echo.

REM Run the comparison script
"%RSCRIPT%" scripts/evaluation/compare_nf_vs_standard_garch.R

if %errorlevel% neq 0 (
  echo.
  echo [ERROR] Script failed with exit code %errorlevel%
  exit /b %errorlevel%
)

echo.
echo ========================================================================
echo Comparison Complete!
echo ========================================================================
echo.
echo Results saved to: results/chronological/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx
echo.

exit /b 0
