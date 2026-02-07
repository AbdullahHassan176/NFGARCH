@echo off
REM Run the fixed Standard GARCH comparison with parametric sampling

cd /d "%~dp0"

REM Hardcode R path
set "RSCRIPT=C:\Program Files\R\R-4.4.2\bin\Rscript.exe"

echo.
echo ========================================================================
echo Running Fixed Standard GARCH Comparison (with parametric sampling)
echo ========================================================================
echo Using: %RSCRIPT%
echo.

"%RSCRIPT%" scripts/evaluation/compare_nf_vs_standard_garch.R
if %errorlevel% neq 0 (
  echo.
  echo [ERROR] Comparison script failed
  exit /b 1
)

echo.
echo ========================================================================
echo Comparison complete!
echo ========================================================================
echo.

exit /b 0
