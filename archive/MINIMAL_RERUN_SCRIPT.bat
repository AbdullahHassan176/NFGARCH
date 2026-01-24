@echo off
REM Minimal Rerun - subset of pipeline (archived)
REM Run from project root: archive\MINIMAL_RERUN_SCRIPT.bat

cd /d "%~dp0\.."

set RSCRIPT="C:\Program Files\R\R-4.5.1\bin\Rscript.exe"

echo MINIMAL RERUN - Simulation, comparison, stress, consolidation, tables
echo.

%RSCRIPT% scripts\simulation_forecasting\simulate_nf_garch_engine.R --engine manual
if %errorlevel% neq 0 (echo [ERROR] Simulation failed & pause & exit /b 1)

%RSCRIPT% scripts\evaluation\compare_nf_vs_standard_garch.R
%RSCRIPT% scripts\evaluation\stress_testing_comprehensive.R
%RSCRIPT% -e "source('scripts/core/consolidation.R'); consolidate_all_results('results/consolidated')"
%RSCRIPT% scripts\evaluation\extract_dissertation_tables.R

echo MINIMAL RERUN COMPLETED
pause
