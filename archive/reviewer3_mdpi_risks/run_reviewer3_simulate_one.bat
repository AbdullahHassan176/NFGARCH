@echo off
REM SUPERSEDED — simulate one reviewer3 folder (from repo root), example:
REM   set NF_RESIDUALS_ONLY_DIR=%CD%\outputs\reviewer3\maf_seed123
REM   set REVIEWER3_RUN_ROOT=%CD%\outputs\reviewer3\maf_seed123
REM   set REVIEWER3_REPRODUCIBILITY_SEED=123
REM   Rscript scripts\simulation_forecasting\simulate_nf_garch_engine.R --split chronological
cd /d "%~dp0..\.."
if "%~1"=="" (
  echo Usage: run from repo root with original args, or use full chain: python scripts\manual\run_reviewer3_full_chain.py --runs ^<run_id^>
  exit /b 1
)
set "RUN_ID=%~1"
set "RSEED=%~2"
if "%RSEED%"=="" set "RSEED=123"
call scripts\utils\find_r_executable.bat
if %errorlevel% neq 0 exit /b 1
set "NF_RESIDUALS_ONLY_DIR=%CD%\outputs\reviewer3\%RUN_ID%"
set "REVIEWER3_RUN_ROOT=%CD%\outputs\reviewer3\%RUN_ID%"
set "REVIEWER3_REPRODUCIBILITY_SEED=%RSEED%"
"%RSCRIPT%" scripts\simulation_forecasting\simulate_nf_garch_engine.R --split chronological
exit /b %errorlevel%
