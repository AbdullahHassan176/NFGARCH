@echo off
cd /d "%~dp0"
REM Minimal rerun: stress testing (with fixed NF residual parsing) then extract crisis table only.
if exist "%~dp0environment\R_library" set "R_LIBS=%~dp0environment\R_library"
call "%~dp0scripts\utils\find_r_executable.bat"
if %errorlevel% neq 0 exit /b 1

echo [0/3] Ensuring R packages (xts, zoo) for stress script...
"%RSCRIPT%" -e "for (p in c('zoo','xts')) { if (!requireNamespace(p, quietly=TRUE)) install.packages(p, repos='https://cloud.r-project.org') }"
if %errorlevel% neq 0 echo Warning: package check had issues. Continuing.

echo [1/3] Running stress testing (--split manual). Expect ~5-15 min ...
"%RSCRIPT%" scripts\evaluation\stress_testing_comprehensive.R --split manual
if %errorlevel% neq 0 (echo Stress testing failed. & exit /b 1)

echo [2/3] Copying Stress_Testing.xlsx to results\consolidated ...
if not exist "results\consolidated" mkdir results\consolidated
if exist "results\manual\consolidated\Stress_Testing.xlsx" (
    copy /Y "results\manual\consolidated\Stress_Testing.xlsx" "results\consolidated\Stress_Testing.xlsx"
) else (
    echo Warning: results\manual\consolidated\Stress_Testing.xlsx not found.
)

echo [3/3] Running extract_dissertation_tables.R (refreshes crisis_forecast_performance.csv/.tex) ...
"%RSCRIPT%" scripts\evaluation\extract_dissertation_tables.R
if %errorlevel% neq 0 (echo Extract failed. & exit /b 1)

echo Done. Update the dissertation crisis table from results\dissertation_tables\crisis_forecast_performance.csv
exit /b 0
