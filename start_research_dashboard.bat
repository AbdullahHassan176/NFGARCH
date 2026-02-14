@echo off
cd /d "%~dp0"
if exist "%CD%\environment\R_library" set "R_LIBS=%CD%\environment\R_library"
set "DASH=%CD%\results\dashboard_visualizations.html"
set "EXCEL=%CD%\results\consolidated\Final_Dashboard.xlsx"

if exist "%DASH%" (
    start "" "%DASH%"
    goto :end
)

echo Generating dashboard...
call scripts\utils\find_r_executable.bat
if %errorlevel% neq 0 (echo [ERROR] Rscript not found. & pause & exit /b 1)
"%RSCRIPT%" scripts\evaluation\generate_dashboard_visualizations.R

if exist "%DASH%" (
    start "" "%DASH%"
) else if exist "%EXCEL%" (
    start "" "%EXCEL%"
) else (
    echo [ERROR] No dashboard found. Run the pipeline first.
    pause
    exit /b 1
)
:end
pause
