@echo off
setlocal enabledelayedexpansion
REM =============================================================================
REM Run Full Dissertation Pipeline
REM Reruns the entire analysis (main pipeline + all additional analysis) and
REM updates overleaf_export\ so everything is ready to import into Overleaf.
REM =============================================================================

cd /d "%~dp0"

if /i "%~1"=="/OverleafOnly" (
  set OLEAF=1
  echo ========================================
  echo OVERLEAF EXPORT ONLY
  echo ========================================
  echo Refreshing overleaf_export from existing results...
  echo.
  goto :overleaf_export
)

echo ========================================
echo FULL DISSERTATION PIPELINE
echo ========================================
echo.
echo This will:
echo  1. Find R and run the full main pipeline (run_all.bat /Y)
echo  2. Run GARCH order robustness (outputs/robust_garch_order)
echo  3. Run complete analysis (results/consolidated/Analysis_Summary.xlsx)
echo  4. Export tables and figures to overleaf_export\ for Overleaf import
echo.
echo Expected time: 60-120+ minutes
echo ========================================
echo.

REM --- Resolve Rscript ---
call scripts\utils\find_r_executable.bat
if %errorlevel% neq 0 (
  echo [ERROR] Rscript not found. Install R or set RSCRIPT.
  pause
  exit /b 1
)
if not defined RSCRIPT (
  echo [ERROR] RSCRIPT not set after find_r_executable.
  pause
  exit /b 1
)
echo Using Rscript: %RSCRIPT%
echo.

REM When called with /Y, skip confirmation (for non-interactive/automated runs)
if /i not "%~1"=="/Y" (
  set /p confirm="Run full pipeline + additional analyses and refresh Overleaf export? (Y/N): "
  if /i not "!confirm!"=="Y" (
    echo Cancelled.
    pause
    exit /b 0
  )
)
echo.

REM =============================================================================
REM 1. MAIN PIPELINE (run_all.bat /Y)
REM =============================================================================
echo ========================================
echo STEP 1: MAIN PIPELINE (run_all.bat)
echo ========================================
call run_all.bat /Y
if %errorlevel% neq 0 (
  echo [ERROR] Main pipeline failed. Stopping.
  pause
  exit /b 1
)
echo.

REM =============================================================================
REM 2. GARCH ORDER ROBUSTNESS
REM =============================================================================
echo ========================================
echo STEP 2: GARCH ORDER ROBUSTNESS
echo ========================================
"%RSCRIPT%" scripts\experiments\robustness_garch_order.R
if %errorlevel% neq 0 (
  echo [WARNING] robustness_garch_order.R had issues; continuing...
) else (
  echo [OK] GARCH order robustness completed
)
echo.

REM =============================================================================
REM 3. COMPLETE ANALYSIS
REM =============================================================================
echo ========================================
echo STEP 3: COMPLETE ANALYSIS
echo ========================================
"%RSCRIPT%" scripts\complete_analysis.R
if %errorlevel% neq 0 (
  echo [WARNING] complete_analysis.R had issues; continuing...
) else (
  echo [OK] Complete analysis - Analysis_Summary.xlsx updated
)
echo.

REM =============================================================================
REM 4. OVERLEAF EXPORT
REM =============================================================================
:overleaf_export
echo ========================================
echo STEP 4: OVERLEAF EXPORT
echo ========================================

if not exist "overleaf_export" mkdir "overleaf_export"
if not exist "overleaf_export\tables" mkdir "overleaf_export\tables"
if not exist "overleaf_export\figures" mkdir "overleaf_export\figures"

REM Dissertation tables (CSVs from extract_dissertation_tables.R)
if exist "results\dissertation_tables" (
  copy /Y "results\dissertation_tables\*.*" "overleaf_export\tables\" >nul 2>&1
  echo   Copied results\dissertation_tables\* to overleaf_export\tables\
) else (
  echo   [NOTE] results\dissertation_tables not found
)

REM GARCH order robustness LaTeX table
if exist "outputs\robust_garch_order\garch_order_robustness_table.tex" (
  copy /Y "outputs\robust_garch_order\garch_order_robustness_table.tex" "overleaf_export\tables\" >nul
  echo   Copied garch_order_robustness_table.tex to overleaf_export\tables\
) else (
  echo   [NOTE] garch_order_robustness_table.tex not found
)

REM Dissertation figures (PNGs from generate_report_figures.R)
if exist "results\figures" (
  copy /Y "results\figures\*.*" "overleaf_export\figures\" >nul 2>&1
  echo   Copied results\figures\* to overleaf_export\figures\
) else (
  echo   [NOTE] results\figures not found
)

REM Overleaf import instructions
(
  echo NF-GARCH dissertation - Overleaf import
  echo ======================================
  echo.
  echo 1. Copy the contents of  overleaf_export/tables/  into your Overleaf project
  echo    ^(e.g. a  tables/  folder^). The folder contains .tex table bodies and .csv files.
  echo    In your main .tex set  \newcommand{\tablesdir}{tables/}  then use e.g.:
  echo      \input{\tablesdir stylized_facts_summary.tex}
  echo      \input{\tablesdir garch_order_robustness_table.tex}
  echo    See  results/dissertation_tables/DISSERTATION_TABLE_INPUTS.md  for the full list.
  echo.
  echo 2. Copy the contents of  overleaf_export/figures/  into your Overleaf project
  echo    ^(e.g. a  figures/  folder^). Reference them in LaTeX with  \includegraphics[width=...]{figures/filename}.
  echo.
  echo 3. Re-run this batch after any pipeline or analysis changes to refresh these exports.
) > "overleaf_export\OVERLEAF_IMPORT.txt"
echo   Created overleaf_export\OVERLEAF_IMPORT.txt
echo.

REM =============================================================================
REM SUMMARY
REM =============================================================================
:summary
echo ========================================
if defined OLEAF (echo OVERLEAF EXPORT COMPLETED) else (echo FULL DISSERTATION PIPELINE COMPLETED)
echo ========================================
echo.
echo Overleaf-ready export:
echo   - overleaf_export\tables\     ^(dissertation CSVs + garch_order_robustness_table.tex^)
echo   - overleaf_export\figures\    ^(dissertation figures: Fig-R1, Fig-R2/R3, Fig-R4/R5, Fig-R7, Fig-R8^)
echo   - overleaf_export\OVERLEAF_IMPORT.txt
echo.
echo Source paths:
echo   - results\dissertation_tables\
echo   - results\figures\
echo   - outputs\robust_garch_order\garch_order_robustness_table.tex
echo.
echo ========================================
pause
