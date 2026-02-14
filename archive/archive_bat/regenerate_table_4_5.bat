@echo off
cd /d "%~dp0"
REM Regenerate Table 4.5 (detailed NF vs Standard) from pipeline output.
REM Requires: NF_vs_Standard_GARCH_Comparison.xlsx in results/consolidated
REM   or in results/chronological/consolidated or results/manual/consolidated (e.g. after run_chronological.bat).

if exist "environment\R_library" set "R_LIBS=%CD%\environment\R_library"
call scripts\utils\find_r_executable.bat
if %errorlevel% neq 0 (
  echo [ERROR] Rscript not found.
  pause
  exit /b 1
)

echo Regenerating dissertation tables (including Table 4.5)...
"%RSCRIPT%" scripts\evaluation\extract_dissertation_tables.R
if %errorlevel% neq 0 (
  echo [ERROR] Extract failed. Do you have the comparison file?
  echo Look for: results\consolidated\NF_vs_Standard_GARCH_Comparison.xlsx
  echo   or: results\chronological\consolidated\NF_vs_Standard_GARCH_Comparison.xlsx
  echo If missing, run the pipeline first: run_chronological.bat
  pause
  exit /b 1
)

echo.
echo Table 4.5 source written to:
echo   results\dissertation_tables\detailed_nf_vs_standard.tex
echo   results\dissertation_tables\detailed_nf_vs_standard.csv
echo.
echo To update the thesis: open detailed_nf_vs_standard.tex, copy the
echo tabularx block (from \begin{tabularx} to \end{tabularx}), and
echo replace the table body in wits project template.tex (Table 4.5).
echo.
pause
