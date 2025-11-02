@echo off
REM Start Results Viewer - Manual Branch
REM Opens consolidated results in Excel or HTML viewer

echo ========================================
echo STARTING RESULTS VIEWER
echo ========================================
echo.
echo This will open the consolidated results in Excel.
echo.

REM Check for consolidated results
if exist "results\consolidated\Final_Dashboard.xlsx" (
    echo ✅ Opening Final Dashboard...
    start "" "results\consolidated\Final_Dashboard.xlsx"
    echo.
) else (
    echo ⚠️ Final Dashboard not found
)

if exist "results\consolidated\NF_GARCH_Results_manual.xlsx" (
    echo ✅ Opening NF-GARCH Results...
    start "" "results\consolidated\NF_GARCH_Results_manual.xlsx"
    echo.
) else (
    echo ⚠️ NF-GARCH Results not found
)

if exist "results\consolidated\NF_vs_Standard_GARCH_Comparison.xlsx" (
    echo ✅ Opening Comparison Results...
    start "" "results\consolidated\NF_vs_Standard_GARCH_Comparison.xlsx"
    echo.
) else (
    echo ⚠️ Comparison Results not found
)

echo.
echo ========================================
echo RESULTS VIEWER
echo ========================================
echo.
echo Available results files opened in Excel:
echo.
echo If files didn't open automatically, navigate to:
echo   results\consolidated\
echo.
echo Files:
if exist "results\consolidated\Final_Dashboard.xlsx" (
    echo   ✓ Final_Dashboard.xlsx
) else (
    echo   ✗ Final_Dashboard.xlsx (not found - run pipeline first)
)

if exist "results\consolidated\NF_GARCH_Results_manual.xlsx" (
    echo   ✓ NF_GARCH_Results_manual.xlsx
) else (
    echo   ✗ NF_GARCH_Results_manual.xlsx (not found - run pipeline first)
)

if exist "results\consolidated\NF_vs_Standard_GARCH_Comparison.xlsx" (
    echo   ✓ NF_vs_Standard_GARCH_Comparison.xlsx
) else (
    echo   ✗ NF_vs_Standard_GARCH_Comparison.xlsx (not found - run pipeline first)
)

echo.
pause
