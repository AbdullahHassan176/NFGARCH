@echo off
REM Main entry point for Manual Branch Pipeline
REM This script runs the optimized manual pipeline

echo ========================================
echo FINANCIAL-SDG-GARCH - MANUAL PIPELINE
echo ========================================
echo.
echo Running optimized Manual pipeline...
echo Expected time: 45-90 minutes
echo.

REM Change to script directory and run
cd /d "%~dp0"
call scripts\manual\run_manual_optimized.bat

pause

