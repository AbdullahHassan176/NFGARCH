@echo off
REM Run GARCH Order Robustness Experiment
REM This script runs the robustness test to check if NF gains persist with higher-order GARCH

echo === GARCH Order Robustness Experiment ===
echo.

REM Check if R is available
where Rscript >nul 2>&1
if %ERRORLEVEL% NEQ 0 (
    echo ERROR: Rscript not found in PATH
    echo Please ensure R is installed and Rscript is in your PATH
    pause
    exit /b 1
)

REM Run the experiment
echo Running robustness experiment...
Rscript scripts/experiments/robustness_garch_order.R

if %ERRORLEVEL% EQU 0 (
    echo.
    echo === Experiment completed successfully ===
    echo Results written to: outputs/robust_garch_order/
) else (
    echo.
    echo === Experiment failed ===
    echo Check the error messages above
)

pause

