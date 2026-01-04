@echo off
REM Run GARCH Order Robustness Experiment
REM This script runs the robustness test to check if NF gains persist with higher-order GARCH

echo === GARCH Order Robustness Experiment ===
echo.

REM Find Rscript executable
call scripts\utils\find_r_executable.bat
if %errorlevel% neq 0 (
    echo [ERROR] Failed to find Rscript executable
    pause
    exit /b 1
)

REM Run the experiment
echo Running robustness experiment...
"%RSCRIPT%" scripts/experiments/robustness_garch_order.R

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

