@echo off
REM GARCH order robustness experiment (NF gains with higher-order GARCH).

call scripts\utils\find_r_executable.bat
if %errorlevel% neq 0 (echo [ERROR] Rscript not found. & pause & exit /b 1)
"%RSCRIPT%" scripts/experiments/robustness_garch_order.R
if %errorlevel% neq 0 (echo [ERROR] Experiment failed. & pause & exit /b 1)
echo Results: outputs/robust_garch_order/
pause
