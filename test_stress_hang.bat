@echo off
cd /d "%~dp0"

echo Testing STEP 9 - Stress Testing
echo.

REM Find Rscript
call scripts\utils\find_r_executable.bat
if %errorlevel% neq 0 (
    echo [ERROR] Rscript not found!
    pause
    exit /b 1
)

echo Using R: %RSCRIPT%
echo.

"%RSCRIPT%" "scripts/evaluation/stress_testing_comprehensive.R" --split chronological

echo.
echo Test complete. Check .cursor\debug.log for instrumentation data.
pause
