@echo off
REM Financial-SDG-GARCH - Modular Pipeline with Checkpointing and Logging
REM Allows step-by-step execution with checkpointing, detailed logging, and resume capability
REM Does everything that run_all.bat does but with modular approach

setlocal enabledelayedexpansion

REM =============================================================================
REM CONFIGURATION
REM =============================================================================

set "CHECKPOINT_DIR=checkpoints"
set "LOG_DIR=logs"
set "STATUS_FILE=%CHECKPOINT_DIR%\pipeline_status.json"
set "LOG_FILE=%LOG_DIR%\pipeline_%date:~-4,4%%date:~-7,2%%date:~-10,2%_%time:~0,2%%time:~3,2%%time:~6,2%.log"
set "LOG_FILE=%LOG_FILE: =0%"
set "RSCRIPT=C:\Program Files\R\R-4.5.1\bin\Rscript.exe"

REM Create directories
if not exist "%CHECKPOINT_DIR%" mkdir "%CHECKPOINT_DIR%"
if not exist "%LOG_DIR%" mkdir "%LOG_DIR%"

REM =============================================================================
REM INITIALIZATION
REM =============================================================================

echo ========================================
echo FINANCIAL-SDG-GARCH - MODULAR PIPELINE
echo ========================================
echo.
echo Modular execution with checkpointing and detailed logging
echo.
echo Features:
echo   - Step-by-step execution with checkpoints
echo   - Resume from any completed step
echo   - Detailed logging to: %LOG_FILE%
echo   - Progress tracking and status saving
echo   - Ability to skip or retry individual steps
echo.
echo Optimizations:
echo   - Assets: 6 (EURUSD, GBPUSD, USDZAR, NVDA, MSFT, AMZN)
echo   - Models: 4 (sGARCH, eGARCH, TGARCH, gjrGARCH)
echo   - CV: 3 folds, max 3 windows
echo   - NF Training: 75 epochs, optimized architecture
echo   - Seed: 123 (for reproducibility)
echo.
echo Expected time: 45-90 minutes (depending on steps)
echo ========================================
echo.

REM Initialize log file
(
    echo === MODULAR PIPELINE LOG ===
    echo Started: %date% %time%
    echo Log file: %LOG_FILE%
    echo Checkpoint file: %STATUS_FILE%
    echo.
) > "%LOG_FILE%"

REM Check for checkpoint file
set "RESUME_MODE=0"
if exist "%STATUS_FILE%" (
    echo Found checkpoint file: %STATUS_FILE%
    echo.
    echo Current pipeline status:
    %RSCRIPT% scripts\utils\checkpoint_manager.R status
    echo.
    set /p resume="Resume from checkpoint? (Y/N): "
    if /i "!resume!"=="Y" (
        set "RESUME_MODE=1"
        echo Resuming from checkpoint (will skip completed steps).
        echo [%date% %time%] Resuming from checkpoint >> "%LOG_FILE%"
    ) else (
        set "RESUME_MODE=0"
        echo Starting fresh (existing checkpoints will be cleared).
        %RSCRIPT% -e "source('scripts/utils/checkpoint_manager.R'); clear_all_checkpoints()" 2>nul
        echo [%date% %time%] Starting fresh pipeline >> "%LOG_FILE%"
    )
    echo.
)

REM =============================================================================
REM HELPER FUNCTIONS
REM =============================================================================

REM Check if step is completed
:is_completed
set "STEP_NUM=%~1"
set "STEP_COMPLETE=0"
set "CHECK_RESULT="
if exist "%STATUS_FILE%" (
    for /f "delims=" %%i in ('%RSCRIPT% scripts\utils\checkpoint_manager.R check %STEP_NUM% 2^>nul') do set "CHECK_RESULT=%%i"
    if "!CHECK_RESULT!"=="COMPLETED" (
        set "STEP_COMPLETE=1"
    )
)
set "%~2=!STEP_COMPLETE!"
goto :eof

REM Log message
:log_message
echo [%date% %time%] %~1 >> "%LOG_FILE%"
goto :eof

REM Save checkpoint
:save_checkpoint
set "STEP_NUM=%~1"
set "STEP_NAME=%~2"
set "STATUS=%~3"
set "ERROR_MSG=%~4"
if "!ERROR_MSG!"=="" set "ERROR_MSG=NULL"

%RSCRIPT% scripts\utils\checkpoint_manager.R save %STEP_NUM% "!STEP_NAME!" !STATUS! "!ERROR_MSG!" 2>nul
echo [%date% %time%] Checkpoint saved: Step %STEP_NUM% - !STEP_NAME! - !STATUS! >> "%LOG_FILE%"
goto :eof

REM Execute step with checkpointing and logging
:execute_step
set "STEP_NUM=%~1"
set "STEP_NAME=%~2"
set "STEP_DESC=%~3"
set "STEP_CMD=%~4"
set "STEP_FILE=%~5"
set "STEP_SKIPABLE=%~6"

echo.
echo ========================================
echo STEP %STEP_NUM%: %STEP_NAME%
echo ========================================
echo.
echo %STEP_DESC%
echo.

REM Check if already completed (if resume mode)
if "%RESUME_MODE%"=="1" (
    call :is_completed %STEP_NUM% STEP_COMPLETE_CHECK
    if !STEP_COMPLETE_CHECK! equ 1 (
        echo [OK] Step %STEP_NUM% already completed (skipping)
        call :log_message "Step %STEP_NUM% skipped (already completed)"
        goto :step_complete
    )
)

REM Log step start
call :log_message "========================================"
call :log_message "Starting Step %STEP_NUM%: %STEP_NAME%"
call :log_message "Description: %STEP_DESC%"
call :log_message "Command: %STEP_CMD%"
if not "%STEP_FILE%"=="" (
    call :log_message "Expected output: %STEP_FILE%"
)
call :log_message "----------------------------------------"

REM Display execution details
echo Executing: %STEP_CMD%
if not "%STEP_FILE%"=="" (
    echo Expected output: %STEP_FILE%
)
echo.
echo [Logging to: %LOG_FILE%]
echo.

REM Record start time
set "START_TIME=%time%"

REM Execute command
%STEP_CMD%
set "STEP_ERRORLEVEL=!errorlevel!"

REM Record end time
set "END_TIME=%time%"

REM Check result
if !STEP_ERRORLEVEL! neq 0 (
    echo.
    echo [ERROR] Step %STEP_NUM% failed with error code !STEP_ERRORLEVEL!
    echo.
    echo Check log file for details: %LOG_FILE%
    echo.
    call :log_message "Step %STEP_NUM% FAILED with error code !STEP_ERRORLEVEL!"
    call :log_message "Start time: %START_TIME%"
    call :log_message "End time: %END_TIME%"
    call :log_message "Duration: (check manually)"
    
    REM Save failed checkpoint
    call :save_checkpoint %STEP_NUM% "%STEP_NAME%" "failed" "Error code: !STEP_ERRORLEVEL!"
    
    REM Handle based on skipable flag
    if "%STEP_SKIPABLE%"=="1" (
        echo.
        echo [WARNING] This step is optional - continuing to next step...
        call :log_message "Step %STEP_NUM% failed but is optional - continuing"
        goto :step_complete
    ) else (
        echo.
        set /p continue="Step failed. Continue to next step anyway? (Y/N): "
        if /i not "!continue!"=="Y" (
            echo Pipeline stopped by user.
            call :log_message "Pipeline stopped by user at Step %STEP_NUM%"
            pause
            exit /b !STEP_ERRORLEVEL!
        ) else (
            call :log_message "User chose to continue after Step %STEP_NUM% failure"
        )
    )
) else (
    echo.
    echo [OK] Step %STEP_NUM% completed successfully
    call :log_message "Step %STEP_NUM% completed successfully"
    call :log_message "Start time: %START_TIME%"
    call :log_message "End time: %END_TIME%"
    
    REM Save successful checkpoint
    call :save_checkpoint %STEP_NUM% "%STEP_NAME%" "completed"
    
    REM Verify output file exists if specified
    if not "%STEP_FILE%"=="" (
        if exist "%STEP_FILE%" (
            echo   Verified output: %STEP_FILE%
            call :log_message "Verified output exists: %STEP_FILE%"
            
            REM Get file size for logging
            for %%F in ("%STEP_FILE%") do (
                call :log_message "Output file size: %%~zF bytes"
            )
        ) else (
            echo   [WARNING] Expected output file not found: %STEP_FILE%
            call :log_message "WARNING: Expected output not found: %STEP_FILE%"
        )
    )
)

:step_complete
call :log_message "========================================"
echo.
set /p continue="Press Enter to continue to next step (or Ctrl+C to stop)..."
echo.
goto :eof

REM =============================================================================
REM CLEAR OUTPUTS FUNCTION
REM =============================================================================

:clear_outputs
echo Clearing outputs/manual directories...
call :log_message "Clearing outputs..."

if exist "outputs\manual\garch_fitting" (
    del /q "outputs\manual\garch_fitting\*.*" 2>nul
    echo   Cleared: garch_fitting
    call :log_message "  Cleared: outputs/manual/garch_fitting"
)
if exist "outputs\manual\residuals_by_model" (
    rd /s /q "outputs\manual\residuals_by_model" 2>nul
    echo   Cleared: residuals_by_model
    call :log_message "  Cleared: outputs/manual/residuals_by_model"
)
if exist "outputs\manual\nf_models" (
    del /q "outputs\manual\nf_models\*.csv" 2>nul
    del /q "outputs\manual\nf_models\*.pth" 2>nul
    echo   Cleared: nf_models (residuals and models)
    call :log_message "  Cleared: outputs/manual/nf_models (CSV and PTH files)"
)

REM Clear consolidated results
echo Clearing consolidated results...
if exist "results\consolidated" (
    del /q "results\consolidated\*.xlsx" 2>nul
    echo   Cleared: consolidated results
    call :log_message "  Cleared: results/consolidated/*.xlsx"
)

REM Clear dashboard plots (optional - keep diagnostics)
if exist "results\dashboard_plots" (
    del /q "results\dashboard_plots\*.png" 2>nul
    echo   Cleared: dashboard plots
    call :log_message "  Cleared: results/dashboard_plots/*.png"
)

REM Recreate directory structure
if not exist "outputs\manual" mkdir "outputs\manual"
if not exist "outputs\manual\garch_fitting" mkdir "outputs\manual\garch_fitting"
if not exist "outputs\manual\residuals_by_model" mkdir "outputs\manual\residuals_by_model"
if not exist "outputs\manual\nf_models" mkdir "outputs\manual\nf_models"
if not exist "outputs\manual\evaluation" mkdir "outputs\manual\evaluation"
if not exist "results\consolidated" mkdir "results\consolidated"
if not exist "results\diagnostics" mkdir "results\diagnostics"
if not exist "results\dashboard_plots" mkdir "results\dashboard_plots"
if not exist "%CHECKPOINT_DIR%" mkdir "%CHECKPOINT_DIR%"
if not exist "%LOG_DIR%" mkdir "%LOG_DIR%"

echo [OK] Outputs cleared and directories recreated
call :log_message "Outputs cleared and directories recreated"
goto :eof

REM =============================================================================
REM STEP 1: CLEARING PREVIOUS OUTPUTS
REM =============================================================================

set "STEP_1_NUM=1"
set "STEP_1_NAME=CLEARING PREVIOUS OUTPUTS"
set "STEP_1_DESC=Clearing outputs/manual directories and consolidated results"
set "STEP_1_CMD=call :clear_outputs"
set "STEP_1_FILE="
set "STEP_1_SKIPABLE=0"
call :execute_step !STEP_1_NUM! "!STEP_1_NAME!" "!STEP_1_DESC!" "!STEP_1_CMD!" "!STEP_1_FILE!" "!STEP_1_SKIPABLE!"

REM =============================================================================
REM STEP 2: GARCH FITTING (Optimized)
REM =============================================================================

set "STEP_2_NUM=2"
set "STEP_2_NAME=GARCH FITTING"
set "STEP_2_DESC=Running optimized GARCH fitting for 6 assets, 4 models (sGARCH, eGARCH, TGARCH, gjrGARCH). Estimated time: 30 minutes"
set "STEP_2_CMD=%RSCRIPT% scripts\manual\manual_garch_fitting.R"
set "STEP_2_FILE=outputs\manual\garch_fitting\model_summary.csv"
set "STEP_2_SKIPABLE=0"
call :execute_step !STEP_2_NUM! "!STEP_2_NAME!" "!STEP_2_DESC!" "!STEP_2_CMD!" "!STEP_2_FILE!" "!STEP_2_SKIPABLE!"

REM =============================================================================
REM STEP 3: NF TRAINING (Optimized)
REM =============================================================================

set "STEP_3_NUM=3"
set "STEP_3_NAME=NF TRAINING"
set "STEP_3_DESC=Training Normalizing Flows on GARCH residuals. Epochs: 75, Batch size: 512, Architecture: 4 layers, 64 hidden features. Estimated time: 20 minutes"
set "STEP_3_CMD=python scripts\manual\manual_nf_training.py"
set "STEP_3_FILE=outputs\manual\nf_models\training_summary.json"
set "STEP_3_SKIPABLE=0"
call :execute_step !STEP_3_NUM! "!STEP_3_NAME!" "!STEP_3_DESC!" "!STEP_3_CMD!" "!STEP_3_FILE!" "!STEP_3_SKIPABLE!"

REM =============================================================================
REM STEP 4: NF-GARCH SIMULATION
REM =============================================================================

set "STEP_4_NUM=4"
set "STEP_4_NAME=NF-GARCH SIMULATION"
set "STEP_4_DESC=Running NF-GARCH simulation with properly standardized residuals. Engine: manual. Estimated time: 15 minutes"
set "STEP_4_CMD=%RSCRIPT% scripts\simulation_forecasting\simulate_nf_garch_engine.R --engine manual"
set "STEP_4_FILE=results\consolidated\NF_GARCH_Results_manual.xlsx"
set "STEP_4_SKIPABLE=1"
call :execute_step !STEP_4_NUM! "!STEP_4_NAME!" "!STEP_4_DESC!" "!STEP_4_CMD!" "!STEP_4_FILE!" "!STEP_4_SKIPABLE!"

REM =============================================================================
REM STEP 5: COMPARE NF-GARCH vs STANDARD GARCH
REM =============================================================================

set "STEP_5_NUM=5"
set "STEP_5_NAME=NF-GARCH vs STANDARD GARCH COMPARISON"
set "STEP_5_DESC=Comparing NF-GARCH and Standard GARCH performance with statistical tests (Wilcoxon, asset-class analysis). Estimated time: 5 minutes"
set "STEP_5_CMD=%RSCRIPT% scripts\evaluation\compare_nf_vs_standard_garch.R"
set "STEP_5_FILE=results\consolidated\NF_vs_Standard_GARCH_Comparison.xlsx"
set "STEP_5_SKIPABLE=1"
call :execute_step !STEP_5_NUM! "!STEP_5_NAME!" "!STEP_5_DESC!" "!STEP_5_CMD!" "!STEP_5_FILE!" "!STEP_5_SKIPABLE!"

REM =============================================================================
REM STEP 6: CALCULATE DISTRIBUTIONAL METRICS
REM =============================================================================

set "STEP_6_NUM=6"
set "STEP_6_NAME=DISTRIBUTIONAL METRICS"
set "STEP_6_DESC=Calculating KS distance, Wasserstein distance, Tail index (Hill estimator), Skewness, Kurtosis. Estimated time: 3 minutes"
set "STEP_6_CMD=%RSCRIPT% scripts\evaluation\calculate_distributional_metrics.R"
set "STEP_6_FILE=results\consolidated\Distributional_Metrics.xlsx"
set "STEP_6_SKIPABLE=1"
call :execute_step !STEP_6_NUM! "!STEP_6_NAME!" "!STEP_6_DESC!" "!STEP_6_CMD!" "!STEP_6_FILE!" "!STEP_6_SKIPABLE!"

REM =============================================================================
REM STEP 7: CALCULATE STYLIZED FACTS
REM =============================================================================

set "STEP_7_NUM=7"
set "STEP_7_NAME=STYLIZED FACTS"
set "STEP_7_DESC=Calculating volatility clustering, leverage effects, autocorrelation decay, heavy tails, gain/loss asymmetry. Estimated time: 3 minutes"
set "STEP_7_CMD=%RSCRIPT% scripts\evaluation\calculate_stylized_facts.R"
set "STEP_7_FILE=results\consolidated\Stylized_Facts.xlsx"
set "STEP_7_SKIPABLE=1"
call :execute_step !STEP_7_NUM! "!STEP_7_NAME!" "!STEP_7_DESC!" "!STEP_7_CMD!" "!STEP_7_FILE!" "!STEP_7_SKIPABLE!"

REM =============================================================================
REM STEP 8: VaR BACKTESTING
REM =============================================================================

set "STEP_8_NUM=8"
set "STEP_8_NAME=VaR BACKTESTING"
set "STEP_8_DESC=Running VaR backtesting with Kupiec test, Christoffersen test, exceedance rates, Expected Shortfall. Estimated time: 5 minutes"
set "STEP_8_CMD=%RSCRIPT% scripts\evaluation\var_backtesting_comprehensive.R"
set "STEP_8_FILE=results\consolidated\VaR_Backtesting.xlsx"
set "STEP_8_SKIPABLE=1"
call :execute_step !STEP_8_NUM! "!STEP_8_NAME!" "!STEP_8_DESC!" "!STEP_8_CMD!" "!STEP_8_FILE!" "!STEP_8_SKIPABLE!"

REM =============================================================================
REM STEP 9: STRESS TESTING
REM =============================================================================

set "STEP_9_NUM=9"
set "STEP_9_NAME=STRESS TESTING"
set "STEP_9_DESC=Running stress tests for historical crises (2008 GFC, 2020 COVID) and hypothetical shocks (price drops, volatility spikes, mean shifts). Estimated time: 5 minutes"
set "STEP_9_CMD=%RSCRIPT% scripts\evaluation\stress_testing_comprehensive.R"
set "STEP_9_FILE=results\consolidated\Stress_Testing.xlsx"
set "STEP_9_SKIPABLE=1"
call :execute_step !STEP_9_NUM! "!STEP_9_NAME!" "!STEP_9_DESC!" "!STEP_9_CMD!" "!STEP_9_FILE!" "!STEP_9_SKIPABLE!"

REM =============================================================================
REM STEP 10: VERIFY RESULTS
REM =============================================================================

set "STEP_10_NUM=10"
set "STEP_10_NAME=VERIFY RESULTS"
set "STEP_10_DESC=Verifying all results and checking data integrity. Checking file existence, data completeness, and consistency. Estimated time: 2 minutes"
set "STEP_10_CMD=%RSCRIPT% scripts\evaluation\verify_all_results.R"
set "STEP_10_FILE=results\diagnostics\verification_summary.csv"
set "STEP_10_SKIPABLE=1"
call :execute_step !STEP_10_NUM! "!STEP_10_NAME!" "!STEP_10_DESC!" "!STEP_10_CMD!" "!STEP_10_FILE!" "!STEP_10_SKIPABLE!"

REM =============================================================================
REM STEP 11: CONSOLIDATE RESULTS
REM =============================================================================

set "STEP_11_NUM=11"
set "STEP_11_NAME=CONSOLIDATE RESULTS"
set "STEP_11_DESC=Consolidating all results into unified format. Merging all Excel files and creating summary tables. Estimated time: 3 minutes"
set "STEP_11_CMD=%RSCRIPT% -e \"source('scripts/core/consolidation.R'); consolidate_all_results('results/consolidated')\""
set "STEP_11_FILE=results\consolidated\consolidated_results.xlsx"
set "STEP_11_SKIPABLE=1"
call :execute_step !STEP_11_NUM! "!STEP_11_NAME!" "!STEP_11_DESC!" "!STEP_11_CMD!" "!STEP_11_FILE!" "!STEP_11_SKIPABLE!"

REM =============================================================================
REM STEP 12: CREATE FINAL EXCEL DASHBOARD
REM =============================================================================

set "STEP_12_NUM=12"
set "STEP_12_NAME=CREATE EXCEL DASHBOARD"
set "STEP_12_DESC=Creating comprehensive Excel dashboard with all metrics and analyses. Includes executive summary, performance metrics, distributional metrics, stylized facts, VaR backtesting, stress testing. Estimated time: 2 minutes"
set "STEP_12_CMD=%RSCRIPT% scripts\core\create_final_dashboard.R"
set "STEP_12_FILE=results\consolidated\Final_Dashboard.xlsx"
set "STEP_12_SKIPABLE=1"
call :execute_step !STEP_12_NUM! "!STEP_12_NAME!" "!STEP_12_DESC!" "!STEP_12_CMD!" "!STEP_12_FILE!" "!STEP_12_SKIPABLE!"

REM =============================================================================
REM STEP 13: GENERATE HTML DASHBOARD VISUALIZATIONS
REM =============================================================================

set "STEP_13_NUM=13"
set "STEP_13_NAME=GENERATE HTML DASHBOARD"
set "STEP_13_DESC=Generating visualization plots (13 PNG files) and interactive HTML dashboard with explanations, citations, and links. Estimated time: 3 minutes"
set "STEP_13_CMD=%RSCRIPT% scripts\evaluation\generate_dashboard_visualizations.R"
set "STEP_13_FILE=results\dashboard_visualizations.html"
set "STEP_13_SKIPABLE=1"
call :execute_step !STEP_13_NUM! "!STEP_13_NAME!" "!STEP_13_DESC!" "!STEP_13_CMD!" "!STEP_13_FILE!" "!STEP_13_SKIPABLE!"

REM =============================================================================
REM FINAL SUMMARY
REM =============================================================================

echo.
echo ========================================
echo PIPELINE COMPLETED SUCCESSFULLY
echo ========================================
echo.

call :log_message "========================================"
call :log_message "Pipeline completed successfully"
call :log_message "End time: %date% %time%"
call :log_message "========================================"

echo All steps completed!
echo.
echo Results saved to:
echo   - results\consolidated\NF_GARCH_Results_manual.xlsx
echo   - results\consolidated\NF_vs_Standard_GARCH_Comparison.xlsx
echo   - results\consolidated\Distributional_Metrics.xlsx
echo   - results\consolidated\Stylized_Facts.xlsx
echo   - results\consolidated\VaR_Backtesting.xlsx
echo   - results\consolidated\Stress_Testing.xlsx
echo   - results\consolidated\Final_Dashboard.xlsx (Excel dashboard)
echo   - results\dashboard_visualizations.html (Interactive HTML dashboard)
echo   - results\dashboard_plots\ (13 visualization plots)
echo.
echo Checkpoint file: %STATUS_FILE%
echo Log file: %LOG_FILE%
echo.
echo Final status:
%RSCRIPT% scripts\utils\checkpoint_manager.R status 2>nul
echo.
echo Next steps:
echo   1. Review results in: results\consolidated\
echo   2. View dashboards:
echo      - Excel: Final_Dashboard.xlsx
echo      - HTML: dashboard_visualizations.html (run start_research_dashboard.bat)
echo   3. Check log file for detailed execution information: %LOG_FILE%
echo.
echo ========================================
echo.
pause
