@echo off
REM Optimized Step-by-Step Pipeline Execution
REM Targets 1-hour execution with proper checkpointing and logging

echo ========================================
echo OPTIMIZED NF-GARCH PIPELINE (STEP-BY-STEP)
echo ========================================
echo.

REM Initialize checkpointing
if not exist "checkpoints" mkdir checkpoints
if not exist "checkpoints\pipeline_status.json" (
    echo {} > checkpoints\pipeline_status.json
)

REM Set optimization parameters
set OPTIMIZED_ASSETS=6
set OPTIMIZED_MODELS=3
set OPTIMIZED_CV_FOLDS=3
set OPTIMIZED_NF_EPOCHS=50
set OPTIMIZED_BATCH_SIZE=256

echo OPTIMIZATION MODE ENABLED - Targeting 1-hour execution
echo - Using %OPTIMIZED_ASSETS% assets instead of 12
echo - Using %OPTIMIZED_MODELS% GARCH models instead of 4  
echo - Using %OPTIMIZED_CV_FOLDS%-fold CV instead of 5-fold
echo - Reduced NF training epochs to %OPTIMIZED_NF_EPOCHS%
echo - Core metrics only
echo.

REM Checkpoint management functions
:checkpoint_completed
set component=%1
echo [CHECKPOINT] %component% completed at %date% %time%
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "library(jsonlite); if(file.exists('checkpoints/pipeline_status.json')) { cp <- fromJSON('checkpoints/pipeline_status.json') } else { cp <- list() }; cp[['%component%']] <- list(status='completed', timestamp=Sys.time(), error=NULL); writeLines(toJSON(cp, auto_unbox=TRUE, pretty=TRUE), 'checkpoints/pipeline_status.json')"
goto :eof

:checkpoint_failed
set component=%1
set error_msg=%2
echo [CHECKPOINT] %component% failed at %date% %time% - %error_msg%
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "library(jsonlite); if(file.exists('checkpoints/pipeline_status.json')) { cp <- fromJSON('checkpoints/pipeline_status.json') } else { cp <- list() }; cp[['%component%']] <- list(status='failed', timestamp=Sys.time(), error='%error_msg%'); writeLines(toJSON(cp, auto_unbox=TRUE, pretty=TRUE), 'checkpoints/pipeline_status.json')"
goto :eof

REM Step 1: Pipeline Diagnostic
echo ========================================
echo STEP 1: PIPELINE DIAGNOSTIC
echo ========================================
echo Running quick pipeline diagnostic...
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\utils\pipeline_diagnostic.R
if %errorlevel% neq 0 (
    call :checkpoint_failed "pipeline_diagnostic" "Pipeline diagnostic failed"
    echo ERROR: Pipeline diagnostic failed
    pause
    exit /b 1
) else (
    call :checkpoint_completed "pipeline_diagnostic"
    echo ✓ Pipeline diagnostic completed successfully
)
echo.

REM Step 2: EDA Analysis
echo ========================================
echo STEP 2: EDA ANALYSIS
echo ========================================
echo Running quick EDA analysis...
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\eda\eda_summary_stats.R
if %errorlevel% neq 0 (
    call :checkpoint_failed "eda" "EDA analysis failed"
    echo ERROR: EDA analysis failed
    pause
    exit /b 1
) else (
    call :checkpoint_completed "eda"
    echo ✓ EDA analysis completed successfully
)
echo.

REM Step 3: GARCH Model Fitting
echo ========================================
echo STEP 3: GARCH MODEL FITTING
echo ========================================
echo Fitting GARCH models (optimized: %OPTIMIZED_MODELS% models, %OPTIMIZED_ASSETS% assets)...
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\model_fitting\fit_garch_models.R
if %errorlevel% neq 0 (
    call :checkpoint_failed "garch_fitting" "GARCH fitting failed"
    echo ERROR: GARCH fitting failed
    pause
    exit /b 1
) else (
    call :checkpoint_completed "garch_fitting"
    echo ✓ GARCH model fitting completed successfully
)
echo.

REM Step 4: Residual Extraction
echo ========================================
echo STEP 4: RESIDUAL EXTRACTION
echo ========================================
echo Extracting residuals for NF training...
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\model_fitting\extract_residuals.R
if %errorlevel% neq 0 (
    call :checkpoint_failed "residual_extraction" "Residual extraction failed"
    echo ERROR: Residual extraction failed
    pause
    exit /b 1
) else (
    call :checkpoint_completed "residual_extraction"
    echo ✓ Residual extraction completed successfully
)
echo.

REM Step 5: NF Model Training
echo ========================================
echo STEP 5: NF MODEL TRAINING
echo ========================================
echo Training NF models (optimized: %OPTIMIZED_NF_EPOCHS% epochs, batch size %OPTIMIZED_BATCH_SIZE%)...
python scripts\model_fitting\train_nf_models.py
if %errorlevel% neq 0 (
    call :checkpoint_failed "nf_training" "NF training failed"
    echo ERROR: NF training failed
    pause
    exit /b 1
) else (
    call :checkpoint_completed "nf_training"
    echo ✓ NF model training completed successfully
)
echo.

REM Step 6: NF Model Evaluation
echo ========================================
echo STEP 6: NF MODEL EVALUATION
echo ========================================
echo Evaluating NF models...
python scripts\model_fitting\evaluate_nf_fit.py
if %errorlevel% neq 0 (
    call :checkpoint_failed "nf_evaluation" "NF evaluation failed"
    echo ERROR: NF evaluation failed
    pause
    exit /b 1
) else (
    call :checkpoint_completed "nf_evaluation"
    echo ✓ NF model evaluation completed successfully
)
echo.

REM Step 7: NF-GARCH Simulation
echo ========================================
echo STEP 7: NF-GARCH SIMULATION
echo ========================================
echo Running NF-GARCH simulation (manual engine)...
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\simulation_forecasting\simulate_nf_garch_engine.R --engine manual
if %errorlevel% neq 0 (
    call :checkpoint_failed "nf_garch_manual" "NF-GARCH simulation failed"
    echo ERROR: NF-GARCH simulation failed
    pause
    exit /b 1
) else (
    call :checkpoint_completed "nf_garch_manual"
    echo ✓ NF-GARCH simulation completed successfully
)
echo.

REM Step 8: Forecasting
echo ========================================
echo STEP 8: FORECASTING
echo ========================================
echo Running forecasts...
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\simulation_forecasting\forecast_garch_variants.R
if %errorlevel% neq 0 (
    call :checkpoint_failed "forecasting" "Forecasting failed"
    echo ERROR: Forecasting failed
    pause
    exit /b 1
) else (
    call :checkpoint_completed "forecasting"
    echo ✓ Forecasting completed successfully
)
echo.

REM Step 9: Forecast Evaluation
echo ========================================
echo STEP 9: FORECAST EVALUATION
echo ========================================
echo Evaluating forecasts...
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\evaluation\wilcoxon_winrate_analysis.R
if %errorlevel% neq 0 (
    call :checkpoint_failed "forecast_evaluation" "Forecast evaluation failed"
    echo ERROR: Forecast evaluation failed
    pause
    exit /b 1
) else (
    call :checkpoint_completed "forecast_evaluation"
    echo ✓ Forecast evaluation completed successfully
)
echo.

REM Step 10: VaR Backtesting
echo ========================================
echo STEP 10: VAR BACKTESTING
echo ========================================
echo Running VaR backtesting...
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\evaluation\var_backtesting.R
if %errorlevel% neq 0 (
    call :checkpoint_failed "var_backtesting" "VaR backtesting failed"
    echo ERROR: VaR backtesting failed
    pause
    exit /b 1
) else (
    call :checkpoint_completed "var_backtesting"
    echo ✓ VaR backtesting completed successfully
)
echo.

REM Step 11: Final Summary
echo ========================================
echo STEP 11: FINAL SUMMARY
echo ========================================
echo Generating optimized final summary...
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "library(openxlsx); cat('=== OPTIMIZED NF-GARCH PIPELINE SUMMARY ===\n'); cat('Date:', Sys.Date(), '\n'); cat('Time:', Sys.time(), '\n\n'); cat('Assets processed:', %OPTIMIZED_ASSETS%, '\n'); cat('Models used:', %OPTIMIZED_MODELS%, '\n'); cat('CV folds:', %OPTIMIZED_CV_FOLDS%, '\n'); cat('NF epochs:', %OPTIMIZED_NF_EPOCHS%, '\n'); output_files <- list.files('outputs', recursive = TRUE, full.names = TRUE); cat('Output files generated:', length(output_files), '\n'); nf_files <- list.files('nf_generated_residuals', pattern = '*.csv', full.names = TRUE); cat('NF residual files:', length(nf_files), '\n'); cat('\n=== OPTIMIZED PIPELINE COMPLETE ===\n')"
if %errorlevel% neq 0 (
    call :checkpoint_failed "final_summary" "Final summary generation failed"
    echo ERROR: Final summary generation failed
    pause
    exit /b 1
) else (
    call :checkpoint_completed "final_summary"
    echo ✓ Final summary generated successfully
)
echo.

REM Step 12: Results Consolidation
echo ========================================
echo STEP 12: RESULTS CONSOLIDATION
echo ========================================
echo Consolidating optimized results...
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "source('scripts/core/consolidation.R'); consolidate_optimized_results()"
if %errorlevel% neq 0 (
    call :checkpoint_failed "consolidation" "Results consolidation failed"
    echo ERROR: Results consolidation failed
    pause
    exit /b 1
) else (
    call :checkpoint_completed "consolidation"
    echo ✓ Results consolidation completed successfully
)
echo.

echo ========================================
echo OPTIMIZED PIPELINE COMPLETE!
echo ========================================
echo.
echo OPTIMIZATION SUMMARY:
echo - Assets processed: %OPTIMIZED_ASSETS% (reduced from 12)
echo - Models used: %OPTIMIZED_MODELS% (reduced from 4)
echo - CV folds: %OPTIMIZED_CV_FOLDS% (reduced from 5)
echo - NF epochs: %OPTIMIZED_NF_EPOCHS% (reduced from 100)
echo - Batch size: %OPTIMIZED_BATCH_SIZE% (increased for efficiency)
echo.
echo Check the following directories for results:
echo - outputs\ (optimized analysis results)
echo - nf_generated_residuals\ (NF residual files)
echo - results\consolidated\ (consolidated results)
echo.
echo Statistical rigor maintained for:
echo - Core forecasting metrics (RMSE, MAE, LogLik)
echo - Distributional fit (KS, Wasserstein)
echo - Risk calibration (VaR backtesting)
echo - Essential stylized facts
echo.
pause


