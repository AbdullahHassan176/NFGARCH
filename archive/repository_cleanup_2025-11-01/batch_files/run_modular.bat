@echo off
REM Modular Pipeline Execution Script - Manual Branch Optimized
REM Checkpointed version with Manual branch optimizations and proper standardization
REM Allows independent execution of pipeline components with checkpointing

REM Check for help command first (before any initialization)
if "%1"=="help" (
    echo.
    echo ========================================
    echo MODULAR MANUAL PIPELINE HELP
    echo ========================================
    echo.
    echo USAGE:
    echo   run_modular.bat                    - Run full pipeline
    echo   run_modular.bat quick              - Run optimized pipeline (45-90 min)
    echo   run_modular.bat status             - Show pipeline status
    echo   run_modular.bat run <component>    - Run specific component
    echo   run_modular.bat reset <component>  - Reset component
    echo   run_modular.bat help               - Show this help
    echo.
    echo COMPONENTS:
    echo   clear_outputs         - Clear previous outputs
    echo   garch_fitting          - Manual GARCH fitting (optimized)
    echo   residual_extraction    - Extract residuals for NF training
    echo   nf_training            - Python NF model training (optimized)
    echo   nf_garch_simulation    - NF-GARCH simulation (with proper standardization)
    echo   nf_vs_standard         - Compare NF-GARCH vs Standard GARCH
    echo   verify_results         - Verify all results
    echo   consolidation          - Consolidate results
    echo   create_dashboard       - Create final dashboard
    echo.
    echo OPTIMIZATIONS:
    echo   - Assets: 6 (EURUSD, GBPUSD, USDZAR, NVDA, MSFT, AMZN)
    echo   - Models: 3 (sGARCH, eGARCH, TGARCH)
    echo   - CV: 3 folds, max 3 windows
    echo   - NF Training: 75 epochs, optimized architecture
    echo   - Proper NF residual standardization
    echo.
    echo EXAMPLES:
    echo   run_modular.bat quick                    - Run optimized pipeline
    echo   run_modular.bat run garch_fitting        - Run GARCH fitting only
    echo   run_modular.bat run nf_garch_simulation - Run NF-GARCH simulation
    echo   run_modular.bat status                   - Check pipeline status
    echo.
    echo ========================================
    echo.
    exit /b 0
)

echo ========================================
echo MODULAR MANUAL PIPELINE
echo ========================================
echo Optimizations: 6 assets, 3 models, 3 CV folds
echo Expected time: 45-90 minutes
echo ========================================

REM Initialize checkpointing
if not exist "checkpoints" mkdir checkpoints
if not exist "checkpoints\pipeline_status.json" (
    echo {} > checkpoints\pipeline_status.json
)
if not exist "results\consolidated" mkdir results\consolidated
if not exist "results\diagnostics" mkdir results\diagnostics

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

REM Component execution functions
:run_component
set component=%1
echo.
echo ========================================
echo RUNNING COMPONENT: %component%
echo ========================================

if "%component%"=="clear_outputs" (
    echo Clearing previous outputs...
    if exist "outputs\manual\garch_fitting" del /q "outputs\manual\garch_fitting\*.*" 2>nul
    if exist "outputs\manual\residuals_by_model" rd /s /q "outputs\manual\residuals_by_model" 2>nul
    if exist "outputs\manual\nf_models" (
        del /q "outputs\manual\nf_models\*.csv" 2>nul
        del /q "outputs\manual\nf_models\*.pth" 2>nul
    )
    if exist "results\consolidated" del /q "results\consolidated\*.xlsx" 2>nul
    if not exist "outputs\manual\garch_fitting" mkdir "outputs\manual\garch_fitting"
    if not exist "outputs\manual\residuals_by_model" mkdir "outputs\manual\residuals_by_model"
    if not exist "outputs\manual\nf_models" mkdir "outputs\manual\nf_models"
    if not exist "outputs\manual\evaluation" mkdir "outputs\manual\evaluation"
    echo ✅ Outputs cleared
    call :checkpoint_completed "clear_outputs"
    
) else if "%component%"=="garch_fitting" (
    echo Running optimized GARCH fitting...
    echo   Assets: 6, Models: 3, CV: 3 folds
    "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\manual\manual_garch_fitting.R
    if %errorlevel% neq 0 (
        call :checkpoint_failed "garch_fitting" "GARCH fitting failed"
        echo ❌ ERROR: GARCH fitting failed
        exit /b 1
    ) else (
        call :checkpoint_completed "garch_fitting"
    )
    
) else if "%component%"=="residual_extraction" (
    echo Extracting residuals for NF training...
    "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\model_fitting\extract_residuals.R
    if %errorlevel% neq 0 (
        call :checkpoint_failed "residual_extraction" "Residual extraction failed"
        echo ⚠️ WARNING: Residual extraction failed, continuing...
    ) else (
        call :checkpoint_completed "residual_extraction"
    )
    
) else if "%component%"=="nf_training" (
    echo Running optimized NF training...
    echo   Epochs: 75, Batch size: 512, Architecture: 4 layers
    python scripts\manual\manual_nf_training.py
    if %errorlevel% neq 0 (
        call :checkpoint_failed "nf_training" "NF training failed"
        echo ❌ ERROR: NF training failed
        exit /b 1
    ) else (
        call :checkpoint_completed "nf_training"
    )
    
) else if "%component%"=="nf_garch_simulation" (
    echo Running NF-GARCH simulation (with proper standardization)...
    echo   Engine: manual, Using: Standardized NF residuals
    "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\simulation_forecasting\simulate_nf_garch_engine.R --engine manual
    if %errorlevel% neq 0 (
        call :checkpoint_failed "nf_garch_simulation" "NF-GARCH simulation failed"
        echo ⚠️ WARNING: NF-GARCH simulation had issues
    ) else (
        call :checkpoint_completed "nf_garch_simulation"
    )
    
) else if "%component%"=="nf_vs_standard" (
    echo Running NF-GARCH vs Standard GARCH comparison...
    "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\evaluation\compare_nf_vs_standard_garch.R
    if %errorlevel% neq 0 (
        call :checkpoint_failed "nf_vs_standard" "Comparison failed"
        echo ⚠️ WARNING: Comparison had issues
    ) else (
        call :checkpoint_completed "nf_vs_standard"
    )
    
) else if "%component%"=="distributional_metrics" (
    echo Calculating distributional metrics...
    "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\evaluation\calculate_distributional_metrics.R
    if %errorlevel% neq 0 (
        call :checkpoint_failed "distributional_metrics" "Distributional metrics failed"
        echo ⚠️ WARNING: Distributional metrics had issues
    ) else (
        call :checkpoint_completed "distributional_metrics"
    )
    
) else if "%component%"=="stylized_facts" (
    echo Calculating stylized facts...
    "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\evaluation\calculate_stylized_facts.R
    if %errorlevel% neq 0 (
        call :checkpoint_failed "stylized_facts" "Stylized facts failed"
        echo ⚠️ WARNING: Stylized facts had issues
    ) else (
        call :checkpoint_completed "stylized_facts"
    )
    
) else if "%component%"=="var_backtesting" (
    echo Running VaR backtesting...
    "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\evaluation\var_backtesting_comprehensive.R
    if %errorlevel% neq 0 (
        call :checkpoint_failed "var_backtesting" "VaR backtesting failed"
        echo ⚠️ WARNING: VaR backtesting had issues
    ) else (
        call :checkpoint_completed "var_backtesting"
    )
    
) else if "%component%"=="stress_testing" (
    echo Running stress testing...
    "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\evaluation\stress_testing_comprehensive.R
    if %errorlevel% neq 0 (
        call :checkpoint_failed "stress_testing" "Stress testing failed"
        echo ⚠️ WARNING: Stress testing had issues
    ) else (
        call :checkpoint_completed "stress_testing"
    )
    
) else if "%component%"=="verify_results" (
    echo Verifying all results...
    "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\evaluation\verify_all_results.R
    if %errorlevel% neq 0 (
        call :checkpoint_failed "verify_results" "Verification failed"
        echo ⚠️ WARNING: Verification had issues
    ) else (
        call :checkpoint_completed "verify_results"
    )
    
) else if "%component%"=="consolidation" (
    echo Consolidating results...
    "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "source('scripts/core/consolidation.R'); consolidate_all_results('results/consolidated')"
    if %errorlevel% neq 0 (
        call :checkpoint_failed "consolidation" "Consolidation failed"
        echo ⚠️ WARNING: Consolidation had issues
    ) else (
        call :checkpoint_completed "consolidation"
    )
    
) else if "%component%"=="create_dashboard" (
    echo Creating final dashboard...
    "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\core\create_final_dashboard.R
    if %errorlevel% neq 0 (
        call :checkpoint_failed "create_dashboard" "Dashboard creation failed"
        echo ⚠️ WARNING: Dashboard creation had issues
    ) else (
        call :checkpoint_completed "create_dashboard"
    )
    
) else (
    echo ❌ ERROR: Unknown component '%component%'
    echo Available components: clear_outputs, garch_fitting, residual_extraction
    echo   nf_training, nf_garch_simulation, nf_vs_standard, distributional_metrics
    echo   stylized_facts, var_backtesting, stress_testing, verify_results
    echo   consolidation, create_dashboard
    exit /b 1
)
goto :eof

REM Main execution logic
if "%1"=="" (
    REM Full pipeline execution
    echo.
    echo Running FULL pipeline with Manual optimizations...
    echo.
    call :run_component "clear_outputs"
    call :run_component "garch_fitting"
    call :run_component "residual_extraction"
    call :run_component "nf_training"
    call :run_component "nf_garch_simulation"
    call :run_component "nf_vs_standard"
    call :run_component "distributional_metrics"
    call :run_component "stylized_facts"
    call :run_component "var_backtesting"
    call :run_component "stress_testing"
    call :run_component "verify_results"
    call :run_component "consolidation"
    call :run_component "create_dashboard"
    
    echo.
    echo ========================================
    echo PIPELINE COMPLETE!
    echo ========================================
    echo.
    echo Results saved to:
    echo   - results\consolidated\NF_GARCH_Results_manual.xlsx
    echo   - results\consolidated\NF_vs_Standard_GARCH_Comparison.xlsx
    echo   - results\consolidated\Final_Dashboard.xlsx
    echo.
    
) else if "%1"=="quick" (
    REM Optimized quick pipeline
    echo.
    echo Running QUICK optimized pipeline...
    echo.
    call :run_component "clear_outputs"
    call :run_component "garch_fitting"
    call :run_component "nf_training"
    call :run_component "nf_garch_simulation"
    call :run_component "nf_vs_standard"
    call :run_component "consolidation"
    call :run_component "create_dashboard"
    
    echo.
    echo ========================================
    echo QUICK PIPELINE COMPLETE!
    echo ========================================
    
) else if "%1"=="status" (
    echo Checking pipeline status...
    echo.
    if exist "checkpoints\pipeline_status.json" (
        "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "library(jsonlite); cp <- fromJSON('checkpoints/pipeline_status.json'); if(length(cp) > 0) { cat('=== PIPELINE STATUS ===\n'); for(i in names(cp)) { cat(sprintf('%-20s: %s (%s)\n', i, cp[[i]]$status, cp[[i]]$timestamp)) }; cat('\n') } else { cat('No checkpoints found.\n') }"
    ) else (
        echo No checkpoints found.
    )
    
) else if "%1"=="run" (
    if "%2"=="" (
        echo ❌ ERROR: Please specify a component to run
        echo Run 'run_modular.bat help' for component list
        exit /b 1
    ) else (
        call :run_component "%2"
    )
    
) else if "%1"=="reset" (
    if "%2"=="" (
        echo ❌ ERROR: Please specify a component to reset
        exit /b 1
    ) else (
        echo Resetting component: %2
        "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "library(jsonlite); if(file.exists('checkpoints/pipeline_status.json')) { cp <- fromJSON('checkpoints/pipeline_status.json'); cp[['%2']] <- NULL; writeLines(toJSON(cp, auto_unbox=TRUE, pretty=TRUE), 'checkpoints/pipeline_status.json'); cat('Component %2 reset.\n') } else { cat('No checkpoints found.\n') }"
    )
    
) else (
    echo ❌ ERROR: Unknown command '%1'
    echo Run 'run_modular.bat help' for usage information
    exit /b 1
)

echo.
pause
