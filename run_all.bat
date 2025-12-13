@echo off
REM Financial-SDG-GARCH - Complete Manual Pipeline
REM Clears outputs, runs entire pipeline, compares NF-GARCH vs Standard GARCH
REM Produces evaluation summaries and dashboard
REM Uses Manual branch optimizations (6 assets, 3 models, fast CV)
REM Includes proper NF residual standardization

echo ========================================
echo FINANCIAL-SDG-GARCH - MANUAL PIPELINE
echo ========================================
echo.
echo This will:
echo  1. Clear previous outputs and results
echo  2. Run complete optimized pipeline (60-120 minutes)
echo  3. Compare NF-GARCH vs Standard GARCH
echo  4. Evaluate and summarize results
echo  5. Run methodology validation tests
echo  6. Create final dashboard
echo  7. Generate dissertation tables and figures
echo.
echo Optimizations:
echo  - Assets: 6 (EURUSD, GBPUSD, USDZAR, NVDA, MSFT, AMZN)
echo  - Models: 4 (sGARCH, eGARCH, TGARCH, gjrGARCH)
echo  - CV: 3 folds, max 3 windows
echo  - NF Training: 75 epochs, optimized architecture
echo  - Seed: 123 (for reproducibility)
echo.
echo Expected time: 60-120 minutes (includes all validation and academic outputs)
echo ========================================
echo.

REM Confirm before proceeding
set /p confirm="Continue with pipeline? (Y/N): "
if /i not "%confirm%"=="Y" (
    echo Pipeline cancelled.
    exit /b 0
)

echo.
echo ========================================
echo STEP 1: CLEARING PREVIOUS OUTPUTS
echo ========================================
echo.

REM Clear outputs (but keep essential structure)
echo Clearing outputs/manual directories...
if exist "outputs\manual\garch_fitting" (
    del /q "outputs\manual\garch_fitting\*.*" 2>nul
    echo   Cleared: garch_fitting
)
if exist "outputs\manual\residuals_by_model" (
    rd /s /q "outputs\manual\residuals_by_model" 2>nul
    echo   Cleared: residuals_by_model
)
if exist "outputs\manual\nf_models" (
    del /q "outputs\manual\nf_models\*.csv" 2>nul
    del /q "outputs\manual\nf_models\*.pth" 2>nul
    echo   Cleared: nf_models (residuals and models)
)

REM Clear consolidated results (but keep directory structure)
echo Clearing consolidated results...
if exist "results\consolidated" (
    del /q "results\consolidated\*.xlsx" 2>nul
    echo   Cleared: consolidated results
)

REM Recreate directory structure
if not exist "outputs\manual" mkdir "outputs\manual"
if not exist "outputs\manual\garch_fitting" mkdir "outputs\manual\garch_fitting"
if not exist "outputs\manual\residuals_by_model" mkdir "outputs\manual\residuals_by_model"
if not exist "outputs\manual\nf_models" mkdir "outputs\manual\nf_models"
if not exist "outputs\manual\evaluation" mkdir "outputs\manual\evaluation"
if not exist "results\consolidated" mkdir "results\consolidated"
if not exist "results\diagnostics" mkdir "results\diagnostics"
if not exist "results\dissertation_tables" mkdir "results\dissertation_tables"
if not exist "results\figures" mkdir "results\figures"

echo.
echo [OK] Outputs cleared and directories recreated
echo.

REM =============================================================================
REM STEP 2: GARCH FITTING (Optimized)
REM =============================================================================

echo ========================================
echo STEP 2: GARCH FITTING (30 minutes)
echo ========================================
echo.
echo Running optimized GARCH fitting...
echo   Assets: EURUSD, GBPUSD, USDZAR, NVDA, MSFT, AMZN (6 total)
echo   Models: sGARCH, eGARCH, TGARCH (3 total)
echo   CV: 3 folds, optimized windows
echo.

"%RSCRIPT%" scripts\manual\manual_garch_fitting.R
if %errorlevel% neq 0 (
    echo [ERROR] GARCH fitting failed
    echo Check outputs\manual\garch_fitting\ for details
    pause
    exit /b 1
) else (
    echo [OK] GARCH fitting completed successfully
)
echo.

REM =============================================================================
REM STEP 3: NF TRAINING (Optimized)
REM =============================================================================

echo ========================================
echo STEP 3: NF TRAINING (20 minutes)
echo ========================================
echo.
echo Running optimized NF training...
echo   Epochs: 75 (reduced from 100)
echo   Batch size: 512 (increased for GPU utilization)
echo   Architecture: 4 layers, 64 hidden features
echo.

python scripts\manual\manual_nf_training.py
if %errorlevel% neq 0 (
    echo [ERROR] NF training failed
    echo Check outputs\manual\nf_models\ for details
    pause
    exit /b 1
) else (
    echo [OK] NF training completed successfully
)
echo.

REM =============================================================================
REM STEP 4: NF-GARCH SIMULATION (with proper standardization)
REM =============================================================================

echo ========================================
echo STEP 4: NF-GARCH SIMULATION (15 minutes)
echo ========================================
echo.
echo Running NF-GARCH simulation...
echo   Engine: manual
echo   Using: Properly standardized NF residuals
echo.

"%RSCRIPT%" scripts\simulation_forecasting\simulate_nf_garch_engine.R --engine manual
if %errorlevel% neq 0 (
    echo [WARNING] NF-GARCH simulation had issues, continuing...
) else (
    echo [OK] NF-GARCH simulation completed
)
echo.

REM =============================================================================
REM STEP 5: COMPARE NF-GARCH vs STANDARD GARCH
REM =============================================================================

echo ========================================
echo STEP 5: NF-GARCH vs STANDARD GARCH COMPARISON
echo ========================================
echo.
echo Running comparison analysis...
echo.

"%RSCRIPT%" scripts\evaluation\compare_nf_vs_standard_garch.R
if %errorlevel% neq 0 (
    echo [WARNING] Comparison analysis had issues, continuing...
) else (
    echo [OK] Comparison analysis completed
)
echo.

REM =============================================================================
REM STEP 6: CALCULATE DISTRIBUTIONAL METRICS
REM =============================================================================

echo ========================================
echo STEP 6: CALCULATING DISTRIBUTIONAL METRICS
echo ========================================
echo.
echo Calculating KS distance, Wasserstein, Tail index, Skewness, Kurtosis...
echo.

"%RSCRIPT%" scripts\evaluation\calculate_distributional_metrics.R
if %errorlevel% neq 0 (
    echo [WARNING] Distributional metrics calculation had issues, continuing...
) else (
    echo [OK] Distributional metrics calculated
)
echo.

REM =============================================================================
REM STEP 7: CALCULATE STYLIZED FACTS
REM =============================================================================

echo ========================================
echo STEP 7: CALCULATING STYLIZED FACTS
echo ========================================
echo.
echo Calculating volatility clustering, leverage effects, autocorrelation...
echo.

"%RSCRIPT%" scripts\evaluation\calculate_stylized_facts.R
if %errorlevel% neq 0 (
    echo [WARNING] Stylized facts calculation had issues, continuing...
) else (
    echo [OK] Stylized facts calculated
)
echo.

REM =============================================================================
REM STEP 8: VaR BACKTESTING
REM =============================================================================

echo ========================================
echo STEP 8: VaR BACKTESTING
echo ========================================
echo.
echo Running VaR backtesting (Kupiec, Christoffersen)...
echo.

"%RSCRIPT%" scripts\evaluation\var_backtesting_comprehensive.R
if %errorlevel% neq 0 (
    echo [WARNING] VaR backtesting had issues, continuing...
) else (
    echo [OK] VaR backtesting completed
)
echo.

REM =============================================================================
REM STEP 9: STRESS TESTING
REM =============================================================================

echo ========================================
echo STEP 9: STRESS TESTING
echo ========================================
echo.
echo Running stress tests (historical crises, hypothetical shocks)...
echo.

"%RSCRIPT%" scripts\evaluation\stress_testing_comprehensive.R
if %errorlevel% neq 0 (
    echo [WARNING] Stress testing had issues, continuing...
) else (
    echo [OK] Stress testing completed
)
echo.

REM =============================================================================
REM STEP 10: METHODOLOGY VALIDATION - RESIDUAL STATIONARITY
REM =============================================================================

echo ========================================
echo STEP 10: METHODOLOGY VALIDATION - RESIDUAL STATIONARITY
echo ========================================
echo.
echo Testing GARCH residuals for stationarity (ADF, KPSS, Ljung-Box, ARCH tests)...
echo.

"%RSCRIPT%" scripts\evaluation\test_residual_stationarity.R
if %errorlevel% neq 0 (
    echo [WARNING] Residual stationarity testing had issues, continuing...
) else (
    echo [OK] Residual stationarity testing completed
)
echo.

REM =============================================================================
REM STEP 11: METHODOLOGY VALIDATION - CONDITIONAL HETEROGENEITY
REM =============================================================================

echo ========================================
echo STEP 11: METHODOLOGY VALIDATION - CONDITIONAL HETEROGENEITY
echo ========================================
echo.
echo Testing for conditional heterogeneity in GARCH residuals...
echo.

"%RSCRIPT%" scripts\evaluation\test_conditional_heterogeneity.R
if %errorlevel% neq 0 (
    echo [WARNING] Conditional heterogeneity testing had issues, continuing...
) else (
    echo [OK] Conditional heterogeneity testing completed
)
echo.

REM =============================================================================
REM STEP 12: VERIFY RESULTS
REM =============================================================================

echo ========================================
echo STEP 12: VERIFYING RESULTS
echo ========================================
echo.
echo Verifying all results...
echo.

"%RSCRIPT%" scripts\evaluation\verify_all_results.R
if %errorlevel% neq 0 (
    echo [WARNING] Verification had issues
) else (
    echo [OK] Results verification completed
)
echo.

REM =============================================================================
REM STEP 13: CONSOLIDATE RESULTS
REM =============================================================================

echo ========================================
echo STEP 13: CONSOLIDATING RESULTS
echo ========================================
echo.
echo Creating consolidated results...
echo.

"%RSCRIPT%" -e "source('scripts/core/consolidation.R'); consolidate_all_results('results/consolidated')"
if %errorlevel% neq 0 (
    echo [WARNING] Consolidation had issues, continuing...
) else (
    echo [OK] Results consolidated
)
echo.

REM =============================================================================
REM STEP 14: CREATE HYPERPARAMETER SENSITIVITY SUMMARY
REM =============================================================================

echo ========================================
echo STEP 14A: CREATING HYPERPARAMETER SENSITIVITY SUMMARY
echo ========================================
echo.
echo Creating hyperparameter selection methodology documentation...
echo.

"%RSCRIPT%" scripts\evaluation\create_hyperparameter_summary.R
if %errorlevel% neq 0 (
    echo [WARNING] Hyperparameter summary creation had issues, continuing...
) else (
    echo [OK] Hyperparameter summary created
)
echo.

REM =============================================================================
REM STEP 14: CREATE METHODOLOGY CONSOLIDATED DOCUMENTATION
REM =============================================================================

echo ========================================
echo STEP 14: CREATING METHODOLOGY CONSOLIDATED DOCUMENTATION
echo ========================================
echo.
echo Consolidating methodology validation results...
echo.

"%RSCRIPT%" scripts\evaluation\create_methodology_consolidated.R
if %errorlevel% neq 0 (
    echo [WARNING] Methodology consolidation had issues, continuing...
) else (
    echo [OK] Methodology consolidated documentation created
)
echo.

REM =============================================================================
REM STEP 15: CREATE FINAL DASHBOARD
REM =============================================================================

echo ========================================
echo STEP 15: CREATING FINAL DASHBOARD
echo ========================================
echo.
echo Creating comprehensive Excel dashboard...
echo.

"%RSCRIPT%" scripts\core\create_final_dashboard.R
if %errorlevel% neq 0 (
    echo [WARNING] Dashboard creation had issues, continuing...
) else (
    echo [OK] Final Excel dashboard created
)
echo.

REM =============================================================================
REM STEP 16: GENERATE HTML DASHBOARD VISUALIZATIONS
REM =============================================================================

echo ========================================
echo STEP 16: GENERATING HTML DASHBOARD
echo ========================================
echo.
echo Generating visualization plots and HTML dashboard...
echo.

"%RSCRIPT%" scripts\evaluation\generate_dashboard_visualizations.R
if %errorlevel% neq 0 (
    echo [WARNING] HTML dashboard generation had issues, continuing...
) else (
    echo [OK] HTML dashboard visualizations generated
)
echo.

REM =============================================================================
REM STEP 17: EXTRACT DISSERTATION TABLES
REM =============================================================================

echo ========================================
echo STEP 17: EXTRACTING DISSERTATION TABLES
echo ========================================
echo.
echo Generating LaTeX tables for dissertation...
echo.

"%RSCRIPT%" scripts\evaluation\extract_dissertation_tables.R
if %errorlevel% neq 0 (
    echo [WARNING] Dissertation tables extraction had issues, continuing...
) else (
    echo [OK] Dissertation tables extracted
)
echo.

REM =============================================================================
REM STEP 18: GENERATE DISSERTATION FIGURES
REM =============================================================================

echo ========================================
echo STEP 18: GENERATING DISSERTATION FIGURES
echo ========================================
echo.
echo Generating dissertation report figures (Fig-R1, Fig-R2/R3, Fig-R4/R5, Fig-R7, Fig-R8)...
echo.

"%RSCRIPT%" scripts\evaluation\generate_report_figures.R
if %errorlevel% neq 0 (
    echo [WARNING] Dissertation figures generation had issues, continuing...
) else (
    echo [OK] Dissertation figures generated
)
echo.

REM =============================================================================
REM SUMMARY
REM =============================================================================

echo.
echo ========================================
echo PIPELINE COMPLETED
echo ========================================
echo.
echo Results saved to:
echo   - results\consolidated\NF_GARCH_Results_manual.xlsx
echo   - results\consolidated\NF_vs_Standard_GARCH_Comparison.xlsx
echo   - results\consolidated\Distributional_Metrics.xlsx
echo   - results\consolidated\Stylized_Facts.xlsx
echo   - results\consolidated\VaR_Backtesting.xlsx
echo   - results\consolidated\Stress_Testing.xlsx
echo   - results\consolidated\Final_Dashboard.xlsx (Excel dashboard)
echo   - results\consolidated\Methodology_Residual_Stationarity.xlsx
echo   - results\consolidated\Methodology_Conditional_Heterogeneity.xlsx
echo   - results\consolidated\Methodology_Consolidated.xlsx
echo   - results\dashboard_visualizations.html (Interactive HTML dashboard)
echo   - results\dashboard_plots\ (13 visualization plots)
echo   - results\dissertation_tables\ (LaTeX tables for dissertation)
echo   - results\figures\ (Dissertation figures: Fig-R1, Fig-R2/R3, Fig-R4/R5, Fig-R7, Fig-R8)
echo   - results\diagnostics\ (investigation summaries)
echo.
echo Next steps:
echo   1. Review results in: results\consolidated\
echo   2. Check comparison: NF_vs_Standard_GARCH_Comparison.xlsx
echo   3. View comprehensive dashboards:
echo      - Excel: Final_Dashboard.xlsx (includes all metrics)
echo      - HTML: dashboard_visualizations.html (interactive with plots)
echo   4. Review distributional metrics: Distributional_Metrics.xlsx
echo   5. Review stylized facts: Stylized_Facts.xlsx
echo   6. Review VaR backtesting: VaR_Backtesting.xlsx
echo   7. Review stress testing: Stress_Testing.xlsx
echo   8. Review methodology validation:
echo      - Residual Stationarity: Methodology_Residual_Stationarity.xlsx
echo      - Conditional Heterogeneity: Methodology_Conditional_Heterogeneity.xlsx
echo      - Consolidated: Methodology_Consolidated.xlsx
echo   9. Review dissertation tables: results\dissertation_tables\
echo  10. Review dissertation figures: results\figures\
echo  11. Run: start_research_dashboard.bat (opens HTML dashboard in browser)
echo.
echo ========================================
echo.
pause
