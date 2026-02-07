@echo off
echo ========================================
echo RUNNING FIXED NF-GARCH PIPELINE
echo Date: 2026-02-07
echo ========================================
echo.
echo This script runs the complete manual pipeline with fixes applied:
echo 1. Filename mismatch resolution
echo 2. Residual standardization validation
echo 3. No forced standardization in NF training
echo.
echo ========================================

REM Find R executable
call scripts\utils\find_r_executable.bat
if errorlevel 1 (
    echo [ERROR] Could not find R installation
    exit /b 1
)

echo.
echo ========================================
echo STEP 1: Manual GARCH Fitting
echo ========================================
echo This will:
echo - Fit GARCH models on training data
echo - Extract standardized residuals
echo - VALIDATE residuals (mean~0, std~1)
echo - Save with correct filenames
echo.

"%RSCRIPT%" scripts/manual/manual_garch_fitting.R
if errorlevel 1 (
    echo.
    echo [ERROR] GARCH fitting failed!
    echo Check output above for standardization validation errors
    exit /b 1
)

echo.
echo [OK] GARCH fitting completed
echo.
echo ========================================
echo STEP 2: NF Training
echo ========================================
echo This will:
echo - Load standardized residuals (with correct filenames)
echo - Train Normalizing Flows
echo - Generate samples WITHOUT forced standardization
echo - VALIDATE samples (mean~0, std~1)
echo.

python scripts/manual/manual_nf_training.py
if errorlevel 1 (
    echo.
    echo [ERROR] NF training failed!
    echo Check output above for sample validation warnings
    exit /b 1
)

echo.
echo [OK] NF training completed
echo.
echo ========================================
echo STEP 3: Verification
echo ========================================
echo.
echo Checking outputs...
echo.

if not exist "outputs\manual\residuals_by_model\" (
    echo [ERROR] Residuals directory not created
    exit /b 1
)

if not exist "outputs\manual\nf_models\" (
    echo [ERROR] NF models directory not created
    exit /b 1
)

echo [OK] Output directories exist
echo.
echo Residuals generated:
dir /b outputs\manual\residuals_by_model\*\*.csv 2>nul | find /c ".csv"
echo.
echo NF models generated:
dir /b /s outputs\manual\nf_models\*.pth 2>nul | find /c ".pth"
echo.
echo ========================================
echo PIPELINE COMPLETED SUCCESSFULLY
echo ========================================
echo.
echo Next steps:
echo 1. Review console output for validation warnings
echo 2. Check that residuals passed standardization validation
echo 3. Check that NF samples are naturally standardized
echo 4. Run comparison: Rscript scripts/evaluation/compare_nf_vs_standard_garch.R
echo.
echo For detailed diagnostics, see:
echo - outputs/manual/garch_fitting/
echo - outputs/manual/nf_models/training_history.csv (for each model)
echo.
pause
