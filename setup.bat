@echo off
setlocal enabledelayedexpansion
REM =============================================================================
REM NF-GARCH Research Repository - Automated Setup
REM This script checks dependencies and installs required R and Python packages
REM =============================================================================

echo ========================================
echo    NF-GARCH SETUP
echo ========================================
echo.
echo This script will:
echo   1. Check R and Python installation
echo   2. Install required R packages
echo   3. Install required Python packages
echo   4. Verify installation
echo.
echo Expected time: 10-20 minutes
echo ========================================
echo.

set /p confirm="Continue with setup? (Y/N): "
if /i not "%confirm%"=="Y" (
    echo Setup cancelled.
    exit /b 0
)
echo.

REM =============================================================================
REM STEP 1: CHECK R INSTALLATION
REM =============================================================================

echo ========================================
echo STEP 1: CHECKING R INSTALLATION
echo ========================================
echo.

REM Try to find R using the existing utility
call scripts\utils\find_r_executable.bat
if %errorlevel% neq 0 (
    echo [ERROR] R not found on your system
    echo.
    echo Please install R from: https://cran.r-project.org/
    echo.
    echo After installation:
    echo   1. Restart this script
    echo   2. OR manually set RSCRIPT environment variable
    echo.
    pause
    exit /b 1
)

if not defined RSCRIPT (
    echo [ERROR] RSCRIPT not set after find_r_executable
    pause
    exit /b 1
)

echo [OK] R found at: %RSCRIPT%
"%RSCRIPT%" --version
echo.

REM =============================================================================
REM STEP 2: CHECK PYTHON INSTALLATION
REM =============================================================================

echo ========================================
echo STEP 2: CHECKING PYTHON INSTALLATION
echo ========================================
echo.

python --version >nul 2>&1
if %errorlevel% neq 0 (
    echo [ERROR] Python not found on your system
    echo.
    echo Please install Python from: https://python.org/
    echo Recommended version: Python 3.8 or higher
    echo.
    echo After installation:
    echo   1. Make sure Python is added to PATH
    echo   2. Restart this script
    echo.
    pause
    exit /b 1
)

echo [OK] Python found
python --version
echo.

REM Check pip
pip --version >nul 2>&1
if %errorlevel% neq 0 (
    echo [WARNING] pip not found, but continuing...
    echo You may need to install packages manually
) else (
    echo [OK] pip is available
    pip --version
)
echo.

REM =============================================================================
REM STEP 3: INSTALL R PACKAGES
REM =============================================================================

echo ========================================
echo STEP 3: INSTALLING R PACKAGES
echo ========================================
echo.
echo This will install required R packages...
echo Using renv for reproducible environment...
echo.

REM Check if renv.lock exists
if exist "environment\renv.lock" (
    echo [INFO] Found renv.lock - using reproducible environment
    echo Installing renv and restoring packages...
    
    "%RSCRIPT%" -e "if (!require('renv', quietly = TRUE)) install.packages('renv', repos='https://cran.r-project.org'); renv::restore(prompt = FALSE)"
    
    if %errorlevel% neq 0 (
        echo [WARNING] renv restore had issues
        echo Falling back to manual package installation...
        goto :manual_r_install
    ) else (
        echo [OK] R packages installed via renv
        goto :python_install
    )
) else (
    echo [INFO] renv.lock not found, using manual installation
    goto :manual_r_install
)

:manual_r_install
echo Installing R packages manually...
echo.

REM Create temporary R installation script
(
echo # Install required R packages
echo required_packages ^<- c^(
echo   "rugarch", "quantmod", "xts", "PerformanceAnalytics", "FinTS",
echo   "tidyverse", "dplyr", "tidyr", "stringr", "ggplot2",
echo   "openxlsx", "moments", "tseries", "forecast", "lmtest"
echo ^)
echo.
echo cat^("Installing R packages...\n"^)
echo for ^(pkg in required_packages^) {
echo   if ^(!require^(pkg, character.only = TRUE, quietly = TRUE^)^) {
echo     cat^("Installing", pkg, "...\n"^)
echo     install.packages^(pkg, repos = "https://cran.r-project.org", dependencies = TRUE^)
echo   } else {
echo     cat^("Package", pkg, "already installed\n"^)
echo   }
echo }
echo cat^("\n[OK] R package installation complete\n"^)
) > "%TEMP%\nfgarch_r_install.R"

"%RSCRIPT%" "%TEMP%\nfgarch_r_install.R"
del "%TEMP%\nfgarch_r_install.R"

if %errorlevel% neq 0 (
    echo [WARNING] Some R packages may have failed to install
    echo Please check error messages above
    echo.
) else (
    echo [OK] R packages installed successfully
)
echo.

REM =============================================================================
REM STEP 4: INSTALL PYTHON PACKAGES
REM =============================================================================

:python_install
echo ========================================
echo STEP 4: INSTALLING PYTHON PACKAGES
echo ========================================
echo.

if exist "environment\requirements.txt" (
    echo [INFO] Installing from requirements.txt...
    echo.
    
    pip install -r environment\requirements.txt
    
    if %errorlevel% neq 0 (
        echo [WARNING] Some Python packages may have failed to install
        echo.
        echo Common issues:
        echo   - PyTorch: Visit https://pytorch.org for installation instructions
        echo   - CUDA: PyTorch will use CPU if CUDA unavailable
        echo.
    ) else (
        echo [OK] Python packages installed successfully
    )
) else (
    echo [WARNING] requirements.txt not found
    echo.
    echo Manually install required packages:
    echo   pip install numpy pandas scikit-learn matplotlib seaborn
    echo   pip install torch torchvision nflows pyyaml openpyxl plotly
    echo.
)
echo.

REM =============================================================================
REM STEP 5: VERIFY INSTALLATION
REM =============================================================================

echo ========================================
echo STEP 5: VERIFYING INSTALLATION
echo ========================================
echo.

REM Verify R packages
echo Checking R packages...
(
echo required_packages ^<- c^("rugarch", "xts", "dplyr", "openxlsx"^)
echo all_installed ^<- all^(sapply^(required_packages, require, character.only = TRUE, quietly = TRUE^)^)
echo if ^(all_installed^) {
echo   cat^("[OK] Core R packages are available\n"^)
echo } else {
echo   cat^("[WARNING] Some R packages may be missing\n"^)
echo }
) > "%TEMP%\nfgarch_r_verify.R"

"%RSCRIPT%" "%TEMP%\nfgarch_r_verify.R"
del "%TEMP%\nfgarch_r_verify.R"
echo.

REM Verify Python packages
echo Checking Python packages...
python -c "import numpy, pandas, torch, nflows; print('[OK] Core Python packages are available')" 2>nul
if %errorlevel% neq 0 (
    echo [WARNING] Some Python packages may be missing
    echo Try running: pip install -r environment\requirements.txt
) 
echo.

REM Check data files
echo Checking data files...
if exist "data\processed\raw (FX + EQ).csv" (
    echo [OK] Data files found
) else (
    echo [WARNING] Data files not found in data\processed\
    echo Make sure to add your data files before running the pipeline
)
echo.

REM =============================================================================
REM SUMMARY
REM =============================================================================

echo ========================================
echo SETUP SUMMARY
echo ========================================
echo.
echo Installation complete!
echo.
echo Installed:
echo   - R packages (rugarch, tidyverse, openxlsx, etc.)
echo   - Python packages (numpy, pandas, torch, nflows, etc.)
echo.
echo Next steps:
echo.
echo   1. For full replication (reviewers):
echo      run_full_dissertation.bat
echo.
echo   2. For main analysis only:
echo      run_all.bat
echo.
echo   3. For quick verification (30 min):
echo      See QUICKSTART.md
echo.
echo   4. For individual experiments:
echo      run_robustness_garch_order.bat
echo      run_synthetic_recovery.bat
echo.
echo Results will be saved to:
echo   - results\dissertation_tables\  (LaTeX tables)
echo   - results\figures\              (Figures)
echo   - results\consolidated\         (Excel dashboards)
echo   - overleaf_export\              (Ready for Overleaf)
echo.
echo For troubleshooting, see INSTALL.md
echo ========================================
echo.
pause
