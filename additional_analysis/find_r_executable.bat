@echo off
REM Helper script to find Rscript executable
REM Sets RSCRIPT variable for use in other batch files

REM Check if RSCRIPT environment variable is already set
if defined RSCRIPT (
    echo Using RSCRIPT from environment: %RSCRIPT%
    exit /b 0
)

REM Try common R installation paths (most recent first)
if exist "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" (
    set "RSCRIPT=C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
    echo Found Rscript: %RSCRIPT%
    exit /b 0
)

if exist "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" (
    set "RSCRIPT=C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
    echo Found Rscript: %RSCRIPT%
    exit /b 0
)

if exist "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" (
    set "RSCRIPT=C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
    echo Found Rscript: %RSCRIPT%
    exit /b 0
)

if exist "C:\Program Files\R\R-4.3.3\bin\Rscript.exe" (
    set "RSCRIPT=C:\Program Files\R\R-4.3.3\bin\Rscript.exe"
    echo Found Rscript: %RSCRIPT%
    exit /b 0
)

REM Try Rscript from PATH
where Rscript.exe >nul 2>&1
if %errorlevel% equ 0 (
    set "RSCRIPT=Rscript.exe"
    echo Found Rscript in PATH: %RSCRIPT%
    exit /b 0
)

REM If nothing found, try without .exe extension
where Rscript >nul 2>&1
if %errorlevel% equ 0 (
    set "RSCRIPT=Rscript"
    echo Found Rscript in PATH: %RSCRIPT%
    exit /b 0
)

echo [ERROR] Rscript not found. Please set RSCRIPT environment variable or install R.
echo Example: set RSCRIPT=C:\Program Files\R\R-4.5.1\bin\Rscript.exe
exit /b 1






