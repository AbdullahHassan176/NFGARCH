@echo off
REM Fix and install NumPy >= 1.25.2 for NF training (numpy.exceptions)
REM Run this from Command Prompt or PowerShell OUTSIDE Cursor (Cursor blocks network in its terminal).

echo === NumPy upgrade for manual_nf_training.py ===
echo.
echo This script clears PIP_NO_INDEX and proxy so pip can reach PyPI.
echo Run from an external terminal (not Cursor) if Cursor blocks network.
echo.

set PIP_NO_INDEX=
set HTTP_PROXY=
set HTTPS_PROXY=
set http_proxy=
set https_proxy=

python -m pip install "numpy>=1.25.2"
if errorlevel 1 (
  echo.
  echo [FAILED] Try running from Command Prompt (cmd) or PowerShell outside Cursor.
  echo If you use a corporate proxy, set HTTP_PROXY/HTTPS_PROXY to your real proxy instead of clearing.
  pause
  exit /b 1
)

python -c "import numpy; print('NumPy', numpy.__version__)"
echo.
echo [OK] NumPy upgraded. Re-run: run_full_dissertation.bat /Y
pause
