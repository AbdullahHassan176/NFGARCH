@echo off
REM SUPERSEDED — from repo root run: python scripts\manual\run_reviewer3_full_chain.py %*
cd /d "%~dp0..\.."
python scripts\manual\run_reviewer3_full_chain.py %*
exit /b %errorlevel%
