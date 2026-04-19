@echo off
REM SUPERSEDED — use: run_chronological.bat /InstallRPackages
cd /d "%~dp0..\.."
call "%CD%\run_chronological.bat" /InstallRPackages
exit /b %errorlevel%
