@echo off
REM Commit and push all changes to branch main_run.
REM Usage: commit_push_main_run.bat [ /NOPAUSE ]
REM   /NOPAUSE  = skip pause (for non-interactive runs)
REM If git add/commit fails with "Permission denied" on .git/objects:
REM   - Close Cursor, then double-click this file in Explorer, or
REM   - Open Command Prompt (Win+R, cmd), cd to this project, run: commit_push_main_run.bat

if /i "%~1"=="/NOPAUSE" set NOPAUSE=1
cd /d "%~dp0"

echo Removing stale git locks...
if exist ".git\index.lock" del /f /q ".git\index.lock" 2>nul
if exist ".git\refs\heads\main_run.lock" del /f /q ".git\refs\heads\main_run.lock" 2>nul
if exist ".git\refs\heads\main-run.lock" del /f /q ".git\refs\heads\main-run.lock" 2>nul

echo.
echo Creating branch main_run...
git checkout -b main_run 2>nul
if errorlevel 1 (
  echo Branch may already exist or lock; continuing on current branch...
)

echo.
echo Staging all changes...
git add -A
if errorlevel 1 (
  echo [WARNING] git add failed. Close Cursor, then double-click this .bat in Explorer and run again.
  if not defined NOPAUSE pause
  exit /b 1
)

echo.
echo Committing...
git commit -m "Main run: .tex table export, dissertation input guides, Overleaf export, and pipeline results"
if errorlevel 1 (
  echo [ERROR] Commit failed. For "Permission denied" on .git/objects: close Cursor, run this .bat from Explorer.
  if not defined NOPAUSE pause
  exit /b 1
)

echo.
echo Pushing to origin main_run...
git push -u origin main_run 2>nul
if errorlevel 1 git push origin HEAD:main_run
if errorlevel 1 (
  echo [ERROR] Push failed. Check network and: git remote -v
  if not defined NOPAUSE pause
  exit /b 1
)

echo.
echo Done. Branch main_run pushed to origin.
if not defined NOPAUSE pause
exit /b 0
