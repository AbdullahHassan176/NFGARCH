@echo off
cd /d "c:\Experimentation\NFGARCH"

 REM Remove stale lock from failed runs (optional)
if exist ".git\index.lock" del /f ".git\index.lock" 2>nul

git add -A
git commit -m "Audit docs, dissertation pipeline, and script updates" -m "- Add CONSISTENCY_AND_METHODOLOGY_AUDIT.md, GARCH_DISTRIBUTION_CONSISTENCY_AUDIT.md, RETURN_VS_VARIANCE_EVALUATION_AUDIT.md" -m "- Add run_full_dissertation.bat and _git_commit_push.bat" -m "- Update run_all.bat, evaluation scripts, manual/manual_garch, robustness_garch_order, extract_residuals, return_forecast_evaluation"

if errorlevel 1 (
  echo.
  echo Commit failed. If you see "index.lock" or "Permission denied", close Cursor/VS Code and any Git GUIs, then run this again.
  pause
  exit /b 1
)

git push
if errorlevel 1 (
  echo.
  echo Push failed. Check remote and credentials.
  pause
  exit /b 1
)

echo.
echo Done: committed and pushed to origin.
pause
