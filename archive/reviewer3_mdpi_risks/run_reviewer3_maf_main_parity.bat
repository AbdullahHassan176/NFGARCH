@echo off
REM SUPERSEDED — MAF-only retrain with main NF settings: from repo root
REM   python scripts\manual\run_reviewer3_robustness.py --maf-seeds 123 --skip-realnvp
REM Then: python scripts\manual\run_reviewer3_full_chain.py --runs maf_seed123
cd /d "%~dp0..\.."
python scripts\manual\run_reviewer3_robustness.py --maf-seeds 123 --skip-realnvp
exit /b %errorlevel%
