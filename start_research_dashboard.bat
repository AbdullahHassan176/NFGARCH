@echo off
REM Start Research Dashboard - Manual Branch
REM Opens the interactive HTML dashboard with visualizations and explanations

echo ========================================
echo STARTING RESEARCH DASHBOARD
echo ========================================
echo.
echo This will open the interactive HTML dashboard in your default browser.
echo.
echo Dashboard files:
echo   - results\dashboard_visualizations.html (Interactive HTML dashboard)
echo   - results\consolidated\Final_Dashboard.xlsx (Excel dashboard)
echo.

REM Check if HTML dashboard exists
if exist "results\dashboard_visualizations.html" (
    echo [OK] HTML Dashboard found: results\dashboard_visualizations.html
    echo.
    echo Opening dashboard in default browser...
    echo.
    
    REM Open HTML dashboard in default browser
    start "" "results\dashboard_visualizations.html"
    
    echo [OK] Dashboard opened successfully!
    echo.
    echo The dashboard includes:
    echo   - Interactive navigation menu
    echo   - Visual plots and explanations
    echo   - Citations and references
    echo   - Links to detailed Excel files
    echo.
    echo You can also access:
    echo   - Excel Dashboard: results\consolidated\Final_Dashboard.xlsx
    echo   - Visualization Plots: results\dashboard_plots\
    echo.
    
    REM Optionally open Excel dashboard as well (open silently in background)
    REM User can access it via the HTML dashboard links if needed
    
) else (
    echo [WARNING] WARNING: HTML dashboard not found: results\dashboard_visualizations.html
    echo.
    echo Attempting to generate visualizations first...
    echo.
    
    REM Check if visualization script exists
    if exist "scripts\evaluation\generate_dashboard_visualizations.R" (
        echo Running visualization generation script...
        "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "scripts\evaluation\generate_dashboard_visualizations.R"
        echo.
        
        REM Check if HTML dashboard was created
        if exist "results\dashboard_visualizations.html" (
            echo [OK] HTML dashboard generated successfully!
            echo.
            echo Opening dashboard in default browser...
            start "" "results\dashboard_visualizations.html"
            echo [OK] Dashboard opened successfully!
        ) else (
            echo [WARNING] HTML dashboard not generated. Falling back to Excel dashboard...
            goto :open_excel
        )
    ) else (
        echo [WARNING] Visualization script not found. Falling back to Excel dashboard...
        goto :open_excel
    )
)

goto :end

:open_excel
REM Fallback to Excel dashboard
if exist "results\consolidated\Final_Dashboard.xlsx" (
    echo.
    echo Opening Excel dashboard...
    start "" "results\consolidated\Final_Dashboard.xlsx"
    echo [OK] Excel dashboard opened
    echo.
    echo Note: To generate the HTML dashboard, run:
    echo   "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts\evaluation\generate_dashboard_visualizations.R
) else (
    echo [ERROR] Dashboard not found. Please run the pipeline first.
    echo.
    echo Expected files:
    echo   - results\dashboard_visualizations.html
    echo   - results\consolidated\Final_Dashboard.xlsx
    echo.
    echo To generate the dashboard, run: run_all.bat
    pause
    exit /b 1
)

:end
echo.
echo Dashboard session complete.
pause
