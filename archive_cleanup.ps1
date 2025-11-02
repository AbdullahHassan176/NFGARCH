# Repository Cleanup Script
# Moves unneeded files to archive for research repository

$archiveDir = "archive/repository_cleanup_$(Get-Date -Format 'yyyy-MM-dd')"
New-Item -ItemType Directory -Path $archiveDir -Force | Out-Null
New-Item -ItemType Directory -Path "$archiveDir/batch_files" -Force | Out-Null
New-Item -ItemType Directory -Path "$archiveDir/documentation" -Force | Out-Null
New-Item -ItemType Directory -Path "$archiveDir/old_residuals" -Force | Out-Null
New-Item -ItemType Directory -Path "$archiveDir/old_outputs" -Force | Out-Null
New-Item -ItemType Directory -Path "$archiveDir/empty_directories" -Force | Out-Null

Write-Host "=== REPOSITORY CLEANUP ==="
Write-Host "Archive directory: $archiveDir"
Write-Host ""

# 1. Archive unneeded batch files (keep run_all.bat and start_research_dashboard.bat)
Write-Host "1. Archiving unneeded batch files..."
$batchFilesToArchive = @(
    "run_modular.bat",
    "run_manual.bat",
    "start_results_viewer.bat",
    "run_remaining_evaluation.R"
)

foreach ($file in $batchFilesToArchive) {
    if (Test-Path $file) {
        Move-Item -Path $file -Destination "$archiveDir/batch_files/" -Force
        Write-Host "  Archived: $file"
    }
}

# Archive script batch file too
if (Test-Path "scripts/manual/run_manual_optimized.bat") {
    Move-Item -Path "scripts/manual/run_manual_optimized.bat" -Destination "$archiveDir/batch_files/" -Force
    Write-Host "  Archived: scripts/manual/run_manual_optimized.bat"
}

# 2. Archive temporary documentation markdown files (keep ai.md and README.md)
Write-Host ""
Write-Host "2. Archiving temporary documentation..."
$docsToArchive = @(
    "COMPLETE_OPTION_B_STEPS.md",
    "CLEANUP_COMPLETE.md",
    "RUGARCH_REMOVAL_CONFIRMED.md",
    "TRAIN_gjrGARCH_NF.md",
    "FIX_DISTRIBUTIONAL_METRICS.md",
    "MANUAL_EXECUTION_STEPS.md",
    "REPRODUCIBILITY_CHECK.md",
    "FIXES_COMPLETE.md",
    "PIPELINE_VS_DISSERTATION_ANALYSIS.md"
)

foreach ($doc in $docsToArchive) {
    if (Test-Path $doc) {
        Move-Item -Path $doc -Destination "$archiveDir/documentation/" -Force
        Write-Host "  Archived: $doc"
    }
}

# Archive results documentation
if (Test-Path "results/DASHBOARD_README.md") {
    Move-Item -Path "results/DASHBOARD_README.md" -Destination "$archiveDir/documentation/" -Force
    Write-Host "  Archived: results/DASHBOARD_README.md"
}

# Archive scripts/manual documentation
$manualDocs = @(
    "scripts/manual/manual_execution_guide.md",
    "scripts/manual/QUICK_REFERENCE.md",
    "scripts/manual/RSTUDIO_EXECUTION_GUIDE.md"
)

foreach ($doc in $manualDocs) {
    if (Test-Path $doc) {
        Move-Item -Path $doc -Destination "$archiveDir/documentation/" -Force
        Write-Host "  Archived: $doc"
    }
}

# 3. Archive old residual directories
Write-Host ""
Write-Host "3. Archiving old residual directories..."
$oldResidualDirs = @(
    "residuals_by_model",
    "nf_generated_residuals"
)

foreach ($dir in $oldResidualDirs) {
    if (Test-Path $dir) {
        Move-Item -Path $dir -Destination "$archiveDir/old_residuals/" -Force
        Write-Host "  Archived: $dir/"
    }
}

# 4. Archive old outputs (keep outputs/manual/)
Write-Host ""
Write-Host "4. Archiving old output directories..."
$oldOutputDirs = @(
    "outputs/eda",
    "outputs/model_eval",
    "outputs/stress_tests",
    "outputs/var_backtest",
    "outputs/diagnostic",
    "outputs/supplementary"
)

foreach ($dir in $oldOutputDirs) {
    if (Test-Path $dir) {
        $dirName = Split-Path $dir -Leaf
        Move-Item -Path $dir -Destination "$archiveDir/old_outputs/" -Force
        Write-Host "  Archived: $dir/"
    }
}

# 5. Archive empty/unused directories
Write-Host ""
Write-Host "5. Archiving empty/unused directories..."
$emptyDirs = @(
    "checkpoints",
    "tools",
    "docs",
    "scripts/eda",
    "scripts/models",
    "scripts/stress_tests"
)

foreach ($dir in $emptyDirs) {
    if (Test-Path $dir) {
        $dirName = Split-Path $dir -Leaf
        Move-Item -Path $dir -Destination "$archiveDir/empty_directories/" -Force -ErrorAction SilentlyContinue
        if (Test-Path $dir) {
            Remove-Item -Path $dir -Recurse -Force -ErrorAction SilentlyContinue
        }
        Write-Host "  Archived: $dir/"
    }
}

# 6. Archive old results plots (keep dashboard_plots and diagnostics)
Write-Host ""
Write-Host "6. Archiving old results plots..."
if (Test-Path "results/plots") {
    Move-Item -Path "results/plots" -Destination "$archiveDir/old_outputs/" -Force
    Write-Host "  Archived: results/plots/"
}

# Create summary
Write-Host ""
Write-Host "=== CLEANUP SUMMARY ==="
Write-Host "Archive location: $archiveDir"
Write-Host ""
Write-Host "Files kept:"
Write-Host "  - run_all.bat (main pipeline)"
Write-Host "  - start_research_dashboard.bat (dashboard launcher)"
Write-Host "  - ai.md (project documentation)"
Write-Host "  - README.md (main readme)"
Write-Host "  - All active scripts/"
Write-Host "  - data/ (all data files)"
Write-Host "  - results/ (consolidated results, dashboard, plots)"
Write-Host "  - outputs/manual/ (current manual outputs)"
Write-Host "  - environment/ (config files)"
Write-Host ""
Write-Host "Cleanup complete!"
