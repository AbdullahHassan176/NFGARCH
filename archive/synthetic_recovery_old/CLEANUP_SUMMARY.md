# Repository Cleanup Summary

**Date**: 2026-01-09

## Files Archived

The following files were moved to `archive/synthetic_recovery_old/` as they are superseded or redundant:

### Analysis Documents
- `ANALYSIS_AUDIT.md` - Superseded by `AUDIT_FIXES.md`
- `EXPERIMENT_ANALYSIS.md` - Superseded by `AUDIT_FIXES.md`
- `PATCH.md` - Historical code fixes document
- `RESULTS.md` - Old single-seed results (superseded by multi-seed results)
- `RUN_SUMMARY.md` - Temporary execution summary

### Old Root-Level Files
- `nf_model.pth` - Old single-seed model (now per-seed)
- `nf_model_samples.csv` - Old single-seed samples (now per-seed)
- `recovery_metrics.csv` - Old single-seed metrics (now aggregated)
- `summary_statistics.csv` - Old single-seed stats (now aggregated)
- `plots/` - Old root-level plots (now per-seed)
- `residuals/` - Old root-level residuals (now per-seed)
- `README.md` - Old README (replaced with new one)

## Changes Made

1. **Removed all emojis** from markdown files (replaced with text or removed)
2. **Archived redundant files** to maintain clean structure
3. **Created new README.md** in `outputs/synthetic_recovery/` documenting current structure
4. **Organized results** - only aggregated results and per-seed directories remain in root

## Current Structure

`outputs/synthetic_recovery/` now contains:
- **AUDIT_FIXES.md** - Comprehensive audit report (primary document)
- **MULTISEED_REPORT.md** - Summary report with winners
- **README.md** - Directory documentation
- **Aggregated CSV files** - All metrics and statistics
- **seed_*/ directories** - Per-seed results (10 directories)

## Notes

All archived files remain accessible in `archive/synthetic_recovery_old/` for reference. The cleanup maintains full reproducibility while removing redundancy.

