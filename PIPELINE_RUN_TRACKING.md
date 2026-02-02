# Pipeline Run Tracking - Live Session

**Run Started**: 2026-02-02 (Current Session)  
**Command**: `.\run_both_pipelines.bat`  
**Mode**: Both Comprehensive Pipelines (Chronological + TS-CV)

---

## 📊 Expected Timeline

### Pipeline 1: Chronological Split
- **Estimated Duration**: 90-180 minutes
- **Status**: 🔄 Starting...
- **Log File**: `logs/chronological_pipeline_*.log`

### Pipeline 2: TS-CV
- **Estimated Duration**: 120-240 minutes  
- **Status**: ⏳ Queued (after chronological)
- **Log File**: `logs/tscv_pipeline_*.log`

### Combined Total
- **Estimated Duration**: 3.5-7 hours
- **Actual Start Time**: TBD
- **Actual End Time**: TBD

---

## 🐛 Issues Encountered

### Session 1: Initial Run Attempt

#### Issue #1: Critical Batch File Bug
- **Step**: All helper functions (logging/timing)
- **Error**: `goto :EOF:GENERATE_SUMMARY` (line 688) - merged label causing "batch label not found"
- **Symptom**: 
  - "Missing operand" errors
  - "The system cannot find the batch label specified - EOF"
  - "The system cannot find the batch label specified - GENERATE_SUMMARY"
  - No log files created
- **Fix**: Split into two lines: `goto :EOF` + newline + `:GENERATE_SUMMARY`
- **Status**: ✅ FIXED in both run_chronological.bat and run_tscv.bat
- **Time Impact**: ~20 seconds wasted, pipeline failed immediately

#### Issue #2: Timing Summary Loop Runaway
- **Step**: GENERATE_SUMMARY function
- **Error**: Timing summary generating hundreds of entries with absurd durations (71h, 68h, etc.)
- **Symptom**:
  - "The system cannot find the path specified" before each timing line
  - Infinite or extremely long loop in summary generation
  - STEP_COUNT variable appears corrupted or not set correctly
  - Process needed to be forcefully killed
- **Root Cause**: Investigating... Likely issues:
  1. LOG function failing to write to log file
  2. STEP_COUNT variable not properly managed
  3. Array variables not persisting correctly in batch
- **Root Cause FOUND**: `:LOG` function called at lines 20-25 **before it was defined** (defined at line 614)
  - Batch files can't call labels that haven't been reached yet
  - This caused "system cannot find path" errors
  - Also caused variables to not be properly initialized
- **Fix**: 
  1. Removed premature LOG calls from initialization section
  2. Moved pipeline header logging into STEP 1 (after functions are reachable)
- **Status**: ✅ FIXED in both run_chronological.bat and run_tscv.bat
- **Time Impact**: Pipeline killed after ~2 minutes of runaway loop

#### Issue #3: Date/Time Parsing Creating Invalid File Path
- **Step**: Log file initialization
- **Error**: Date parsing with `%date:~-4%...` creates paths with slashes
- **Symptom**:
  - Log file path: `logs\chronological_pipeline_2/02206/_233122.log` (invalid)
  - "The system cannot find the path specified" when trying to write to log
  - Should be: `logs\chronological_pipeline_20260202_233122.log`
- **Root Cause**: Date string contains `/` characters that get interpreted as path separators
- **Fix**: Use `for /f` with `date /t` and `time` to parse date/time properly
- **Status**: ✅ FIXED in both run_chronological.bat and run_tscv.bat (lines 11-14)
- **Time Impact**: Pipeline failed immediately (~3 seconds)
- **Files Fixed**: 
  - run_chronological.bat (line 688)
  - run_tscv.bat (line 687)

---

## ⏱️ Actual Timing (Per Step)

### Chronological Pipeline
*Will be populated from log file after completion*

### TS-CV Pipeline
*Will be populated from log file after completion*

---

## 📝 Notes

- Using auto-confirmation with `echo Y` command
- Both pipelines have full logging enabled
- Will monitor terminal output for real-time issues
- Log files will be analyzed for detailed timing breakdown

---

**Status**: 🔄 **IN PROGRESS**  
**Last Updated**: Starting run...
