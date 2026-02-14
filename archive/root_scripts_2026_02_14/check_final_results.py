import pandas as pd
import os

print("="*70)
print("FINAL RESULTS CHECK - BOTH PIPELINES")
print("="*70)

# Check TS-CV results
tscv_files = [
    "results/tscv/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx",
    "results/tscv/Consolidated_Results_all.xlsx",
    "results/tscv/Dissertation_Consolidated_Results.xlsx"
]

print("\n### TS-CV PIPELINE ###")
tscv_found = None
for f in tscv_files:
    if os.path.exists(f):
        print(f"[OK] Found: {f}")
        tscv_found = f
        break
    else:
        print(f"[X] Not found: {f}")

if tscv_found:
    xl = pd.ExcelFile(tscv_found)
    print(f"\nSheets: {xl.sheet_names[:5]}")
    if 'Summary' in xl.sheet_names:
        df = pd.read_excel(tscv_found, sheet_name='Summary')
        print("\nTS-CV Summary:")
        print(df.to_string(index=False))

# Check Chronological results  
chrono_files = [
    "results/chronological/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx",
    "results/chronological/Consolidated_Results_all.xlsx",
    "results/chronological/Dissertation_Consolidated_Results.xlsx"
]

print("\n\n### CHRONOLOGICAL PIPELINE ###")
chrono_found = None
for f in chrono_files:
    if os.path.exists(f):
        print(f"[OK] Found: {f}")
        chrono_found = f
        break
    else:
        print(f"[X] Not found: {f}")

if chrono_found:
    xl = pd.ExcelFile(chrono_found)
    print(f"\nSheets: {xl.sheet_names[:5]}")
    if 'Summary' in xl.sheet_names:
        df = pd.read_excel(chrono_found, sheet_name='Summary')
        print("\nChronological Summary:")
        print(df.to_string(index=False))
