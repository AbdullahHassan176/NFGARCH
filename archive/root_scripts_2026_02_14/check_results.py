import pandas as pd
import os

print("="*60)
print("CHRONOLOGICAL PIPELINE RESULTS CHECK")
print("="*60)

# Check for results files
result_files = [
    "results/chronological/Consolidated_Results_all.xlsx",
    "results/chronological/Dissertation_Consolidated_Results.xlsx",
]

found_file = None
for file in result_files:
    if os.path.exists(file):
        print(f"\n[OK] Found: {file}")
        found_file = file
        break
    else:
        print(f"[X] Not found: {file}")

if found_file:
    # Read and show sheets
    xl = pd.ExcelFile(found_file)
    print(f"\nSheets in {found_file}:")
    for sheet in xl.sheet_names:
        print(f"  - {sheet}")
    
    # Try to find comparison data
    if 'Overall_Comparison' in xl.sheet_names:
        df = pd.read_excel(found_file, sheet_name='Overall_Comparison')
        print(f"\n{sheet} Summary:")
        print(df.to_string(index=False))
    elif 'Summary' in xl.sheet_names:
        df = pd.read_excel(found_file, sheet_name='Summary')
        print("\nSummary:")
        print(df.to_string(index=False))
else:
    print("\nNo results files found.")
    print("Pipeline may have failed to generate outputs.")
