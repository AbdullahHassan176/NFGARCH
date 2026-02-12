import pandas as pd

file = "results/chronological/Dissertation_Consolidated_Results.xlsx"
xl = pd.ExcelFile(file)

print("Chronological sheets:")
for s in xl.sheet_names:
    print(f"  - {s}")

# Try to find the right sheet
if "Chrono_Split_NF_GARCH" in xl.sheet_names:
    df = pd.read_excel(file, sheet_name="Chrono_Split_NF_GARCH")
    print("\nChronological NF-GARCH Results:")
    print(f"Total rows: {len(df)}")
    print("\nColumns:", list(df.columns))
    print("\nSample data:")
    print(df.head(10).to_string(index=False))
