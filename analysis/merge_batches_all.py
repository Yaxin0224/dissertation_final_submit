#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
merge_batches_all.py
Merge all batch_*.csv files in the .../output/full_london_gvi directory.
- all_rows: pure concatenation, keep all rows and all columns (columns aligned by union).
- latest_per_lsoa: deduplicate by lsoa_code, by default keep the most recent record per LSOA (based on filename timestamp).
"""
from pathlib import Path
import argparse
import re
import pandas as pd

DEFAULT_BASE = Path(r"C:\Users\z1782\OneDrive - University College London\Attachments\004\methodology\Dissertation_v2")
DEFAULT_FOLDER = DEFAULT_BASE / "output" / "full_london_gvi"

def parse_args():
    ap = argparse.ArgumentParser(description="Merge all batch_*.csv in full_london_gvi (keep all rows & all columns)")
    ap.add_argument("--folder", type=str, default=str(DEFAULT_FOLDER), help="Folder containing batch files")
    ap.add_argument("--keep", choices=["last", "first"], default="last",
                    help="Which record to keep when multiple records exist for the same lsoa_code (based on file time order)")
    ap.add_argument("--excel", action="store_true", help="Also export XLSX (two worksheets)")
    return ap.parse_args()

def sort_key(p: Path):
    # Parse timestamps from batch_YYYYMMDD_HHMMSS.csv for sorting
    m = re.search(r"batch_(\d{8})_(\d{6})\.csv$", p.name)
    return (m.group(1), m.group(2)) if m else ("00000000", "000000")

def main():
    args = parse_args()
    folder = Path(args.folder)
    folder.mkdir(parents=True, exist_ok=True)

    files = sorted(folder.glob("batch_*.csv"), key=sort_key)
    if not files:
        print(f"⚠️ No batch_*.csv files found in {folder}")
        return

    dfs = []
    for f in files:
        try:
            df = pd.read_csv(f)
            # Add source info for traceability
            m = re.search(r"batch_(\d{8})_(\d{6})\.csv$", f.name)
            ts = f"{m.group(1)}_{m.group(2)}" if m else ""
            df["__source_file"] = f.name
            df["__batch_time"] = ts
            dfs.append(df)
        except Exception as e:
            print(f"⚠️ Failed to read: {f.name} -> {e}")

    if not dfs:
        print("⚠️ No data available to merge.")
        return

    # 1) Pure concatenation: columns aligned by union, missing filled with NaN — keep all rows and all columns
    all_rows = pd.concat(dfs, ignore_index=True, sort=False)

    # 2) Deduplicated version: sorted by batch time, keep the latest/earliest record per lsoa_code
    if "lsoa_code" in all_rows.columns:
        # Add ordering based on batch time to control "latest/earliest"
        all_rows = all_rows.copy()
        all_rows["__order"] = all_rows["__batch_time"]
        keep = "last" if args.keep == "last" else "first"
        latest = all_rows.drop_duplicates(subset=["lsoa_code"], keep=keep).drop(columns="__order")
    else:
        latest = all_rows.copy()  # No lsoa_code -> do not deduplicate

    # Output CSVs
    out_all = folder / "inner_london_gvi_results_all_rows.csv"
    out_latest = folder / "inner_london_gvi_results_latest_per_lsoa.csv"
    all_rows.to_csv(out_all, index=False)
    latest.to_csv(out_latest, index=False)

    # Optional: export an Excel file with two sheets
    if args.excel:
        xlsx = folder / "merged_results.xlsx"
        with pd.ExcelWriter(xlsx, engine="openpyxl") as w:
            # Note: Excel single sheet max ~1,048,576 rows — if exceeded, write only latest
            all_rows.to_excel(w, index=False, sheet_name="all_rows")
            latest.to_excel(w, index=False, sheet_name="latest_per_lsoa")

    # Print summary
    print("✅ Merge complete")
    print(f"   Number of batches: {len(files)}")
    print(f"   all_rows rows: {len(all_rows)}  ->  {out_all}")
    if "mean_gvi" in latest.columns:
        print(f"   latest_per_lsoa rows: {len(latest)}, mean GVI: {latest['mean_gvi'].mean():.2f}%  ->  {out_latest}")
    else:
        print(f"   latest_per_lsoa rows: {len(latest)} -> {out_latest}")

if __name__ == "__main__":
    main()
