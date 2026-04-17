#!/usr/bin/env python3
"""
Merge MOSAIKS features with forest cover data for a single country.

This script performs an inner join on (lat, lon) coordinates to combine:
- Forest cover data from Hansen Global Forest Change (from get_forest_change.py)
- MOSAIKS feature vectors (from get_mosaiks.py)

The merge is an inner join, so only grid cells present in BOTH datasets are retained.

Usage:
    python 03_merge.py --country uganda --resolution 0.01 --year 2011

Input:
    data/raw/forest/mosaiks_features/<country>_mosaiks_features_<resolution>deg.csv
    data/raw/forest/forest_cover/<country>_forest_cover_<year>_<resolution>deg.csv

Output:
    data/clean/forest/<country>_<year>_<resolution>deg.csv
"""

import argparse
from pathlib import Path
import pandas as pd

# Paths
DATA_DIR = Path("data")


def main():
    """
    Main function to merge MOSAIKS features with forest cover data.

    Steps:
    1. Parse command-line arguments (country, resolution, year)
    2. Load both datasets
    3. Merge on (lat, lon) coordinates
    4. Save merged data to CSV
    """
    parser = argparse.ArgumentParser(
        description="Merge MOSAIKS features with forest cover data for a single country.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    python 03_merge.py --country uganda --resolution 0.01 --year 2011
        """
    )
    parser.add_argument(
        "--country",
        type=str,
        default="uganda",
        help="Country short name (default: uganda).",
    )
    parser.add_argument(
        "--resolution",
        type=str,
        choices=["0.1", "0.01"],
        default="0.01",
        help="Grid resolution in degrees (default: 0.01).",
    )
    parser.add_argument(
        "--year",
        type=int,
        default=2011,
        help="Target year for forest cover (default: 2011).",
    )
    args = parser.parse_args()

    country = args.country.lower()
    resolution = args.resolution
    target_year = args.year

    print("=" * 60)
    print("Merge: MOSAIKS Data with Forest Cover")
    print("=" * 60)
    print(f"Country: {country.upper()}")
    print(f"Resolution: {resolution}°")
    print(f"Forest cover year: {target_year}")
    print()

    # Build file paths
    mosaik_file = DATA_DIR / f"raw/forest/mosaiks_features/{country}_mosaiks_features_{resolution}deg.csv"
    forest_file = DATA_DIR / f"raw/forest/forest_cover/{country}_forest_cover_{target_year}_{resolution}deg.csv"

    # Check input files exist
    if not mosaik_file.exists():
        print(f"ERROR: MOSAIKS file not found: {mosaik_file}")
        print("Run get_mosaiks.py first.")
        return

    if not forest_file.exists():
        print(f"ERROR: Forest file not found: {forest_file}")
        print("Run get_forest_change.py first.")
        return

    # Load datasets
    print(f"Loading MOSAIKS features: {mosaik_file.name}")
    mosaiks_df = pd.read_csv(mosaik_file)
    print(f"  {len(mosaiks_df):,} rows")

    print(f"Loading forest cover: {forest_file.name}")
    forest_df = pd.read_csv(forest_file)
    print(f"  {len(forest_df):,} rows")

    # Merge on (lat, lon) coordinates
    # Inner join: only keep grid cells present in both datasets
    print("\nMerging on (lat, lon)...")
    df = forest_df.merge(mosaiks_df, on=['lat', 'lon'], how='inner')
    print(f"  {len(df):,} matched observations")

    if len(df) == 0:
        print("WARNING: No matching coordinates found!")
        print("  Check that both datasets use the same resolution and coordinate system.")
        return

    # Report merge statistics
    forest_only = len(forest_df) - len(df)
    mosaiks_only = len(mosaiks_df) - len(df)
    if forest_only > 0:
        print(f"  {forest_only:,} forest cover rows had no MOSAIKS match")
    if mosaiks_only > 0:
        print(f"  {mosaiks_only:,} MOSAIKS rows had no forest cover match")

    # Save merged data
    output_file = DATA_DIR / f"clean/forest/{country}_{target_year}_{resolution}deg.csv"
    output_file.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(output_file, index=False)

    print(f"\nSaved: {output_file}")
    print(f"  Columns: {list(df.columns)[:5]} ... ({len(df.columns)} total)")

    print("\n" + "=" * 60)
    print("COMPLETE!")
    print("=" * 60)


if __name__ == "__main__":
    main()
