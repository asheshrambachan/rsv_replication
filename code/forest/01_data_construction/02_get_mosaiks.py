#!/usr/bin/env python3
"""
Download MOSAIKS features from Redivis for a single country.

MOSAIKS (Multi-task Observation using Satellite Imagery & Kitchen Sinks) provides
pre-computed feature vectors derived from satellite imagery. These features can be
used to predict various socioeconomic and environmental outcomes.

Data source: SDSS Data Repository on Redivis
- Dataset: mosaiks:8bqm:v1_0
- Tables available at different resolutions:
  - 0.1° resolution: 0_1_x_0_1_deg_area_weighted_table (area-weighted aggregation)
  - 0.01° resolution: mosaiks_2019_planet (individual point features from Planet imagery)

Usage:
    python 02_get_mosaiks.py --country uganda --resolution 0.01

Output:
    data/raw/forest/mosaiks_features/<country>_mosaiks_features_<resolution>deg.csv
"""

from pathlib import Path
import math
import argparse
import geopandas as gpd
from cartopy.io import shapereader
import pandas as pd
import redivis

# Paths
DATA_DIR = Path("data/raw/forest/mosaiks_features")
TMP_DIR = DATA_DIR / "_tmp_tiles"

# Country name mapping: short name -> SOVEREIGNT field in Natural Earth Admin 0 shapefile
# Natural Earth data: https://www.naturalearthdata.com/downloads/50m-cultural-vectors/
# Use lowercase short names; the SOVEREIGNT value must match exactly (case-sensitive)
COUNTRY_MAPPING = {
    # Africa
    'uganda': 'Uganda',
    'kenya': 'Kenya',
    'tanzania': 'Tanzania',
    'ethiopia': 'Ethiopia',
    'drc': 'Democratic Republic of the Congo',
    'congo': 'Republic of Congo',
    'cameroon': 'Cameroon',
    'nigeria': 'Nigeria',
    'ghana': 'Ghana',
    'ivory_coast': "Ivory Coast",
    'liberia': 'Liberia',
    'madagascar': 'Madagascar',
    'mozambique': 'Mozambique',
    'zambia': 'Zambia',
    'zimbabwe': 'Zimbabwe',
    'south_africa': 'South Africa',
    # Americas
    'brazil': 'Brazil',
    'peru': 'Peru',
    'colombia': 'Colombia',
    'bolivia': 'Bolivia',
    'ecuador': 'Ecuador',
    'venezuela': 'Venezuela',
    'guyana': 'Guyana',
    'suriname': 'Suriname',
    'mexico': 'Mexico',
    'guatemala': 'Guatemala',
    'honduras': 'Honduras',
    'nicaragua': 'Nicaragua',
    'costa_rica': 'Costa Rica',
    'panama': 'Panama',
    # Asia
    'indonesia': 'Indonesia',
    'malaysia': 'Malaysia',
    'myanmar': 'Myanmar',
    'thailand': 'Thailand',
    'vietnam': 'Vietnam',
    'laos': 'Laos',
    'cambodia': 'Cambodia',
    'philippines': 'Philippines',
    'india': 'India',
    'china': 'China',
    'papua_new_guinea': 'Papua New Guinea',
}

# Redivis table names by resolution
# The MOSAIKS dataset provides different tables for different resolutions
TABLES_BY_RESOLUTION = {
    "0.1": "0_1_x_0_1_deg_area_weighted_table:j5zh",
    "0.01": "mosaiks_2019_planet:ergr",
}

def iter_tiles(lon_min, lat_min, lon_max, lat_max, tile_deg=0.5):
    """
    Generate tiles covering a bounding box for chunked downloads.

    For high-resolution (0.01°) MOSAIKS data, downloading the entire country
    at once can cause Redivis query timeouts. This function divides the
    bounding box into smaller tiles that can be downloaded individually.

    Args:
        lon_min, lat_min, lon_max, lat_max: Bounding box coordinates
        tile_deg: Size of each tile in degrees (default: 0.5°)

    Yields:
        (lon0, lat0, lon1, lat1) tuples defining each tile's bounding box
    """
    lon_start = math.floor(lon_min / tile_deg) * tile_deg
    lat_start = math.floor(lat_min / tile_deg) * tile_deg
    lon_end = math.ceil(lon_max / tile_deg) * tile_deg
    lat_end = math.ceil(lat_max / tile_deg) * tile_deg

    lon = lon_start
    while lon < lon_end:
        lat = lat_start
        while lat < lat_end:
            yield (lon, lat, min(lon + tile_deg, lon_end), min(lat + tile_deg, lat_end))
            lat += tile_deg
        lon += tile_deg


def get_mosaiks(country: str, resolution: str, resume: bool):
    """
    Query MOSAIKS features from Redivis for a single country.

    This function:
    1. Loads country boundary from Natural Earth shapefile
    2. Queries MOSAIKS data within the country's bounding box from Redivis
    3. For high-resolution (0.01°), downloads in 0.5° tiles to avoid query timeouts
    4. Filters to points that fall within the country polygon
    5. Saves filtered data to CSV

    Args:
        country: Country short name (must be a key in COUNTRY_MAPPING)
        resolution: Grid resolution ('0.1' or '0.01')
        resume: If True, cache per-tile downloads to allow resuming after disconnects

    Raises:
        ValueError: If country is not found in COUNTRY_MAPPING
    """
    if country not in COUNTRY_MAPPING:
        available = ', '.join(sorted(COUNTRY_MAPPING.keys()))
        raise ValueError(f"Unknown country '{country}'. Available: {available}")

    sovereignt = COUNTRY_MAPPING[country]

    # Create output directories if they don't exist
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    if resume:
        TMP_DIR.mkdir(parents=True, exist_ok=True)

    # Load country boundary from Natural Earth shapefile (50m resolution)
    shapefile = (
        gpd.read_file(shapereader.natural_earth('50m', 'cultural', 'admin_0_countries'))
        .query(f"SOVEREIGNT == '{sovereignt}'")
        .reset_index(drop=True)
    )

    if len(shapefile) == 0:
        raise ValueError(f"No country found with SOVEREIGNT='{sovereignt}' in Natural Earth data")

    # Get bounding box coordinates (lon_min, lat_min, lon_max, lat_max)
    lon_min, lat_min, lon_max, lat_max = shapefile.total_bounds
    print(f"  Bounding box: lon [{lon_min:.2f}, {lon_max:.2f}], lat [{lat_min:.2f}, {lat_max:.2f}]")

    # Query MOSAIKS dataset from Redivis using bounding box filter
    # Dataset: SDSS Data Repository MOSAIKS collection
    dataset = redivis.dataset("sdss_data_repository.mosaiks:8bqm:v1_0")
    table_name = TABLES_BY_RESOLUTION[resolution]

    print(f"  Redivis table: {table_name}")

    # For 0.01° resolution, download in small tiles to avoid query timeouts
    # Each tile is 0.5° x 0.5°, queried separately and then concatenated
    if table_name == "mosaiks_2019_planet:ergr":
        dfs = []
        tiles = list(iter_tiles(lon_min, lat_min, lon_max, lat_max))
        print(f"  Downloading in {len(tiles)} tiles (0.5° each)...")

        for i, (lon0, lat0, lon1, lat1) in enumerate(tiles, start=1):
            # Use <= for the last tile in each direction to include boundary points
            last_lon = lon1 >= lon_max - 1e-9
            last_lat = lat1 >= lat_max - 1e-9
            lon_op = "<=" if last_lon else "<"
            lat_op = "<=" if last_lat else "<"

            tile_df = None
            tile_path = None

            # Check for cached tile if resuming
            if resume:
                tile_path = TMP_DIR / f"{country}_{resolution}_tile_{i:04d}.csv"
                if tile_path.exists():
                    tile_df = pd.read_csv(tile_path)

            # Download tile if not cached
            if tile_df is None:
                query = dataset.query(f"""
                    SELECT *
                    FROM {table_name}
                    WHERE lon >= {lon0} AND
                        lon {lon_op} {lon1} AND
                        lat >= {lat0} AND
                        lat {lat_op} {lat1}
                """)
                tile_df = query.to_pandas_dataframe()

                # Cache tile if resuming
                if resume:
                    tile_df.to_csv(tile_path, index=False)

            if not tile_df.empty:
                dfs.append(tile_df)

            cached = " (cached)" if resume and tile_path and tile_path.exists() else ""
            print(f"    tile {i}/{len(tiles)}: [{lon0:.2f}, {lat0:.2f}] -> [{lon1:.2f}, {lat1:.2f}] ({len(tile_df):,} rows){cached}")

        # Concatenate all tiles
        if dfs:
            df = pd.concat(dfs, ignore_index=True)
        else:
            df = pd.DataFrame()

        # Remove duplicates that may occur at tile boundaries
        if not df.empty:
            df = df.drop_duplicates(subset=['lon', 'lat'], ignore_index=True)
    else:
        # For 0.1° resolution, download entire country in one query
        print("  Downloading entire country...")
        query = dataset.query(f"""
            SELECT *
            FROM {table_name}
            WHERE lon >= {lon_min} AND
                lon <= {lon_max} AND
                lat >= {lat_min} AND
                lat <= {lat_max}
        """)
        df = query.to_pandas_dataframe()

    # Clip to country boundary (bounding box may include neighboring countries)
    print(f"  Clipping to country boundary...")
    if df.empty:
        within_polygon = []
    else:
        within_polygon = (
            gpd.GeoDataFrame(
                df[["lon", "lat"]],
                geometry=gpd.points_from_xy(df["lon"], df["lat"]),
                crs="EPSG:4326"
            )
            .within(shapefile.loc[0, 'geometry'])
        )

    # Filter to only points within the country polygon
    if not df.empty:
        df = df[within_polygon].reset_index(drop=True)

    # Clean up unnecessary columns from the 0.1° table
    if table_name == "0_1_x_0_1_deg_area_weighted_table:j5zh" and not df.empty:
        cols_to_drop = [c for c in ['_unnamed_var', 'continent'] if c in df.columns]
        if cols_to_drop:
            df = df.drop(columns=cols_to_drop)

    # Save filtered data to CSV
    output_file = DATA_DIR / f"{country}_mosaiks_features_{resolution}deg.csv"
    df.to_csv(output_file, index=False)

    print(f"  {country.upper()}: {len(df):,} rows -> {output_file.name}")


def main():
    """
    Main function to download MOSAIKS features for a single country.

    Steps:
    1. Parse command-line arguments (country, resolution, resume)
    2. Load country boundary and compute bounding box
    3. Query MOSAIKS data from Redivis (in tiles for 0.01° resolution)
    4. Clip to country boundary and save to CSV
    """
    parser = argparse.ArgumentParser(
        description="Download MOSAIKS features from Redivis for a single country.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=f"""
Examples:
    python 02_get_mosaiks.py --country uganda --resolution 0.01

Available countries:
    {', '.join(sorted(COUNTRY_MAPPING.keys()))}

Notes:
    - Requires Redivis authentication (run `redivis login` first)
    - For 0.01° resolution, data is downloaded in 0.5° tiles to avoid timeouts
    - Use --resume to cache tiles and allow resuming interrupted downloads
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
        choices=sorted(TABLES_BY_RESOLUTION.keys()),
        default="0.01",
        help="Grid resolution in degrees (default: 0.01).",
    )
    parser.add_argument(
        "--resume",
        action="store_true",
        help="Cache per-tile downloads to disk so runs can resume after disconnects.",
    )
    args = parser.parse_args()

    country = args.country.lower()

    # Validate country
    if country not in COUNTRY_MAPPING:
        available = ', '.join(sorted(COUNTRY_MAPPING.keys()))
        print(f"ERROR: Unknown country '{country}'")
        print(f"Available countries: {available}")
        return

    print("=" * 60)
    print("Get MOSAIKS Features from Redivis")
    print("=" * 60)
    print(f"Country: {country.upper()} (SOVEREIGNT: {COUNTRY_MAPPING[country]})")
    print(f"Resolution: {args.resolution}°")
    print(f"Resume mode: {'enabled' if args.resume else 'disabled'}")
    print(f"Output: data/raw/forest/mosaiks_features/{country}_mosaiks_features_{args.resolution}deg.csv")
    print()

    get_mosaiks(country, args.resolution, args.resume)

    print("\n" + "=" * 60)
    print("COMPLETE!")
    print("=" * 60)


if __name__ == "__main__":
    main()
