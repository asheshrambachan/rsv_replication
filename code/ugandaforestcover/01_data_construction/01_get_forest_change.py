#!/usr/bin/env python3
"""
Download and process Hansen Global Forest Change dataset for a single country.

This script performs two main tasks:
1. Downloads Hansen GFC tiles (treecover2000, lossyear, datamask) for the specified country
2. Processes tiles to create forest cover data at the specified resolution

The Hansen Global Forest Change dataset (Hansen et al., 2013) provides:
- treecover2000: Tree canopy cover percentage (0-100%) in year 2000
- lossyear: Year of forest loss (0 = no loss, 1-19 = 2001-2019)
- datamask: Land/water classification (0 = nodata, 1 = land, 2 = water)

Usage:
    python 01_get_forest_change.py --country uganda --resolution 0.01 --year 2011

Output:
- Downloaded tiles: data/raw/ugandaforestcover/forest_cover/hansen_tiles/<country>/*.tif
- Processed data: data/raw/ugandaforestcover/forest_cover/<country>_forest_cover_<year>_<resolution>deg.csv
"""

import warnings
import argparse
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed
from typing import List, Tuple, Dict

import numpy as np
import pandas as pd
import rasterio
import requests
import geopandas as gpd
from cartopy.io import shapereader

# ============================================================================
# Configuration
# ============================================================================

# Hansen GFC dataset configuration
# Data source: https://glad.earthengine.app/view/global-forest-change
BASE_URL = "https://storage.googleapis.com/earthenginepartners-hansen/GFC-2019-v1.7"
LAYERS = ['treecover2000', 'lossyear', 'datamask']

# Directory paths
DATA_DIR = Path("data")
TILES_DIR = DATA_DIR / "raw" / "forest" / "forest_cover" / "hansen_tiles"
OUTPUT_DIR = DATA_DIR / "raw" / "forest" / "forest_cover"

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


# ============================================================================
# Tile Generation Functions
# ============================================================================

def get_country_tiles(country: str) -> List[Tuple[str, str]]:
    """
    Generate 10×10 degree Hansen tile identifiers for a single country.

    Hansen GFC tiles are distributed as 10×10 degree rasters. Each tile is named
    by its NORTHERN latitude edge and WESTERN longitude edge. For example:

        Tile "00N_020E" covers:
        - Latitude: -10° to 0° (00N is the northern edge)
        - Longitude: 20° to 30° (020E is the western edge)

    This function:
    1. Loads country boundary from Natural Earth Admin 0 shapefile
    2. Computes bounding box of the country
    3. Generates list of Hansen tile IDs that cover the country

    Args:
        country: Country short name (must be a key in COUNTRY_MAPPING)

    Returns:
        List of (lat_label, lon_label) tile identifiers, e.g., [('10N', '030E'), ...]

    Raises:
        ValueError: If country is not found in COUNTRY_MAPPING
    """
    if country not in COUNTRY_MAPPING:
        available = ', '.join(sorted(COUNTRY_MAPPING.keys()))
        raise ValueError(f"Unknown country '{country}'. Available: {available}")

    sovereignt = COUNTRY_MAPPING[country]

    # Load country boundary from Natural Earth shapefile (50m resolution)
    shapefile = (
        gpd.read_file(
            shapereader.natural_earth('50m', 'cultural', 'admin_0_countries')
        )
        .query(f"SOVEREIGNT == '{sovereignt}'")
        .reset_index(drop=True)
    )

    if len(shapefile) == 0:
        raise ValueError(f"No country found with SOVEREIGNT='{sovereignt}' in Natural Earth data")

    # Get bounding box coordinates (lon_min, lat_min, lon_max, lat_max)
    lon_min, lat_min, lon_max, lat_max = shapefile.total_bounds

    # Calculate 10-degree grid boundaries aligned to Hansen tiles
    # lon_start: western edge of westernmost tile
    # lat_start: northern edge of southernmost tile (ceil because tiles go south)
    lon_start = int(np.floor(lon_min / 10.0) * 10)
    lon_end = int(np.floor(lon_max / 10.0) * 10)
    lat_start = int(np.ceil(lat_min / 10.0) * 10)
    lat_end = int(np.ceil(lat_max / 10.0) * 10)

    # Generate grid of latitudes and longitudes (10-degree intervals)
    lats = list(range(lat_start, lat_end + 10, 10))
    lons = list(range(lon_start, lon_end + 10, 10))

    # Create meshgrid and convert to DataFrame
    lat_grid, lon_grid = np.meshgrid(lats, lons, indexing='ij')
    df = pd.DataFrame({
        'lat': lat_grid.ravel(),
        'lon': lon_grid.ravel(),
    })

    # Convert grid points to Hansen tile IDs
    tiles = []
    for _, r in df.iterrows():
        N_max = r.lat  # Northern edge of tile
        E_min = r.lon  # Western edge of tile

        # Format latitude label (e.g., 00N, 10S)
        if N_max < 0:
            N_label = f"{abs(int(N_max)):02d}S"
        else:
            N_label = f"{int(N_max):02d}N"

        # Format longitude label (e.g., 020E, 070W)
        if E_min < 0:
            E_label = f"{abs(int(E_min)):03d}W"
        else:
            E_label = f"{int(E_min):03d}E"

        tiles.append((N_label, E_label))

    return tiles


# ============================================================================
# Download Functions
# ============================================================================

def build_tile_url(layer: str, lat: str, lon: str) -> str:
    """
    Build the download URL for a specific Hansen tile and layer.

    Args:
        layer: Layer name (e.g., 'treecover2000', 'lossyear', 'datamask')
        lat: Latitude label (e.g., '00N', '10S')
        lon: Longitude label (e.g., '020E', '070W')

    Returns:
        Full URL to the tile file
    """
    filename = f"Hansen_GFC-2019-v1.7_{layer}_{lat}_{lon}.tif"
    return f"{BASE_URL}/{filename}"


def download_tile(url: str, output_path: Path) -> Tuple[bool, str]:
    """
    Download a single Hansen tile.

    Args:
        url: URL of the tile to download
        output_path: Path where the tile should be saved

    Returns:
        Tuple of (success: bool, message: str)
    """
    # Skip if file already exists
    if output_path.exists():
        return True, f"Already exists: {output_path.name}"

    try:
        response = requests.get(url, stream=True, timeout=300)

        if response.status_code == 200:
            # Create directory if needed
            output_path.parent.mkdir(parents=True, exist_ok=True)

            # Download file in chunks
            with open(output_path, 'wb') as f:
                for chunk in response.iter_content(chunk_size=8192):
                    f.write(chunk)

            return True, f"Downloaded: {output_path.name}"

        elif response.status_code == 404:
            return False, f"Not found (may be ocean): {output_path.name}"

        else:
            return False, f"Error {response.status_code}: {output_path.name}"

    except Exception as e:
        return False, f"Failed: {output_path.name} - {str(e)}"


def download_country_tiles(country: str, tiles: List[Tuple[str, str]], layers: List[str]) -> Dict:
    """
    Download all tiles for a country using parallel downloads.

    Args:
        country: Country code (e.g., 'DRC', 'BRAZIL')
        tiles: List of (lat, lon) tile identifiers
        layers: List of layers to download

    Returns:
        Dictionary with keys 'success', 'failed', 'skipped' containing lists of messages
    """
    country_dir = TILES_DIR / country.lower()
    country_dir.mkdir(parents=True, exist_ok=True)

    # Build list of download tasks
    download_tasks = []
    for lat, lon in tiles:
        for layer in layers:
            url = build_tile_url(layer, lat, lon)
            filename = f"Hansen_GFC-2019-v1.7_{layer}_{lat}_{lon}.tif"
            output_path = country_dir / filename
            download_tasks.append((url, output_path))

    results = {'success': [], 'failed': [], 'skipped': []}

    print(f"\nDownloading {len(download_tasks)} tiles for {country}...")

    # Download tiles in parallel
    with ThreadPoolExecutor(max_workers=4) as executor:
        futures = {executor.submit(download_tile, url, path): (url, path)
                   for url, path in download_tasks}

        for future in as_completed(futures):
            success, message = future.result()
            print(f"  {message}")

            if success:
                if "Already exists" in message:
                    results['skipped'].append(message)
                else:
                    results['success'].append(message)
            else:
                results['failed'].append(message)

    return results


# ============================================================================
# Processing Functions
# ============================================================================

def process_tile(
    tile_lat: str,
    tile_lon: str,
    country_dir: Path,
    target_year: int = 2019,
    resolution: float = 0.1,
) -> pd.DataFrame:
    """
    Process a single Hansen tile and aggregate to specified resolution.

    This function:
    1. Reads treecover2000, lossyear, and datamask layers
    2. Masks non-land pixels (water, nodata) as NaN
    3. Calculates forest cover at target year by removing losses
    4. Aggregates to resolution using area-weighted averaging
    5. Returns DataFrame with lon, lat, forest_cover columns

    MOSAIKS Grid Alignment:
    Output coordinates are aligned to the MOSAIKS grid system where grid cell
    centers are at: n * resolution + resolution/2. This ensures:
    - For 0.01° resolution: coordinates end in .xx5 (e.g., 30.005, 30.015, 32.355)
    - For 0.1° resolution: coordinates end in .x5 (e.g., 29.75, 29.85, 30.05)

    Args:
        tile_lat: Latitude label (e.g., '00N', '10S')
        tile_lon: Longitude label (e.g., '020E', '070W')
        country_dir: Directory containing the tile files
        target_year: Year for forest cover calculation (default: 2019)
        resolution: Output grid resolution in degrees (default: 0.1)

    Returns:
        DataFrame with columns: lon, lat, forest_cover (0-100%)
    """
    # Build file paths for the three required layers
    forest2000_path = country_dir / f"Hansen_GFC-2019-v1.7_treecover2000_{tile_lat}_{tile_lon}.tif"
    lossyear_path = country_dir / f"Hansen_GFC-2019-v1.7_lossyear_{tile_lat}_{tile_lon}.tif"
    datamask_path = country_dir / f"Hansen_GFC-2019-v1.7_datamask_{tile_lat}_{tile_lon}.tif"

    # Check if all required files exist
    if not all([forest2000_path.exists(), lossyear_path.exists(), datamask_path.exists()]):
        print(f"    Skipping {tile_lat}_{tile_lon}: Missing layer files")
        return pd.DataFrame()

    try:
        # Read all three layers
        with rasterio.open(forest2000_path) as tc, \
             rasterio.open(lossyear_path) as ly, \
             rasterio.open(datamask_path) as dm:

            # Read raster data
            forest2000 = tc.read(1).astype("float32")  # uint8, 0-100% canopy in 2000
            lossyear = ly.read(1)  # uint8, 0=no loss, 1=2001, 2=2002, ...
            datamask = dm.read(1)  # 0=nodata, 1=land, 2=water

            # Get raster dimensions and pixel resolution
            h, w = forest2000.shape
            pixel_w = tc.transform.a  # Longitude pixel width in degrees
            pixel_h = abs(tc.transform.e)  # Latitude pixel height in degrees

            # Parse tile coordinates to get geographic bounds
            N_max = int(tile_lat[:-1])  # Northern edge
            if tile_lat.endswith('S'):
                N_max = -N_max

            E_min = int(tile_lon[:-1])  # Western edge
            if tile_lon.endswith('W'):
                E_min = -E_min

        # Set non-land pixels (water, nodata) to NaN
        land = (datamask == 1)
        forest2000[~land] = np.nan

        # Calculate forest cover at target year
        # Forest that survives to target_year has either:
        #  - lossyear == 0 (no loss recorded)
        #  - lossyear > (target_year - 2000) (loss occurred after target year)
        loss_threshold = target_year - 2000
        forest_target = forest2000.copy()
        forest_target[(lossyear > 0) & (lossyear <= loss_threshold)] = 0

        # Aggregate to requested resolution
        # Calculate number of native pixels per output grid cell
        # For MOSAIKS alignment, resolution should be an exact multiple of pixel size
        block_cols = int(round(resolution / pixel_w))
        block_rows = int(round(resolution / pixel_h))

        # Verify alignment: resolution should equal block_size * pixel_size
        actual_lon_res = block_cols * pixel_w
        actual_lat_res = block_rows * pixel_h
        if abs(actual_lon_res - resolution) > 1e-6 or abs(actual_lat_res - resolution) > 1e-6:
            print(f"    WARNING: Resolution {resolution}° does not align perfectly with pixel size")
            print(f"    Actual cell size: {actual_lon_res:.6f}° x {actual_lat_res:.6f}°")

        # Trim array to be evenly divisible by block size
        new_h = (h // block_rows) * block_rows
        new_w = (w // block_cols) * block_cols
        forest_trimmed = forest_target[:new_h, :new_w]

        # Reshape into blocks for aggregation
        forest_blocks = forest_trimmed.reshape(
            new_h // block_rows, block_rows,
            new_w // block_cols, block_cols
        )

        # Compute mean for each block, ignoring NaN values
        # Suppress warning for blocks that are entirely NaN (ocean/nodata)
        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', message='Mean of empty slice')
            forest_agg = np.nanmean(forest_blocks, axis=(1, 3))

        # Create lat/lon grid aligned to MOSAIKS coordinate system
        # MOSAIKS grid centers are at: n * resolution + resolution/2
        # - For 0.01° resolution: 0.005, 0.015, 0.025, ... (3rd decimal = 5)
        # - For 0.1° resolution: 0.05, 0.15, 0.25, ... (2nd decimal = 5)
        #
        # Since Hansen tiles start at multiples of 10° (E_min = 10, 20, 30, ...),
        # the first grid center is at E_min + resolution/2, which aligns with MOSAIKS.

        n_lons = forest_agg.shape[1]
        n_lats = forest_agg.shape[0]

        # Use explicit indexing to avoid floating-point accumulation errors
        # lons[i] = E_min + resolution/2 + i * resolution
        # lats[j] = N_max - resolution/2 - j * resolution
        lons = E_min + resolution / 2 + np.arange(n_lons) * resolution
        lats = N_max - resolution / 2 - np.arange(n_lats) * resolution

        # Round to appropriate decimal places to ensure clean MOSAIKS-aligned values
        # 0.1° resolution → 2 decimal places (e.g., 30.05, 30.15)
        # 0.01° resolution → 3 decimal places (e.g., 30.005, 30.015)
        decimals = 2 if resolution >= 0.1 else 3
        lons = np.round(lons, decimals)
        lats = np.round(lats, decimals)

        # Create coordinate meshgrid
        lon_grid, lat_grid = np.meshgrid(lons, lats)

        # Build DataFrame with results
        df = pd.DataFrame({
            'lon': lon_grid.ravel(),
            'lat': lat_grid.ravel(),
            'forest_cover': forest_agg.ravel()
        })

        # Remove rows where all pixels in the block were NaN (ocean/nodata areas)
        df = df.dropna(subset=['forest_cover']).reset_index(drop=True)

        return df

    except Exception as e:
        print(f"    Error processing {tile_lat}_{tile_lon}: {str(e)}")
        return pd.DataFrame()


def process_country(
    country_short: str,
    tiles: List[Tuple[str, str]],
    target_year: int = 2019,
    resolution: float = 0.1,
) -> pd.DataFrame:
    """
    Process all tiles for a country and combine into single dataset.

    Args:
        country_short: Country short name (e.g., 'drc', 'brazil')
        tiles: List of (lat, lon) tile identifiers
        target_year: Year for forest cover calculation

    Returns:
        Combined DataFrame with all tiles for the country
    """
    country_upper = country_short.upper()
    country_dir = TILES_DIR / country_short

    if not country_dir.exists():
        print(f"  Country directory not found: {country_dir}")
        return pd.DataFrame()

    print(f"\n{country_upper}: Processing {len(tiles)} tiles for year {target_year}")

    # Process each tile
    all_data = []
    for idx, (tile_lat, tile_lon) in enumerate(tiles, 1):
        print(f"  [{idx}/{len(tiles)}] Processing tile {tile_lat}_{tile_lon}")
        tile_df = process_tile(tile_lat, tile_lon, country_dir, target_year, resolution)

        if not tile_df.empty:
            all_data.append(tile_df)
            print(f"    Added {len(tile_df):,} grid cells")

    if not all_data:
        print(f"  No data processed for {country_upper}")
        return pd.DataFrame()

    # Combine all tiles into single DataFrame
    combined_df = pd.concat(all_data, ignore_index=True)

    # Remove duplicates (edge cells might appear in multiple tiles)
    combined_df = combined_df.drop_duplicates(subset=['lon', 'lat'], keep='first')

    print(f"  Total: {len(combined_df):,} grid cells at {resolution}° resolution")

    return combined_df


# ============================================================================
# Main Execution
# ============================================================================

def main():
    """
    Main function to download and process Hansen GFC tiles.

    Steps:
    1. Parse command-line arguments (country, resolution, year)
    2. Generate list of Hansen tiles covering the country
    3. Download tiles in parallel (skips existing files)
    4. Process tiles to compute forest cover at target year
    5. Clip to country boundary and save to CSV
    """
    parser = argparse.ArgumentParser(
        description="Download and process Hansen Global Forest Change data for a single country.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=f"""
Examples:
    python 01_get_forest_change.py --country uganda --resolution 0.01 --year 2011

Available countries:
    {', '.join(sorted(COUNTRY_MAPPING.keys()))}
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
        help="Output grid resolution in degrees (default: 0.01).",
    )
    parser.add_argument(
        "--year",
        type=int,
        default=2011,
        help="Target year for forest cover calculation (default: 2011). "
             "Forest cover is computed as year 2000 cover minus losses up to this year.",
    )
    args = parser.parse_args()

    country = args.country.lower()
    resolution = float(args.resolution)
    target_year = args.year

    # Validate country
    if country not in COUNTRY_MAPPING:
        available = ', '.join(sorted(COUNTRY_MAPPING.keys()))
        print(f"ERROR: Unknown country '{country}'")
        print(f"Available countries: {available}")
        return

    sovereignt = COUNTRY_MAPPING[country]

    print("=" * 60)
    print("Hansen Global Forest Change Dataset Pipeline")
    print("=" * 60)
    print(f"Country: {country.upper()} (SOVEREIGNT: {sovereignt})")
    print(f"Resolution: {args.resolution}°")
    print(f"Target year: {target_year}")
    print("=" * 60)

    # Create output directories
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    TILES_DIR.mkdir(parents=True, exist_ok=True)
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    # Generate tiles for the country based on boundaries
    tiles = get_country_tiles(country)
    print(f"\nTiles to download: {len(tiles)} tiles × {len(LAYERS)} layers = {len(tiles) * len(LAYERS)} files")

    # Download tiles
    results = download_country_tiles(country.upper(), tiles, LAYERS)

    # Print download summary
    print("\n" + "=" * 60)
    print("DOWNLOAD SUMMARY")
    print("=" * 60)
    print(f"  Downloaded: {len(results['success'])}")
    print(f"  Already existed: {len(results['skipped'])}")
    print(f"  Failed/Not found: {len(results['failed'])}")
    print(f"\nTiles directory: {TILES_DIR / country}")

    # Process tiles to requested resolution
    print("\n" + "=" * 60)
    print(f"PROCESSING TILES TO {args.resolution}° RESOLUTION")
    print("=" * 60)
    print(f"Computing forest cover for year {target_year}")
    print("  (forest2000 - losses through target year)\n")

    # Process tiles
    forest_df = process_country(country, tiles, target_year, resolution)

    if forest_df.empty:
        print("ERROR: No data processed")
        return

    # Clip to country boundary (tiles may extend beyond country borders)
    print("\nClipping to country boundary...")
    shapefile = (
        gpd.read_file(shapereader.natural_earth('50m', 'cultural', 'admin_0_countries'))
        .query(f"SOVEREIGNT == '{sovereignt}'")
        .reset_index(drop=True)
    )
    within_polygon = (
        gpd.GeoDataFrame(
            forest_df[["lon", "lat"]],
            geometry=gpd.points_from_xy(forest_df["lon"], forest_df["lat"]),
            crs=shapefile.crs
        )
        .within(shapefile.loc[0, 'geometry'])
    )
    forest_df = forest_df[within_polygon].reset_index(drop=True)
    print(f"  {len(forest_df):,} grid cells within country boundary")

    # Save output
    output_file = OUTPUT_DIR / f"{country}_forest_cover_{target_year}_{args.resolution}deg.csv"
    forest_df.to_csv(output_file, index=False)
    print(f"\nSaved: {output_file}")

    print("\n" + "=" * 60)
    print("COMPLETE!")
    print("=" * 60)


if __name__ == "__main__":
    main()
