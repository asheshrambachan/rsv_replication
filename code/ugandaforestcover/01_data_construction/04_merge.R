# =============================================================================
# Uganda Forest Cover — Merge MOSAIKS Features with Forest Cover
#
# Performs an inner join on (lat, lon) to combine:
#   - Forest cover data from Hansen Global Forest Change (01_get_forest_change.py)
#   - MOSAIKS satellite feature vectors (02_get_mosaiks.py)
#
# Only grid cells present in both datasets are retained.
#
# Input:
#   data/raw/ugandaforestcover/forest_cover/<country>_forest_cover_<year>_<resolution>deg.csv
#   data/raw/ugandaforestcover/mosaiks_features/<country>_mosaiks_features_<resolution>deg.csv
#
# Output:
#   data/interim/ugandaforestcover/data.csv
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(argparse)
})

# -----------------------------------------------------------------------------
# 1. Parse Arguments
# -----------------------------------------------------------------------------

parser <- ArgumentParser()
parser$add_argument("--country",    default = "uganda", help = "Country short name (default: uganda).")
parser$add_argument("--resolution", default = "0.01",   help = "Grid resolution in degrees (default: 0.01).")
parser$add_argument("--year",       default = "2011",   help = "Forest cover year (default: 2011).")
args       <- parser$parse_args()
country    <- tolower(args$country)
resolution <- args$resolution
year       <- args$year

# -----------------------------------------------------------------------------
# 2. Load Data
# -----------------------------------------------------------------------------

forest_path  <- sprintf("data/raw/ugandaforestcover/forest_cover/%s_forest_cover_%s_%sdeg.csv",
                        country, year, resolution)
mosaiks_path <- sprintf("data/raw/ugandaforestcover/mosaiks_features/%s_mosaiks_features_%sdeg.csv",
                        country, resolution)

stopifnot(file.exists(forest_path),  file.exists(mosaiks_path))

cat("Loading forest cover:   ", basename(forest_path),  "\n")
forest  <- fread(forest_path)
cat(sprintf("  %s rows\n", format(nrow(forest), big.mark = ",")))

cat("Loading MOSAIKS features:", basename(mosaiks_path), "\n")
mosaiks <- fread(mosaiks_path)
cat(sprintf("  %s rows\n", format(nrow(mosaiks), big.mark = ",")))

# -----------------------------------------------------------------------------
# 3. Merge on (lat, lon)
# -----------------------------------------------------------------------------

cat("\nMerging on (lat, lon)...\n")
dt <- merge(forest, mosaiks, by = c("lat", "lon"))
cat(sprintf("  %s matched observations\n", format(nrow(dt), big.mark = ",")))

stopifnot(nrow(dt) > 0)

forest_only  <- nrow(forest)  - nrow(dt)
mosaiks_only <- nrow(mosaiks) - nrow(dt)
if (forest_only  > 0) cat(sprintf("  %s forest cover rows had no MOSAIKS match\n",  format(forest_only,  big.mark = ",")))
if (mosaiks_only > 0) cat(sprintf("  %s MOSAIKS rows had no forest cover match\n", format(mosaiks_only, big.mark = ",")))

# -----------------------------------------------------------------------------
# 4. Save
# -----------------------------------------------------------------------------

out_path <- "data/interim/ugandaforestcover/data.csv"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
fwrite(dt, out_path)

cat(sprintf("\nSaved: %s\n", out_path))
cat(sprintf("  %s rows, %d columns\n", format(nrow(dt), big.mark = ","), ncol(dt)))
