# =============================================================================
# Uganda Forest Cover — Data Splits
#
# Takes the merged forest cover + MOSAIKS dataset and produces two splits:
#
#   data_train.csv  — predictor-training half (06_rf_train.R)
#   data_sim.csv    — simulation half (03_sims_noexpoutcomes/,
#                     04_sims_expoutcomes/)
#
# Three outcomes are constructed from forest cover:
#   Ybin  — binary: 1 if forest cover exceeds the 80th percentile of
#            the PES imagery region (used in noexpoutcomes simulations)
#   Ydisc — 5-bin discretisation based on quintiles of the imagery
#            region distribution; bins represented by midpoints
#   Ycont — raw forest cover proportion (used in expoutcomes sims)
#
# Sample membership flags are also pre-computed here so that downstream
# scripts can read them directly from the CSV rather than re-running the
# spatial joins each time:
#   S_e       — 1 if the cell falls inside the PES imagery boundary
#               (experimental sample in the simulation design)
#   S_o_02km  — 1 if outside the boundary and within 2 km (observational)
#   S_o_05km  — 1 if outside the boundary and within 5 km (observational)
#   S_o_10km  — 1 if outside the boundary and within 10 km (observational)
#
# Outcome thresholds are computed within the PES imagery boundary so that
# the distribution is anchored to the ecologically relevant study region.
#
# Input:
#   data/interim/ugandaforestcover/data.csv
#   data/raw/ugandaforestcover/jayachandran_et_al_replication_files/
#     CashForCarbon_full/BaselineShapesWGS84_dissolved.shp
#   data/clean/ugandaforestcover/shapefiles/imagery_boundary.gpkg
#
# Outputs:
#   data/interim/ugandaforestcover/data_train.csv
#   data/interim/ugandaforestcover/data_sim.csv
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(argparse)
  library(data.table)
  library(sf)
})

set.seed(42)

# -----------------------------------------------------------------------------
# 1. Parse Arguments
# -----------------------------------------------------------------------------

parser <- ArgumentParser()
parser$add_argument("--country",    default = "uganda", help = "Country short name (default: uganda).")
parser$add_argument("--resolution", default = "0.01",   help = "Grid resolution in degrees (default: 0.01).")
parser$add_argument("--year",       default = "2011",   help = "Target year for forest cover (default: 2011).")
args       <- parser$parse_args()
country    <- args$country
resolution <- args$resolution
year       <- args$year

# -----------------------------------------------------------------------------
# 2. Load Data
# -----------------------------------------------------------------------------

input_path   <- "data/interim/ugandaforestcover/data.csv"
imagery_path <- "data/clean/ugandaforestcover/shapefiles/imagery_boundary.gpkg"

out_train <- file.path("data/interim/ugandaforestcover", "data_train.csv")
out_sim   <- file.path("data/interim/ugandaforestcover", "data_sim.csv")

dt <- fread(input_path)
stopifnot(all(c("lon", "lat", "forest_cover") %in% names(dt)))

# Hansen GFC reports canopy cover as 0–100%; convert to proportion for
# consistency with the [0, 1] scale used throughout the simulations.
dt[, forest_cover := forest_cover / 100]

cat(sprintf("Loaded %d grid cells from %s\n", nrow(dt), input_path))

# -----------------------------------------------------------------------------
# 3. Compute Sample Membership Flags
#
# S_e flags cells inside the PES imagery boundary (experimental sample).
# S_o_* flags cells just outside, in buffer bands used as the observational
# sample in the simulation designs. Distances are computed in UTM (metres)
# for accuracy; WGS84 is used for the within-boundary test.
#
# These flags are saved into the output CSVs so that all downstream scripts
# (params, KS test, figures) can read them directly without rerunning the
# spatial joins.
# -----------------------------------------------------------------------------

epsg_wgs84 <- 4326L
epsg_utm   <- 32636L   # UTM zone 36N — covers Uganda

imagery_wgs84 <- st_read(imagery_path, quiet = TRUE) |> st_transform(epsg_wgs84)
imagery_utm   <- st_transform(imagery_wgs84, epsg_utm)

pts_wgs84 <- st_as_sf(dt, coords = c("lon", "lat"), crs = epsg_wgs84, remove = FALSE)
pts_utm   <- st_transform(pts_wgs84, epsg_utm)

# S_e: inside the imagery boundary
dt[, S_e := as.integer(lengths(st_within(pts_wgs84, imagery_wgs84)) > 0)]

# Distance (km) from each cell to the nearest imagery polygon edge
d_km <- as.numeric(apply(st_distance(pts_utm, imagery_utm), 1, min)) / 1000
dt[, dist_km := d_km]

# S_o_*: outside the boundary (S_e == 0) and within each distance band
dt[, S_o_02km := as.integer(S_e == 0 & dist_km <= 2)]
dt[, S_o_05km := as.integer(S_e == 0 & dist_km <= 5)]
dt[, S_o_10km := as.integer(S_e == 0 & dist_km <= 10)]
dt[, dist_km  := NULL]   # intermediate; not needed in outputs

cat(sprintf("  S_e:      %d cells inside imagery boundary\n",   sum(dt$S_e)))
cat(sprintf("  S_o_02km: %d cells within  2 km outside\n", sum(dt$S_o_02km)))
cat(sprintf("  S_o_05km: %d cells within  5 km outside\n", sum(dt$S_o_05km)))
cat(sprintf("  S_o_10km: %d cells within 10 km outside\n", sum(dt$S_o_10km)))

# -----------------------------------------------------------------------------
# 4. Construct Outcomes
#
# All thresholds are computed on the imagery-region subsample (S_e == 1).
#
# Ybin: binary outcome used in the no-experimental-outcomes simulations.
#   A cell is "high forest cover" (Y = 1) if it exceeds the 80th percentile
#   of the imagery-region distribution. This mirrors the policy-relevant
#   threshold in Jayachandran et al.
#
# Ydisc: 5-bin discretisation used in the experimental-outcomes
#   simulations. Quintile boundaries are computed within the imagery region;
#   each bin is represented by its midpoint so the outcome is numeric.
#
# Ycont: raw forest cover proportion, used as a continuous outcome
#   in the experimental-outcomes simulations.
# -----------------------------------------------------------------------------

# Binary outcome: above 80th percentile within imagery region
tau80 <- quantile(dt[S_e == 1, forest_cover], probs = 0.80, na.rm = TRUE)
dt[, Ybin := as.integer(forest_cover > tau80)]

# Discrete outcome: 5 quantile bins from imagery-region distribution
qbreaks <- unique(as.numeric(quantile(
  dt[S_e == 1, forest_cover],
  probs = seq(0, 1, length.out = 6),
  na.rm = TRUE
)))

# Guard against degenerate case where breaks collapse (e.g. very sparse data)
if (length(qbreaks) < 3) {
  warning("Quantile breaks collapsed (too many ties). Falling back to pretty breaks.")
  qbreaks <- pretty(dt$forest_cover, n = 5)
}

# Ensure breaks span the full [0, 1] range so all cells are assigned a bin
qbreaks[1]               <- min(0, qbreaks[1])
qbreaks[length(qbreaks)] <- max(1, qbreaks[length(qbreaks)])

midpoints <- (head(qbreaks, -1) + tail(qbreaks, -1)) / 2
cat("Discrete bin midpoints (rounded):\n")
print(round(midpoints, 3))

dt[, Ydisc := midpoints[cut(
  forest_cover,
  breaks         = qbreaks,
  labels         = midpoints,
  include.lowest = TRUE,
  right          = TRUE
)]]

dt[, Ycont := forest_cover]

# -----------------------------------------------------------------------------
# 5. Split into Training and Simulation Halves
#
# A random 50/50 row split separates:
#   train — used to fit the satellite predictor models (01_data_construction/)
#   sim   — held out for the simulation studies; never seen by the predictors
#
# Geolocation columns from the admin boundary join (shapeGroup, adm IDs) are
# dropped before saving as they are not used downstream.
# -----------------------------------------------------------------------------

dt[, c("shapeGroup", "adm2_shapeID_geoBoundaries",
       "adm1_shapeID_geoBoundaries") := NULL]

n         <- nrow(dt)
train_idx <- sample.int(n, size = floor(n / 2), replace = FALSE)
train_dt  <- dt[train_idx]
sim_dt    <- dt[-train_idx]

cat(sprintf("Train set:      %d rows\n", nrow(train_dt)))
cat(sprintf("Simulation set: %d rows\n", nrow(sim_dt)))

# -----------------------------------------------------------------------------
# 6. Save
# -----------------------------------------------------------------------------

dir.create("data/interim/ugandaforestcover", showWarnings = FALSE, recursive = TRUE)
fwrite(train_dt, out_train)
fwrite(sim_dt,   out_sim)

cat(sprintf("Saved:\n  %s\n  %s\n", out_train, out_sim))
