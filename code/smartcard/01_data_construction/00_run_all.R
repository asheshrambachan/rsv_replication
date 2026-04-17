# =============================================================================
# Smartcard — Data Construction: Run All
#
# Runs the four data construction scripts in order. Script 02 downloads
# satellite features from the MOSAIKS API and may take significant time;
# it can be skipped on subsequent runs if both satellite_features.Rds and
# centroid_coords.Rds already exist.
# =============================================================================

source("code/smartcard/01_data_construction/01_study_data.R")
source("code/smartcard/01_data_construction/02_satellite_features.R")
source("code/smartcard/01_data_construction/03_merge_and_pca.R")
source("code/smartcard/01_data_construction/04_shapefiles.R")
