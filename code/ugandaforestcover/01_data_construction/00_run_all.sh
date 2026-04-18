#!/usr/bin/env bash
# =============================================================================
# Run All — Forest Data Construction
#
# Builds the analysis dataset and trains satellite predictors for Uganda at
# 0.01° resolution, year 2011. Override defaults by passing arguments directly
# to each script, e.g.:
#   python 01_get_forest_change.py --country brazil --year 2015
#
# Steps:
#   01_get_forest_change.py  — download and process Hansen GFC tiles
#   02_get_mosaiks.py        — download MOSAIKS features from Redivis
#   03_shapefiles.R          — clean and save shapefiles to data/clean/ugandaforestcover/shapefiles/
#   04_merge.py              — merge forest cover and MOSAIKS features
#   05_data_splits.R         — create train/sim splits and compute S_e/S_o sample flags
#   06_rf_train.R    — train random forest satellite predictors
#   07_rf_predict.R — apply predictors to sim split; save predictions
# =============================================================================

set -euo pipefail

# Run from the replication package root
cd "$(dirname "$0")/../../.."

python  code/ugandaforestcover/01_data_construction/01_get_forest_change.py --country uganda --resolution 0.01 --year 2011
python  code/ugandaforestcover/01_data_construction/02_get_mosaiks.py       --country uganda --resolution 0.01
Rscript code/ugandaforestcover/01_data_construction/03_shapefiles.R
Rscript code/ugandaforestcover/01_data_construction/04_merge.R             --country uganda --resolution 0.01 --year 2011
Rscript code/ugandaforestcover/01_data_construction/05_data_splits.R        --country uganda --resolution 0.01 --year 2011
Rscript code/ugandaforestcover/01_data_construction/06_rf_train.R
Rscript code/ugandaforestcover/01_data_construction/07_rf_predict.R
