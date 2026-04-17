#!/usr/bin/env bash
# =============================================================================
# Run All — Forest Application
#
# Executes the full forest pipeline in order:
#
#   01_data_construction/  — download data, build dataset, train predictors
#   02_assumption_tests/   — KS stability test
#   03_sims_noexpoutcomes/ — simulation study: no experimental outcomes (slow)
#   04_sims_expoutcomes/   — simulation study: experimental outcomes (slow)
#   05_figures/            — all figures
#   06_tables/             — all tables
#
# Each subdirectory has its own 00_run_all.R (or 00_run_all.sh) for running
# steps independently.
# =============================================================================

set -euo pipefail

# Run from the replication package root
cd "$(dirname "$0")/../.."

bash code/forest/01_data_construction/00_run_all.sh

Rscript -e 'source("code/forest/02_assumption_tests/00_run_all.R")'
Rscript -e 'source("code/forest/03_sims_noexpoutcomes/00_run_all.R")'
Rscript -e 'source("code/forest/04_sims_expoutcomes/00_run_all.R")'
Rscript -e 'source("code/forest/05_figures/00_run_all.R")'
Rscript -e 'source("code/forest/06_tables/00_run_all.R")'
