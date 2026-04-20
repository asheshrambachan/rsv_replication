# =============================================================================
# Run All — Smartcard Application
#
# Executes the full smartcard pipeline in order:
#
#   01_data_construction/  — build the analysis dataset (slow: MOSAIKS download)
#   02_assumption_tests/   — KS tests for Assumption 2(i) and 3(ii)
#   03_sims_noexpoutcomes/ — simulation study: calibrate and RSV (slow)
#   04_empirical/          — empirical RSV and benchmark estimation
#   05_figures/            — all figures
#   06_tables/             — all tables
#
# Each subdirectory has its own 00_run_all.R for running steps independently.
# =============================================================================

source("code/smartcards/01_data_construction/00_run_all.R")
source("code/smartcards/02_assumption_tests/00_run_all.R")
source("code/smartcards/03_sims_noexpoutcomes/00_run_all.R")
source("code/smartcards/04_empirical/00_run_all.R")
source("code/smartcards/05_figures/00_run_all.R")
source("code/smartcards/06_tables/00_run_all.R")
