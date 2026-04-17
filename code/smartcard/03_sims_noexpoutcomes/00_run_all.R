# =============================================================================
# Run All — No Experimental Outcomes Simulations
#
# Runs the full noexpoutcomes simulation pipeline in order:
#   01_params.R    — build parameter grid
#   02_calibrate.R — calibrate estimator simulations (slow)
#   03_rsv.R       — RSV estimator simulations (slow)
#   04_summarize.R — summarize results across replicates; save to data/clean/smartcard/
#
# Scripts 02 and 03 are computationally intensive. Each config is cached to
# disk so interrupted runs can be resumed without recomputation.
# =============================================================================

source("code/smartcard/03_sims_noexpoutcomes/01_params.R")
source("code/smartcard/03_sims_noexpoutcomes/02_calibrate.R")
source("code/smartcard/03_sims_noexpoutcomes/03_rsv.R")
source("code/smartcard/03_sims_noexpoutcomes/04_summarize.R")
