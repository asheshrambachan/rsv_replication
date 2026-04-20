# =============================================================================
# Run All — No Experimental Outcomes Simulations
#
# Runs the full noexpoutcomes simulation pipeline in order:
#   01_params.R    — build parameter grid
#   02_plugin.R    — plug-in estimator simulations (slow)
#   03_calibrate.R — calibrate estimator simulations (slow)
#   04_rsv.R       — RSV estimator simulations (slow)
#   05_summarize.R — summarize results across replicates; save to data/clean/ugandaforestcover/
#
# Scripts 02–04 are computationally intensive. Each config is cached to
# disk so interrupted runs can be resumed without recomputation.
# =============================================================================

source("code/ugandaforestcover/03_sims_noexpoutcomes/01_params.R")
source("code/ugandaforestcover/03_sims_noexpoutcomes/02_plugin.R")
source("code/ugandaforestcover/03_sims_noexpoutcomes/03_calibrate.R")
source("code/ugandaforestcover/03_sims_noexpoutcomes/04_rsv.R")
source("code/ugandaforestcover/03_sims_noexpoutcomes/05_summarize.R")
