# =============================================================================
# Run All — Experimental Outcomes Simulations
#
# Runs the full expoutcomes simulation pipeline in order:
#   01_params.R    — build parameter grid
#   02_benchmark.R — benchmark estimator simulations (slow)
#   03_ppio.R      — PPIO estimator simulations (slow)
#   04_ppiv.R      — PPIV estimator simulations (slow)
#   05_rsv.R       — RSV estimator simulations (slow)
#   06_summarize.R — summarize results across replicates; save to data/clean/ugandaforestcover/
#
# Scripts 02–05 are computationally intensive. Each config is cached to
# disk so interrupted runs can be resumed without recomputation.
# =============================================================================

source("code/ugandaforestcover/04_sims_expoutcomes/01_params.R")
source("code/ugandaforestcover/04_sims_expoutcomes/02_benchmark.R")
source("code/ugandaforestcover/04_sims_expoutcomes/03_ppio.R")
source("code/ugandaforestcover/04_sims_expoutcomes/04_ppiv.R")
source("code/ugandaforestcover/04_sims_expoutcomes/05_rsv.R")
source("code/ugandaforestcover/04_sims_expoutcomes/06_summarize.R")
