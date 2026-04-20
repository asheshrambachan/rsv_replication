# =============================================================================
# Run All — Figures
#
# Produces all smartcard figures in order:
#   01_maps.R              — study area maps
#   02_stability.R         — Assumption 2(i) density figures
#   03_no_direct_effect.R  — Assumption 3(ii) density figures
#   04_sims_noexpoutcomes.R — simulation bias and coverage figures
#   05_te.R                — empirical treatment effect figures
#   06_representations.R   — efficient vs simple representation figures
# =============================================================================

source("code/smartcards/05_figures/01_maps.R")
source("code/smartcards/05_figures/02_stability.R")
source("code/smartcards/05_figures/03_no_direct_effect.R")
source("code/smartcards/05_figures/04_sims_noexpoutcomes.R")
source("code/smartcards/05_figures/05_te.R")
source("code/smartcards/05_figures/06_representations.R")
