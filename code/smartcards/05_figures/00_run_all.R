# =============================================================================
# Run All -- Figures
#
# Produces all smartcard figures in order:
#   01_maps.R               -- study area maps (full sample)
#   02_map_full_Stilde.R    -- study area maps (Stilde overlap design)
#   03_stability_S.R        -- Assumption 2(i) density figures, S definition
#   04_stability_Stilde.R   -- Assumption 2(i) density figures, Stilde definition
#   05_no_direct_effect.R   -- Assumption 3(ii) density figures
#   06_sims_noexpoutcomes.R -- simulation bias and coverage figures
#   07_te.R                 -- empirical treatment effect figures
#   08_representations.R    -- efficient vs simple representation figures
# =============================================================================

source("code/smartcards/05_figures/01_maps.R")
source("code/smartcards/05_figures/02_map_full_Stilde.R")
source("code/smartcards/05_figures/03_stability_S.R")
source("code/smartcards/05_figures/04_stability_Stilde.R")
source("code/smartcards/05_figures/05_no_direct_effect.R")
source("code/smartcards/05_figures/06_sims_noexpoutcomes.R")
source("code/smartcards/05_figures/07_te.R")
source("code/smartcards/05_figures/08_representations.R")
