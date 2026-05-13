# =============================================================================
# Run All -- Tables
#
# Produces all smartcard tables in order:
#   01_tab_summary_stats.R    -- summary statistics
#   02_tab_stability_S.R      -- Assumption 2(i) KS test results (S definition)
#   03_tab_stability_Stilde.R -- Assumption 2(i) KS test results (Stilde definition)
#   04_tab_no_direct_effect.R -- Assumption 3(ii) KS test results
#   05_tab_relevance.R        -- RSV relevance
#   06_tab_rsv.R              -- RSV vs benchmark estimates
#   07_tab_estimators_jtest.R -- RSV vs naive weights and J-test
# =============================================================================

source("code/smartcards/06_tables/01_tab_summary_stats.R")
source("code/smartcards/06_tables/02_tab_stability_S.R")
source("code/smartcards/06_tables/03_tab_stability_Stilde.R")
source("code/smartcards/06_tables/04_tab_no_direct_effect.R")
source("code/smartcards/06_tables/05_tab_relevance.R")
source("code/smartcards/06_tables/06_tab_rsv.R")
source("code/smartcards/06_tables/07_tab_estimators_jtest.R")
