# =============================================================================
# Run All — Tables
#
# Produces all smartcard tables in order:
#   01_tab_summary_stats.R          — summary statistics
#   02_tab_stability.R              — Assumption 2(i) KS test results
#   03_tab_no_direct_effect.R       — Assumption 3(ii) KS test results
#   04_tab_relevance.R              — RSV relevance
#   05_tab_rsv.R                    — RSV vs benchmark estimates
#   06_tab_estimators_jtest.R — RSV vs naive weights and J-test
# =============================================================================

source("code/smartcards/06_tables/01_tab_summary_stats.R")
source("code/smartcards/06_tables/02_tab_stability.R")
source("code/smartcards/06_tables/03_tab_no_direct_effect.R")
source("code/smartcards/06_tables/04_tab_relevance.R")
source("code/smartcards/06_tables/05_tab_rsv.R")
source("code/smartcards/06_tables/06_tab_estimators_jtest.R")
