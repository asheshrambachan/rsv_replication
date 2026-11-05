# Stability table for Stilde, conditional on Y and binary secondary outcome.
# Requires a column; reads saved clustered tests without rerunning them.
# Run from the project root:
# Rscript code/smartcards/06_tables/09_tab_stability_Stilde_secondary.R \
#   ent_own_share --threshold 0 --direction above
# Without --threshold or --percentile, the column must already be binary.
# Output: tables/smartcards/tab_stability_Stilde_secondary_SPEC.tex

source("code/utils/stability_secondary_tables.R")

tab_stability_Stilde_secondary <- function(...) {
  tab_stability_binary_secondary(..., sample_definition = "Stilde")
}

if (sys.nframe() == 0L) {
  run_stability_secondary_tables_cli("Stilde")
}
