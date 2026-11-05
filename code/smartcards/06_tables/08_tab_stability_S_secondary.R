# Stability table for S, conditional on Y and binary secondary outcome.
# Requires a column; reads saved clustered tests without rerunning them.
# Run from the project root:
# Rscript code/smartcards/06_tables/08_tab_stability_S_secondary.R \
#   ent_own_share --threshold 0 --direction above
# Without --threshold or --percentile, the column must already be binary.
# Output: tables/smartcards/tab_stability_S_secondary_SPEC.tex

source("code/utils/stability_secondary_tables.R")

tab_stability_S_secondary <- function(...) {
  tab_stability_binary_secondary(..., sample_definition = "S")
}

if (sys.nframe() == 0L) {
  run_stability_secondary_tables_cli("S")
}
