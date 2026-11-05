# Stability density figures for S, conditional on Y and binary secondary Y.
# Requires a column name; reads existing test results only.
# Run: Rscript code/smartcards/05_figures/09_stability_S_secondary.R \
#   ent_own_share --percentile 25 --direction above
# Without --percentile or --threshold, the column must already be binary.
# Output: figures/smartcards/stability_S_secondary/SPEC/OUTCOME/

source("code/utils/stability_secondary_figures.R")

plot_stability_S_secondary <- function(...) {
  plot_stability_binary_secondary(..., sample_definition = "S")
}

if (sys.nframe() == 0L) {
  run_stability_secondary_figures_cli(plot_stability_S_secondary)
}
