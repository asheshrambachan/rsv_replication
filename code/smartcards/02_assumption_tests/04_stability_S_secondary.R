# Stability conditional on primary Y and binary secondary outcome (widetilde Y).
# S: study villages versus non-study villages, conditional on D=0.
# Run from the project root:
# Rscript code/smartcards/02_assumption_tests/04_stability_S_secondary.R \
#   house_type1 --percentile 25 --direction above
# Supports binary inputs, --threshold, --percentile and --direction.
# Mandal multiplier bootstrap: B=2000; cutoff and sample masks are unchanged.
# Output: data/clean/smartcards/assumption_stability_S_secondary_SPEC.csv
# Sourcing only defines functions; direct execution runs this design.

source("code/utils/stability_binary_secondary.R")

if (sys.nframe() == 0L) {
  run_stability_binary_secondary_cli("S")
}
