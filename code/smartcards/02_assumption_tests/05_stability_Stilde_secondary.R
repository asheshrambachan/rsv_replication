# Stability conditional on primary Y and binary secondary outcome (widetilde Y).
# Stilde: buffer villages contribute to both sample roles, conditional on D=0.
# Run from the project root:
# Rscript code/smartcards/02_assumption_tests/05_stability_Stilde_secondary.R \
#   house_type1 --percentile 25 --direction above
# Supports binary inputs, --threshold, --percentile and --direction.
# Mandal multiplier bootstrap: B=2000; cutoff and sample masks are unchanged.
# --seed is the base seed; this script adds 12 to preserve the combined run.
# Output: data/clean/smartcards/assumption_stability_Stilde_secondary_SPEC.csv
# Sourcing only defines functions; direct execution runs this design.

source("code/utils/stability_binary_secondary.R")

if (sys.nframe() == 0L) {
  run_stability_binary_secondary_cli("Stilde")
}
