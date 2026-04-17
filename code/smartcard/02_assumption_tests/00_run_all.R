# =============================================================================
# Smartcard — Assumption Tests: Run All
#
# Runs KS tests that check the two key assumptions underlying RSV:
#   01 — No direct effect (Assumption 3(ii)): R | Y has the same distribution
#        for D=0 and D=1 within the experimental sample, i.e. D ⊥ R | S=e, Y.
#   02 — Stability (Assumption 2(i)): R | D, Y has the same distribution
#        across experimental and observational samples, i.e. S ⊥ R | D=0, Y.
#
# Requires: data/clean/smartcard/smartcard_data.csv
# =============================================================================

source("code/smartcard/02_assumption_tests/01_no_direct_effect.R")
source("code/smartcard/02_assumption_tests/02_stability.R")
