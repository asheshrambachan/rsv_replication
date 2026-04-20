# =============================================================================
# Forest — Assumption Tests: Run All
#
# Runs the KS test that checks the key assumption underlying RSV:
#   01 — Stability (Assumption 2(i)): R | Y has the same distribution across
#        the experimental and observational samples for each distance band,
#        i.e. F(R | S_e=1, Y=y) = F(R | S_o=1, Y=y).
#
# Requires: data/clean/ugandaforestcover/data_Ybin.csv
# =============================================================================

source("code/ugandaforestcover/02_assumption_tests/01_stability.R")
