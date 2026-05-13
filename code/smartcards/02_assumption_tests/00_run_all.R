# =============================================================================
# Smartcard -- Assumption Tests: Run All
#
# Runs KS tests that check the two key assumptions underlying RSV:
#   01 -- No direct effect (Assumption 3(ii)): D ⊥ R | Stilde=e, Y.
#         Compares F(R_PC1 | Stilde=e, D=0, Y=y) vs F(R_PC1 | Stilde=e, D=1, Y=y).
#   02 -- Stability, S definition (Assumption 2(i)): R ⊥ S | D=0, Y.
#         Compares F(R_PC1 | S=e, D=0, Y=y) vs F(R_PC1 | S=o, D=0, Y=y).
#         Buffer Mandals (2011) appear only in S=e.
#   03 -- Stability, Stilde definition (Assumption 2(i)): R ⊥ Stilde | D=0, Y.
#         Compares F(R_PC1 | Stilde=e, D=0, Y=y) vs F(R_PC1 | Stilde=o, D=0, Y=y).
#         Buffer Mandals (2011) appear in both Stilde=e and Stilde=o.
#
# Requires: data/clean/smartcards/data.csv
# Outputs:
#   data/clean/smartcards/assumption_no_direct_effect.csv
#   data/clean/smartcards/assumption_stability_S.csv
#   data/clean/smartcards/assumption_stability_Stilde.csv
# =============================================================================

source("code/smartcards/02_assumption_tests/01_no_direct_effect.R")
source("code/smartcards/02_assumption_tests/02_stability_S.R")
source("code/smartcards/02_assumption_tests/03_stability_Stilde.R")
