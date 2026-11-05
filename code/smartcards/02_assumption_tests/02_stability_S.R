suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(tidyr)
})

# ==============================================================================
# Assumption 2(i) -- Stability: KS test (S definition)
#
# Tests R ⊥ S | D=0, Y=y using the non-overlapping sample definition S:
#   S=e: Treated Mandals (2010), Untreated Mandals (2012), Buffer Mandals (2011)
#   S=o: Non-Study Mandals (observational)
#
# Compares the conditional distribution of R_PC1 between the experimental and
# observational samples among untreated units (D=0), separately for each
# outcome and Y level:
#
#   H0: F(R_PC1 | S=e, D=0, Y=y) = F(R_PC1 | S=o, D=0, Y=y)
#
# The KS distance is unchanged; ks_pval now uses a mandal-level multiplier
# bootstrap (B=2000). Inference is asymptotic and requires independent
# mandals, no dominating cluster, and adequate support. Failure to reject
# does not establish stability. This diagnostic concerns PC1, not all R.
#
# See 03_stability_Stilde.R for the analogous test using the overlap design
# (Stilde), where Buffer Mandals (2011) appear in both Stilde=e and Stilde=o.
#
# Output: data/clean/smartcards/assumption_stability_S.csv
# ==============================================================================

source("code/utils/stability_clustered.R")

# ------------------------------------------------------------------------------
# Run KS tests
# ------------------------------------------------------------------------------
Y_vars   <- c("Ycons", "Ylowinc", "Ymidinc")
B        <- 2000L
seed     <- 42L

cluster_results <- run_stability_clustered(
  Y_vars = Y_vars, sample_definitions = "S", B = B, seed = seed
)
# Preserve the original six-column schema for downstream table scripts.
# Full cluster counts, missingness and statuses remain in cluster_results.
ks_results <- cluster_results[c("outcome", "Y", "ks_stat", "cluster_pval", "n_exp", "n_obs")]
names(ks_results)[names(ks_results) == "cluster_pval"] <- "ks_pval"
print(ks_results)

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
out_csv <- "data/clean/smartcards/assumption_stability_S.csv"
write.csv(ks_results, out_csv, row.names = FALSE)
cat("\nSaved:", out_csv, "\n")
