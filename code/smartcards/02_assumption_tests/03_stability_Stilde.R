suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(tidyr)
})

# ==============================================================================
# Assumption 2(i) -- Stability: KS test (Stilde definition)
#
# Tests R ⊥ Stilde | D=0, Y=y using the overlap sample definition Stilde:
#   Stilde=e: Treated Mandals (2010), Untreated Mandals (2012),
#             Buffer Mandals (2011)
#   Stilde=o: Buffer Mandals (2011), Non-Study Mandals (observational)
#
# Buffer Mandals (2011) appear in both Stilde=e and Stilde=o (overlap units).
# Each village remains one record with two sample-role indicators. Both
# contributions use the same mandal multiplier in each bootstrap draw.
#
# Compares the conditional distribution of R_PC1 between the experimental and
# observational samples among untreated units (D=0), separately for each
# outcome and Y level:
#
#   H0: F(R_PC1 | Stilde=e, D=0, Y=y) = F(R_PC1 | Stilde=o, D=0, Y=y)
#
# The KS distance is unchanged; ks_pval now uses a mandal-level multiplier
# bootstrap (B=2000), accounting for clustering and sample overlap.
# Inference is asymptotic and requires independent mandals, no dominating
# cluster, and adequate support. Failure to reject does not establish
# stability. This diagnostic concerns PC1, not all R.
#
# See 02_stability_S.R for the analogous test using the non-overlapping
# definition S, where Buffer Mandals (2011) appear only in S=e.
#
# Output: data/clean/smartcards/assumption_stability_Stilde.csv
# ==============================================================================

source("code/utils/stability_clustered.R")

# ------------------------------------------------------------------------------
# Run KS tests
# ------------------------------------------------------------------------------
Y_vars   <- c("Ycons", "Ylowinc", "Ymidinc")
B        <- 2000L
# Continue after S's six tests to preserve the previous combined-run seeds.
seed     <- 42L + 2L * length(Y_vars)

cluster_results <- run_stability_clustered(
  Y_vars = Y_vars, sample_definitions = "Stilde", B = B, seed = seed
)
# Preserve the original six-column schema for downstream table scripts.
# Full cluster counts, missingness and statuses remain in cluster_results.
ks_results <- cluster_results[c("outcome", "Y", "ks_stat", "cluster_pval", "n_exp", "n_obs")]
names(ks_results)[names(ks_results) == "cluster_pval"] <- "ks_pval"
print(ks_results)

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
out_csv <- "data/clean/smartcards/assumption_stability_Stilde.csv"
write.csv(ks_results, out_csv, row.names = FALSE)
cat("\nSaved:", out_csv, "\n")
