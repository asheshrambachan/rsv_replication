# =============================================================================
# Forest — Assumption: Stability (KS Test)
#
# Tests the stability assumption by comparing the conditional distribution of
# the predicted score R = E[Y|X] between the experimental sample (inside the
# PES imagery boundary) and each observational buffer band (just outside):
#
#   H0: F(R | S_e=1, Y=y) = F(R | S_o=1, Y=y)
#
# Failure to reject supports stability: the sensing mechanism f(R | X, Y) is
# the same across the two samples, so the R–Y relationship learned on the
# experimental sample can be transported to the observational sample.
#
# The test is run separately for each Y class and each distance band
# (S_o_02km, S_o_05km, S_o_10km), following the design in Jayachandran et al.
#
# Input:
#   data/clean/ugandaforestcover/data_Ybin.csv
#
# Output:
#   data/clean/ugandaforestcover/assumption_stability.csv
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
})

OUTCOME <- "Ybin"

in_path <- file.path("data/clean/ugandaforestcover", paste0("data_", OUTCOME, ".csv"))
DT <- fread(in_path)

# -----------------------------------------------------------------------------
# 1. Construct Scalar Predicted Score
#
# R = E[Y|X] = sum_k P(Y=k|X) * k, computed from the class probability
# columns (R0, R1, ...) produced by 01_data_construction/07_rf_predict.R.
# -----------------------------------------------------------------------------

y_levels <- sort(unique(na.omit(DT$Y)))
r_cols   <- paste0("R", y_levels)
P        <- as.matrix(DT[, ..r_cols])
DT[, R  := as.numeric(P %*% as.numeric(y_levels))]

# -----------------------------------------------------------------------------
# 2. Run KS Tests
#
# For each observational distance band and each Y class, compare the
# distribution of R in the experimental sample (S_e=1) against the
# observational sample (S_o_*=1).
# -----------------------------------------------------------------------------

So_cols <- c("S_o_02km", "S_o_05km", "S_o_10km")

ks_list <- list()
for (So_col in So_cols) {
  so_label <- paste0("0 - ", sub("^S_o_0?", "", So_col))   # e.g. "0 - 2km"
  for (y_val in y_levels) {
    r_exp <- DT[S_e == 1          & Y == y_val, R]
    r_obs <- DT[get(So_col) == 1  & Y == y_val, R]

    ks <- ks.test(r_exp, r_obs)

    ks_list[[paste0(So_col, "_", y_val)]] <- data.table(
      S_o     = so_label,
      Y       = y_val,
      ks_stat = unname(ks$statistic),
      ks_pval = ks$p.value,
      n_exp   = length(r_exp),
      n_obs   = length(r_obs)
    )
  }
}

ks_results <- rbindlist(ks_list)
print(ks_results)

# -----------------------------------------------------------------------------
# 3. Save
# -----------------------------------------------------------------------------

out_csv <- "data/clean/ugandaforestcover/assumption_stability.csv"
write.csv(ks_results, out_csv, row.names = FALSE)
cat("\nSaved:", out_csv, "\n")
