suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(tidyr)
})

# ==============================================================================
# Assumption 2(i) — Stability: KS test
#
# Tests S ⊥ R | D=0, Y=y by comparing the conditional distribution of the
# remote sensing predictor R between the experimental and observational samples
# for untreated units (D=0), separately for each outcome and level of Y:
#
#   H0: F(R_PC1 | S=e, D=0, Y=y) = F(R_PC1 | S=o, D=0, Y=y)
#
# Failure to reject supports the stability assumption: the sensing mechanism
# f(R | X, D, Y) is the same across the experimental and observational samples,
# so the R–Y relationship learned in one sample can be transported to the other.
# ==============================================================================

## Load required columns from the cleaned dataset
data <- read_csv("data/clean/smartcards/data.csv",
                 col_select = c(wave, D, R_PC1_scaled, Ycons, Ylowinc, Ymidinc)) %>%
  mutate(
    S = case_when(
      wave == "Experimental: Treated (2010)"   ~ "e",
      wave == "Experimental: Untreated (2011)" ~ "e",
      wave == "Experimental: Untreated (2012)" ~ "e",
      wave == "Observational (N/A)"            ~ "o"
    )
  ) %>%
  separate_rows(S, sep = ",") %>%
  select(D, S, R_PC1_scaled, Ycons, Ylowinc, Ymidinc)

## Filter to D = 0
data_d0 <- data %>% filter(D == 0)

# ------------------------------------------------------------------------------
# Run KS tests
# ------------------------------------------------------------------------------
Y_vars   <- c("Ycons", "Ylowinc", "Ymidinc")
y_levels <- c(0, 1)

ks_list <- list()
for (Y_var in Y_vars) {
  for (y_val in y_levels) {
    r_exp <- data_d0 %>% filter(S %in% c("both", "e"), !!sym(Y_var) == y_val) %>% pull(R_PC1_scaled)
    r_obs <- data_d0 %>% filter(S %in% c("both", "o"), !!sym(Y_var) == y_val) %>% pull(R_PC1_scaled)
    
    ks <- ks.test(r_exp, r_obs)

    ks_list[[paste0(Y_var, "_", y_val)]] <- data.frame(
      outcome = Y_var,
      Y       = y_val,
      ks_stat = unname(ks$statistic),
      ks_pval = ks$p.value,
      n_exp   = length(r_exp),
      n_obs   = length(r_obs)
    )
  }
}

ks_results <- do.call(rbind, ks_list)
print(ks_results)

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
out_csv <- "data/clean/smartcards/assumption_stability.csv"
write.csv(ks_results, out_csv, row.names = FALSE)
cat("\nSaved:", out_csv, "\n")
