suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(tidyr)
})

# ==============================================================================
# Assumption 3(ii) — No Direct Effect: KS test
#
# Tests D ⊥ R | S=e, Y=y by comparing the conditional distribution of the
# remote sensing predictor R between treated and untreated units within the
# experimental sample, separately for each outcome and level of Y:
#
#   H0: F(R_PC1 | S=e, D=0, Y=y) = F(R_PC1 | S=e, D=1, Y=y)
#
# Failure to reject supports the no-direct-effect assumption: treatment
# affects Y but not R (the satellite signal), so R can serve as a valid
# proxy for Y in the observational sample.
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

## Filter to experimental sample
data_exp <- data %>% filter(S %in% c("both", "e"))

# ------------------------------------------------------------------------------
# Run KS tests
# ------------------------------------------------------------------------------
Y_vars   <- c("Ycons", "Ylowinc", "Ymidinc")
y_levels <- c(0, 1)

ks_list <- list()
for (Y_var in Y_vars) {
  for (y_val in y_levels) {
    r_d1 <- data_exp %>% filter(D == 1, !!sym(Y_var) == y_val) %>% pull(R_PC1_scaled)
    r_d0 <- data_exp %>% filter(D == 0, !!sym(Y_var) == y_val) %>% pull(R_PC1_scaled)

    ks <- ks.test(r_d1, r_d0)

    ks_list[[paste0(Y_var, "_", y_val)]] <- data.frame(
      outcome = Y_var,
      Y       = y_val,
      ks_stat = unname(ks$statistic),
      ks_pval = ks$p.value,
      n_d1    = length(r_d1),
      n_d0    = length(r_d0)
    )
  }
}

ks_results <- do.call(rbind, ks_list)
print(ks_results)

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
out_csv <- "data/clean/smartcards/assumption_no_direct_effect.csv"
write.csv(ks_results, out_csv, row.names = FALSE)
cat("\nSaved:", out_csv, "\n")
