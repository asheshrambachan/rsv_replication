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
# This is implemented via separate_rows() on the "e,o" label assigned to the
# Experimental: Untreated (2011) wave.
#
# Compares the conditional distribution of R_PC1 between the experimental and
# observational samples among untreated units (D=0), separately for each
# outcome and Y level:
#
#   H0: F(R_PC1 | Stilde=e, D=0, Y=y) = F(R_PC1 | Stilde=o, D=0, Y=y)
#
# Failure to reject supports stability: the sensing mechanism f(R | D, Y) is
# the same across the experimental and observational samples, so the R-Y
# relationship learned in one sample can be transported to the other.
#
# See 02_stability_S.R for the analogous test using the non-overlapping
# definition S, where Buffer Mandals (2011) appear only in S=e.
#
# Output: data/clean/smartcards/assumption_stability_Stilde.csv
# ==============================================================================

## Load required columns from the cleaned dataset
data <- read_csv("data/clean/smartcards/data.csv",
                 col_select = c(wave, D, R_PC1_scaled, Ycons, Ylowinc, Ymidinc)) %>%
  mutate(
    S = case_when(
      wave == "Experimental: Treated (2010)"   ~ "e",
      wave == "Experimental: Untreated (2011)" ~ "e,o",
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
    r_exp <- data_d0 %>% filter(S == "e", !!sym(Y_var) == y_val) %>% pull(R_PC1_scaled)
    r_obs <- data_d0 %>% filter(S == "o", !!sym(Y_var) == y_val) %>% pull(R_PC1_scaled)
    
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
out_csv <- "data/clean/smartcards/assumption_stability_Stilde.csv"
write.csv(ks_results, out_csv, row.names = FALSE)
cat("\nSaved:", out_csv, "\n")
