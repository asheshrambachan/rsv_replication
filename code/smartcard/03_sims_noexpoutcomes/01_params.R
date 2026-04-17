library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
source("code/utils/compute_p1_kl_mean.R")

# =============================================================================
# Simulation Parameters — No Experimental Outcomes
#
# Builds the parameter grid for the noexpoutcomes simulations. For each
# outcome, the script:
#   1. Computes empirical outcome distributions in the experimental (alpha_e)
#      and observational (alpha_o) samples
#   2. Sets p0 = alpha_e (untreated potential outcome distribution)
#   3. Tilts p0 via KL minimization to obtain p1, the treated potential outcome
#      distribution implied by treatment effect tau
#   4. Crosses all scalar grid dimensions (tau, n_e, n_o, model, etc.)
#
# Output: data/interim/smartcard/binary_noexpoutcomes/params.Rds
# =============================================================================

# -----------------------------
# User-defined grid dimensions
# -----------------------------
OUTCOMES  <- c("Ycons", "Ylowinc", "Ymidinc")

TAU_GRID       <- seq(0, 0.2, 0.05)
N_E_GRID       <- c(3000)
N_O_GRID       <- c(500, 1000, 1500)
MODEL_GRID     <- "rf"
NUM_TREES_GRID <- 100L
SE_METHOD_GRID <- "score_bootstrap"
SEED_GRID      <- 42L
B_GRID         <- 500L
B_SE_GRID      <- 100L

out_dir <- "data/interim/smartcard/binary_noexpoutcomes"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# -----------------------------
# Build grids
# -----------------------------
# S_e uses untreated experimental units (2011/2012) to identify the baseline
# outcome distribution; treated (2010) units are excluded since their Y
# reflects the treatment and would inflate alpha_e.
dt <- fread("data/clean/smartcard/smartcard_data.csv") %>%
  mutate(
    S_e = as.numeric(wave %in% c("Experimental: Untreated (2011)", "Experimental: Untreated (2012)")),
    S_o = as.numeric(wave == "Observational (N/A)")
  )

grid_by_outcome <- list()

for (outcome in OUTCOMES) {
  # Outcome support (levels)
  dt$Y <- dt[[outcome]]
  y_levels <- sort(unique(na.omit(dt$Y)))
  K <- length(y_levels)
  
  # ------------------------------------------------------------------------
  # 1) Empirical outcome distributions within each sample definition
  # ------------------------------------------------------------------------
  # Creates a long table of fractions:
  #   sample_flag | outcome_level | n | frac
  frac_tbl <- dt %>%
    select(Y, S_e, S_o) %>%
    pivot_longer(
      cols      = c("S_e", "S_o"),
      names_to  = "sample_flag",
      values_to = "in_sample"
    ) %>%
    filter(in_sample == 1) %>%
    count(sample_flag, Y, name = "n") %>%
    complete(
      sample_flag,
      Y := y_levels,
      fill = list(n = 0)
    ) %>%
    group_by(sample_flag) %>%
    mutate(frac = n / sum(n)) %>%
    ungroup()
  
  # ------------------------------------------------------------------------
  # 2) alpha_e (experimental baseline distribution)
  # ------------------------------------------------------------------------
  alpha_e_vec <- frac_tbl %>%
    filter(sample_flag == "S_e") %>%
    arrange(match(.data$Y, y_levels)) %>%
    pull(frac)
  
  assert_prob_vector(alpha_e_vec, K)
  
  # ------------------------------------------------------------------------
  # 3) alpha_o  (observational baseline distributions)
  # ------------------------------------------------------------------------
  alpha_o_vec <- frac_tbl %>%
    filter(sample_flag == "S_o") %>%
    arrange(match(.data$Y, y_levels)) %>%
    pull(frac)
  
  assert_prob_vector(alpha_o_vec, K)
  
  # ------------------------------------------------------------------------
  # 4) Scalar grid (tau, n_e, n_o) with list-cols for vectors + level mapping
  # ------------------------------------------------------------------------
  param_grid <- tidyr::crossing(
    outcome   = outcome,
    n_e       = N_E_GRID,
    n_o       = N_O_GRID,
    tau       = TAU_GRID,
    model     = MODEL_GRID,
    num_trees = NUM_TREES_GRID,
    se_method = SE_METHOD_GRID,
    seed      = SEED_GRID,
    B         = B_GRID,
    B_se      = B_SE_GRID
  ) %>%
    mutate(
      y_levels = list(y_levels),    # mapping vector positions -> outcome levels
      alpha_e  = list(alpha_e_vec),  # experimental alpha (list-col)
      alpha_o  = list(alpha_o_vec)
    )

  param_grid$p0 <- param_grid$alpha_e
  param_grid$p1 <- purrr::pmap(
    list(param_grid$alpha_e, param_grid$y_levels, param_grid$tau),
    ~ compute_p1_kl_mean(p0 = ..1, y = ..2, tau = ..3)
  )
  
  grid_by_outcome[[outcome]] <- param_grid
}


params <- bind_rows(grid_by_outcome)

# -----------------------------
# Save 
# -----------------------------
output_path <- file.path(out_dir, "params.Rds")
saveRDS(params, output_path)
cat("Saved parameter grid to: ", output_path, "\n", sep = "")

cat("\n=== Parameter Grid Summary ===\n")
cat("Total parameter combinations: ", nrow(params), "\n", sep = "")
print(params)
