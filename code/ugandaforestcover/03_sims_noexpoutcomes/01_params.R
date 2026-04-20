suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(purrr)
})
source("code/utils/compute_p1_kl_mean.R")

# -----------------------------
# User-defined grid dimensions
# -----------------------------
outcome <- "Ybin"

TAU_GRID <- seq(0, 0.2, 0.05)
N_E_GRID <- c(1000)
N_O_GRID <- c(500, 1000, 2000)
MODEL_GRID     <- "logit"
SE_METHOD_GRID <- "score_bootstrap"
SEED_GRID      <- 42L
B_GRID         <- 500L
B_SE_GRID      <- 100L

out_dir <- file.path("data/interim/ugandaforestcover", paste0("sims_noexpoutcomes_", outcome))
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# -----------------------------
# Sample definitions (derived)
# -----------------------------
EXPERIMENTAL_FLAG <- "S_e"
OBSERVATIONAL_FLAGS <- c("S_o_02km", "S_o_05km", "S_o_10km")
ALL_SAMPLE_FLAGS <- c(EXPERIMENTAL_FLAG, OBSERVATIONAL_FLAGS)

# -----------------------------
# Build grid
# -----------------------------

sim_path <- "data/interim/ugandaforestcover/data_sim.csv"
stopifnot(file.exists(sim_path))

s_cols <- c("S_e", "S_o_02km", "S_o_05km", "S_o_10km")
DT <- fread(sim_path, select = c("lon", "lat", outcome, s_cols))

setnames(DT, outcome, "Y")

# Outcome support (levels)
y_levels <- sort(unique(na.omit(DT$Y)))
K <- length(y_levels)

# ------------------------------------------------------------------------
# 1) Empirical outcome distributions within each sample definition
# ------------------------------------------------------------------------
frac_tbl <- DT %>%
  select(Y, all_of(ALL_SAMPLE_FLAGS)) %>%
  pivot_longer(
    cols      = all_of(ALL_SAMPLE_FLAGS),
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
  filter(sample_flag == EXPERIMENTAL_FLAG) %>%
  arrange(match(.data$Y, y_levels)) %>%
  pull(frac)

assert_prob_vector(alpha_e_vec, K)

# ------------------------------------------------------------------------
# 3) alpha_o options (observational baseline distributions)
# ------------------------------------------------------------------------
alpha_o_tbl <- frac_tbl %>%
  filter(sample_flag %in% OBSERVATIONAL_FLAGS) %>%
  group_by(sample_flag) %>%
  arrange(match(.data$Y, y_levels), .by_group = TRUE) %>%
  summarise(alpha_o = list(frac), .groups = "drop") %>%
  mutate(alpha_o_id = sub("^S_o_", "", sample_flag)) %>%
  select(-sample_flag)

purrr::walk(alpha_o_tbl$alpha_o, assert_prob_vector, K = K)

# ------------------------------------------------------------------------
# 4) Scalar grid (tau, n_e, n_o) with list-cols for vectors + level mapping
# ------------------------------------------------------------------------
scalar_grid <- tidyr::crossing(
  outcome = outcome,
  n_e     = N_E_GRID,
  n_o     = N_O_GRID,
  tau     = TAU_GRID,
  model     = MODEL_GRID,
  se_method = SE_METHOD_GRID,
  seed      = SEED_GRID,
  B         = B_GRID,
  B_se      = B_SE_GRID
) %>%
  mutate(
    y_levels = list(y_levels),
    alpha_e  = list(alpha_e_vec)
  )

# ------------------------------------------------------------------------
# 5) Combine scalar grid with observational alpha choices (cartesian join)
# ------------------------------------------------------------------------
params <- tidyr::crossing(scalar_grid, alpha_o_tbl)

params$p0 <- params$alpha_e
params$p1 <- purrr::pmap(
  list(params$alpha_e, params$y_levels, params$tau),
  ~ compute_p1_kl_mean(p0 = ..1, y = ..2, tau = ..3)
)

# -----------------------------
# Save
# -----------------------------
output_path <- file.path(out_dir, "params.Rds")
saveRDS(params, output_path)
cat("Saved parameter grid to: ", output_path, "\n", sep = "")

cat("\n=== Parameter Grid Summary ===\n")
cat("Total parameter combinations: ", nrow(params), "\n", sep = "")
print(params)
