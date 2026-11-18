# Four-cell probabilities: (Y, secondary) = (0,0), (0,1), (1,0), (1,1).
source("code/smartcards/03_sims_noexpoutcomes_secondary/settings.R")
source("code/smartcards/03_sims_noexpoutcomes_secondary/simulation_utils.R")
source("code/utils/compute_p1_kl_mean.R")

cfg <- secondary_sim_settings()
header <- names(data.table::fread(cfg$input_csv, nrows = 0L))
needed <- unique(c("wave", cfg$outcomes, cfg$secondary_column))
if (any(!needed %in% header)) stop("Missing input columns: ", paste(setdiff(needed, header), collapse = ", "))
dt <- data.table::fread(cfg$input_csv, select = needed)
binary <- secondary_sim_binary(dt[[cfg$secondary_column]], cfg)
waves <- c("Experimental: Treated (2010)", "Experimental: Untreated (2011)",
           "Experimental: Untreated (2012)", "Observational (N/A)")
if (anyNA(dt$wave) || any(!dt$wave %in% waves)) stop("Unexpected wave labels.")
experimental <- dt$wave %in% waves[2:3]
observational <- dt$wave == waves[4]
cells <- secondary_sim_cells()
info <- file.info(cfg$input_csv)
identity <- list(path = normalizePath(cfg$input_csv), size = info$size, mtime = as.numeric(info$mtime))
grid <- diagnostics <- vector("list", length(cfg$outcomes))

for (j in seq_along(cfg$outcomes)) {
  outcome <- cfg$outcomes[j]
  baseline <- secondary_sim_probabilities(dt[[outcome]], binary$values, experimental)
  obs <- secondary_sim_probabilities(dt[[outcome]], binary$values, observational)
  # Validate imagery support before starting expensive simulations.
  invisible(secondary_sim_R_lookup(cfg, outcome, binary$cutoff, binary$rule))
  row <- expand.grid(n_e = cfg$n_e_grid, n_o = cfg$n_o_grid, tau = cfg$tau_grid,
                     KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  row$outcome <- outcome
  row$secondary_column <- cfg$secondary_column
  row$secondary_cutoff <- binary$cutoff
  row$secondary_rule <- binary$rule
  row$model <- cfg$model; row$num_trees <- cfg$num_trees
  row$seed <- cfg$seed; row$B <- cfg$B; row$B_se <- cfg$B_se
  row$nfolds <- cfg$nfolds; row$level <- cfg$level
  row$sigma2.method <- cfg$sigma2.method; row$sigma2.quantile <- cfg$sigma2.quantile
  row$se_method <- "score_bootstrap"
  row$p0 <- rep(list(baseline$probabilities), nrow(row))
  row$alpha_o <- rep(list(obs$probabilities), nrow(row))
  # Tilt only by primary Y: this changes its mean by tau while preserving
  # P(secondary | Y) in the experimental population. No extra direct pathway
  # through secondary beyond Y is introduced by this initial treatment tilt.
  row$p1 <- lapply(row$tau, function(tau) compute_p1_kl_mean(
    baseline$probabilities, cells$Y, tau))
  row$input_identity <- rep(list(identity), nrow(row))
  row$rf_options <- rep(list(cfg$rf_options), nrow(row))
  row$r_columns <- rep(list(cfg$r_col_fn(header)), nrow(row))
  row$n_baseline_e <- baseline$n; row$n_baseline_o <- obs$n
  row$n_missing_e <- baseline$n_missing; row$n_missing_o <- obs$n_missing
  grid[[j]] <- row
  diagnostics[[j]] <- data.frame(outcome = outcome, secondary_column = cfg$secondary_column,
    secondary_cutoff = binary$cutoff, cells,
    experimental_probability = baseline$probabilities,
    observational_probability = obs$probabilities)
}
params <- do.call(rbind, grid)
rownames(params) <- NULL
dir.create(cfg$interim_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(params, file.path(cfg$interim_dir, "params.Rds"))
utils::write.csv(do.call(rbind, diagnostics),
  file.path(cfg$interim_dir, "joint_probabilities.csv"), row.names = FALSE)
cat("Saved", nrow(params), "joint-outcome configurations to", cfg$interim_dir, "\n")
