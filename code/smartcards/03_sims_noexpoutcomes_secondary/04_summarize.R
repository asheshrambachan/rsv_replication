# Coverage among available CIs AND among all attempted replications, with
# explicit point/bootstrap failure counts. No failed replications are hidden.
source("code/smartcards/03_sims_noexpoutcomes_secondary/settings.R")
cfg <- secondary_sim_settings()
params <- readRDS(file.path(cfg$interim_dir, "params.Rds"))
records <- list()
for (method in c("calibrate", "rsv")) for (i in seq_len(nrow(params))) {
  row <- params[i, , drop = FALSE]
  name <- gsub("[^[:alnum:]_-]", "_", paste(row$secondary_column, row$outcome, sep = "_"))
  path <- file.path(cfg$interim_dir, method, sprintf("%s_config%03d.Rds", name, i))
  if (!file.exists(path)) stop("Missing simulation checkpoint: ", path)
  checkpoint <- readRDS(path)
  if (!identical(checkpoint$configuration, as.list(row))) stop("Configuration mismatch: ", path)
  records[[length(records) + 1L]] <- checkpoint$results
}
results <- data.table::rbindlist(records)
by <- c("outcome", "secondary_column", "secondary_cutoff", "secondary_rule",
        "tau", "n_e", "n_o", "sigma2.method", "level", "B_se", "nfolds", "estimator", "stage")
summary <- results[, {
  target_tau <- .BY$tau
  point_ok <- status == "ok" & is.finite(tauhat)
  ci_ok <- point_ok & is.finite(se) & is.finite(ci_lower) & is.finite(ci_upper)
  covered <- ci_ok & target_tau >= ci_lower & target_tau <= ci_upper
  covered[is.na(covered)] <- FALSE
  list(avg_bias = if (any(point_ok)) mean(tauhat[point_ok] - target_tau) else NA_real_,
    sd_est = if (sum(point_ok) > 1L) stats::sd(tauhat[point_ok]) else NA_real_,
    avg_se = if (any(ci_ok)) mean(se[ci_ok]) else NA_real_,
    rmse = if (any(point_ok)) sqrt(mean((tauhat[point_ok] - target_tau)^2)) else NA_real_,
    coverage = if (any(ci_ok)) mean(covered[ci_ok]) else NA_real_,
    coverage_all_attempts = mean(covered),
    n_sims = .N, n_point_success = sum(point_ok), n_inference_success = sum(ci_ok),
    n_point_failed = sum(!point_ok), n_ci_failed = sum(!ci_ok),
    n_bootstrap_failed_draws = sum(failed_draws, na.rm = TRUE))
}, by = by]
dir.create(dirname(cfg$summary_csv), recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(summary, cfg$summary_csv)
cat("Saved:", cfg$summary_csv, "\n")
