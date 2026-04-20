library(data.table)
library(ranger)
library(parallel)
library(nnet)
library(dplyr)
library(tidyr)
library(tibble)
source("code/utils/sim_utils.R")
source("code/utils/summarize_sim_results.R")

# Number of parallel cores
n_cores <- min(100, parallel::detectCores() - 1)
cat("=== Calibration Simulations ===\n")
cat("Using", n_cores, "cores for parallel processing\n\n")

# Create output directory
outcome   <- "Ybin"
estimator <- "calibrate"
out_dir   <- file.path("data/interim/ugandaforestcover", paste0("sims_noexpoutcomes_", outcome), estimator)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# =============================================================================
# Estimator
# =============================================================================
# Source RSV helper functions
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

compute_calibrate_estimate <- function(Y, R, D, S_e, S_o, y_levels, spec,
                                       seed = NULL, num.threads = 1) {
  obs_idx <- (S_o == 1)
  exp_idx <- (S_e == 1)
  
  fit_Y <- fit_one_model_multicat(
    XR_train = R[obs_idx, , drop = FALSE],
    y        = Y[obs_idx],
    XR_test  = R[exp_idx, , drop = FALSE],
    y_levels = y_levels,
    spec     = spec,
    seed     = seed,
    num.threads = num.threads
  )
  
  # Calibrated conditional mean: m_o(R_e) = sum_k y_k * P(Y=k | R)
  Y_e_hat <- as.numeric(as.matrix(fit_Y$predictions) %*% y_levels)
  D_e     <- D[exp_idx]
  tauhat  <- unname(lm.fit(x = cbind(1, D_e), y = Y_e_hat)$coefficients[2])
  
  result <- list(
    coefficients = structure(tauhat, names = "D"),
    Y_e_hat  = Y_e_hat,
    D_e      = D_e,
    # stored for add_se(method = "bootstrap")
    Y = Y, R = R, D = D, S_e = S_e, S_o = S_o,
    spec = spec, y_levels = y_levels
  )
  class(result) <- "calibrate"
  result
}


# =============================================================================
# Standard errors
# =============================================================================

# Score bootstrap: resamples (D_e, Y_e_hat) pairs without refitting the model
.score_bootstrap_calibrate <- function(Y_e_hat, D_e, B, seed = NULL, num.threads = 1) {
  n_e <- length(D_e)
  run_one_boot <- function(b) {
    if (!is.null(seed)) set.seed(seed + b)
    idx_b <- sample(n_e, n_e, replace = TRUE)
    tryCatch(
      unname(lm.fit(cbind(1, D_e[idx_b]), Y_e_hat[idx_b])$coefficients[2]),
      error = function(e) NA_real_
    )
  }
  boot_tauhat <- vapply(
    parallel::mclapply(seq_len(B), run_one_boot, mc.cores = num.threads),
    as.numeric, FUN.VALUE = numeric(1)
  )
  n_failed <- sum(is.na(boot_tauhat))
  if (n_failed > 0)
    warning(sprintf("Score bootstrap: %d of %d replications failed", n_failed, B))
  sd(boot_tauhat, na.rm = TRUE)
}

# Full bootstrap: refits the model on each resample
.bootstrap_calibrate <- function(Y, R, D, S_e, S_o, B, y_levels, spec,
                                 seed = NULL, num.threads = 1, clusters = NULL) {
  if (is.null(clusters)) {
    clusters      <- seq_len(length(Y))
    unique_clusters <- clusters
  } else {
    unique_clusters <- unique(clusters)
  }
  
  run_one_boot <- function(r) {
    if (!is.null(seed)) set.seed(seed + r)
    clusters_b <- sample(unique_clusters, length(unique_clusters), replace = TRUE)
    idx_b <- unlist(lapply(clusters_b, function(cl) which(clusters == cl)))
    tryCatch(
      compute_calibrate_estimate(
        Y[idx_b], R[idx_b, , drop = FALSE], D[idx_b],
        S_e[idx_b], S_o[idx_b], y_levels, spec,
        seed = NULL, num.threads = 1  # 1: outer mclapply provides parallelism
      )$coefficients,
      error = function(e) NA_real_
    )
  }
  
  boot_tauhat <- vapply(
    parallel::mclapply(seq_len(B), run_one_boot, mc.cores = num.threads),
    as.numeric, FUN.VALUE = numeric(1)
  )
  n_failed <- sum(is.na(boot_tauhat))
  if (n_failed > 0)
    warning(sprintf("Bootstrap: %d of %d replications failed", n_failed, B))
  sd(boot_tauhat, na.rm = TRUE)
}

add_se.calibrate <- function(object, method = c("score_bootstrap", "bootstrap"),
                             B = 1000, seed = NULL, num.threads = 1) {
  method  <- match.arg(method)
  se_call <- match.call()
  
  se <- if (method == "score_bootstrap") {
    .score_bootstrap_calibrate(object$Y_e_hat, object$D_e, B, seed, num.threads)
  } else {
    .bootstrap_calibrate(
      Y = object$Y, R = object$R, D = object$D,
      S_e = object$S_e, S_o = object$S_o,
      B = B, y_levels = object$y_levels,
      spec = object$spec, seed = seed, num.threads = num.threads
    )
  }
  
  object$se        <- structure(se, names = "D")
  object$se.method <- method
  object$se.call   <- se_call
  object
}


run_single_config <- function(config_idx, params, out_dir) {
  outcome   <- params$outcome[config_idx]
  n_e       <- params$n_e[config_idx]
  n_o       <- params$n_o[config_idx]
  tau       <- params$tau[config_idx]
  y_levels  <- params$y_levels[config_idx][[1]]
  alpha_o   <- params$alpha_o[config_idx][[1]]
  alpha_o_id <- params$alpha_o_id[config_idx]
  p0        <- params$p0[config_idx][[1]]
  p1        <- params$p1[config_idx][[1]]
  model     <- params$model[config_idx]
  se_method <- params$se_method[config_idx]
  seed      <- params$seed[config_idx]
  B         <- params$B[config_idx]
  B_se      <- params$B_se[config_idx]
  
  filename <- sprintf("%s_ne%d_no%d_tau%.2f_%s.Rds", outcome, n_e, n_o, tau, alpha_o_id)
  if (file.exists(file.path(out_dir, filename)))
    return(readRDS(file.path(out_dir, filename)))
  
  # Get pre-computed R values for this outcome
  R_given_Y <- get_R_given_Y(
    outcome,
    data_path = sprintf("data/clean/ugandaforestcover/data_%s.csv", outcome),
    r_col_fn  = function(nms) grep("^R[0-9]+", nms, value = TRUE),
    y_col     = "Y"
  )

  run_one <- function(b) {
    b_seed <- seed + b
    set.seed(b_seed)
    
    dat <- sim_noexpoutcomes(n_e, n_o, y_levels, p0, p1, alpha_o, R_given_Y)
    Y <- dat$Y; D <- dat$D; R <- dat$R; S_e <- dat$S_e; S_o <- dat$S_o
    
    # num.threads = 1: parallelism comes from mc.cores in the outer mclapply
    spec <- list(model = model)
    fit <- compute_calibrate_estimate(Y, R, D, S_e, S_o, y_levels,
                                      spec = spec, seed = NULL, num.threads = 1)
    fit <- add_se(fit, method = se_method, B = B_se, seed = NULL, num.threads = 1)
    list(tauhat = fit$coefficients, se = fit$se, seed_b = b_seed)
  }
  
  results <- rbindlist(mclapply(seq_len(B), run_one, mc.cores = min(n_cores, B)))
  
  sim_results <- cbind(
    params[config_idx,],
    data.table(
      b      = seq_len(B),
      seed_b = results$seed_b,
      tauhat = results$tauhat,
      se     = results$se
    )
  )

  saveRDS(sim_results, file.path(out_dir, filename))
  invisible(sim_results)
}

# =============================================================================
# Run
# =============================================================================

params <- readRDS(file.path("data/interim/ugandaforestcover", paste0("sims_noexpoutcomes_", outcome), "params.Rds"))
n_configs <- nrow(params)
cat("Loaded", n_configs, "parameter configurations\n")
cat("Using", n_cores, "cores for parallel MC sims\n\n")

results <- list()
for (idx in seq_len(n_configs)) {
  cat("Starting idx =", idx)
  start_time     <- Sys.time()
  results[[idx]] <- run_single_config(idx, params, out_dir)
  elapsed        <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  
  out <- as.data.frame(summarize_sim_results(
    data.table(results[[idx]]), 
    by_cols = c("outcome", "tau", "n_e", "n_o", "alpha_o_id"), 
    alpha = 0.1
  ))
  
  cat(sprintf("; Normalized bias = %.3f; 90%%Coverage = %.3f; Total time = %.0f minutes\n", out$normalized_bias, out$coverage, elapsed))
}

n_errors <- sum(sapply(results, inherits, "try-error"))
if (n_errors > 0) cat("WARNING:", n_errors, "configurations failed\n")
cat("Total configurations:", n_configs, "\n")
cat("Successful:", n_configs - n_errors, "\n")
cat("Failed:", n_errors, "\n")
cat("Results saved to:", out_dir, "\n")