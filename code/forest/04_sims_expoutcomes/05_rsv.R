library(data.table)

library(ranger)
library(parallel)
library(dplyr)
library(tidyr)
library(tibble)
source("code/utils/sim_utils.R")
source("code/utils/summarize_sim_results.R")

# Source RSV helper functions
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

# Number of parallel cores
n_cores <- min(100, parallel::detectCores() - 1)
cat("=== RSV Simulations (Cross-fit, Logit) ===\n")
cat("Using", n_cores, "cores for parallel processing\n\n")

# Create output directory
estimator <- "rsv"
out_dir   <- file.path("data/interim/forest/binary_expoutcomes", estimator)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

run_single_config <- function(config_idx, params, out_dir, nfolds = 2) {
  outcome   <- params$outcome[config_idx]
  n_e       <- params$n_e[config_idx]
  n_o       <- params$n_o[config_idx]
  n_eo      <- params$n_eo[config_idx]
  tau       <- params$tau[config_idx]
  y_levels  <- params$y_levels[config_idx][[1]]
  yK        <- y_levels[1]
  alpha_o   <- params$alpha_o[config_idx][[1]]
  alpha_o_id <- params$alpha_o_id[config_idx]
  p0        <- params$p0[config_idx][[1]]
  p1        <- params$p1[config_idx][[1]]
  model     <- params$model[config_idx]
  se_method <- params$se_method[config_idx]
  seed      <- params$seed[config_idx]
  B         <- params$B[config_idx]
  B_se      <- params$B_se[config_idx]
  
  filename <- sprintf("%s_ne%d_neo%d_no%d_tau%.2f_%s.Rds", outcome, n_e, n_eo, n_o, tau, alpha_o_id)
  if (file.exists(file.path(out_dir, filename)))
    return(readRDS(file.path(out_dir, filename)))
  
  # Get pre-computed R values for this outcome
  R_given_Y <- get_R_given_Y(
    outcome,
    data_path = sprintf("data/clean/forest/uganda_sim_%s_w_pred.csv", outcome),
    r_col_fn  = function(nms) grep("^R[0-9]+", nms, value = TRUE),
    y_col     = "Y"
  )
  
  run_one <- function(b) {
    b_seed <- seed + b
    set.seed(b_seed)
    dat <- sim_expoutcomes(n_e, n_o, n_eo, y_levels, p0, p1, alpha_o, R_given_Y)
    Y <- dat$Y; D <- dat$D; R <- dat$R; S_e <- dat$S_e; S_o <- dat$S_o
    
    # num.threads = 1: parallelism comes from mc.cores in the outer mclapply
    spec <- list(model = model)
    fit <- cv.rsv(
      Y = Y, D = D, S_e = S_e, S_o = S_o, R = R,
      y_levels = y_levels, yK = yK,
      models = list(Y = spec, D = spec, S_e = spec, S_o = spec),
      nfolds = nfolds, seed = NULL, num.threads = 1
    )
    fit <- add_se(fit, method = se_method, B = B_se, seed = NULL, num.threads = 1)
    list(tauhat = fit$coefficients, se = fit$se, seed_b = b_seed)
  }
  
  results <- rbindlist(mclapply(seq_len(B), run_one, mc.cores = min(n_cores, B)))
  
  sim_results <- cbind(
    params[config_idx,],
    data.table(
      b      = seq_len(B),
      seed_b = results$seed_b,
      nfolds = nfolds,
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

params <- readRDS("data/interim/forest/binary_expoutcomes/params.Rds")
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
    by_cols = c("outcome", "tau", "n_e", "n_o", "n_eo", "alpha_o_id"), 
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
