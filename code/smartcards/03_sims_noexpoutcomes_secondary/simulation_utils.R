# Helpers local to the new pipeline; the original simulation helpers are intact.
secondary_sim_cells <- function() data.frame(Y = c(0L, 0L, 1L, 1L), secondary = c(0L, 1L, 0L, 1L))

secondary_sim_binary <- function(x, cfg) {
  if (!(is.numeric(x) || is.logical(x)) || any(!is.na(x) & !is.finite(x))) stop("Secondary measurement must be numeric and finite where observed.")
  observed <- x[!is.na(x)]
  if (!length(observed)) stop("No observed secondary measurements.")
  if (is.null(cfg$secondary_cutoff) && all(observed %in% 0:1)) {
    return(list(values = as.integer(x), cutoff = NA_real_, rule = "existing_binary"))
  }
  cutoff <- cfg$secondary_cutoff
  if (is.null(cutoff)) {
    if (length(cfg$secondary_quantile) != 1L || !is.finite(cfg$secondary_quantile) ||
        cfg$secondary_quantile <= 0 || cfg$secondary_quantile >= 1) stop("Invalid secondary_quantile.")
    cutoff <- unname(stats::quantile(observed, cfg$secondary_quantile, type = 7))
  }
  if (length(cutoff) != 1L || !is.finite(cutoff)) stop("Invalid secondary cutoff.")
  list(values = as.integer(x > cutoff), cutoff = cutoff, rule = "value > fixed cutoff")
}

secondary_sim_probabilities <- function(Y, secondary, selected) {
  valid <- selected & !is.na(Y) & !is.na(secondary)
  if (!any(valid)) stop("No complete baseline outcome pairs.")
  if (any(!Y[valid] %in% 0:1)) stop("Primary Y must be binary.")
  counts <- tabulate(1L + 2L * Y[valid] + secondary[valid], nbins = 4L)
  if (any(counts == 0L)) stop("All four joint outcome groups must be observed in each baseline sample; choose another cutoff/measurement.")
  list(probabilities = counts / sum(counts), n = sum(counts), n_missing = sum(selected) - sum(counts))
}

secondary_sim_R_lookup <- function(cfg, outcome, cutoff, rule) {
  header <- names(data.table::fread(cfg$input_csv, nrows = 0L))
  r_cols <- cfg$r_col_fn(header)
  if (!length(r_cols) || anyDuplicated(r_cols) || any(!r_cols %in% header)) stop("Missing/duplicate imagery columns.")
  d <- data.table::fread(cfg$input_csv, select = unique(c(outcome, cfg$secondary_column, r_cols)))
  secondary <- if (rule == "existing_binary") as.integer(d[[cfg$secondary_column]]) else {
    as.integer(d[[cfg$secondary_column]] > cutoff)
  }
  Y <- d[[outcome]]
  valid <- !is.na(Y) & !is.na(secondary)
  if (any(!Y[valid] %in% 0:1) || any(!secondary[valid] %in% 0:1)) stop("Invalid binary outcome pair.")
  R <- as.matrix(d[, ..r_cols])
  valid <- valid & rowSums(!is.finite(R)) == 0L
  cell <- 1L + 2L * Y + secondary
  lookup <- lapply(1:4, function(j) R[which(valid & cell == j), , drop = FALSE])
  if (any(vapply(lookup, nrow, integer(1)) == 0L)) stop("A joint outcome group has no finite imagery rows.")
  lookup
}

secondary_sim_generate <- function(n_e, n_o, p0, p1, alpha_o, lookup, keep_truth = FALSE) {
  for (p in list(p0, p1, alpha_o)) {
    if (length(p) != 4L || any(!is.finite(p)) || any(p < 0) || abs(sum(p) - 1) > 1e-8) stop("Invalid joint probabilities.")
  }
  cells <- secondary_sim_cells()
  D <- stats::rbinom(n_e, 1, .5)
  cell_e <- integer(n_e)
  for (d in 0:1) {
    i <- which(D == d)
    cell_e[i] <- sample.int(4L, length(i), replace = TRUE, prob = if (d == 0) p0 else p1)
  }
  cell_o <- sample.int(4L, n_o, replace = TRUE, prob = alpha_o)
  cell <- c(cell_e, cell_o)
  R <- matrix(NA_real_, n_e + n_o, ncol(lookup[[1]]), dimnames = list(NULL, colnames(lookup[[1]])))
  for (j in 1:4) {
    i <- which(cell == j)
    R[i, ] <- lookup[[j]][sample.int(nrow(lookup[[j]]), length(i), replace = TRUE), , drop = FALSE]
  }
  Y <- cells$Y[cell]; secondary <- cells$secondary[cell]
  truth <- if (keep_truth) data.frame(Y = Y, secondary = secondary) else NULL
  Y[seq_len(n_e)] <- NA_integer_; secondary[seq_len(n_e)] <- NA_integer_
  list(Y = Y, secondary = secondary, R = R, D = c(D, rep(NA_integer_, n_o)),
    S_e = c(rep(1L, n_e), rep(0L, n_o)), S_o = c(rep(0L, n_e), rep(1L, n_o)), truth = truth)
}

# Same common-practice point estimator and fixed-prediction experimental-row
# bootstrap as the original pipeline. Failed draws remain visible.
secondary_sim_common <- function(dat, spec, seed, B_se, level) {
  obs <- dat$S_o == 1L; exp <- dat$S_e == 1L
  fit <- fit_one_model_multicat(dat$R[obs, , drop = FALSE], dat$Y[obs],
    dat$R[exp, , drop = FALSE], y_levels = c(0, 1), spec = spec,
    seed = seed, num.threads = 1L)
  predicted <- as.numeric(as.matrix(fit$predictions) %*% c(0, 1))
  D <- dat$D[exp]
  coefficient <- function(i) unname(stats::lm.fit(cbind(1, D[i]), predicted[i])$coefficients[2])
  tauhat <- coefficient(seq_along(D))
  draws <- vapply(seq_len(B_se), function(b) {
    set.seed(seed + b)
    tryCatch(coefficient(sample.int(length(D), length(D), TRUE)), error = function(e) NA_real_)
  }, numeric(1))
  failed <- sum(!is.finite(draws))
  se <- if (!failed && is.finite(tauhat)) stats::sd(draws) else NA_real_
  z <- stats::qnorm(1 - (1 - level) / 2)
  data.frame(estimator = "Common practice", stage = "common_practice", tauhat = tauhat,
    se = se, ci_lower = tauhat - z * se, ci_upper = tauhat + z * se,
    failed_draws = failed, M_condition = NA_real_, balance = NA_real_,
    status = if (is.finite(tauhat)) "ok" else "Nonfinite point estimate",
    inference_status = if (is.finite(se)) "ok" else "Bootstrap inference unavailable")
}

secondary_sim_run <- function(cfg, params, estimator) {
  output_dir <- file.path(cfg$interim_dir, estimator)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(params))) {
    row <- params[i, , drop = FALSE]
    configuration <- as.list(row)
    name <- gsub("[^[:alnum:]_-]", "_", paste(row$secondary_column, row$outcome, sep = "_"))
    path <- file.path(output_dir, sprintf("%s_config%03d.Rds", name, i))
    info <- file.info(cfg$input_csv)
    identity <- list(path = normalizePath(cfg$input_csv), size = info$size, mtime = as.numeric(info$mtime))
    if (!identical(identity, row$input_identity[[1]])) stop("Input data changed; rebuild parameters before running.")
    if (file.exists(path)) {
      cached <- readRDS(path)
      if (!identical(cached$configuration, configuration)) stop("Cached configuration differs: ", path, ". Use a separate interim_dir for the new run.")
      next
    }
    lookup_cfg <- cfg
    lookup_cfg$secondary_column <- row$secondary_column
    lookup_cfg$r_col_fn <- function(nms) row$r_columns[[1]]
    lookup <- secondary_sim_R_lookup(lookup_cfg, row$outcome, row$secondary_cutoff, row$secondary_rule)
    spec <- utils::modifyList(list(model = "rf", num.trees = row$num_trees, verbose = FALSE), row$rf_options[[1]])
    stages <- if (estimator == "rsv") c("uncalibrated_unit", "calibrated_unit", "uncalibrated_weighted", "calibrated_weighted") else "common_practice"
    run_one <- function(b) {
      seed <- row$seed + b
      set.seed(seed)
      output <- tryCatch({
        dat <- secondary_sim_generate(row$n_e, row$n_o, row$p0[[1]], row$p1[[1]], row$alpha_o[[1]], lookup)
        if (estimator == "calibrate") {
          secondary_sim_common(dat, spec, seed, row$B_se, row$level)
        } else {
          models <- setNames(rep(list(spec), 4L), c("Y", "D", "S_e", "S_o"))
          fit <- cv.rsv_secondary(dat$Y, dat$D, dat$S_e, dat$S_o, dat$R,
            dat$secondary, models, nfolds = row$nfolds, seed = seed,
            sigma2.method = row$sigma2.method, sigma2.quantile = row$sigma2.quantile,
            num.threads = 1L)
          fit <- add_se(fit, method = "score_bootstrap", B = row$B_se,
            seed = seed, level = row$level, num.threads = 1L)
          data.frame(estimator = "RSV", stage = fit$results$stage,
            tauhat = fit$results$tau, se = fit$inference$se,
            ci_lower = fit$inference$ci_lower, ci_upper = fit$inference$ci_upper,
            failed_draws = fit$inference$failed_draws,
            M_condition = fit$results$M_condition, balance = fit$results$balance,
            status = fit$results$status, inference_status = fit$inference$status)
        }
      }, error = function(e) data.frame(estimator = if (estimator == "rsv") "RSV" else "Common practice",
        stage = stages, tauhat = NA_real_, se = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
        failed_draws = NA_integer_, M_condition = NA_real_, balance = NA_real_,
        status = conditionMessage(e), inference_status = "Point fit failed"))
      output$b <- b; output$seed_b <- seed
      output
    }
    draws <- data.table::rbindlist(parallel::mclapply(seq_len(row$B), run_one,
      mc.cores = min(cfg$n_cores, row$B)))
    scalar <- row[setdiff(names(row), c("p0", "p1", "alpha_o", "input_identity", "rf_options", "r_columns"))]
    results <- cbind(scalar[rep(1L, nrow(draws)), , drop = FALSE], as.data.frame(draws))
    saveRDS(list(configuration = configuration, results = results), path)
    cat("Saved:", path, "\n")
  }
  invisible(NULL)
}
