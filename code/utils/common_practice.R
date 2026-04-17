# =============================================================================
# Common-Practice Treatment Effect Estimator
#
# Estimates the treatment effect of D on an outcome R using OLS on the
# experimental sample, with optional fixed effects. This is the
# "common-practice" estimator: regress the remote sensing predictor R on
# treatment D, using only plots with experimental variation.
#
# Functions:
#   fit_common_practice()     — point estimate
#   add_se_common_practice()  — cluster bootstrap standard errors
#
# Dependencies: fixest, parallel
# =============================================================================

# -----------------------------------------------------------------------------
# fit_common_practice()
#
# Returns the OLS coefficient on D from the regression:
#   R ~ D [+ fixed effects]
# estimated on the experimental sample only.
#
# Arguments:
#   D   — binary treatment vector
#   R   — numeric outcome vector (remote sensing predictor)
#   S_e — logical vector; TRUE for plots in the experimental sample
#   X   — data frame of fixed effect covariates, or NULL. All columns are used.
# -----------------------------------------------------------------------------

fit_common_practice <- function(D, R, S_e, X = NULL) {
  df_exp <- data.frame(D = D[S_e], R = R[S_e])
  if (!is.null(X))
    df_exp <- cbind(df_exp, X[S_e, , drop = FALSE])

  fml_rhs <- if (is.null(X)) {
    "D"
  } else {
    paste("D +", paste0("i(", colnames(X), ")", collapse = " + "))
  }
  model <- fixest::feols(
    as.formula(paste("R ~", fml_rhs)),
    data  = df_exp,
    notes = FALSE
  )
  unname(coef(model)["D"])
}


# -----------------------------------------------------------------------------
# add_se_common_practice()
#
# Cluster bootstrap standard errors for fit_common_practice(). Resamples
# clusters with replacement B times, re-estimates on each resample, and
# returns the standard deviation of the bootstrap distribution as the SE.
#
# Arguments:
#   coef        — point estimate from fit_common_practice()
#   D           — binary treatment vector
#   R           — numeric outcome vector (remote sensing predictor)
#   S_e         — logical vector; TRUE for plots in the experimental sample
#   X           — data frame of fixed effect covariates, or NULL
#   clusters    — vector of cluster IDs (same length as D)
#   B           — number of bootstrap replications (default 5000)
#   seed        — integer seed for reproducibility, or NULL
#   num.threads — number of parallel threads (default 1)
#
# Returns a list with:
#   coefficients — point estimate (named "D")
#   se           — bootstrap SE (named "D")
#   bootstrap    — list with B, n_failed, seed, coef_draws, seed_draws
# -----------------------------------------------------------------------------

add_se_common_practice <- function(coef, D, R, S_e, X = NULL,
                                   clusters, B = 5000, seed = NULL,
                                   num.threads = 1) {
  unique_clusters <- unique(clusters)

  run_one <- function(b) {
    seed_b <- if (!is.null(seed)) seed + b else NA_integer_
    if (!is.na(seed_b)) set.seed(seed_b)
    clusters_b <- sample(unique_clusters, length(unique_clusters), replace = TRUE)
    idx_b      <- unlist(lapply(clusters_b, function(cl) which(clusters == cl)))
    list(
      seed_b = seed_b,
      coef   = tryCatch(
        fit_common_practice(D[idx_b], R[idx_b], S_e[idx_b],
                            if (!is.null(X)) X[idx_b, , drop = FALSE] else NULL),
        error = function(e) NA_real_
      )
    )
  }

  boot_list  <- parallel::mclapply(seq_len(B), run_one, mc.cores = num.threads)
  boot_coefs <- vapply(boot_list, `[[`, numeric(1), "coef")
  boot_seeds <- vapply(boot_list, `[[`, numeric(1), "seed_b")

  n_failed <- sum(is.na(boot_coefs))
  if (n_failed > 0)
    warning(sprintf("%d of %d bootstrap replications failed", n_failed, B))

  list(
    coefficients = structure(coef,                         names = "D"),
    se           = structure(sd(boot_coefs, na.rm = TRUE), names = "D"),
    bootstrap    = list(
      B          = B,
      n_failed   = n_failed,
      seed       = seed,
      coef_draws = boot_coefs,
      seed_draws = boot_seeds
    )
  )
}
