# Edit these settings, then Source 00_run_all.R from the project root.
# Full-sample fitting and estimation; no secondary outcomes or calibration.
variance_empirical_settings <- list(
  Y_vars = c("Ycons", "Ylowinc", "Ymidinc"),
  samples = c("full", "nospillover"),
  B = 1000L,
  level = 0.90,
  sigma2.quantile = 0.01,
  seed = 42L,
  num.threads = 4L,
  rf_spec = list(model = "rf", num.trees = 1000, min.node.size = 20,
                 max.depth = 10, verbose = FALSE),
  input_csv = "data/clean/smartcards/data.csv",
  interim_dir = "data/interim/smartcards/empirical_variance_regression",
  clean_dir = "data/clean/smartcards/empirical_variance_regression"
)
variance_empirical_settings <- utils::modifyList(
  variance_empirical_settings, getOption("rsv.variance.settings", list()))

# One reporting convention for point checkpoints and completed bootstrap fits.
# If any effect draw failed, do not report successful-draw-only SEs or CIs.
variance_empirical_summary <- function(saved, level) {
  z <- stats::qnorm((1 + level) / 2)
  rows <- lapply(names(saved$fits), function(stage) {
    fit <- saved$fits[[stage]]
    H <- as.numeric(fit$weights[[1]])
    sd_o <- stats::sd(H[as.logical(fit$data$S_o)])
    tau <- unname(fit$coefficients)
    denominator <- unname(fit$relevance)
    pending <- is.null(fit$bootstrap)
    failed <- if (pending) NA_integer_ else sum(!is.finite(fit$bootstrap$tau_draws))
    se <- if (!pending && failed == 0L && is.finite(fit$se)) unname(fit$se) else NA_real_
    relevance_se <- if (!pending && failed == 0L && is.finite(fit$relevance_se)) {
      unname(fit$relevance_se)
    } else NA_real_
    estimated <- stage == "uncalibrated_updated"
    data.frame(
      sample = saved$sample, Y_var = saved$Y_var, stage = stage,
      tau = tau, numerator = as.numeric(fit$numerator$numerator[[1]]),
      denominator = denominator,
      separation = if (is.finite(sd_o) && sd_o > 0) abs(denominator) / sd_o else NA_real_,
      se = se, ci_lower = tau - z * se, ci_upper = tau + z * se,
      ci_type = if (is.finite(se)) "normal" else "unavailable",
      denominator_se = relevance_se,
      denom_ci_lower = denominator - z * relevance_se,
      denom_ci_upper = denominator + z * relevance_se,
      H_min = min(H), H_max = max(H),
      variance_method = if (estimated) "regression" else "unit",
      preliminary_tau = unname(saved$fit$variance$preliminary_effect),
      variance_floor = if (estimated) saved$fit$variance$floor else 1,
      floored_share = if (estimated) saved$fit$variance$floored_share else 0,
      sigma2_quantile = if (estimated) saved$settings$sigma2.quantile else NA_real_,
      B = if (pending) NA_integer_ else fit$bootstrap$B,
      level = level, failed_draws = failed,
      n_obs = fit$n.obs, n_exp = fit$n.exp, n_overlap = fit$n.both,
      status = if (is.finite(tau)) "ok" else "Undefined effect.",
      inference_status = if (pending) "Bootstrap not completed; point-estimate checkpoint." else {
        if (failed > 0L) "Failed/nonfinite bootstrap draws; SE/CI unavailable." else {
          if (is.finite(se)) "Score-bootstrap SE and normal confidence interval." else {
            "Nonfinite bootstrap SE; confidence interval unavailable."
          }
        }
      }, row.names = NULL
    )
  })
  do.call(rbind, rows)
}
