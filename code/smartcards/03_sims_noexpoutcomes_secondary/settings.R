# Set options(rsv.secondary.sim.settings = list(secondary_column = "..."))
# before running. Binary inputs are used directly; continuous inputs use > cutoff
# (a supplied cutoff or, by default, the full input sample's median).
secondary_sim_settings <- function() {
  detected <- parallel::detectCores()
  cfg <- utils::modifyList(list(
    secondary_column = NULL, secondary_cutoff = NULL, secondary_quantile = .5,
    input_csv = "data/clean/smartcards/data.csv",
    interim_dir = "data/interim/smartcards/sims_noexpoutcomes_secondary",
    summary_csv = "data/clean/smartcards/sims_noexpoutcomes_secondary.csv",
    outcomes = c("Ycons", "Ylowinc", "Ymidinc"), tau_grid = seq(0, .2, .05),
    n_e_grid = 3000L, n_o_grid = c(500L, 1000L, 1500L),
    model = "rf", num_trees = 100L, seed = 42L,
    B = 500L, B_se = 100L, nfolds = 2L, level = .95,
    sigma2.method = "plugin", sigma2.quantile = .01,
    rf_options = list(), n_cores = if (is.na(detected)) 1L else max(1L, min(100L, detected - 1L)),
    r_col_fn = function(nms) c(grep("^luminosity_", nms, value = TRUE), paste0("satellite_", 1:1000))
  ), getOption("rsv.secondary.sim.settings", list()), keep.null = TRUE)
  if (is.null(cfg$secondary_column) || length(cfg$secondary_column) != 1L ||
      !is.character(cfg$secondary_column) || is.na(cfg$secondary_column) || !nzchar(cfg$secondary_column)) {
    stop("Choose secondary_column through options(rsv.secondary.sim.settings = list(secondary_column = 'your_column')).")
  }
  if (any(cfg$outcomes == cfg$secondary_column)) stop("Secondary and primary outcomes must be distinct.")
  if (!is.character(cfg$outcomes) || !length(cfg$outcomes) || anyNA(cfg$outcomes) ||
      any(!nzchar(cfg$outcomes)) || anyDuplicated(cfg$outcomes)) stop("Invalid outcomes.")
  if (!identical(cfg$model, "rf") || !cfg$sigma2.method %in% c("plugin", "regression")) stop("Use RF and plugin/regression variance.")
  for (nm in c("B", "B_se", "nfolds", "num_trees", "n_cores")) {
    x <- cfg[[nm]]
    if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x < 1 || x != as.integer(x)) stop("Invalid ", nm)
  }
  if (cfg$B_se < 2L || cfg$nfolds < 2L) stop("B_se and nfolds must be at least two.")
  for (nm in c("n_e_grid", "n_o_grid")) {
    x <- cfg[[nm]]
    if (!is.numeric(x) || !length(x) || any(!is.finite(x)) || any(x < 2) ||
        any(x != as.integer(x))) stop("Invalid ", nm)
  }
  if (!is.numeric(cfg$tau_grid) || !length(cfg$tau_grid) || any(!is.finite(cfg$tau_grid))) stop("Invalid tau_grid.")
  if (length(cfg$seed) != 1L || !is.numeric(cfg$seed) || !is.finite(cfg$seed) ||
      cfg$seed < 0 || cfg$seed != as.integer(cfg$seed) ||
      cfg$seed + cfg$B + cfg$B_se > .Machine$integer.max) stop("Invalid seed.")
  if (!is.list(cfg$rf_options) || !is.function(cfg$r_col_fn)) stop("Invalid RF options or imagery selector.")
  if (length(cfg$level) != 1L || !is.finite(cfg$level) || cfg$level <= 0 || cfg$level >= 1) stop("Invalid CI level.")
  cfg
}
