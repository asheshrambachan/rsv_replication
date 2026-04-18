# =============================================================================
# Empirical RSV Estimation
#
# Fits the RSV estimator and an OLS benchmark for each outcome (Ycons,
# Ylowinc, Ymidinc). Reports two variants of RSV: efficient weights and simple
# (naive) weights — the simple-weights estimate reuses the same fitted random
# forests without refitting. Standard errors are clustered score bootstrap.
#
# Runs twice:
#   full        — all villages in the analysis sample
#   nospillover — restricted to villages not flagged by the 20 km spillover rule
#
# Output: data/interim/smartcards/empirical/{full,nospillover}/{Y_var}.Rds
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(data.table)
  library(parallel)
  library(fixest)
})
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

Y_vars <- c("Ycons", "Ylowinc", "Ymidinc")
B      <- 1000L
cores  <- min(detectCores() - 1, 100L)
seed   <- 42L


# =============================================================================
# Estimation
# =============================================================================

run_outcome <- function(Y_var, df) {
  # Y is already masked to NA for experimental-only rows in the data prep step;
  # S_e and S_o are derived from missingness patterns rather than a wave column.
  Y        <- df[[Y_var]]
  y_levels <- sort(unique(na.omit(Y)))
  yK       <- y_levels[1]   # reference outcome level (dropped from multiclass model)
  D        <- df$D
  R        <- df %>% select(starts_with("luminosity_"), starts_with("satellite_"))
  S_e      <- !is.na(D) & (rowSums(is.na(R)) == 0)
  S_o      <- !is.na(Y) & (rowSums(is.na(R)) == 0)
  clusters <- df$clusters   # geographic clusters for cluster-robust bootstrap

  # Fit RSV (efficient + naive weights; stores predictions in fit$data)
  rf_spec <- list(model = "rf", num.trees = 1000, min.node.size = 20, max.depth = 10, verbose = FALSE)
  models  <- list(Y = rf_spec, D = rf_spec, S_e = rf_spec, S_o = rf_spec)
  fit <- rsv(Y = Y, D = D, S_e = S_e, S_o = S_o, R = R,
             y_levels = y_levels, yK = yK, models = models,
             seed = seed, num.threads = cores)

  # Score bootstrap for RSV (draws stored in fit$bootstrap$tau_draws)
  fit <- add_se(fit, clusters = clusters, method = "score_bootstrap",
                B = B, seed = seed, num.threads = cores)

  # Benchmark point estimate
  fit_benchmark <- feols(
    fml     = as.formula(paste0(Y_var, " ~ D")),
    data    = df,
    cluster = ~clusters,
    notes   = FALSE
  )
  bench_coef <- coef(fit_benchmark)[["D"]]
  
  # Benchmark bootstrap: replay the same seeds as add_se so cluster samples
  # are identical to those used for RSV → paired draws → valid diff_se
  unique_clusters <- unique(clusters)
  bench_formula   <- as.formula(paste0(Y_var, " ~ D"))
  bench_draws <- vapply(seq_len(B), function(b) {
    set.seed(seed + b)
    idx_b <- unlist(lapply(
      sample(unique_clusters, length(unique_clusters), replace = TRUE),
      function(cl) which(clusters == cl)
    ))
    lm(bench_formula, data = df[idx_b, , drop = FALSE])$coefficients[["D"]]
  }, numeric(1))

  bench_se <- sd(bench_draws, na.rm = TRUE)
  diff_se  <- sd(bench_draws - fit$bootstrap$tau_draws, na.rm = TRUE)

  result <- data.frame(
    Y_var             = Y_var,
    rsv_coef          = fit$coefficients,
    rsv_se            = fit$se,
    denom_coef        = fit$relevance,
    denom_se          = fit$relevance_se,
    simple_coef       = fit$coefficients_naive,
    simple_se         = fit$se_naive,
    simple_denom_coef = fit$relevance_naive,
    simple_denom_se   = fit$relevance_naive_se,
    bench_coef        = bench_coef,
    bench_se          = bench_se,
    diff_coef         = bench_coef - fit$coefficients,
    diff_se           = diff_se,
    B                 = B
  )

  list(fit = fit, fit_benchmark = fit_benchmark, result = result)
}


data <- fread("data/clean/smartcards/data.csv") %>%
  mutate(
    S = case_when(
      wave == "Experimental: Treated (2010)"   ~ "e",
      wave == "Experimental: Untreated (2011)" ~ "both",
      wave == "Experimental: Untreated (2012)" ~ "e",
      wave == "Observational (N/A)"            ~ "o"
    ),
    D = if_else(S %in% c("both", "e"), D, NA_real_),
    Ycons   = if_else(S %in% c("both", "o"), Ycons,   NA_real_),
    Ylowinc = if_else(S %in% c("both", "o"), Ylowinc, NA_real_),
    Ymidinc = if_else(S %in% c("both", "o"), Ymidinc, NA_real_)
  ) %>%
  filter(!is.na(S))

# Main estimates on the full analysis sample
out_dir <- "data/interim/smartcards/empirical/full"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
results <- list()
for (Y_var in Y_vars) {
  out_path         <- file.path(out_dir, paste0(Y_var, ".Rds"))
  results[[Y_var]] <- run_outcome(Y_var, df = data)
  saveRDS(results[[Y_var]], out_path)
  cat(sprintf("Saved: %s\n", out_path))
}

# Robustness: exclude villages within 20 km of a treated unit (spillover check)
out_dir <- "data/interim/smartcards/empirical/nospillover"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

data_nospillover <- data %>%
  filter(spillover_20km == FALSE)

results <- list()
for (Y_var in Y_vars) {
  out_path         <- file.path(out_dir, paste0(Y_var, ".Rds"))
  results[[Y_var]] <- run_outcome(Y_var, df = data_nospillover)
  saveRDS(results[[Y_var]], out_path)
  cat(sprintf("Saved: %s\n", out_path))
}
