# =============================================================================
# Crop Burning — RSV Estimator (Observational Sample)
#
# Fits the RSV estimator using cross-validation (cv.rsv) on the observational
# study sample. The predictor R is the continuous RS score (R_prob). Sample
# membership indicators S_e and S_o are derived from the data: S_e flags plots
# with observed treatment D, S_o flags plots with observed outcome Y.
#
# Input:  data/clean/cropburn/obs_data.csv
# Output: data/interim/cropburn/fit_rsv_obs.Rds
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(readr)
})
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)


# -----------------------------------------------------------------------------
# 1. Load data and extract model inputs
# -----------------------------------------------------------------------------

df <- read_csv("data/clean/cropburn/obs_data.csv", show_col_types = FALSE)

Y        <- df$Y
D        <- df$D
R        <- df$R_prob
S_e      <- !is.na(D) & !is.na(R)   # experimental sample: has treatment and RS predictor
S_o      <- !is.na(Y) & !is.na(R)   # observational sample: has outcome and RS predictor
clusters <- df$village_id
y_levels <- sort(unique(na.omit(Y)))


# -----------------------------------------------------------------------------
# 2. Set model and estimation parameters
# -----------------------------------------------------------------------------

# All four nuisance models use logistic regression
models <- list(
  Y   = list(model = "logit", verbose = FALSE),
  D   = list(model = "logit", verbose = FALSE),
  S_e = list(model = "logit", verbose = FALSE),
  S_o = list(model = "logit", verbose = FALSE)
)

seed        <- 42L
B_se        <- 5000
num.threads <- 7
nfolds      <- 2


# -----------------------------------------------------------------------------
# 3. Fit RSV estimator and compute bootstrap SEs
# -----------------------------------------------------------------------------

fit <- cv.rsv(Y = Y, D = D, S_e = S_e, S_o = S_o,
              R = R, y_levels = y_levels, models = models,
              nfolds = nfolds, clusters = clusters,
              seed = seed, num.threads = num.threads)

fit <- add_se(fit, B = B_se, clusters = clusters,
              method = "score_bootstrap", seed = seed, num.threads = num.threads)


# -----------------------------------------------------------------------------
# 4. Save
# -----------------------------------------------------------------------------

out_path <- "data/interim/cropburn/fit_rsv_obs.Rds"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(fit, out_path)
cat("Saved to", out_path, "\n")
