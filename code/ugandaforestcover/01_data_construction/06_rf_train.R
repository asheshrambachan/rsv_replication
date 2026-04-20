# =============================================================================
# Uganda Forest Cover — Train Random Forest Predictors
#
# Trains a random forest model on the training split to predict each outcome
# from MOSAIKS features (X_0, X_1, ..., X_N). The fitted model is used in
# 07_rf_predict.R to generate predicted class probabilities on the simulation
# split, which serve as the remote sensing predictor throughout the analysis.
#
# Hyperparameters (mtry, min.node.size) are tuned by minimising OOB error
# over a small grid. The best configuration is then refitted on the full
# training set. OOB AUC is reported as a diagnostic.
#
# Input:
#   data/interim/ugandaforestcover/data_train.csv
#
# Output:
#   data/interim/ugandaforestcover/predictors/rf_<outcome>.Rds
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(ranger)
  library(pROC)
})
source("code/utils/rf_utils.R")

# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------

countries             <- c("uganda")
outcomes              <- c("Ybin")
mtry_options          <- c(500, 1000)   # candidates for tuning grid
min_node_size_options <- c(1)
num_trees             <- 500
seed                  <- 42
n_threads             <- min(50, parallel::detectCores() - 1)

# Grid search over mtry x min.node.size using OOB error; returns best-tuned model
train_rf_tuned <- function(X, y_factor, mtry_options, min_node_size_options,
                           num_trees, seed, n_threads) {
  best_oob_err <- Inf
  best_params  <- list()

  for (mtry in mtry_options) {
    for (mns in min_node_size_options) {
      rf_tmp <- ranger(
        x = X, y = y_factor,
        num.trees     = num_trees,
        mtry          = min(mtry, ncol(X)),
        min.node.size = mns,
        probability   = TRUE,
        num.threads   = n_threads,
        seed          = seed
      )
      if (rf_tmp$prediction.error < best_oob_err) {
        best_oob_err <- rf_tmp$prediction.error
        best_params  <- list(mtry = min(mtry, ncol(X)), min.node.size = mns)
      }
    }
  }

  cat("    Best params: mtry =", best_params$mtry,
      ", min.node.size =", best_params$min.node.size, "\n")

  ranger(
    x = X, y = y_factor,
    num.trees     = num_trees,
    mtry          = best_params$mtry,
    min.node.size = best_params$min.node.size,
    probability   = TRUE,
    num.threads   = n_threads,
    seed          = seed
  )
}

# -----------------------------------------------------------------------------
# 3. Train Models
# -----------------------------------------------------------------------------

dir.create("data/interim/ugandaforestcover/predictors", recursive = TRUE, showWarnings = FALSE)

for (country in countries) {
  cat("=== Processing", country, "===\n")
  set.seed(seed)

  train_path   <- file.path("data/interim/ugandaforestcover", "data_train.csv")
  dt           <- fread(train_path)
  feature_cols <- grep("^X_[0-9]+$", names(dt), value = TRUE)
  X            <- as.matrix(dt[, ..feature_cols])
  cat(sprintf("  Loaded %d rows, %d MOSAIKS features\n", nrow(dt), length(feature_cols)))

  for (outcome in outcomes) {
    cat("  Training RF for", outcome, "...\n")

    y_raw        <- dt[[outcome]]
    outcome_type <- get_outcome_type(y_raw)
    y_prep       <- prepare_outcome(y_raw, outcome_type)

    rf_model <- train_rf_tuned(
      X                     = X,
      y_factor              = y_prep$y,
      mtry_options          = mtry_options,
      min_node_size_options = min_node_size_options,
      num_trees             = num_trees,
      seed                  = seed,
      n_threads             = n_threads
    )

    auc_val    <- compute_auc(y_raw, rf_model$predictions, outcome_type)
    model_path <- file.path("data/interim/ugandaforestcover/predictors",
                            paste0("rf_", outcome, ".Rds"))
    saveRDS(rf_model, model_path)
    cat(sprintf("    OOB AUC: %.4f  →  saved: %s\n", auc_val, model_path))
  }
}

