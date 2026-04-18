# =============================================================================
# Random Forest Utilities
#
# Helper functions for training and evaluating ranger models used in the
# ugandaforestcover application.
# =============================================================================

# Classify outcome as binary (0/1) or multiclass (any other discrete values)
get_outcome_type <- function(y) {
  unique_vals <- sort(unique(na.omit(y)))
  if (length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))) "binary" else "multiclass"
}

# Convert outcome to factor with fixed level ordering for ranger
prepare_outcome <- function(y, outcome_type) {
  lvls <- if (outcome_type == "binary") c(0, 1) else sort(unique(na.omit(y)))
  list(y = factor(y, levels = lvls), levels = lvls)
}

# AUC: for binary uses P(Y=1); for multiclass uses full probability matrix
compute_auc <- function(y_true, predictions, outcome_type = get_outcome_type(y_true)) {
  if (outcome_type == "binary") {
    as.numeric(auc(roc(response = y_true, predictor = predictions[, 2], quiet = TRUE)))
  } else {
    as.numeric(multiclass.roc(response = y_true, predictor = predictions, quiet = TRUE)$auc)
  }
}

# One-hot encode a numeric outcome vector into a data.table of indicator columns.
# Column names are paste0(prefix, level) for each unique level of y.
make_onehot_Y <- function(y, prefix = "Y") {
  lvls <- sort(unique(na.omit(y)))
  dt   <- as.data.table(setNames(
    lapply(lvls, function(l) as.integer(y == l)),
    paste0(prefix, lvls)
  ))
  dt
}
