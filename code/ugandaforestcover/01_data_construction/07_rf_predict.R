# =============================================================================
# Uganda Forest Cover — Apply Random Forest Predictor
#
# Loads a fitted random forest model and generates predicted class
# probabilities on both the training and simulation splits. Reports AUC on
# both splits as a diagnostic and saves predictions for the simulation split
# as the remote sensing predictor used in assumption tests and simulations.
#
# Input:
#   data/interim/ugandaforestcover/data_train.csv
#   data/interim/ugandaforestcover/data_sim.csv
#   data/interim/ugandaforestcover/predictors/rf_<outcome>.Rds
#
# Outputs:
#   data/clean/ugandaforestcover/data_<outcome>.csv   (sim predictions)
#   data/clean/ugandaforestcover/rf_auc_<outcome>.csv (AUC metrics)
# =============================================================================

library(data.table)
library(ranger)
library(pROC)
source("code/utils/rf_utils.R")

# =============================================================================
# Config
# =============================================================================
OUTCOME <- "Ybin"

TRAIN_PATH <- "data/interim/ugandaforestcover/data_train.csv"
SIM_PATH   <- "data/interim/ugandaforestcover/data_sim.csv"
PRED_DIR   <- "data/interim/ugandaforestcover/predictors"
OUT_DIR    <- "data/clean/ugandaforestcover"

# =============================================================================
# Load data
# =============================================================================
stopifnot(file.exists(TRAIN_PATH), file.exists(SIM_PATH))
dt_train <- fread(TRAIN_PATH)
dt_sim   <- fread(SIM_PATH)
cat("Loaded", nrow(dt_train), "train /", nrow(dt_sim), "sim observations\n")

feature_cols <- grep("^X_[0-9]+$", names(dt_sim), value = TRUE)
stopifnot(length(feature_cols) > 0, all(feature_cols %in% names(dt_train)))
X_train <- as.matrix(dt_train[, ..feature_cols])
X_sim   <- as.matrix(dt_sim[,   ..feature_cols])

# =============================================================================
# Evaluate
# =============================================================================
cat("\nEvaluating", OUTCOME, "...\n")

model_path <- file.path(PRED_DIR, paste0("rf_", OUTCOME, ".Rds"))
stopifnot(file.exists(model_path))
rf_model <- readRDS(model_path)

pred_train <- predict(rf_model, data = X_train)$predictions
pred_sim   <- predict(rf_model, data = X_sim)$predictions

auc_train <- compute_auc(dt_train[[OUTCOME]], pred_train)
auc_sim   <- compute_auc(dt_sim[[OUTCOME]],   pred_sim)
cat("  Train AUC:", round(auc_train, 4), "\n")
cat("  Sim   AUC:", round(auc_sim,   4), "\n")

# =============================================================================
# Save metrics
# =============================================================================
results <- rbindlist(list(
  data.table(outcome = OUTCOME, split = "train", AUC = auc_train),
  data.table(outcome = OUTCOME, split = "sim",   AUC = auc_sim)
))

out_csv <- file.path(OUT_DIR, paste0("rf_auc_", OUTCOME, ".csv"))
fwrite(results, out_csv)
cat("\nSaved metrics to:", out_csv, "\n\n")
print(results)

# =============================================================================
# Predictions CSV (sim only)
# =============================================================================
s_cols   <- grep("^S_e$|^S_o_", names(dt_sim), value = TRUE)
out_dt   <- cbind(dt_sim[, .(lon, lat, Y = get(OUTCOME))], dt_sim[, ..s_cols])
pred_dt  <- as.data.table(pred_sim)
setnames(pred_dt, names(pred_dt), paste0("R", names(pred_dt)))
out_dt   <- cbind(out_dt, make_onehot_Y(out_dt$Y, prefix = "Y"), pred_dt)

out_path <- file.path(OUT_DIR, paste0("data_", OUTCOME, ".csv"))
fwrite(out_dt, out_path)
cat("  Wrote:", out_path, "\n")
