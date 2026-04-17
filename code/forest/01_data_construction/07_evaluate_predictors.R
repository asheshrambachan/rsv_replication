library(data.table)
library(ranger)
library(pROC)

# =============================================================================
# Config
# =============================================================================
OUTCOME <- "Y_bin80"

TRAIN_PATH <- "data/interim/forest/uganda_train.csv"
SIM_PATH   <- "data/interim/forest/uganda_sim.csv"
PRED_DIR   <- "data/interim/forest/predictors"
OUT_DIR    <- "data/clean/forest"

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

model_path <- file.path(PRED_DIR, paste0("uganda", "_", OUTCOME, "_rf.Rds"))
stopifnot(file.exists(model_path))
rf_model <- readRDS(model_path)

pred_train <- predict(rf_model, data = X_train)$predictions
pred_sim   <- predict(rf_model, data = X_sim)$predictions

auc_train <- compute_auc(dt_train[[OUTCOME]], pred_train)
auc_sim   <- compute_auc(dt_sim[[OUTCOME]],   pred_sim)
cat("  Train AUC:", round(auc_train, 4), "\n")
cat("  Sim   AUC:", round(auc_sim,   4), "\n")

# =============================================================================
# Predictions CSV (sim only)
# =============================================================================
s_cols   <- grep("^S_e$|^S_o_", names(dt_sim), value = TRUE)
out_dt   <- cbind(dt_sim[, .(lon, lat, Y = get(OUTCOME))], dt_sim[, ..s_cols])
pred_dt  <- as.data.table(pred_sim)
setnames(pred_dt, names(pred_dt), paste0("R", names(pred_dt)))
out_dt   <- cbind(out_dt, make_onehot_Y(out_dt$Y, prefix = "Y"), pred_dt)

out_path <- file.path(OUT_DIR, paste0("uganda", "_sim_", OUTCOME, "_w_pred.csv"))
fwrite(out_dt, out_path)
cat("  Wrote:", out_path, "\n")

# =============================================================================
# Save metrics
# =============================================================================
results <- rbindlist(list(
  data.table(country = "uganda", outcome = OUTCOME, split = "train", metric = "AUC", value = auc_train),
  data.table(country = "uganda", outcome = OUTCOME, split = "sim",   metric = "AUC", value = auc_sim)
))

out_rds <- file.path(PRED_DIR, paste0("evaluation_", OUTCOME, ".Rds"))
saveRDS(results, out_rds)
cat("\nSaved metrics to:", out_rds, "\n\n")
print(results)
