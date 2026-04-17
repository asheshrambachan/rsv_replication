library(data.table)

# =============================================================================
# Simulation Utilities
# Primitive samplers and combined dataset generators shared across applications.
# Call get_R_given_Y() once per outcome to build the lookup, then pass it to
# sample_R_given_Y() and sim_noexpoutcomes() / sim_expoutcomes().
# =============================================================================


# -----------------------------------------------------------------------------
# Outcome samplers
# -----------------------------------------------------------------------------

# Sample Y for experimental units given treatment assignment D.
# Uses outcome-specific treatment probabilities p0 (D=0) and p1 (D=1).
sample_Y_exp <- function(D, y_levels, p0, p1) {
  n <- length(D)
  K <- length(y_levels)
  Y_idx <- integer(n)

  n0 <- sum(D == 0)
  n1 <- n - n0

  if (n0) Y_idx[D == 0] <- sample.int(K, n0, replace = TRUE, prob = p0)
  if (n1) Y_idx[D == 1] <- sample.int(K, n1, replace = TRUE, prob = p1)

  y_levels[Y_idx]
}


# Sample Y for observational units from the marginal outcome distribution alpha.
sample_Y_obs <- function(n, y_levels, alpha) {
  K     <- length(y_levels)
  Y_idx <- sample.int(K, size = n, replace = TRUE, prob = alpha)
  y_levels[Y_idx]
}


# -----------------------------------------------------------------------------
# RSV sampler
# -----------------------------------------------------------------------------

# Load the RSV matrix from disk and group rows by outcome level.
# Returns a list of length K where element k contains all RSV rows observed
# at y_levels[k]. Pass the result to sample_R_given_Y().
#
# Arguments:
#   outcome    - outcome name (used to construct data_path or select y_col)
#   data_path  - path to the CSV file containing R and Y columns
#   r_col_fn   - function(col_names) -> character vector of R column names to use
#   y_col      - name of the outcome column in the data (default "Y")
get_R_given_Y <- function(outcome, data_path, r_col_fn, y_col = "Y") {
  dt <- fread(data_path)

  R_cols <- r_col_fn(names(dt))
  R <- as.matrix(dt[, ..R_cols])
  Y <- dt[[y_col]]

  y_levels <- sort(unique(na.omit(Y)))

  R_given_Y <- vector("list", length(y_levels))
  for (i in seq_along(y_levels)) {
    idx <- which(Y == y_levels[i])
    R_given_Y[[i]] <- if (length(idx) > 0) R[idx, ] else matrix(0, 1, ncol(R))
  }
  R_given_Y
}


# Sample RSV rows for a vector of outcome values y_vals using the precomputed
# R_given_Y lookup. Draws with replacement within each outcome level.
sample_R_given_Y <- function(R_given_Y, y_vals, y_levels) {
  n       <- length(y_vals)
  lvl_idx <- match(y_vals, y_levels)
  out     <- matrix(NA_real_, nrow = n, ncol = ncol(R_given_Y[[1]]))

  for (k in seq_along(y_levels)) {
    idx <- which(lvl_idx == k)
    if (!length(idx)) next
    draw     <- sample.int(nrow(R_given_Y[[k]]), size = length(idx), replace = TRUE)
    out[idx, ] <- R_given_Y[[k]][draw, , drop = FALSE]
  }

  colnames(out) <- paste0("R_", colnames(R_given_Y[[1]]))
  out
}


# -----------------------------------------------------------------------------
# Combined dataset generators
# -----------------------------------------------------------------------------

# Generate one simulated dataset for binary_noexpoutcomes estimators.
#
# Experimental sample (n_e obs): D randomised, R observed, Y masked (S_e=1, S_o=0).
# Observational sample (n_o obs): Y and R observed, D = NA  (S_e=0, S_o=1).
#
# Returns a named list: Y, D, R, S_e, S_o.
sim_noexpoutcomes <- function(n_e, n_o, y_levels, p0, p1, alpha_o, R_given_Y) {

  # Experimental sample: generate Y to draw realistic R, then mask Y
  D_e <- rbinom(n_e, 1, 0.5)
  Y_e <- sample_Y_exp(D = D_e, y_levels = y_levels, p0 = p0, p1 = p1)
  R_e <- sample_R_given_Y(R_given_Y, Y_e, y_levels)
  Y_e <- rep(NA_real_, n_e)   # mask: Y unobserved in experimental sample

  # Observational sample: Y and R observed, D unassigned
  D_o <- rep(NA_real_, n_o)
  Y_o <- sample_Y_obs(n_o, y_levels, alpha_o)
  R_o <- sample_R_given_Y(R_given_Y, Y_o, y_levels)

  list(
    Y   = c(Y_e, Y_o),
    D   = c(D_e, D_o),
    R   = rbind(R_e, R_o),
    S_e = c(rep(1L, n_e), rep(0L, n_o)),
    S_o = c(rep(0L, n_e), rep(1L, n_o))
  )
}


# Generate one simulated dataset for binary_expoutcomes estimators.
#
# Returns a named list: Y, D, R, S_e, S_o.
sim_expoutcomes <- function(n_e, n_o, n_eo, y_levels, p0, p1, alpha_o, R_given_Y) {
  D_e <- rbinom(n_e, 1, 0.5)
  Y_e <- sample_Y_exp(D_e, y_levels, p0, p1)
  R_e <- sample_R_given_Y(R_given_Y, Y_e, y_levels)

  val_idx     <- sample.int(n_e, n_eo, replace = FALSE)
  exp_idx     <- setdiff(1:n_e, val_idx)
  n_exp_only  <- length(exp_idx)

  D_val <- D_e[val_idx]
  Y_val <- Y_e[val_idx]
  R_val <- R_e[val_idx, , drop = FALSE]

  D_exp <- D_e[exp_idx]
  Y_exp <- rep(NA_real_, n_exp_only)
  R_exp <- R_e[exp_idx, , drop = FALSE]

  D_obs <- rep(NA_real_, n_o)
  Y_obs <- sample_Y_obs(n_o, y_levels, alpha_o)
  R_obs <- sample_R_given_Y(R_given_Y, Y_obs, y_levels)

  list(
    Y   = c(Y_val, Y_exp, Y_obs),
    D   = c(D_val, D_exp, D_obs),
    R   = rbind(R_val, R_exp, R_obs),
    S_e = c(rep(1L, length(val_idx)), rep(1L, n_exp_only), rep(0L, n_o)),
    S_o = c(rep(1L, length(val_idx)), rep(0L, n_exp_only), rep(1L, n_o))
  )
}
