# Run from the project root: Rscript tests/test_secondary_calibration.R
source("R/secondary_calibration.R")

expect_reason <- function(expr, reason) {
  e <- tryCatch(force(expr), error = identity)
  stopifnot(inherits(e, "secondary_target_error"), identical(e$reason, reason))
  e
}
# Two observational Y groups; row 6 overlaps; experimental outcomes/secondary
# measurements are deliberately missing.
Y <- c(0, 0, 0, 1, 1, 1, NA, NA)
T <- c(0, 0, 1, 0, 1, 1, NA, NA)
S_o <- c(rep(TRUE, 6), FALSE, FALSE)
S_e <- c(rep(FALSE, 5), TRUE, TRUE, TRUE)
state <- fit_secondary_targets(Y, T, S_o, S_e)
q <- apply_secondary_targets(state, Y, S_o, T, S_e)
expected <- cbind(
  c(-1/3, -1/3, 2/3, 0, 0, 0, 0, 0),
  c(0, 0, 0, -2/3, 1/3, 1/3, 0, 0)
)
stopifnot(max(abs(q - expected)) < 1e-14, max(abs(colMeans(q))) < 1e-14)
stopifnot(q[6, 2] != 0, all(q[7:8, ] == 0), state$n_overlap_training == 1L)
stopifnot(identical(state$groups[[1]]$n_observational, 3L))
# Version-1 states used the same binary means/transforms, without the new
# specification metadata or group-specific target-column mapping.
legacy <- state
legacy$version <- 1L
stopifnot(identical(q, apply_secondary_targets(legacy, Y, S_o, T, S_e)))
# Applying state to new data uses TRAINING means and supports all-experimental
# input with no secondary measurements at all.
applied <- apply_secondary_targets(state, c(0, 1, NA), c(TRUE, TRUE, FALSE),
                                   c(1, 0, NA), c(FALSE, FALSE, TRUE))
stopifnot(max(abs(applied - rbind(c(2/3, 0), c(0, -2/3), c(0, 0)))) < 1e-14)
stopifnot(all(apply_secondary_targets(state, c(NA_real_, NA_real_), c(FALSE, FALSE)) == 0))
# Normalization/scaling use population covariance and pooled JOINT cell mass.
normalized <- fit_secondary_targets(Y, T, S_o, S_e,
                                    normalize_covariance = TRUE, scale_cells = TRUE)
qn <- apply_secondary_targets(normalized, Y, S_o, T, S_e)
stopifnot(max(abs(crossprod(qn) / length(Y) - diag(2))) < 1e-13)
stopifnot(all.equal(normalized$groups[[1]]$cell_probability, 3/8))
# Centering/scaling is preserved through serialization.
path <- tempfile()
saveRDS(normalized, path)
stopifnot(identical(qn, apply_secondary_targets(readRDS(path), Y, S_o, T, S_e)))
unlink(path)

badT <- T
badT[2] <- NA
e <- expect_reason(fit_secondary_targets(Y, badT, S_o, S_e), "missing_secondary")
stopifnot(identical(e$rows, 2L))
invisible(expect_reason(apply_secondary_targets(state, Y, S_o, badT, S_e), "missing_secondary"))
badY <- Y
badY[1] <- NA
invisible(expect_reason(fit_secondary_targets(badY, T, S_o, S_e), "missing_outcome"))
e <- expect_reason(fit_secondary_targets(Y, T, S_o, S_e, y_levels = c(0, 1, 2)),
                   "invalid_groups")
stopifnot(e$diagnostics$status[3] == "empty_group")
constant <- T
constant[1:3] <- 0
constant_state <- fit_secondary_targets(Y, constant, S_o, S_e)
constant_q <- apply_secondary_targets(constant_state, Y, S_o, constant, S_e)
stopifnot(constant_state$diagnostics$status[1] == "no_secondary_variation",
          ncol(constant_q) == 1L, all(constant_q[1:3, ] == 0))
# An explicit continuous basis is allowed and must be full-rank in each group.
continuous <- c(0, 1, 2, 0, 2, 4, NA, NA)
basis <- function(x) cbind(linear = x[[1]], quadratic = x[[1]]^2)
s <- fit_secondary_targets(Y, continuous, S_o, S_e, basis = basis)
qb <- apply_secondary_targets(s, Y, S_o, continuous, S_e)
stopifnot(ncol(qb) == 4L, max(abs(colMeans(qb))) < 1e-14)
redundant <- function(x) cbind(a = x[[1]], b = 2 * x[[1]])
e <- expect_reason(fit_secondary_targets(Y, continuous, S_o, S_e, basis = redundant),
                   "invalid_groups")
stopifnot(all(e$diagnostics$status == "singular_covariance"))
# Reject continuous inputs without a deliberately specified basis, and X until
# conditional centering is implemented.
stopifnot(inherits(tryCatch(fit_secondary_targets(Y, continuous, S_o, S_e),
                           error = identity), "error"))
stopifnot(inherits(tryCatch(fit_secondary_targets(Y, T, S_o, S_e, X = rep(0, 8)),
                           error = identity), "error"))
# Verify transformation functions cannot read experimental-only secondary data.
observational_only_basis <- function(x) {
  stopifnot(nrow(x) == 6L, !anyNA(x))
  cbind(value = x[[1]])
}
invisible(fit_secondary_targets(Y, T, S_o, S_e, basis = observational_only_basis))
cat("PASS: target formula, overlap, missingness, support, basis rank, normalization,\n",
    "frozen-state application, serialization, and observational-only transformations.\n", sep = "")

# Explicit polynomial bases retain the original continuous measurements.
polynomial <- fit_secondary_targets(
  Y, continuous, S_o, S_e, basis = secondary_polynomial_basis(2)
)
stopifnot(identical(polynomial$basis_type, "polynomial"),
          identical(polynomial$calibration_specification$scope, "selected_moments"),
          polynomial$calibration_specification$span_assumption_required,
          !polynomial$calibration_specification$full_conditional_calibration_established,
          !polynomial$normalize_covariance, !polynomial$scale_cells)
stopifnot(max(abs(apply_secondary_targets(polynomial, Y, S_o, continuous, S_e) - qb)) < 1e-14)
one <- apply_secondary_targets(polynomial, 0, TRUE, 1, FALSE)
stopifnot(identical(dim(one), c(1L, 4L)))
# Selected moment balance need not imply mean-independence. A cubic residual
# is orthogonal to 1,T,T^2 but its conditional mean varies with T.
ct <- rep(-3:3, 2)
cy <- rep(c(0, 1), each = 7)
cs <- fit_secondary_targets(cy, ct, rep(TRUE, 14), basis = secondary_polynomial_basis(2))
cq <- apply_secondary_targets(cs, cy, rep(TRUE, 14), ct)
h <- residuals(lm(I(ct^3) ~ ct + I(ct^2)))
stopifnot(max(abs(colMeans(cq * h))) < 1e-12, stats::var(h) > 0)

# Joint indicators span all retained joint categories, not just marginals.
grid <- expand.grid(a = 0:1, b = 0:1)
joint <- grid[rep(1:4, 4), ]
jy <- rep(c(0, 1), each = 8)
jo <- rep(TRUE, 16)
js <- fit_secondary_targets(jy, joint, jo, basis = secondary_joint_basis())
jq <- apply_secondary_targets(js, jy, jo, joint)
stopifnot(ncol(jq) == 6L, max(abs(colMeans(jq))) < 1e-14,
          identical(js$calibration_specification$scope, "joint_category_moments"),
          !js$calibration_specification$span_assumption_required,
          !js$calibration_specification$population_support_coverage_verified,
          !js$calibration_specification$full_conditional_calibration_established)
for (g in js$groups) stopifnot(nrow(g$support) == 4L, length(g$target_columns) == 3L)
# XOR has equal means by each marginal category, but differs by joint category.
marginal <- function(x) as.matrix(x)
ms <- fit_secondary_targets(jy, joint, jo, basis = marginal)
mq <- apply_secondary_targets(ms, jy, jo, joint)
xor_h <- as.numeric(joint$a != joint$b)
stopifnot(max(abs(colMeans(mq * xor_h))) < 1e-14,
          max(abs(colMeans(jq * xor_h))) > 0.01,
          ms$calibration_specification$span_assumption_required)

# Each Y group freezes its own support and reference category.
unequal <- data.frame(a = c(0, 1, 2, 0, 1, 2, 0, 1, 0, 1),
                      b = c("a", "b", "c", "a", "b", "c", "a", "b", "a", "b"))
uy <- c(rep(0, 6), rep(1, 4))
us <- fit_secondary_targets(uy, unequal, rep(TRUE, 10), basis = secondary_joint_basis())
uq <- apply_secondary_targets(us, uy, rep(TRUE, 10), unequal)
stopifnot(ncol(uq) == 3L,
          identical(vapply(us$groups, function(g) length(g$target_columns), integer(1)), c(2L, 1L)),
          max(abs(colMeans(uq))) < 1e-14)
invisible(expect_reason(
  apply_secondary_targets(us, 1, TRUE, data.frame(a = 2, b = "c")), "unseen_category"
))
invisible(expect_reason(
  apply_secondary_targets(constant_state, 0, TRUE, 1), "unseen_category"
))
# No-variation groups contribute no restriction, even with normalization enabled.
zero <- fit_secondary_targets(c(0, 0, 1, 1), c(0, 0, 1, 1), rep(TRUE, 4),
                              normalize_covariance = TRUE, scale_cells = TRUE)
zero_q <- apply_secondary_targets(zero, c(0, 0, 1, 1), rep(TRUE, 4), c(0, 0, 1, 1))
stopifnot(identical(dim(zero_q), c(4L, 0L)),
          all(zero$diagnostics$status == "no_secondary_variation"))

# String/factor categories work jointly; missing observational categories fail.
factor_joint <- data.frame(a = factor(joint$a), b = ifelse(joint$b == 1, "yes", "no"))
fs <- fit_secondary_targets(jy, factor_joint, jo, basis = secondary_joint_basis())
stopifnot(ncol(apply_secondary_targets(fs, jy, jo, factor_joint)) == 6L)
factor_joint$a[3] <- NA
invisible(expect_reason(fit_secondary_targets(jy, factor_joint, jo,
                                             basis = secondary_joint_basis()), "missing_secondary"))

# The existing binary artifacts remain readable; fitting the same observations
# with the extended module gives numerically identical unscaled binary targets.
legacy_paths <- list.files(
  "data/interim/smartcards/secondary_targets/ent_own_share_threshold_0_above",
  pattern = "[.]Rds$", recursive = TRUE, full.names = TRUE
)
for (path in legacy_paths) {
  old <- readRDS(path)
  stopifnot(identical(old$q, apply_secondary_targets(
    old$state, old$Y, old$S_o, old$secondary, old$S_e
  )))
  updated <- fit_secondary_targets(old$Y, old$secondary, old$S_o, old$S_e)
  updated_q <- apply_secondary_targets(updated, old$Y, old$S_o, old$secondary, old$S_e)
  stopifnot(identical(old$q, updated_q), !updated$normalize_covariance, !updated$scale_cells)
}
source("code/smartcards/04_empirical/04_secondary_targets.R")
args <- parse_smartcards_secondary_target_args(c("ent_own_share", "--basis", "polynomial", "--degree", "2"))
stopifnot(identical(args$basis$type, "polynomial"), is.null(args$threshold), is.null(args$percentile))
args <- parse_smartcards_secondary_target_args(c("a,b", "--basis", "joint"))
stopifnot(identical(args$secondary_vars, c("a", "b")), identical(args$basis$type, "joint_categories"))
stopifnot(inherits(tryCatch(parse_smartcards_secondary_target_args(
  c("a", "--basis", "polynomial", "--degree", "2", "--threshold", "0")
), error = identity), "error"))
cat("PASS: continuous selected moments, joint vs marginal categories, frozen supports,\n",
    "no-variation groups, span metadata, CLI options, and existing binary artifacts.\n", sep = "")

# End-to-end driver checks on a temporary fixture, without changing real results.
task_dir <- tempfile("secondary-target-check-")
dir.create(task_dir)
waves <- c("Experimental: Treated (2010)", "Experimental: Untreated (2011)",
           "Experimental: Untreated (2012)", "Observational (N/A)")
fixture <- data.frame(
  shrid2 = seq_len(48), clusters = paste0("m", rep(1:8, each = 6)),
  wave = rep(waves, each = 12), Ycons = rep(rep(0:1, each = 6), 4),
  Ylowinc = rep(rep(0:1, each = 6), 4),
  Ymidinc = rep(rep(1:0, each = 6), 4),
  ent_own_share = rep((0:5)/5, 8), category = rep(c("a", "b", "c"), 16)
)
fixture_path <- file.path(task_dir, "input.csv")
utils::write.csv(fixture, fixture_path, row.names = FALSE)
for (args in list(
  c("ent_own_share", "--threshold", "0"),
  c("ent_own_share", "--basis", "polynomial", "--degree", "2"),
  c("category", "--basis", "joint")
)) {
  invisible(capture.output(run <- suppressMessages(run_smartcards_secondary_targets(
    args, input_csv = fixture_path, output_dir = file.path(task_dir, "targets")
  ))))
  stopifnot(length(run$paths) == 6L, all(run$diagnostics$max_abs_mean_q < 1e-12),
            !any(run$diagnostics$full_conditional_calibration_established))
  for (path in run$paths) {
    artifact <- readRDS(path)
    stopifnot(!artifact$state$normalize_covariance, !artifact$state$scale_cells,
              all(artifact$q[!artifact$S_o, , drop = FALSE] == 0))
  }
}
unlink(task_dir, recursive = TRUE)
cat("PASS: binary, polynomial and joint-category driver runs on temporary data.\n")
