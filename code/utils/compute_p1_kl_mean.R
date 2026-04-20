# =============================================================================
# KL Tilting Utility
#
# Computes p1 = argmin_p KL(p || p0) subject to:
#   sum_k p_k = 1
#   sum_k y_k p_k = mu_target
#   p_k >= 0
#
# This is exponential tilting: p1_k ∝ p0_k * exp(lambda * y_k)
# where lambda is chosen so E_{p1}[y] = mu_target.
# =============================================================================

# -----------------------------------------------------------------------------
# Helper: validate prob vectors
# -----------------------------------------------------------------------------
assert_prob_vector <- function(p, K, tol = 1e-8) {
  stopifnot(
    is.numeric(p),
    length(p) == K,
    all(is.finite(p))
  )
  stopifnot(abs(sum(p) - 1) < tol)
}

# -----------------------------------------------------------------------------
# Main function
# -----------------------------------------------------------------------------
compute_p1_kl_mean <- function(p0, y, tau, tol = 1e-10) {
  p0 <- as.numeric(p0)
  y  <- as.numeric(y)

  stopifnot(length(p0) == length(y))
  stopifnot(all(is.finite(p0)))
  stopifnot(all(p0 >= 0))

  # Drop NA y-levels (and corresponding p0 mass), then renormalize
  keep_y <- !is.na(y)
  p0 <- p0[keep_y]
  y  <- y[keep_y]

  s0 <- sum(p0)
  if (!is.finite(s0) || s0 <= 0) stop("p0 has zero total mass after dropping NA y.", call. = FALSE)
  p0 <- p0 / s0

  mu0 <- sum(y * p0)
  mu_target <- mu0 + tau

  # Support restriction: KL tilt can't create mass where p0 == 0
  support <- p0 > 0
  p0s <- p0[support]
  ys  <- y[support]

  y_min <- min(ys)
  y_max <- max(ys)
  if (mu_target < y_min - 1e-12 || mu_target > y_max + 1e-12) {
    stop(sprintf(
      "Infeasible target mean: mu_target=%.6g not in [%.6g, %.6g] given support(p0>0).",
      mu_target, y_min, y_max
    ), call. = FALSE)
  }

  tilted_probs_on_support <- function(lambda) {
    z <- lambda * ys
    z <- z - max(z)     # stabilize
    w <- p0s * exp(z)
    w / sum(w)
  }

  mean_given_lambda <- function(lambda) {
    p <- tilted_probs_on_support(lambda)
    sum(ys * p)
  }

  # Degenerate y on support => mean fixed
  if (max(ys) - min(ys) < tol) {
    if (abs(mu_target - ys[1]) > 1e-8) {
      stop("Degenerate y on support: cannot change mean with tilting.", call. = FALSE)
    }
    return(p0)  # p1 = p0 (on reduced y)
  }

  f <- function(lambda) mean_given_lambda(lambda) - mu_target

  lo <- -50; hi <- 50
  flo <- f(lo); fhi <- f(hi)

  iter <- 0
  while (flo > 0 && iter < 20) { lo <- lo * 2; flo <- f(lo); iter <- iter + 1 }
  iter <- 0
  while (fhi < 0 && iter < 20) { hi <- hi * 2; fhi <- f(hi); iter <- iter + 1 }

  if (flo > 0 || fhi < 0) {
    stop("Failed to bracket lambda for root finding. Check y/p0/tau scaling.", call. = FALSE)
  }

  lambda_hat <- uniroot(f, lower = lo, upper = hi, tol = tol)$root

  p1 <- numeric(length(p0))
  p1[support] <- tilted_probs_on_support(lambda_hat)

  if (abs(sum(p1) - 1) > 1e-8) stop("p1 did not normalize to 1.", call. = FALSE)
  if (abs(sum(y * p1) - mu_target) > 1e-6) stop("p1 mean did not hit target.", call. = FALSE)

  p1
}
