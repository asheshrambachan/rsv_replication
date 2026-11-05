# Secondary-outcome balancing targets (issue #8).
#
# Without baseline X, q_y = 1{S_o=1,Y=y} [basis(secondary) - mu_y].
# Fit centering/normalization on the designated estimation records, then apply
# that frozen state to any other records. Applying never re-estimates means.
# This module constructs q only: it does not fit g, adjust H, or calibrate H.
#
# Default basis: one binary secondary measurement. Alternatively use
# secondary_polynomial_basis(degree) or secondary_joint_basis(). For a fixed basis,
# supply function(data_frame) returning a numeric vector/matrix/data.frame.
# It is evaluated ONLY on observational records. The function must be stateless;
# learned transformations must be estimated separately on the intended fold.
# A finite continuous basis imposes selected moments. Full conditional
# calibration additionally requires the appendix span assumption for H0 and
# every component of g/sigma2; constructing q does not establish this assumption.
# Several binary measurements likewise require joint-category
# indicators to encode full joint conditional-mean calibration. Omit one
# reference category to avoid redundant centered indicator columns.
#
# Optional normalization uses within-Y population covariance (divisor n_y).
# Optional cell scaling divides by sqrt(n_y / n), with overlap counted once
# in n. Both are off by default, matching main_revision.tex's centered q.
#
# X is intentionally unsupported. With X, centering must condition on (X,Y),
# and normalization/scaling must use a specified conditional-X design.
# Do not silently pool X strata. Continuous X needs a separate nuisance model.

.secondary_target_stop <- function(reason, message, rows = integer(), diagnostics = NULL) {
  stop(structure(list(message = message, call = NULL, reason = reason,
                      rows = rows, diagnostics = diagnostics),
                 class = c("secondary_target_error", "error", "condition")))
}

.secondary_target_roles <- function(Y, S_o, S_e, y_levels, X) {
  if (!is.null(X)) stop("X is not supported yet: centering must condition on (X,Y).")
  n <- length(Y)
  if (!n || !(is.numeric(Y) || is.logical(Y)) || !is.null(dim(Y))) {
    stop("Y must be a nonempty numeric or logical vector.")
  }
  if (!is.numeric(y_levels) || !length(y_levels) || anyNA(y_levels) ||
      any(!is.finite(y_levels)) || anyDuplicated(y_levels)) {
    stop("y_levels must contain distinct finite numeric outcome levels.")
  }
  check_role <- function(x, name) {
    if (length(x) != n || !is.null(dim(x)) ||
        !(is.numeric(x) || is.logical(x)) || anyNA(x) || any(!x %in% c(0, 1))) {
      stop(name, " must contain one nonmissing 0/1 indicator per record.")
    }
    as.logical(x)
  }
  S_o <- check_role(S_o, "S_o")
  if (is.null(S_e)) S_e <- !S_o else S_e <- check_role(S_e, "S_e")
  if (any(!S_o & !S_e)) stop("Every record must have an experimental or observational role.")
  bad <- which(S_o & (is.na(Y) | !is.finite(Y)))
  if (length(bad)) .secondary_target_stop(
    "missing_outcome", "Observational Y is missing or nonfinite; records were not dropped.", bad
  )
  bad <- which(S_o & !Y %in% y_levels)
  if (length(bad)) .secondary_target_stop(
    "unknown_outcome", "Observational Y contains levels outside y_levels.", bad
  )
  list(n = n, S_o = S_o, S_e = S_e)
}

.secondary_target_basis <- function(secondary, roles, basis, expected = NULL) {
  obs <- which(roles$S_o)
  if (is.null(secondary)) {
    if (length(obs)) .secondary_target_stop(
      "missing_secondary", "Secondary measurements are required for observational records.", obs
    )
    return(NULL)
  }
  if (is.numeric(secondary) || is.logical(secondary)) {
    if (is.null(dim(secondary))) secondary <- data.frame(secondary = secondary)
  }
  if (!(is.matrix(secondary) || is.data.frame(secondary))) {
    stop("secondary must be a numeric vector, matrix, or data frame.")
  }
  secondary <- as.data.frame(secondary)
  if (nrow(secondary) != roles$n || !ncol(secondary) ||
      anyDuplicated(names(secondary)) || any(!nzchar(names(secondary)))) {
    stop("secondary must have one row per record and unique nonempty column names.")
  }
  if (!all(vapply(secondary, function(x) is.numeric(x) || is.logical(x), logical(1)))) {
    stop("Secondary columns must be numeric or logical; encode categories explicitly.")
  }
  if (!is.null(expected) && !identical(names(secondary), expected)) {
    stop("Secondary columns must match the fitted state's names and order.")
  }
  x <- secondary[obs, , drop = FALSE]
  if (!length(obs)) return(NULL)
  bad <- which(!stats::complete.cases(x) | rowSums(!is.finite(as.matrix(x))) > 0)
  if (length(bad)) .secondary_target_stop(
    "missing_secondary",
    "Observational secondary measurements are missing or nonfinite; no imputation or deletion applied.",
    obs[bad]
  )
  if (is.null(basis)) {
    if (ncol(x) != 1L || any(!as.matrix(x) %in% c(0, 1))) {
      stop("The default basis requires one binary secondary column. Supply a fixed basis explicitly otherwise.")
    }
    b <- as.matrix(x)
  } else {
    if (!is.function(basis)) stop("basis must be NULL or a fixed transformation function.")
    b <- basis(x)
    if (is.numeric(b) || is.logical(b)) {
      if (is.null(dim(b))) b <- matrix(b, ncol = 1L)
    }
    if (is.data.frame(b)) {
      if (!all(vapply(b, is.numeric, logical(1)))) stop("The basis must be numeric.")
      b <- as.matrix(b)
    }
    if (!is.matrix(b) || !(is.numeric(b) || is.logical(b))) stop("The basis must be numeric.")
  }
  if (nrow(b) != length(obs) || !ncol(b)) stop("The basis must return one row per observational record.")
  if (anyNA(b) || any(!is.finite(b))) .secondary_target_stop(
    "invalid_basis", "The basis produced missing or nonfinite observational values.",
    obs[which(rowSums(!is.finite(b)) > 0)]
  )
  if (is.null(colnames(b))) colnames(b) <- paste0("basis", seq_len(ncol(b)))
  if (anyNA(colnames(b)) || any(!nzchar(colnames(b))) || anyDuplicated(colnames(b))) {
    stop("Basis columns must have unique nonempty names.")
  }
  storage.mode(b) <- "double"
  list(values = b, rows = obs, secondary_names = names(secondary))
}

#' Declare a finite raw-power basis for continuous measurements.
#' No intercept is included because the targets are centered within Y.
#' Separate powers of several columns are selected moments, not a joint basis.
secondary_polynomial_basis <- function(degree) {
  if (!is.numeric(degree) || length(degree) != 1L || !is.finite(degree) ||
      degree < 1 || degree > .Machine$integer.max || degree != as.integer(degree)) {
    stop("degree must be a positive integer.")
  }
  structure(list(type = "polynomial", degree = as.integer(degree)),
            class = "secondary_basis_spec")
}

#' Declare joint-category indicators for finite-support secondary measurements.
#' The joint support and omitted reference are estimated separately within each
#' observational Y group. Supports are frozen when applying the state; unseen
#' categories are errors. An observed support need not exhaust population support.
secondary_joint_basis <- function() {
  structure(list(type = "joint_categories"), class = "secondary_basis_spec")
}

.secondary_target_input <- function(secondary, roles, expected = NULL) {
  obs <- which(roles$S_o)
  if (is.null(secondary)) {
    if (length(obs)) .secondary_target_stop(
      "missing_secondary", "Secondary measurements are required for observational records.", obs
    )
    return(NULL)
  }
  if (is.atomic(secondary) && is.null(dim(secondary))) {
    secondary <- data.frame(secondary = secondary)
  }
  if (!(is.matrix(secondary) || is.data.frame(secondary))) {
    stop("secondary must be a vector, matrix, or data frame.")
  }
  secondary <- as.data.frame(secondary)
  if (nrow(secondary) != roles$n || !ncol(secondary) ||
      anyNA(names(secondary)) || anyDuplicated(names(secondary)) ||
      any(!nzchar(names(secondary)))) {
    stop("secondary must have one row per record and unique nonempty column names.")
  }
  allowed <- vapply(secondary, function(x) {
    (is.numeric(x) || is.logical(x) || is.character(x) || is.factor(x)) && is.null(dim(x))
  }, logical(1))
  if (!all(allowed)) stop("Unsupported secondary-column type.")
  if (!is.null(expected) && !identical(names(secondary), expected)) {
    stop("Secondary columns must match the fitted state's names and order.")
  }
  for (v in names(secondary)) if (is.factor(secondary[[v]])) {
    secondary[[v]] <- as.character(secondary[[v]])
  }
  x <- secondary[obs, , drop = FALSE]
  if (!length(obs)) return(NULL)
  bad <- !stats::complete.cases(x)
  for (v in names(x)) if (is.numeric(x[[v]])) bad <- bad | !is.finite(x[[v]])
  if (any(bad)) .secondary_target_stop(
    "missing_secondary",
    "Observational secondary measurements are missing or nonfinite; no imputation or deletion applied.",
    obs[bad]
  )
  list(values = x, rows = obs, secondary_names = names(secondary))
}

.secondary_basis_type <- function(basis) {
  if (is.null(basis)) return("binary")
  if (is.function(basis)) return("custom")
  if (inherits(basis, "secondary_basis_spec") &&
      basis$type %in% c("polynomial", "joint_categories")) return(basis$type)
  stop("basis must be NULL, a fixed function, or a secondary basis specification.")
}

.secondary_evaluate_basis <- function(x, basis, type) {
  if (!all(vapply(x, function(v) is.numeric(v) || is.logical(v), logical(1)))) {
    stop("Binary and continuous bases require numeric columns; use secondary_joint_basis() for categories.")
  }
  if (type == "binary") {
    if (ncol(x) != 1L || any(!as.matrix(x) %in% c(0, 1))) {
      stop("The default basis requires one binary secondary column. Specify a basis explicitly otherwise.")
    }
    b <- as.matrix(x)
  } else if (type == "polynomial") {
    b <- do.call(cbind, lapply(x, function(v) {
      powers <- matrix(0, nrow = nrow(x), ncol = basis$degree)
      for (k in seq_len(basis$degree)) powers[, k] <- v^k
      powers
    }))
    colnames(b) <- unlist(lapply(names(x), function(v) {
      paste0(v, "_power", seq_len(basis$degree))
    }))
  } else {
    b <- basis(x)
    if ((is.numeric(b) || is.logical(b)) && is.null(dim(b))) b <- matrix(b, ncol = 1L)
    if (is.data.frame(b)) {
      if (!all(vapply(b, function(v) is.numeric(v) || is.logical(v), logical(1)))) {
        stop("The basis must be numeric.")
      }
      b <- as.matrix(b)
    }
  }
  if (!is.matrix(b) || !(is.numeric(b) || is.logical(b)) ||
      nrow(b) != nrow(x) || !ncol(b)) {
    stop("The basis must return a numeric matrix with one row per observational record.")
  }
  if (anyNA(b) || any(!is.finite(b))) .secondary_target_stop(
    "invalid_basis", "The basis produced missing or nonfinite observational values."
  )
  if (is.null(colnames(b))) colnames(b) <- paste0("basis", seq_len(ncol(b)))
  if (anyNA(colnames(b)) || any(!nzchar(colnames(b))) || anyDuplicated(colnames(b))) {
    stop("Basis columns must have unique nonempty names.")
  }
  storage.mode(b) <- "double"
  b
}

.secondary_joint_match <- function(x, support) {
  result <- rep(NA_integer_, nrow(x))
  for (k in seq_len(nrow(support))) {
    hit <- rep(TRUE, nrow(x))
    for (v in names(x)) hit <- hit & x[[v]] == support[[v]][k]
    result[hit] <- k
  }
  result
}

.secondary_joint_support <- function(x) {
  support <- unique(x)
  support <- support[do.call(order, c(unname(as.list(support)), list(method = "radix"))), , drop = FALSE]
  rownames(support) <- NULL
  support
}

.secondary_joint_indicators <- function(x, support) {
  category <- .secondary_joint_match(x, support)
  if (anyNA(category)) stop("Internal joint-support mismatch.")
  k <- nrow(support)
  b <- matrix(0, nrow = nrow(x), ncol = max(0L, k - 1L))
  if (k > 1L) {
    for (j in 2:k) b[, j - 1L] <- as.integer(category == j)
    colnames(b) <- paste0("joint_category", 2:k)
  }
  b
}

#' Estimate observational centering and optional normalization state.
#'
#' The default encodes a single binary outcome. Continuous/custom bases encode
#' selected moments. Full conditional calibration then additionally requires
#' the conditional means of H0 and EVERY component of g/sigma2 to lie in the
#' intercept-plus-basis span (main_revision.tex, Calibration specification).
#' A joint-category basis can span finite conditional support, provided that
#' population support is covered. Neither assumption is established by this fit.
#' No-variation groups contribute zero restrictions; empty Y groups and other
#' insufficient-support/rank failures remain errors with diagnostics.
fit_secondary_targets <- function(
    Y, secondary, S_o, S_e = NULL, y_levels = c(0, 1), basis = NULL,
    normalize_covariance = FALSE, scale_cells = FALSE, rank_tolerance = 1e-10,
    X = NULL) {
  for (flag in list(normalize_covariance, scale_cells)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) {
      stop("Normalization flags must be TRUE or FALSE.")
    }
  }
  if (!is.numeric(rank_tolerance) || length(rank_tolerance) != 1L ||
      !is.finite(rank_tolerance) || rank_tolerance <= 0 || rank_tolerance >= 1) {
    stop("rank_tolerance must lie strictly between zero and one.")
  }
  roles <- .secondary_target_roles(Y, S_o, S_e, y_levels, X)
  if (!any(roles$S_o)) .secondary_target_stop("empty_group", "No observational records.")
  input <- .secondary_target_input(secondary, roles)
  type <- .secondary_basis_type(basis)
  b <- if (type != "joint_categories") .secondary_evaluate_basis(input$values, basis, type)
  scope <- if (type %in% c("binary", "joint_categories")) "joint_category_moments" else {
    "selected_moments"
  }
  span_required <- scope == "selected_moments"
  groups <- diagnostics <- vector("list", length(y_levels))
  target_names <- character()
  for (j in seq_along(y_levels)) {
    idx <- which(Y[input$rows] == y_levels[j])
    x <- input$values[idx, , drop = FALSE]
    n_y <- length(idx)
    support <- .secondary_joint_support(x)
    no_variation <- n_y > 0L && nrow(support) == 1L
    if (type == "joint_categories") {
      group_basis <- .secondary_joint_indicators(x, support)
    } else {
      group_basis <- b[idx, , drop = FALSE]
      if (no_variation) group_basis <- matrix(0, nrow = n_y, ncol = 0L)
    }
    p <- ncol(group_basis)
    mu <- if (n_y) colMeans(group_basis) else rep(NA_real_, p)
    centered <- sweep(group_basis, 2L, mu)
    V <- if (n_y) crossprod(centered) / n_y else matrix(NA_real_, p, p)
    eig <- if (n_y && p) eigen(V, symmetric = TRUE) else NULL
    rank <- if (!is.null(eig)) {
      sum(eig$values > rank_tolerance * max(0, eig$values))
    } else 0L
    constant <- if (n_y && p) vapply(seq_len(p), function(k) {
      length(unique(group_basis[, k])) < 2L
    }, logical(1)) else logical()
    status <- if (!n_y) "empty_group" else if (no_variation) {
      "no_secondary_variation"
    } else if (n_y <= p) "insufficient_support" else if (any(constant)) {
      "constant_basis"
    } else if (rank < p) "singular_covariance" else "ok"
    eig_values <- if (is.null(eig)) numeric() else eig$values
    diagnostics[[j]] <- data.frame(
      Y = y_levels[j], n_observational = n_y, cell_probability = n_y / roles$n,
      basis_dimension = p, rank = rank, n_joint_categories_observed = nrow(support),
      eigenvalue_min = if (length(eig_values)) min(eig_values) else NA_real_,
      eigenvalue_max = if (length(eig_values)) max(eig_values) else NA_real_,
      covariance_condition = if (!p) NA_real_ else if (rank == p) {
        max(eig_values) / min(eig_values)
      } else Inf,
      calibration_scope = scope, span_assumption_required = span_required,
      full_conditional_calibration_established = FALSE, status = status
    )
    transform <- diag(p)
    valid <- status %in% c("ok", "no_secondary_variation")
    if (valid && p && normalize_covariance) {
      transform <- eig$vectors %*% diag(1 / sqrt(eig$values), nrow = p) %*% t(eig$vectors)
    }
    if (valid && p && scale_cells) transform <- transform / sqrt(n_y / roles$n)
    columns <- length(target_names) + seq_len(p)
    names_y <- if (p) paste0("q_Y", y_levels[j], "_", make.names(colnames(group_basis))) else character()
    target_names <- c(target_names, names_y)
    groups[[j]] <- list(
      Y = y_levels[j], mean = mu, covariance = V, transform = transform,
      basis_names = colnames(group_basis), target_columns = columns,
      no_secondary_variation = no_variation,
      # For continuous bases only constant groups need an empirical support
      # restriction. Joint indicators always require frozen category support.
      support = if (type == "joint_categories" || no_variation) support else NULL,
      n_observational = n_y, cell_probability = n_y / roles$n
    )
  }
  diagnostics <- do.call(rbind, diagnostics)
  bad <- !diagnostics$status %in% c("ok", "no_secondary_variation")
  if (any(bad)) .secondary_target_stop(
    "invalid_groups", paste0("Cannot construct targets: ", paste(
      paste0("Y=", diagnostics$Y[bad], ": ", diagnostics$status[bad]), collapse = "; "
    )), diagnostics = diagnostics
  )
  structure(list(
    version = 2L, y_levels = y_levels, basis = basis, basis_type = type,
    secondary_names = input$secondary_names,
    basis_names = if (type != "joint_categories") colnames(b) else NULL,
    target_names = make.unique(target_names), groups = groups,
    normalize_covariance = normalize_covariance, scale_cells = scale_cells,
    rank_tolerance = rank_tolerance, n_training = roles$n,
    n_overlap_training = sum(roles$S_o & roles$S_e),
    calibration_specification = list(
      scope = scope, span_assumption_required = span_required,
      span_condition = paste(
        "Within each (X,Y,S=o), the conditional means of H0 and every component",
        "of g/sigma2 must lie in the span of an intercept and the chosen basis."
      ),
      population_support_coverage_verified = FALSE,
      full_conditional_calibration_established = FALSE
    ),
    diagnostics = diagnostics
  ), class = "secondary_target_state")
}

#' Apply frozen state. Experimental-only measurements are never required.
#' New observations do not re-estimate the basis support, centering, or scaling.
#' A novel joint category, or variation in a previously constant group, is an
#' explicit support error rather than a silently zero-filled restriction.
apply_secondary_targets <- function(state, Y, S_o, secondary = NULL, S_e = NULL, X = NULL) {
  if (!inherits(state, "secondary_target_state") || !state$version %in% c(1L, 2L)) {
    stop("state must come from fit_secondary_targets().")
  }
  roles <- .secondary_target_roles(Y, S_o, S_e, state$y_levels, X)
  q <- matrix(0, nrow = roles$n, ncol = length(state$target_names),
              dimnames = list(NULL, state$target_names))
  # Preserve compatibility with the previously saved binary target states.
  if (identical(state$version, 1L)) {
    b <- .secondary_target_basis(secondary, roles, state$basis, state$secondary_names)
    if (is.null(b)) return(q)
    if (!identical(colnames(b$values), state$basis_names)) {
      stop("The basis columns differ from the fitted state.")
    }
    p <- length(state$basis_names)
    for (j in seq_along(state$groups)) {
      idx <- which(Y[b$rows] == state$y_levels[j])
      if (!length(idx)) next
      columns <- (j - 1L) * p + seq_len(p)
      q[b$rows[idx], columns] <- sweep(
        b$values[idx, , drop = FALSE], 2L, state$groups[[j]]$mean
      ) %*% state$groups[[j]]$transform
    }
    return(q)
  }
  input <- .secondary_target_input(secondary, roles, state$secondary_names)
  if (is.null(input)) return(q)
  type <- state$basis_type
  b <- if (type != "joint_categories") {
    .secondary_evaluate_basis(input$values, state$basis, type)
  }
  if (type != "joint_categories" && !identical(colnames(b), state$basis_names)) {
    stop("The basis columns differ from the fitted state.")
  }
  for (j in seq_along(state$groups)) {
    group <- state$groups[[j]]
    idx <- which(Y[input$rows] == state$y_levels[j])
    if (!length(idx)) next
    x <- input$values[idx, , drop = FALSE]
    if (!is.null(group$support)) {
      category <- .secondary_joint_match(x, group$support)
      if (anyNA(category)) .secondary_target_stop(
        "unseen_category",
        "New observational secondary values fall outside the fitted group's category support.",
        input$rows[idx[is.na(category)]]
      )
    }
    if (group$no_secondary_variation) next
    group_basis <- if (type == "joint_categories") {
      .secondary_joint_indicators(x, group$support)
    } else b[idx, , drop = FALSE]
    q[input$rows[idx], group$target_columns] <- sweep(
      group_basis, 2L, group$mean
    ) %*% group$transform
  }
  q
}

