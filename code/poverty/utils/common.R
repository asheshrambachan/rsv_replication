# =============================================================================
# Common Utilities
# =============================================================================

## Load libraries
suppressPackageStartupMessages({
  library(dplyr)
})

#' Convert binary numeric vector to logical (TRUE/FALSE)
to_logical <- function(x) {
  if (is.logical(x)) {
    return(x)
  } else {
    vals <- unique(x)
    if (all(vals %in% c(0, 1))) {
      return(as.logical(x))
    } else {
      stop("to_logical: expected values 0/1 or TRUE/FALSE; got: ", paste(vals, collapse = ", "))
    }
  }
}

#' Cluster-level bootstrap resampling
cluster_sample <- function(data, cluster_var="clusters") {
  ids <- unique(data[[cluster_var]])
  sampled <- sample(ids, length(ids), replace = TRUE)
  data[data[[cluster_var]] %in% sampled, , drop = FALSE]
}

#' Synthetic Data Generating Process
gen_synth_data <- function(n, tau, X, D, Y) {
  Y1_id <- which(Y == 1)
  Y0_id <- which(Y == 0)
  p_Y1_D1 <- mean(Y[D == 1], na.rm = TRUE) + tau
  p_Y1_D0 <- mean(Y[D == 0], na.rm = TRUE)

  # draw new D and outcomes
  D <- rbinom(n, 1, prob = 0.5)
  Y_D1 <- rbinom(n, 1, prob = p_Y1_D1)
  Y_D0 <- rbinom(n, 1, prob = p_Y1_D0)
  X_Y1_id <- sample(Y1_id, size = n, replace = TRUE)
  X_Y0_id <- sample(Y0_id, size = n, replace = TRUE)

  # define Y
  Y <- D * Y_D1 + (1 - D) * Y_D0

  # define X
  X_Y1 <- X[X_Y1_id, , drop = FALSE] 
  X_Y0 <- X[X_Y0_id, , drop = FALSE]
  X <- Y * X_Y1 + (1 - Y) * X_Y0
  
  # define Se and So
  Se <- rep(TRUE, n) # S = e for D = 0 and D = 1
  So <- D != 1 # S = o for D = 0
  Y <- ifelse(So == TRUE, Y, NA)

  list(X = X, D = D, Y = Y, Se = Se, So = So)
}


#' Generic cross-validation wrapper
cv_fun <- function(fun, nfold = 5, ...) {
  args <- list(...)
  
  if (!"D" %in% names(args)) stop("cv_fun: 'D' required")

  n <- length(args$D)
  folds <- sample.int(nfold, n, replace = TRUE)
  out <- vector("list", nfold)
  
  for (k in seq_len(nfold)) {
    train_id <- folds != k
    test_id <- folds == k

    train_args <- lapply(args, function(el) if (is.matrix(el) || is.data.frame(el)) el[train_id, , drop = FALSE] else el[train_id])
    test_args  <- lapply(args, function(el) if (is.matrix(el) || is.data.frame(el)) el[test_id, , drop = FALSE] else el[test_id])
    
    names(train_args) <- paste0(names(train_args), "_train")
    names(test_args)  <- paste0(names(test_args),  "_test")
    
    all_args <- c(train_args, test_args)
    call_args <- all_args[names(all_args) %in% names(formals(fun))]
    out[[k]] <- do.call(fun, call_args)
  }

  coef_cv <- mean(sapply(out, function(x) x$coef), na.rm=TRUE)
  list(coef_cv = coef_cv, out = out)
}
