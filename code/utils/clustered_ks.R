# Mandal-cluster multiplier bootstrap for a two-sample KS distance.
# Records may contribute to both sample roles; their contributions use the
# same mandal multiplier. Inference is asymptotic, requiring independent
# mandals, no dominating cluster, and sufficient support in both roles.
# Callers validate inputs and supply finite scores with complete cluster IDs.
# warn_clusters is a reporting heuristic, not a minimum for valid inference.

clustered_ks <- function(r, clusters, in_exp, in_obs,
                         B = 2000L, seed = 42L, warn_clusters = 10L) {
  n_exp <- sum(in_exp)
  n_obs <- sum(in_obs)
  result <- list(
    ks_stat = NA_real_, cluster_pval = NA_real_,
    n_exp = n_exp, n_obs = n_obs,
    n_overlap = sum(in_exp & in_obs),
    clusters_exp = length(unique(clusters[in_exp])),
    clusters_obs = length(unique(clusters[in_obs])),
    max_cluster_share = NA_real_, status = "insufficient_support"
  )
  result$small_cluster_warning <- min(result$clusters_exp, result$clusters_obs) < warn_clusters
  result$cluster_warning_threshold <- warn_clusters
  if (n_exp == 0L || n_obs == 0L) return(result)

  grid <- sort(unique(r))
  f_exp <- findInterval(grid, sort(r[in_exp])) / n_exp
  f_obs <- findInterval(grid, sort(r[in_obs])) / n_obs
  result$ks_stat <- max(abs(f_exp - f_obs))
  if (length(grid) < 2L) {
    result$status <- "constant_R"
    return(result)
  }

  cluster_levels <- unique(clusters)
  G <- length(cluster_levels)
  result$max_cluster_share <- max(table(clusters)) / length(clusters)
  if (G < 2L) {
    result$status <- "insufficient_clusters"
    return(result)
  }
  # Influence of each mandal on the difference between the two cell CDFs.
  # An overlap record contributes to BOTH roles within its one mandal.
  influence <- matrix(0, nrow = G, ncol = length(grid))
  for (g in seq_along(cluster_levels)) {
    idx <- which(clusters == cluster_levels[g])
    e <- idx[in_exp[idx]]
    o <- idx[in_obs[idx]]
    e_counts <- cumsum(tabulate(match(r[e], grid), nbins = length(grid)))
    o_counts <- cumsum(tabulate(match(r[o], grid), nbins = length(grid)))
    influence[g, ] <- (e_counts - length(e) * f_exp) / n_exp -
      (o_counts - length(o) * f_obs) / n_obs
  }
  influence <- sweep(influence, 2L, colMeans(influence)) * sqrt(G / (G - 1))
  if (max(abs(influence)) <= .Machine$double.eps) {
    result$status <- "degenerate_cluster_variance"
    return(result)
  }

  set.seed(seed)
  boot_stat <- numeric(B)
  # Process draws in batches to avoid a grid-by-B matrix for large cells.
  for (start in seq.int(1L, B, by = 50L)) {
    draws <- start:min(start + 49L, B)
    multipliers <- matrix(
      sample(c(-1, 1), G * length(draws), replace = TRUE), nrow = G
    )
    boot_stat[draws] <- apply(abs(crossprod(influence, multipliers)), 2L, max)
  }
  result$cluster_pval <- (1 + sum(boot_stat >= result$ks_stat)) / (B + 1)
  result$status <- if (result$small_cluster_warning) "few_clusters" else "ok"
  if (result$small_cluster_warning) {
    warning(sprintf(
      "Clustered p-value computed with %d experimental and %d observational mandals; %d is a warning threshold, not a validity guarantee.",
      result$clusters_exp, result$clusters_obs, warn_clusters
    ), call. = FALSE)
  }
  result
}
