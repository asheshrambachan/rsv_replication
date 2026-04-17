# =============================================================================
# Spillover Detection Utility
#
# Identifies units contaminated by treatment spillovers within a geographic
# buffer. Uses a minimum vertex cover (Kőnig's theorem) on the bipartite
# conflict graph between treated and untreated units to find the smallest set
# of units that "explains" all cross-treatment adjacencies; the complement is
# declared spillover-free.
#
# Depends: sf, igraph
# =============================================================================

#' Identify spillover-affected units using bipartite matching
#'
#' @param D             Treatment indicator (0, 1, or NA; NA treated as 0)
#' @param centroid_lat  Latitude of unit centroids
#' @param centroid_lon  Longitude of unit centroids
#' @param b_km          Distance threshold in kilometres
#' @return Logical vector: TRUE = spillover-affected
spillover_affected_bkm <- function(D, centroid_lat, centroid_lon, b_km) {
  # 1) Preprocess D (NA -> 0) and coerce to {0,1}
  D[is.na(D)] <- 0
  D <- as.integer(D)
  if (!all(D %in% c(0, 1))) stop("D must take values in {0,1,NA}.")

  d <- data.frame(D = D, centroid_lon = centroid_lon, centroid_lat = centroid_lat)
  n <- nrow(d)

  if (n <= 1) return(rep(FALSE, n))

  # 2) Build geodesic neighbour structure (within b_km)
  pts     <- st_as_sf(d, coords = c("centroid_lon", "centroid_lat"), crs = 4326, remove = FALSE)
  nb_list <- st_is_within_distance(pts, pts, dist = b_km * 1000, sparse = TRUE)

  # 3) Conflict edges: connect i<->j if dist <= b AND D_i != D_j
  edges_i <- integer(0)
  edges_j <- integer(0)
  for (i in seq_len(n)) {
    js <- nb_list[[i]]
    if (length(js)) {
      js <- js[js > i]          # skip self; ensure i < j
      if (length(js)) {
        diffD <- (D[i] != D[js])
        if (any(diffD)) {
          pick    <- js[diffD]
          edges_i <- c(edges_i, rep.int(i, length(pick)))
          edges_j <- c(edges_j, pick)
        }
      }
    }
  }

  # If no conflicts, no unit is spillover-affected
  if (length(edges_i) == 0) return(rep(FALSE, n))

  # 4) Bipartition by D
  U_idx <- which(D == 0)   # left part (control)
  V_idx <- which(D == 1)   # right part (treated)
  types <- rep(NA, n)
  types[U_idx] <- TRUE
  types[V_idx] <- FALSE

  # 5) Build conflict graph
  g <- make_empty_graph(n = n, directed = FALSE)
  g <- add_edges(g, as.vector(rbind(edges_i, edges_j)))
  V(g)$type <- types

  # 6) Maximum bipartite matching (robust across igraph versions)
  fn <- NULL
  if ("max_bipartite_match"         %in% getNamespaceExports("igraph")) fn <- max_bipartite_match
  else if ("maximum_bipartite_matching"  %in% getNamespaceExports("igraph")) fn <- maximum_bipartite_matching
  else if ("maximum.bipartite.matching"  %in% getNamespaceExports("igraph")) fn <- maximum.bipartite.matching
  else stop("No bipartite matching function found in your igraph installation.")

  mm <- fn(g, types = V(g)$type)

  # 7) Unify to a full 'mate' vector of length n (0 = unmatched)
  mate <- rep(0, n)
  m    <- mm$matching
  if (is.null(m)) stop("Matching result missing.")

  vn       <- V(g)$name
  fill_sym <- function(idx, vals) {
    vals2 <- as.integer(vals); vals2[is.na(vals2)] <- 0
    mate[idx] <<- ifelse(vals2 > 0, vals2, 0)
    ok <- which(mate[idx] > 0)
    if (length(ok)) {
      u <- idx[ok]; v <- mate[idx][ok]
      mate[v] <<- u
    }
  }

  if (length(m) == n) {
    fill_sym(seq_len(n), m)                                          # igraph 2.x
  } else if (length(m) == length(U_idx)) {
    fill_sym(U_idx, m)                                               # older: U side
  } else if (length(m) == length(V_idx)) {
    fill_sym(V_idx, m)                                               # older: V side
  } else if (!is.null(names(m))) {
    idx <- if (is.null(vn)) as.integer(names(m)) else match(names(m), vn)
    fill_sym(idx, m)
  } else {
    stop("Unrecognized matching format from igraph; please update igraph.")
  }

  if ("is_matching" %in% getNamespaceExports("igraph")) {
    ok <- is_matching(g, ifelse(mate == 0, NA_integer_, mate), types = V(g)$type)
    if (!isTRUE(ok)) warning("Constructed 'mate' does not validate as a matching.")
  }

  # 8) Alternating-BFS to get a minimum vertex cover (Kőnig's theorem)
  match_u_to_v <- rep(0, n); match_v_to_u <- rep(0, n)
  match_u_to_v[U_idx] <- mate[U_idx]
  match_v_to_u[V_idx] <- mate[V_idx]

  adj      <- adjacent_vertices(g, V(g), mode = "all")
  visitedU <- rep(FALSE, n)
  visitedV <- rep(FALSE, n)

  queue <- U_idx[match_u_to_v[U_idx] == 0]   # start from unmatched control units
  if (length(queue)) visitedU[queue] <- TRUE

  while (length(queue) > 0) {
    u    <- queue[1]; queue <- queue[-1]
    nbrs <- as.integer(adj[[u]])
    if (length(nbrs)) {
      for (v in nbrs) {
        if (!visitedV[v] && match_u_to_v[u] != v) {
          visitedV[v] <- TRUE
          u2 <- match_v_to_u[v]
          if (u2 != 0 && !visitedU[u2]) {
            visitedU[u2] <- TRUE
            queue <- c(queue, u2)
          }
        }
      }
    }
  }

  # Minimum vertex cover: C = (U \ visitedU) ∪ (V ∩ visitedV)
  min_cover <- c(U_idx[!visitedU[U_idx]], V_idx[visitedV[V_idx]])

  # 9) Maximum independent set = complement of min vertex cover
  mis_idx           <- setdiff(seq_len(n), min_cover)
  spillover_affected <- !(seq_len(n) %in% mis_idx)
  return(spillover_affected)
}
