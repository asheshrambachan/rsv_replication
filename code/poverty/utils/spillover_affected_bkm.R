# Largest subset with consistent D within radius b (great-circle distance).
# Keeps the maximal number of rows such that any pair within b_km has equal D
# (treating NA in D as 0). Returns 'keep' in {0,1}.
#
# Dependencies: sf, igraph

## Load libraries
suppressPackageStartupMessages({
  library(sf)
  library(igraph)
})

sf_use_s2(TRUE)

spillover_affected_bkm <- function(
    D,
    centroid_lat,
    centroid_lon,
    b_km
) {
  # 1) Preprocess D (NA -> 0) and coerce to {0,1}
  D[is.na(D)] <- 0
  D <- as.integer(D)
  if (!all(D %in% c(0, 1))) stop("D must take values in {0,1,NA}.")

  d <- data.frame(
    D    = D,
    centroid_lon  = centroid_lon,
    centroid_lat  = centroid_lat
  )
  n <- nrow(d)
  
  if (n <= 1) {
    spillover_affected <- rep(FALSE, n)
    return(spillover_affected)
  }
  
  # 2) Build geodesic neighbor structure (within b_km)
  pts <- st_as_sf(d, coords = c("centroid_lon", "centroid_lat"), crs = 4326, remove = FALSE)
  
  # For each i, indices j within b (includes i)
  nb_list <- st_is_within_distance(pts, pts, dist = b_km * 1000, sparse = TRUE)
  
  # 3) Conflict edges: connect i<->j if dist<=b AND D_i != D_j
  edges_i <- integer(0)
  edges_j <- integer(0)
  for (i in seq_len(n)) {
    js <- nb_list[[i]]
    if (length(js)) {
      js <- js[js > i]  # skip self; ensure i<j
      if (length(js)) {
        diffD <- (D[i] != D[js])
        if (any(diffD)) {
          pick <- js[diffD]
          edges_i <- c(edges_i, rep.int(i, length(pick)))
          edges_j <- c(edges_j, pick)
        }
      }
    }
  }
  
  # If no conflicts, keep everyone
  if (length(edges_i) == 0) {
    spillover_affected <- rep(FALSE, n)
    return(spillover_affected)
  }
  
  # 4) Bipartition by D
  U_idx <- which(D == 0)   # left part
  V_idx <- which(D == 1)   # right part
  types <- rep(NA, n)
  types[U_idx] <- TRUE
  types[V_idx] <- FALSE
  
  # 5) Graph with all vertices present; add conflict edges
  g <- make_empty_graph(n = n, directed = FALSE)
  g <- add_edges(g, as.vector(rbind(edges_i, edges_j)))
  V(g)$type <- types
  
  # 6) Maximum bipartite matching (robust across igraph versions)
  fn <- NULL
  if ("max_bipartite_match" %in% getNamespaceExports("igraph")) {
    fn <- max_bipartite_match
  } else if ("maximum_bipartite_matching" %in% getNamespaceExports("igraph")) {
    fn <- maximum_bipartite_matching
  } else if ("maximum.bipartite.matching" %in% getNamespaceExports("igraph")) {
    fn <- maximum.bipartite.matching
  } else stop("No bipartite matching function found in your igraph installation.")
  
  mm <- fn(g, types = V(g)$type)
  
  # 7) Unify to a full 'mate' vector of length n (0 = unmatched)
  mate <- rep(0, n)
  m <- mm$matching
  if (is.null(m)) stop("Matching result missing.")
  
  vn <- V(g)$name
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
    # igraph 2.x: one entry per vertex; NA = unmatched
    fill_sym(seq_len(n), m)
  } else if (length(m) == length(U_idx)) {
    # older igraph: vector for U side
    fill_sym(U_idx, m)
  } else if (length(m) == length(V_idx)) {
    # older igraph: vector for V side
    fill_sym(V_idx, m)
  } else if (!is.null(names(m))) {
    # map by vertex names
    idx <- if (is.null(vn)) as.integer(names(m)) else match(names(m), vn)
    fill_sym(idx, m)
  } else {
    stop("Unrecognized matching format from igraph; please update igraph.")
  }
  
  # Optional validation if available
  if ("is_matching" %in% getNamespaceExports("igraph")) {
    ok <- is_matching(g, ifelse(mate == 0, NA_integer_, mate), types = V(g)$type)
    if (!isTRUE(ok)) warning("Constructed 'mate' does not validate as a matching.")
  }
  
  # 8) Alternating-BFS to get a minimum vertex cover (Kőnig)
  # Free U-vertices:
  match_u_to_v <- rep(0, n); match_v_to_u <- rep(0, n)
  match_u_to_v[U_idx] <- mate[U_idx]
  match_v_to_u[V_idx] <- mate[V_idx]
  
  adj <- adjacent_vertices(g, V(g), mode = "all")
  
  visitedU <- rep(FALSE, n)
  visitedV <- rep(FALSE, n)
  
  queue <- U_idx[match_u_to_v[U_idx] == 0]  # unmatched on U side
  if (length(queue)) visitedU[queue] <- TRUE
  
  while (length(queue) > 0) {
    u <- queue[1]; queue <- queue[-1]
    
    # Traverse unmatched edges u -> v
    nbrs <- as.integer(adj[[u]])
    if (length(nbrs)) {
      for (v in nbrs) {
        if (!visitedV[v] && match_u_to_v[u] != v) {
          visitedV[v] <- TRUE
          # If v is matched to u2, traverse back via matched edge
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
  mis_idx <- setdiff(seq_len(n), min_cover)
  spillover_affected <- !(seq_len(n) %in% mis_idx)
  return(spillover_affected)
}