# Geometry helpers for the three-dimensional identified-region plot.

project_region_3d <- function(xyz, pmat) {
  projected <- cbind(xyz, 1) %*% pmat
  cbind(
    x = projected[, 1] / projected[, 4],
    y = projected[, 2] / projected[, 4],
    depth = projected[, 3] / projected[, 4]
  )
}

delaunay_triangles <- function(points, max_edge) {
  candidates <- utils::combn(seq_len(nrow(points)), 3L)
  keep <- vapply(seq_len(ncol(candidates)), function(k) {
    ids <- candidates[, k]
    p <- points[ids, , drop = FALSE]
    den <- 2 * (
      p[1, 1] * (p[2, 2] - p[3, 2]) +
        p[2, 1] * (p[3, 2] - p[1, 2]) +
        p[3, 1] * (p[1, 2] - p[2, 2])
    )
    if (abs(den) < 1e-12) {
      return(FALSE)
    }
    sq <- rowSums(p^2)
    ux <- sum(sq * c(
      p[2, 2] - p[3, 2], p[3, 2] - p[1, 2],
      p[1, 2] - p[2, 2]
    )) / den
    uy <- sum(sq * c(
      p[3, 1] - p[2, 1], p[1, 1] - p[3, 1],
      p[2, 1] - p[1, 1]
    )) / den
    radius_sq <- sum((p[1, ] - c(ux, uy))^2)
    other <- setdiff(seq_len(nrow(points)), ids)
    other_sq <- rowSums(sweep(points[other, , drop = FALSE], 2, c(ux, uy))^2)
    edges <- c(
      sqrt(sum((p[1, ] - p[2, ])^2)),
      sqrt(sum((p[2, ] - p[3, ])^2)),
      sqrt(sum((p[3, ] - p[1, ])^2))
    )
    all(other_sq >= radius_sq - 1e-10) && max(edges) <= max_edge
  }, logical(1))
  candidates[, keep, drop = FALSE]
}

build_region_mesh <- function(sys, lims, h = 0.16, seed = 0L,
                              max_edge_factor = 1.5) {
  row_values <- seq(-0.05, 1.06, by = h * sqrt(3) / 2)
  lattice <- do.call(rbind, lapply(seq_along(row_values), function(i) {
    offset <- if ((i - 1L) %% 2L == 0L) 0 else h / 2
    cbind(seq(-0.05 + offset, 1.06, by = h), row_values[i])
  }))
  set.seed(seed)
  jitter <- stats::runif(length(lattice), -0.16 * h, 0.16 * h)
  lattice <- lattice + matrix(jitter, ncol = 2)
  b2 <- lims[[2]][1] + lattice[, 1] * diff(lims[[2]])
  b3 <- lims[[3]][1] + lattice[, 2] * diff(lims[[3]])
  envelope <- region_envelope(sys, 1L, matrix(b2), matrix(b3))
  width <- as.vector(envelope$H - envelope$L)
  min_width <- 5 * diff(lims[[1]]) / 399
  good <- as.vector(envelope$M) <= 0 & width >= min_width
  points <- lattice[good, , drop = FALSE]
  triangles <- delaunay_triangles(points, max_edge_factor * h)
  high <- cbind(as.vector(envelope$H)[good], b2[good], b3[good])
  low <- cbind(as.vector(envelope$L)[good], b2[good], b3[good])

  tri_rows <- t(triangles)
  all_edges <- rbind(
    tri_rows[, c(1, 2), drop = FALSE],
    tri_rows[, c(2, 3), drop = FALSE],
    tri_rows[, c(3, 1), drop = FALSE]
  )
  all_edges <- t(apply(all_edges, 1, sort))
  edge_keys <- apply(all_edges, 1, paste, collapse = ":")
  counts <- table(edge_keys)
  edges <- unique(all_edges)
  keys <- apply(edges, 1, paste, collapse = ":")
  boundary <- edges[keys %in% names(counts[counts == 1L]), , drop = FALSE]

  faces <- list()
  for (k in seq_len(nrow(tri_rows))) {
    ids <- tri_rows[k, ]
    faces <- c(faces, list(high[ids, , drop = FALSE], low[ids, , drop = FALSE]))
  }
  for (k in seq_len(nrow(boundary))) {
    i <- boundary[k, 1]
    j <- boundary[k, 2]
    faces <- c(
      faces,
      list(rbind(high[i, ], high[j, ], low[j, ])),
      list(rbind(high[i, ], low[j, ], low[i, ]))
    )
  }

  segments <- list()
  for (k in seq_len(nrow(edges))) {
    i <- edges[k, 1]
    j <- edges[k, 2]
    segments <- c(segments, list(
      rbind(high[i, ], high[j, ]),
      rbind(low[i, ], low[j, ])
    ))
  }
  for (k in seq_len(nrow(boundary))) {
    i <- boundary[k, 1]
    j <- boundary[k, 2]
    segments <- c(segments, list(rbind(high[i, ], low[j, ])))
  }
  for (i in sort(unique(as.vector(boundary)))) {
    segments <- c(segments, list(rbind(high[i, ], low[i, ])))
  }
  list(
    faces = faces,
    segments = segments,
    n_vertices = nrow(points),
    n_triangles = nrow(tri_rows),
    n_boundary = nrow(boundary)
  )
}
