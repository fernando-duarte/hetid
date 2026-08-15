#' One Sweep of the Identified-Set Box Search
#'
#' Internal single pass of the box search. Each coordinate of the search
#' frame takes a turn as the free coordinate: the remaining coordinates
#' are gridded over the current window, and the exact feasible hull is
#' solved along the free direction at every node. Because
#' \eqn{\theta = center + basis \cdot u} is affine in the line parameter,
#' every linear objective of \eqn{\theta} attains its extreme on that line
#' at a hull endpoint, so one hull updates every objective's running
#' bounds rather than only the free coordinate's.
#'
#' Every bound the sweep reports is attained at a point that satisfies all
#' constraints, so the result never overstates the true extremes. It can
#' understate them, which is what the caller's growth loop and the edge
#' flags are for. Two edge vectors come back: \code{edge_primary}, raised
#' only by the first \code{n_primary} objectives, drives the caller's first
#' growth phase; \code{edge}, raised by any objective, drives the second.
#'
#' @param center Numeric length-I point, feasible by construction
#' @param basis Numeric I x I matrix mapping frame coordinates to theta
#' @param half Numeric length-I window half-widths in frame coordinates
#' @param quadratic Quadratic form list
#' @param n_grid Points per gridded coordinate
#' @param tol Feasibility tolerance
#' @param objectives Numeric I x m matrix; column k is the linear
#'   functional of theta whose extremes are tracked. The identity tracks
#'   the coordinates themselves
#' @param n_primary Number of leading objectives whose improvements raise
#'   \code{edge_primary}
#' @return List with \code{lower}, \code{upper} (length-m), \code{arg_lower},
#'   \code{arg_upper} (m x I, row k the attaining theta for objective k),
#'   \code{edge} and \code{edge_primary} (logical length-I, TRUE where a
#'   bound was attained on the window boundary) and \code{n_feasible}
#' @noRd
identified_set_box_pass <- function(center, basis, half, quadratic,
                                    n_grid, tol, objectives, n_primary) {
  n_components <- length(center)
  n_objectives <- ncol(objectives)
  state <- list(
    lower = rep(Inf, n_objectives),
    upper = rep(-Inf, n_objectives),
    arg_lower = matrix(NA_real_, n_objectives, n_components),
    arg_upper = matrix(NA_real_, n_objectives, n_components),
    edge = rep(FALSE, n_components),
    edge_primary = rep(FALSE, n_components),
    n_feasible = 0L
  )
  slope <- crossprod(objectives, basis)
  primary <- seq_len(n_objectives) <= n_primary
  for (j in seq_len(n_components)) {
    others <- setdiff(seq_len(n_components), j)
    nodes <- identified_set_nodes(half[others], n_grid)
    for (r in seq_len(nrow(nodes))) {
      u_base <- numeric(n_components)
      u_base[others] <- nodes[r, ]
      hull <- line_feasible_hull(
        center + drop(basis %*% u_base), basis[, j], quadratic, tol
      )
      if (is.null(hull)) {
        next
      }
      state$n_feasible <- state$n_feasible + 1L
      at_edge <- others[node_on_boundary(nodes[r, ], half[others])]
      state <- absorb_line_hull(
        state, hull, center, basis, u_base, j, at_edge, objectives,
        slope[, j], primary
      )
    }
  }
  state
}

#' Grid Nodes for the Gridded Coordinates of One Sweep
#'
#' @param half Numeric half-widths of the gridded coordinates, possibly
#'   of length zero when the system has a single component
#' @param n_grid Points per coordinate
#' @return Numeric matrix of nodes, one row per node; a single empty row
#'   when there is nothing to grid
#' @noRd
identified_set_nodes <- function(half, n_grid) {
  if (length(half) == 0L) {
    return(matrix(numeric(0), nrow = 1L, ncol = 0L))
  }
  axes <- lapply(half, function(h) seq(-h, h, length.out = n_grid))
  as.matrix(expand.grid(axes, KEEP.OUT.ATTRS = FALSE))
}

#' Flag Gridded Coordinates Sitting on the Window Boundary
#'
#' @param node Numeric node coordinates
#' @param half Numeric half-widths of the same coordinates
#' @return Logical vector, TRUE where the node sits on the boundary
#' @noRd
node_on_boundary <- function(node, half) {
  if (length(node) == 0L) {
    return(logical(0))
  }
  abs(abs(node) - half) <= 1e-9 * half
}

#' Fold One Line Hull into the Running Bounds
#'
#' An infinite endpoint is not evaluated as a point. The line runs to
#' infinity, so every objective the direction actually moves becomes
#' unbounded on the corresponding side, with the side flipping where the
#' objective falls along the direction.
#'
#' @param state Running sweep state
#' @param hull Numeric \code{c(lower, upper)} from \code{line_feasible_hull()}
#' @param center,basis,u_base Search frame and the node's base point
#' @param j Index of the free coordinate
#' @param at_edge Indices of gridded coordinates on the window boundary
#' @param objectives Numeric I x m matrix of tracked linear functionals
#' @param slope Numeric length-m, each objective's rate along the free
#'   direction \code{basis[, j]}
#' @param primary Logical length-m, TRUE for the objectives that raise
#'   \code{edge_primary}
#' @return The updated state
#' @noRd
absorb_line_hull <- function(state, hull, center, basis, u_base, j, at_edge,
                             objectives, slope, primary) {
  for (t_val in hull) {
    if (is.infinite(t_val)) {
      rising <- if (t_val > 0) slope > 0 else slope < 0
      falling <- if (t_val > 0) slope < 0 else slope > 0
      state$upper[rising] <- Inf
      state$lower[falling] <- -Inf
      next
    }
    theta <- center + drop(basis %*% replace(u_base, j, t_val))
    value <- drop(crossprod(objectives, theta))
    below <- value < state$lower
    above <- value > state$upper
    state$lower[below] <- value[below]
    state$upper[above] <- value[above]
    state$arg_lower[below, ] <- rep(theta, each = sum(below))
    state$arg_upper[above, ] <- rep(theta, each = sum(above))
    improved <- below | above
    if (any(improved)) {
      state$edge[at_edge] <- TRUE
    }
    if (any(improved[primary])) {
      state$edge_primary[at_edge] <- TRUE
    }
  }
  state
}
