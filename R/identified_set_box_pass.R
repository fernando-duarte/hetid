#' One Sweep of the Identified-Set Box Search
#'
#' Internal single pass of the box search. Each coordinate of the search
#' frame takes a turn as the free coordinate: the remaining coordinates
#' are gridded over the current window, and the feasible interval hull is
#' solved along the free direction at every node. Because
#' \eqn{\theta = center + basis \cdot u} is affine in the line parameter,
#' every linear objective of \eqn{\theta} attains its extreme on that line
#' at a finite hull endpoint or is unbounded along a feasible tail, so one
#' hull updates every objective's running bounds rather than only the free
#' coordinate's.
#'
#' Finite bounds come from feasible interval endpoints, subject to root
#' rounding; infinite bounds come from feasible tails. The line solver omits
#' isolated feasible points. The sweep can miss more extreme values, which
#' is what the caller's growth loop and the edge flags are for. Two edge
#' vectors come back: \code{edge_primary}, raised
#' only by the first \code{n_primary} objectives, drives the caller's first
#' growth phase; \code{edge}, raised by any objective, drives the second.
#'
#' Inputs are prepared by the caller: finite numeric frame and objective
#' values, positive half-widths, and no missing values. With \code{evidence = TRUE},
#' nonfinite objective slopes or finite-endpoint calculations signal a
#' structured \code{hetid_error}. Line-solver numerical errors also propagate.
#' No files, options, or random state are changed.
#'
#' @param center Numeric length-I feasible point, where I is the theta dimension.
#' @param basis Numeric I x I matrix mapping frame coordinates to theta.
#' @param half Numeric length-I positive window half-widths in frame units.
#' @param quadratic List with constraint-wise \code{A_i}, \code{b_i}, and \code{c_i}.
#' @param n_grid Integer points per gridded coordinate; callers supply an odd
#'   value of at least three so the grid includes zero.
#' @param objectives Numeric I x m matrix; column k is the linear
#'   functional of theta whose extremes are tracked. The identity tracks
#'   the coordinates themselves.
#' @param n_primary Number of leading objectives whose improvements raise
#'   \code{edge_primary}; an integer from zero to m.
#' @param evidence Logical scalar; retain tail witnesses when \code{TRUE}
#'   (default \code{FALSE}).
#' @return List with \code{lower}, \code{upper} (length-m), \code{arg_lower},
#'   \code{arg_upper} (m x I, row k the attaining theta for a finite bound),
#'   \code{edge} and \code{edge_primary} (logical length-I, flagged when a
#'   finite endpoint improves any or a primary objective at a gridded boundary),
#'   and \code{n_feasible} (integer count of lines with a nonempty retained hull).
#'   Infinite bounds do not update witness rows or edge flags. With no retained
#'   hull, lower/upper remain \code{Inf}/\code{-Inf}, witnesses are \code{NA},
#'   flags are \code{FALSE}, and the count is zero. If every retained hull has
#'   only infinite endpoints, zero-slope objectives retain the same bounds and
#'   witnesses. With \code{evidence = TRUE}, \code{tail_lower} and
#'   \code{tail_upper} are length-m lists; entries are
#'   \code{NULL} or lists with \code{kind = "line_tail"}, numeric length-I
#'   \code{origin}, and numeric length-I \code{direction} for a feasible tail.
#' @noRd
identified_set_box_pass <- function(center, basis, half, quadratic,
                                    n_grid, objectives, n_primary, evidence = FALSE) {
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
  if (evidence) {
    state$tail_lower <- state$tail_upper <- vector("list", n_objectives)
  }
  slope <- crossprod(objectives, basis)
  if (evidence && any(!is.finite(slope))) stop_hetid("Objective slope exceeds numeric range")
  primary <- seq_len(n_objectives) <= n_primary
  for (j in seq_len(n_components)) {
    others <- setdiff(seq_len(n_components), j)
    nodes <- identified_set_nodes(half[others], n_grid)
    for (r in seq_len(nrow(nodes))) {
      u_base <- numeric(n_components)
      u_base[others] <- nodes[r, ]
      hull <- line_feasible_hull(
        center + drop(basis %*% u_base), basis[, j], quadratic
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
#'   of length zero when the system has a single component.
#' @param n_grid Integer points per coordinate, supplied by the caller.
#' @return Numeric matrix of nodes, one row per node; a single empty row
#'   when there is nothing to grid. Each axis spans minus to plus its half-width.
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
#' @param node Numeric node coordinates, without missing values.
#' @param half Numeric half-widths of the same coordinates, without missing values.
#' @return Logical vector, \code{TRUE} within the relative boundary tolerance
#'   \code{HETID_CONSTANTS$BOX_BOUNDARY_TOLERANCE}; \code{logical(0)} for no nodes.
#'   Missing inputs propagate to \code{NA} flags.
#' @noRd
node_on_boundary <- function(node, half) {
  if (length(node) == 0L) {
    return(logical(0))
  }
  abs(abs(node) - half) <= HETID_CONSTANTS$BOX_BOUNDARY_TOLERANCE * half
}

#' Fold One Line Hull into the Running Bounds
#'
#' An infinite endpoint is not evaluated as a point. The line runs to
#' infinity by the signs of its constraint polynomials, so every objective
#' the direction actually moves becomes
#' unbounded on the corresponding side, with the side flipping where the
#' objective falls along the direction.
#'
#' @param state Running sweep state from \code{identified_set_box_pass()}.
#' @param hull Numeric \code{c(lower, upper)} from \code{line_feasible_hull()}.
#' @param center,basis,u_base Numeric length-I center, I x I basis, and length-I
#'   frame coordinates of the node's base point.
#' @param j Integer index of the free coordinate.
#' @param at_edge Integer indices of gridded coordinates on the window boundary.
#' @param objectives Numeric I x m matrix of tracked linear functionals.
#' @param slope Numeric length-m, each objective's rate along the free
#'   direction \code{basis[, j]}.
#' @param primary Logical length-m, TRUE for the objectives that raise
#'   \code{edge_primary}.
#' @return The updated state list. Finite endpoint improvements update bounds,
#'   witnesses, and edge flags. Infinite endpoints update bounds and retained
#'   tail evidence, leaving witness rows and edge flags unchanged.
#' @noRd
absorb_line_hull <- function(state, hull, center, basis, u_base, j, at_edge,
                             objectives, slope, primary) {
  for (t_val in hull) {
    if (is.infinite(t_val)) {
      rising <- if (t_val > 0) slope > 0 else slope < 0
      falling <- if (t_val > 0) slope < 0 else slope > 0
      if (!is.null(state$tail_lower)) {
        proof <- list(
          kind = "line_tail", origin = center + drop(basis %*% u_base),
          direction = sign(t_val) * basis[, j]
        )
        state$tail_upper[rising] <- rep(list(proof), sum(rising))
        state$tail_lower[falling] <- rep(list(proof), sum(falling))
      }
      state$upper[rising] <- Inf
      state$lower[falling] <- -Inf
      next
    }
    theta <- center + drop(basis %*% replace(u_base, j, t_val))
    value <- drop(crossprod(objectives, theta))
    if (!is.null(state$tail_lower) && any(!is.finite(c(theta, value)))) {
      stop_hetid("Finite line objective exceeds the numeric range")
    }
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
