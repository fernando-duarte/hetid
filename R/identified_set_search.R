#' Frame and Growth Loop for the Box Search
#'
#' Search-frame construction and the extent-doubling loop used by
#' \code{\link{compute_identified_set_box}} and
#' \code{\link{compute_linear_functional_bounds}}. The frame rescales a
#' local slab approximation; the loop accumulates bounds across windows.
#'
#' @name identified_set_search
#' @keywords internal
NULL

#' Local Search Frame
#'
#' The frame normalizes the local slab approximation
#' \eqn{\{|Q_i'\delta| \le \rho_i\}}, where \eqn{\rho_i} is the square
#' root of the negated constraint value at the center. Mapping the unit
#' cube through \eqn{Q^{-1}\mathrm{diag}(\rho)} gives that approximation.
#' Gridding that frame keeps the node density independent of how
#' ill-conditioned \eqn{Q} is, which is what an axis-aligned grid loses.
#'
#' @param components Components list carrying \code{Q_i}, with one
#'   constraint per theta component and a nonsingular stacked matrix.
#' @param center Finite numeric length-I center strictly inside the set.
#' @param quadratic Quadratic form list at the search slack.
#' @return Numeric I x I basis matrix mapping frame coordinates to theta.
#' @noRd
identified_set_basis <- function(components, center, quadratic) {
  q_mat <- do.call(rbind, components$Q_i)
  assert_dimension_ok(
    nrow(q_mat) == ncol(q_mat),
    paste0(
      "the box search needs one constraint per component: constraints = ",
      nrow(q_mat), "; n_components = ", ncol(q_mat)
    )
  )
  rho <- sqrt(-make_system_checker(quadratic)(center))
  inverse <- tryCatch(
    solve(q_mat),
    error = function(e) {
      stop_hetid(paste0(
        "the Q stack is singular, so no search frame exists: ",
        conditionMessage(e)
      ))
    }
  )
  inverse %*% diag(rho, nrow = length(rho))
}

#' Extent-Doubling Search Over the Frame
#'
#' Each pass sweeps every free coordinate at the current window. An
#' improved bound attained at a gridded window boundary flags those
#' coordinates for doubling, subject to the pass and window limits.
#' Boundary improvement is a search heuristic, not proof of unboundedness.
#' Bounds accumulate across passes, so the result only ever grows.
#' The state starts at the
#' center, a feasible point whose objective values a hull endpoint can
#' only improve on, so a constant objective reports its value with the
#' center as witness rather than an empty search.
#'
#' Growth runs in two phases. The first \code{n_primary} objectives drive
#' the window first, along exactly the path they would take alone, while
#' every objective's bounds accumulate; only once that path has ended may
#' the remaining objectives extend the window. Grids re-laid at a wider
#' window are not nested in the narrower ones, so letting later objectives
#' steer the first phase could change, and even narrow, what the leading
#' ones find. Each phase allows \code{max_growth} new passes. The second
#' may reuse the preceding sweep and need zero new passes. When the first
#' ends on its pass budget with primary flags still raised, the second may
#' also carry that primary growth on.
#'
#' @param center Finite numeric length-I center strictly inside the set.
#' @param basis Numeric I x I frame mapping frame coordinates to theta.
#' @param quadratic Quadratic form list at the search slack.
#' @param n_grid Odd integer of at least three points per gridded coordinate.
#' @param objectives Finite numeric I x m matrix of tracked linear
#'   functionals, one per column.
#' @param n_primary Number of leading objectives that drive the first
#'   growth phase; the default lets every objective drive it.
#' @param evidence Logical scalar; \code{FALSE} by default. If \code{TRUE},
#'   retain tail witnesses and phase termination evidence.
#' @param max_growth Positive integer pass budget per growth phase;
#'   defaults to \code{IDENTIFIED_SET_CONTROL$MAX_GROWTH}.
#' @param search_limit Finite scalar at least two, limiting window
#'   half-widths in frame units; defaults to
#'   \code{IDENTIFIED_SET_CONTROL$SEARCH_LIMIT}.
#' @return List with \code{lower}, \code{upper} (length-m), \code{arg_lower},
#'   \code{arg_upper} (m x I, row k a theta witness for objective k).
#'   Infinite bounds can come from feasible line tails; their point rows
#'   are placeholders until the caller applies recession bounds. With
#'   \code{evidence = TRUE}, also contains length-m lists \code{tail_lower},
#'   \code{tail_upper}, and \code{search} with phase records and limits.
#' @noRd
identified_set_search <- function(center, basis, quadratic, n_grid,
                                  objectives, n_primary = ncol(objectives),
                                  evidence = FALSE,
                                  max_growth = IDENTIFIED_SET_CONTROL$MAX_GROWTH,
                                  search_limit = IDENTIFIED_SET_CONTROL$SEARCH_LIMIT) {
  n_components <- length(center)
  n_objectives <- ncol(objectives)
  half <- rep(2, n_components)
  at_center <- drop(crossprod(objectives, center))
  best <- list(
    lower = at_center,
    upper = at_center,
    arg_lower = matrix(center, n_objectives, n_components, byrow = TRUE),
    arg_upper = matrix(center, n_objectives, n_components, byrow = TRUE)
  )
  if (evidence) {
    if (any(!is.finite(at_center))) stop_hetid("Objective at center exceeds numeric range")
    best$tail_lower <- best$tail_upper <- vector("list", n_objectives)
  }
  phase_history <- list()
  edge_key <- "edge_primary"
  passes <- 0L
  repeat {
    swept <- identified_set_box_pass(
      center, basis, half, quadratic, n_grid, objectives, n_primary, evidence
    )
    best <- merge_box_state(best, swept)
    passes <- passes + 1L
    room <- half * 2 <= search_limit
    grow <- swept[[edge_key]] & room
    if (edge_key == "edge_primary" && n_primary < n_objectives &&
      (!any(grow) || passes >= max_growth)) {
      if (evidence) {
        phase_history <- append_growth_trace(
          phase_history, edge_key, passes, half,
          swept[[edge_key]], room, max_growth
        )
      }
      edge_key <- "edge"
      passes <- 0L
      grow <- swept[[edge_key]] & room
    }
    if (!any(grow) || passes >= max_growth) {
      if (evidence) {
        phase_history <- append_growth_trace(
          phase_history, edge_key, passes, half,
          swept[[edge_key]], room, max_growth
        )
      }
      break
    }
    half[grow] <- half[grow] * 2
  }
  if (evidence) {
    best$search <- list(
      phases = phase_history, max_growth = max_growth, search_limit = search_limit
    )
  }
  best
}

#' Merge One Sweep into the Running Bounds
#'
#' @param best Running state list from the growth loop.
#' @param swept One sweep's state list, with the same objective ordering.
#' @return The running state list with strictly improved bounds, point
#'   rows and, when present, tail witnesses copied from the sweep.
#' @noRd
merge_box_state <- function(best, swept) {
  below <- swept$lower < best$lower
  above <- swept$upper > best$upper
  best$lower[below] <- swept$lower[below]
  best$upper[above] <- swept$upper[above]
  best$arg_lower[below, ] <- swept$arg_lower[below, ]
  best$arg_upper[above, ] <- swept$arg_upper[above, ]
  if (!is.null(best$tail_lower)) {
    best$tail_lower[below] <- swept$tail_lower[below]
    best$tail_upper[above] <- swept$tail_upper[above]
  }
  best
}
