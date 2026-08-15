#' Frame, Growth Loop and Recession Bounds for the Box Search
#'
#' Internals of \code{compute_identified_set_box()}: the search frame in
#' which the set is locally a cube, the extent-doubling loop over that
#' frame, and the recession bounds that turn a witnessed unbounded
#' direction into infinite coordinates.
#'
#' @name identified_set_search
#' @keywords internal
NULL

#' Local Search Frame
#'
#' Near the center the set is the slab intersection
#' \eqn{\{|Q_i'\delta| \lesssim \rho_i\}}, so mapping frame coordinates
#' through \eqn{Q^{-1}\mathrm{diag}(\rho)} makes it locally the unit cube.
#' Gridding that frame keeps the node density independent of how
#' ill-conditioned \eqn{Q} is, which is what an axis-aligned grid loses.
#'
#' @param components Components list carrying \code{Q_i}
#' @param center Numeric feasible center
#' @param quadratic Quadratic form list
#' @return Numeric I x I basis matrix
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
#' Each pass sweeps every free coordinate at the current window. A bound
#' attained on the window boundary means the set continues past it, so
#' those coordinates double and the sweep repeats. Bounds accumulate
#' across passes, so the result only ever grows. The state starts at the
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
#' ones find. Each phase has \code{MAX_GROWTH} passes. The second phase is
#' a guard the package's fixtures do not trigger, and when the first phase
#' ends on its pass budget with primary flags still raised, the second may
#' also carry that primary growth on.
#'
#' @param center Numeric feasible center
#' @param basis Numeric I x I frame
#' @param quadratic Quadratic form list
#' @param n_grid Points per gridded coordinate
#' @param objectives Numeric I x m matrix of tracked linear functionals
#' @param n_primary Number of leading objectives that drive the first
#'   growth phase; the default lets every objective drive it
#' @return List with \code{lower}, \code{upper} (length-m), \code{arg_lower},
#'   \code{arg_upper} (m x I)
#' @noRd
identified_set_search <- function(center, basis, quadratic, n_grid,
                                  objectives, n_primary = ncol(objectives)) {
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
  edge_key <- "edge_primary"
  passes <- 0L
  repeat {
    swept <- identified_set_box_pass(
      center, basis, half, quadratic, n_grid, IDENTIFIED_SET_CONTROL$FEAS_TOL,
      objectives, n_primary
    )
    best <- merge_box_state(best, swept)
    passes <- passes + 1L
    room <- half * 2 <= IDENTIFIED_SET_CONTROL$SEARCH_LIMIT
    grow <- swept[[edge_key]] & room
    if (edge_key == "edge_primary" && n_primary < n_objectives &&
      (!any(grow) || passes >= IDENTIFIED_SET_CONTROL$MAX_GROWTH)) {
      # the leading objectives' path has ended: the rest may now grow the
      # window, judged from this same sweep, with a fresh pass budget
      edge_key <- "edge"
      passes <- 0L
      grow <- swept$edge & room
    }
    if (!any(grow) || passes >= IDENTIFIED_SET_CONTROL$MAX_GROWTH) {
      break
    }
    half[grow] <- half[grow] * 2
  }
  best
}

#' Merge One Sweep into the Running Bounds
#'
#' @param best Running state
#' @param swept One sweep's state
#' @return The merged state
#' @noRd
merge_box_state <- function(best, swept) {
  below <- swept$lower < best$lower
  above <- swept$upper > best$upper
  best$lower[below] <- swept$lower[below]
  best$upper[above] <- swept$upper[above]
  best$arg_lower[below, ] <- swept$arg_lower[below, ]
  best$arg_upper[above, ] <- swept$arg_upper[above, ]
  best
}

#' Apply Witnessed Unboundedness
#'
#' A recession direction is a proof that the set runs to infinity, and it
#' does so in both orientations because \eqn{v'A_iv} is unchanged by
#' negating \eqn{v}. The set of such directions is open, so once one
#' exists no hyperplane contains it and every objective that is not
#' identically zero is unbounded on both sides. The witness therefore
#' certifies existence and its direction is not used; a zero objective is
#' constant and keeps its finite value. Search failure never reaches here
#' as \code{NA}: the state is seeded from the feasible center.
#'
#' @param found Running state from \code{identified_set_search()}
#' @param quadratic Quadratic form list
#' @param objectives Numeric I x m matrix of tracked linear functionals
#' @return The state with infinite bounds applied and their witnesses
#'   cleared
#' @noRd
apply_recession_bounds <- function(found, quadratic, objectives) {
  if (!is.null(recession_direction(quadratic))) {
    moved <- colSums(objectives != 0) > 0L
    found$lower[moved] <- -Inf
    found$upper[moved] <- Inf
  }
  found$arg_lower[!is.finite(found$lower), ] <- NA_real_
  found$arg_upper[!is.finite(found$upper), ] <- NA_real_
  found
}
