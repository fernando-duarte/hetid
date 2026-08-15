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
#' across passes, so the result only ever grows.
#'
#' @param center Numeric feasible center
#' @param basis Numeric I x I frame
#' @param quadratic Quadratic form list
#' @param n_grid Points per gridded coordinate
#' @return List with \code{lower}, \code{upper}, \code{arg_lower},
#'   \code{arg_upper}
#' @noRd
identified_set_search <- function(center, basis, quadratic, n_grid) {
  n_components <- length(center)
  half <- rep(2, n_components)
  best <- list(
    lower = rep(Inf, n_components),
    upper = rep(-Inf, n_components),
    arg_lower = matrix(NA_real_, n_components, n_components),
    arg_upper = matrix(NA_real_, n_components, n_components)
  )
  for (pass in seq_len(IDENTIFIED_SET_CONTROL$MAX_GROWTH)) {
    swept <- identified_set_box_pass(
      center, basis, half, quadratic, n_grid, IDENTIFIED_SET_CONTROL$FEAS_TOL
    )
    best <- merge_box_state(best, swept)
    grow <- swept$edge &
      half * 2 <= IDENTIFIED_SET_CONTROL$SEARCH_LIMIT
    if (!any(grow)) {
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

#' Apply Witnessed Unboundedness and Empty-Search Sentinels
#'
#' A recession direction is a proof that the set runs to infinity, and it
#' does so in both orientations because \eqn{v'A_iv} is unchanged by
#' negating \eqn{v}. Every coordinate the direction actually moves is
#' therefore unbounded on both sides. Coordinates the search never
#' reached at all report \code{NA}, which says the search found nothing,
#' not that the set is empty.
#'
#' @param found Running state from \code{identified_set_search()}
#' @param quadratic Quadratic form list
#' @return The state with infinite and missing bounds applied
#' @noRd
apply_recession_bounds <- function(found, quadratic) {
  witness <- recession_direction(quadratic)
  if (!is.null(witness)) {
    moved <- abs(witness) > IDENTIFIED_SET_CONTROL$FEAS_TOL
    found$lower[moved] <- -Inf
    found$upper[moved] <- Inf
  }
  empty <- found$lower > found$upper
  found$lower[empty] <- NA_real_
  found$upper[empty] <- NA_real_
  found$arg_lower[!is.finite(found$lower), ] <- NA_real_
  found$arg_upper[!is.finite(found$upper), ] <- NA_real_
  found
}
