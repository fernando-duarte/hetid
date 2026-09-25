#' Apply Witnessed Unboundedness
#'
#' A recession direction is a proof that the set runs to infinity, and it
#' does so in both orientations because \eqn{v'A_iv} is unchanged by
#' negating \eqn{v}. The set of such directions is open, so once one
#' exists no hyperplane contains it and every objective that is not
#' identically zero is unbounded on both sides. The witness therefore
#' establishes existence without requiring that direction to move every
#' objective; a zero objective keeps its finite value. Search failure never reaches here
#' as \code{NA}: the state is seeded from the feasible center.
#'
#' @param found Running state from \code{identified_set_search()}
#' @param quadratic Quadratic form list
#' @param objectives Numeric I x m matrix of tracked linear functionals
#' @return The state with infinite bounds, NA attaining points for those
#'   bounds, and optional retained tail evidence
#' @noRd
apply_recession_bounds <- function(found, quadratic, objectives) {
  direction <- recession_direction(quadratic)
  if (!is.null(direction)) {
    moved <- colSums(objectives != 0) > 0L
    found$lower[moved] <- -Inf
    found$upper[moved] <- Inf
    if (!is.null(found$tail_lower)) {
      curvature <- vapply(quadratic$A_i, function(a) {
        drop(crossprod(direction, a %*% direction))
      }, numeric(1))
      if (any(!is.finite(curvature)) || any(curvature >= 0)) {
        stop_hetid("Recession evidence fails strict finite curvature verification")
      }
      proof <- list(kind = "strict_curvature", direction = direction)
      found$tail_lower[moved] <- found$tail_upper[moved] <- rep(list(proof), sum(moved))
    }
  }
  found$arg_lower[!is.finite(found$lower), ] <- NA_real_
  found$arg_upper[!is.finite(found$upper), ] <- NA_real_
  found
}
