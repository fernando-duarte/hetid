#' Apply Witnessed Unboundedness
#'
#' Updates search bounds when a sampled strict-curvature direction proves
#' that every nonzero linear objective is unbounded on both sides.
#'
#' @details
#' A direction with \eqn{v'A_i v < 0} for every constraint is a proof that
#' the set runs to infinity in both orientations because curvature is unchanged by
#' negating \eqn{v}. The set of such directions is open, so once one
#' exists no hyperplane contains it and every objective that is not
#' identically zero is unbounded on both sides. The witness therefore
#' establishes existence without requiring that direction to move every
#' objective; a zero objective keeps its finite value. Search failure never reaches here
#' as \code{NA}: the state is seeded from the feasible center.
#' A missing sampled witness does not prove boundedness; it leaves the
#' accumulated bounds unchanged, including any previously witnessed line tails.
#' The sampler restores \code{.Random.seed}, including its absence. Samples depend
#' on \code{RNGkind()}; the cached Box-Muller normal deviate is not restored.
#' When tail evidence is retained, nonfinite or nonnegative witness curvature
#' raises a structured \code{hetid_error}.
#'
#' @param found Running list from \code{identified_set_search()}, with length-m
#'   bounds and m x I attaining-point matrices, optionally retaining tail evidence.
#' @param quadratic Quadratic form list with one finite I x I matrix in
#'   \code{A_i} per enforced maturity constraint.
#' @param objectives Finite numeric I x m matrix of tracked linear functionals;
#'   rows index theta components and columns index objectives.
#' @return The running list, with both bounds infinite for each nonzero objective
#'   when a strict-curvature witness is found, and otherwise the accumulated bounds.
#'   Attaining-point rows for all nonfinite bounds are \code{NA_real_}; zero
#'   objectives retain their finite values and points. If tail evidence is present,
#'   affected sides receive a \code{strict_curvature} record containing the direction.
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
