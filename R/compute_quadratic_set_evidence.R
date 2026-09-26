#' Evidence for Boundedness of a Quadratic Feasible Set
#'
#' Search for sufficient evidence about linear objectives over the intersection
#' of quadratic inequalities. This function does not optimize finite endpoints.
#'
#' @param quadratic List containing nonempty parallel lists `A_i`, `b_i` and a
#'   numeric vector `c_i`, representing `x' A_i x + b_i' x + c_i <= 0`.
#'   Matrices must be finite and symmetric and share one positive dimension.
#' @param objectives Finite numeric matrix, with one objective loading per column
#'   and one row per coordinate. Only exactly zero loadings denote constants.
#' @param points Optional finite numeric matrix of candidate points, one per row.
#'   Nonzero points are also tried as direction candidates.
#' @param directions Optional finite numeric matrix of candidate directions,
#'   one per row. These are checked independently before accepting any evidence.
#' @param n_dir Number of additional sampled directions; zero disables sampling.
#' @param maxit Iteration budget for multivariate candidate searches; zero
#'   disables deterministic optimization. Scalar weight searches use a fixed
#'   tolerance. Search exhaustion never proves boundedness.
#'
#' @details
#' A positive definite nonnegative combination of constraint Hessians proves
#' that the feasible set is bounded if it is nonempty. Nonemptiness requires a
#' checked point or a feasible infinite tail. A strict negative-curvature
#' direction for every constraint makes all nonconstant linear objectives
#' two-sided unbounded, including objectives orthogonal to that direction.
#' Infinite line tails give side-specific evidence. Simple coordinate half-spaces
#' and structurally zero null-coordinate blocks also provide directional bounds.
#'
#' Every certificate is sufficient, and searches are incomplete. An unresolved
#' side is not evidence of emptiness, finiteness or infinity. Numerical sign
#' checks use conservative rounding margins; near-zero cancellation and poorly
#' conditioned systems can remain unresolved. Finite optimizer output alone
#' supplies no boundedness evidence.
#'
#' Direction sampling uses the existing fixed-seed search and restores
#' `.Random.seed`, including its absence. Reproducibility assumes the same
#' `RNGkind()`. The Box-Muller cached deviate, which is outside `.Random.seed`,
#' cannot be restored by that search. Use `n_dir = 0` to avoid RNG use entirely.
#'
#' @return A list with `summary` (one row per objective, with lower and upper
#'   states `bounded`, `unbounded` or `unresolved`, and an exact-constant flag),
#'   `nonempty`, `boundedness`, `directional`, `strict_direction`, `tails`, and
#'   `feasible_points`. The `check_point` closure uses the same strict membership
#'   check as finite nonemptiness witnesses. It returns false for unresolved
#'   membership and uses no optimizer feasibility tolerance. The `outer_bounds`
#'   closure, `outer_bounds(objectives, refine = TRUE, pool = NULL)`, returns
#'   numerical lower and upper bounds that contain the set for every objective
#'   column, from a checked positive definite combination of the constraints.
#'   Bounds are `NA` when no combination is verified, and exact zero loadings
#'   give exact zeros. These bounds need not be attained. Pass the returned
#'   `pool` attribute back to reuse candidate weights across calls. Every weight
#'   vector is verified again. Multivariate searches use `maxit`; scalar weight
#'   searches use a fixed tolerance. The search consumes no random numbers.
#' @export
#' @examples
#' ball <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
#' compute_quadratic_set_evidence(ball, diag(2), n_dir = 0)$summary
compute_quadratic_set_evidence <- function(quadratic, objectives,
                                           points = NULL, directions = NULL,
                                           n_dir = IDENTIFIED_SET_CONTROL$N_DIR,
                                           maxit = HETID_CONSTANTS$QUADRATIC_EVIDENCE_MAXIT) {
  input <- validate_quadratic_evidence(quadratic, objectives, points, directions)
  assert_scalar_integer_in_range(n_dir, "n_dir", 0, .Machine$integer.max)
  assert_scalar_integer_in_range(maxit, "maxit", 0, .Machine$integer.max)
  count <- ncol(objectives)
  objective_names <- quadratic_objective_names(objectives)
  constant <- colSums(objectives != 0) == 0L
  candidate_search <- quadratic_boundedness_search(quadratic, maxit)
  certificate <- candidate_search$certificate
  points <- quadratic_collect_points(quadratic, input$points, certificate, maxit)
  tail_result <- quadratic_collect_tails(
    quadratic, objectives, input, points, candidate_search, n_dir, maxit
  )
  nonempty <- nrow(points) > 0L || tail_result$nonempty
  lower <- tail_result$lower
  upper <- tail_result$upper
  directional <- quadratic_directional_bounds(quadratic, objectives)
  finite_lower <- rep(!is.null(certificate), count) | constant | directional$lower
  finite_upper <- rep(!is.null(certificate), count) | constant | directional$upper
  if (any(lower & finite_lower) || any(upper & finite_upper)) {
    stop_hetid("Quadratic geometry certificates conflict")
  }
  lower_state <- ifelse(lower, "unbounded",
    ifelse(finite_lower & nonempty, "bounded", "unresolved")
  )
  upper_state <- ifelse(upper, "unbounded",
    ifelse(finite_upper & nonempty, "bounded", "unresolved")
  )
  list(
    summary = data.frame(
      objective = objective_names, lower_state, upper_state,
      constant = constant, stringsAsFactors = FALSE
    ),
    nonempty = nonempty, boundedness = certificate,
    directional = directional$certificates,
    strict_direction = tail_result$strict,
    tails = stats::setNames(tail_result$tails, objective_names),
    feasible_points = points,
    check_point = quadratic_point_verifier(quadratic),
    outer_bounds = quadratic_outer_bounder(quadratic, certificate, maxit, nonempty)
  )
}
