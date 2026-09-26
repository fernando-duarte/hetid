# Shared adapter for linear objectives over a quadratic set.
profile_objective_direction <- function(objective) {
  magnitude <- max(abs(objective))
  if (magnitude == 0) {
    return(objective)
  }
  normalized <- objective / magnitude
  normalized <- normalized / sqrt(sum(normalized^2))
  if (any(objective != 0 & normalized == 0)) {
    return(NULL)
  }
  normalized
}

.solve_linear_objective_bound <- function(
  quadratic, objective_vec, direction, boxes, feas_tol, xtol_rel, maxeval,
  coordinate_index = NULL, evidence = NULL, evidence_index = 1L
) {
  direction <- match.arg(direction, c("min", "max"))
  assert_quadratic_symmetric(quadratic)
  dimension <- ncol(quadratic$A_i[[1L]])
  stopifnot(length(objective_vec) == dimension, length(boxes) == 3L)
  if (is.null(evidence)) {
    evidence <- paper_profile_evidence(quadratic, matrix(objective_vec, ncol = 1L))
  }
  delta <- .derive_theta_scale(quadratic)
  omega <- .derive_constraint_scales(quadratic, delta)
  sign_mult <- if (direction == "min") 1 else -1
  solver_objective <- profile_objective_direction(objective_vec)
  solve_box <- function(box) {
    .solve_scaled(
      quadratic, if (is.null(coordinate_index)) NA_integer_ else coordinate_index,
      sign_mult, delta, omega, box, xtol_rel, maxeval,
      objective = if (is.null(coordinate_index)) solver_objective else NULL
    )
  }
  .classify_profile_search(
    objective_vec, direction, boxes, solve_box, delta, evidence, evidence_index,
    candidate_is_endpoint = function(theta) {
      residual <- .feasibility_residual(quadratic, theta, omega)
      is.finite(residual) && abs(residual) <= feas_tol
    }
  )
}
