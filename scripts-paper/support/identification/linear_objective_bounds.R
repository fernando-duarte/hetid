# Shared façade adapter for linear objectives over a quadratic set.

.solve_linear_objective_bound <- function(
  quadratic,
  objective_vec,
  direction,
  boxes,
  feas_tol,
  xtol_rel,
  maxeval,
  coordinate_index = NULL
) {
  direction <- match.arg(direction, c("min", "max"))
  assert_quadratic_symmetric(quadratic)
  dimension <- ncol(quadratic$A_i[[1L]])
  stopifnot(
    length(objective_vec) == dimension,
    length(boxes) == 3L
  )
  coordinate <- !is.null(coordinate_index)
  delta <- .derive_theta_scale(quadratic)
  omega <- .derive_constraint_scales(quadratic, delta)
  sign_mult <- if (direction == "min") 1 else -1
  solve_box <- function(box) {
    .solve_scaled(
      quadratic,
      if (coordinate) coordinate_index else NA_integer_,
      sign_mult,
      delta,
      omega,
      box,
      xtol_rel,
      maxeval,
      objective = if (coordinate) NULL else objective_vec
    )
  }
  value_at <- if (coordinate) {
    function(result) delta * result$phi[coordinate_index]
  } else {
    function(result) delta * sum(objective_vec * result$phi)
  }
  finalize <- if (coordinate) {
    function(result) {
      .finalize_bound(
        quadratic, delta, omega, result,
        coordinate_index, feas_tol
      )
    }
  } else {
    function(result) {
      .finalize_linear_bound(
        quadratic, delta, omega, result,
        objective_vec, feas_tol
      )
    }
  }
  .classify_profile_search(
    quadratic = quadratic,
    delta = delta,
    omega = omega,
    boxes = boxes,
    solve_box = solve_box,
    value_at = value_at,
    finalize = finalize,
    trusted_at = if (coordinate) {
      function(result, box) {
        abs(result$phi[coordinate_index]) <
          PAPER_QUADRATIC_CONTROL$bound_edge_rtol * box
      }
    } else {
      function(result, box) TRUE
    },
    unbounded_value =
      if (direction == "min") -Inf else Inf,
    target_edge_is_unbounded = coordinate,
    feasibility_tolerance = feas_tol
  )
}
