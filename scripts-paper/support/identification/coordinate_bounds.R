# Coordinate profile bounds over the paper's quadratic identified set.

# Returns list(bound, bounded, valid). The shared classifier grows the solver
# box and distinguishes finite bounds, unbounded rays, and failed searches.
solve_profile_bound <- function(
  quadratic,
  component_index,
  direction = c("min", "max"),
  box1 = PAPER_QUADRATIC_CONTROL$solver_boxes[[1L]],
  box2 = PAPER_QUADRATIC_CONTROL$solver_boxes[[2L]],
  box3 = PAPER_QUADRATIC_CONTROL$solver_boxes[[3L]],
  feas_tol = PAPER_QUADRATIC_CONTROL$feasibility_tolerance,
  xtol_rel = PAPER_QUADRATIC_CONTROL$solver_xtol_rel,
  maxeval = PAPER_QUADRATIC_CONTROL$solver_maxeval
) {
  direction <- match.arg(direction)
  assert_quadratic_symmetric(quadratic)

  delta <- .derive_theta_scale(quadratic)
  omega <- .derive_constraint_scales(quadratic, delta)
  sign_mult <- if (direction == "min") 1 else -1
  unbounded_value <- if (direction == "min") -Inf else Inf
  solve_box <- function(box) {
    .solve_scaled(
      quadratic,
      component_index,
      sign_mult,
      delta,
      omega,
      box,
      xtol_rel,
      maxeval
    )
  }
  value_at <- function(result) {
    delta * result$phi[component_index]
  }
  finalize <- function(result) {
    .finalize_bound(
      quadratic,
      delta,
      omega,
      result,
      component_index,
      feas_tol
    )
  }
  trusted_at <- function(result, box) {
    abs(result$phi[component_index]) <
      PAPER_QUADRATIC_CONTROL$bound_edge_rtol * box
  }

  .classify_profile_search(
    quadratic = quadratic,
    delta = delta,
    omega = omega,
    boxes = c(box1, box2, box3),
    solve_box = solve_box,
    value_at = value_at,
    finalize = finalize,
    trusted_at = trusted_at,
    unbounded_value = unbounded_value,
    target_edge_is_unbounded = TRUE,
    feasibility_tolerance = feas_tol
  )
}
