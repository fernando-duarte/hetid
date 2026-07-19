# Coordinate profile bounds over the paper's quadratic identified set.

# Returns list(bound, bounded, valid). The shared classifier grows the solver
# box and distinguishes finite bounds, unbounded rays, and failed searches.
solve_profile_bound <- function(
  quadratic,
  component_index,
  direction = c("min", "max"),
  boxes = PAPER_QUADRATIC_CONTROL$solver_boxes,
  feas_tol = PAPER_QUADRATIC_CONTROL$feasibility_tolerance,
  xtol_rel = PAPER_QUADRATIC_CONTROL$solver_xtol_rel,
  maxeval = PAPER_QUADRATIC_CONTROL$solver_maxeval
) {
  dimension <- ncol(quadratic$A_i[[1L]])
  objective_vec <- numeric(dimension)
  objective_vec[[component_index]] <- 1
  .solve_linear_objective_bound(
    quadratic,
    objective_vec,
    direction,
    boxes,
    feas_tol,
    xtol_rel,
    maxeval,
    coordinate_index = component_index
  )
}
