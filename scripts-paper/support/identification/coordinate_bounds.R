# Coordinate profiles use shared evidence when called as a group.
solve_profile_bound <- function(
  quadratic, component_index, direction = c("min", "max"),
  boxes = PAPER_QUADRATIC_CONTROL$solver_boxes,
  feas_tol = PAPER_QUADRATIC_CONTROL$feasibility_tolerance,
  xtol_rel = PAPER_QUADRATIC_CONTROL$solver_xtol_rel,
  maxeval = PAPER_QUADRATIC_CONTROL$solver_maxeval,
  evidence = NULL, evidence_index = 1L
) {
  dimension <- ncol(quadratic$A_i[[1L]])
  objective_vec <- numeric(dimension)
  objective_vec[[component_index]] <- 1
  .solve_linear_objective_bound(
    quadratic, objective_vec, direction, boxes, feas_tol, xtol_rel, maxeval,
    coordinate_index = component_index, evidence = evidence, evidence_index = evidence_index
  )
}
