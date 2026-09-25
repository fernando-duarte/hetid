# Linear-functional and aggregate profile bounds for quadratic identified sets.

solve_linear_functional_bound <- function(
  quadratic,
  objective_vec,
  direction = c("min", "max"),
  boxes = PAPER_QUADRATIC_CONTROL$solver_boxes,
  feas_tol = PAPER_QUADRATIC_CONTROL$feasibility_tolerance,
  xtol_rel = PAPER_QUADRATIC_CONTROL$solver_xtol_rel,
  maxeval = PAPER_QUADRATIC_CONTROL$solver_maxeval
) {
  .solve_linear_objective_bound(
    quadratic,
    objective_vec,
    direction,
    boxes,
    feas_tol,
    xtol_rel,
    maxeval
  )
}

# Profile bounds for every coordinate. Width is Inf or NA when a side is
# unbounded or failed.
solve_all_profile_bounds <- function(quadratic, ...) {
  n_comp <- ncol(quadratic$A_i[[1]])
  rows <- lapply(seq_len(n_comp), function(k) {
    lo <- solve_profile_bound(quadratic, k, "min", ...)
    hi <- solve_profile_bound(quadratic, k, "max", ...)
    data.frame(
      component = k,
      lower = lo$bound,
      upper = hi$bound,
      width = hi$bound - lo$bound,
      bounded_lower = lo$bounded,
      bounded_upper = hi$bounded,
      valid_lower = lo$valid,
      valid_upper = hi$valid,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
