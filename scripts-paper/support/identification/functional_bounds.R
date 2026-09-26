# Linear-functional and aggregate bounds share one geometry evaluation.
solve_linear_functional_bound <- function(
  quadratic, objective_vec, direction = c("min", "max"),
  boxes = PAPER_QUADRATIC_CONTROL$solver_boxes,
  feas_tol = PAPER_QUADRATIC_CONTROL$feasibility_tolerance,
  xtol_rel = PAPER_QUADRATIC_CONTROL$solver_xtol_rel,
  maxeval = PAPER_QUADRATIC_CONTROL$solver_maxeval,
  evidence = NULL, evidence_index = 1L
) {
  .solve_linear_objective_bound(
    quadratic, objective_vec, direction, boxes, feas_tol, xtol_rel, maxeval,
    evidence = evidence, evidence_index = evidence_index
  )
}

solve_all_profile_bounds <- function(quadratic, ..., evidence = NULL, points = NULL) {
  n_comp <- ncol(quadratic$A_i[[1]])
  if (is.null(evidence)) {
    evidence <- paper_profile_evidence(quadratic, diag(n_comp), points = points)
  }
  rows <- lapply(seq_len(n_comp), function(k) {
    lo <- solve_profile_bound(quadratic, k, "min", ...,
      evidence = evidence, evidence_index = k
    )
    hi <- solve_profile_bound(quadratic, k, "max", ...,
      evidence = evidence, evidence_index = k
    )
    tab <- data.frame(
      component = k, lower = lo$bound, upper = hi$bound,
      width = hi$bound - lo$bound,
      bounded_lower = lo$bounded, bounded_upper = hi$bounded,
      valid_lower = lo$valid, valid_upper = hi$valid, stringsAsFactors = FALSE
    )
    list(
      tab = tab, points = Filter(Negate(is.null), list(lo$theta, hi$theta)),
      corrections = rbind(
        profile_correction_record(lo, k, "min"),
        profile_correction_record(hi, k, "max")
      )
    )
  })
  out <- do.call(rbind, lapply(rows, `[[`, "tab"))
  attr(out, "profile_points") <- unlist(lapply(rows, `[[`, "points"), recursive = FALSE)
  attr(out, "profile_corrections") <- do.call(rbind, lapply(rows, `[[`, "corrections"))
  out
}
