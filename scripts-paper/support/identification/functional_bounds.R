# Linear-functional and aggregate profile bounds for quadratic identified sets.

solve_linear_functional_bound <- function(
  quadratic,
  objective_vec,
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

  dimension <- ncol(quadratic$A_i[[1L]])
  if (length(objective_vec) != dimension) {
    stop(
      "objective_vec length (",
      length(objective_vec),
      ") must equal the theta dimension (",
      dimension,
      ")",
      call. = FALSE
    )
  }
  delta <- .derive_theta_scale(quadratic)
  omega <- .derive_constraint_scales(quadratic, delta)
  sign_mult <- if (direction == "min") 1 else -1
  unbounded_value <- if (direction == "min") -Inf else Inf
  solve_box <- function(box) {
    .solve_scaled(
      quadratic,
      NA_integer_,
      sign_mult,
      delta,
      omega,
      box,
      xtol_rel,
      maxeval,
      objective = objective_vec
    )
  }
  value_at <- function(result) {
    delta * sum(objective_vec * result$phi)
  }
  finalize <- function(result) {
    .finalize_linear_bound(
      quadratic,
      delta,
      omega,
      result,
      objective_vec,
      feas_tol
    )
  }

  .classify_profile_search(
    quadratic = quadratic,
    delta = delta,
    omega = omega,
    boxes = c(box1, box2, box3),
    solve_box = solve_box,
    value_at = value_at,
    finalize = finalize,
    trusted_at = function(result, box) TRUE,
    unbounded_value = unbounded_value,
    target_edge_is_unbounded = FALSE,
    feasibility_tolerance = feas_tol
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

# Closed-form point identification at tau = 0. The constraints are perfect
# squares, so a full-rank and consistent Q theta = L system identifies a point.
solve_point_identification <- function(
  components,
  tol = PAPER_QUADRATIC_CONTROL$point_identification_tolerance
) {
  qmat <- do.call(rbind, components$Q_i)
  lvec <- components$L_i
  if (nrow(qmat) < ncol(qmat) ||
    qr(qmat, tol = tol)$rank < ncol(qmat)) {
    return(NULL)
  }
  point <- qr.solve(qmat, lvec, tol = tol)
  if (any(!is.finite(point))) {
    return(NULL)
  }
  if (max(abs(qmat %*% point - lvec)) >
    tol * max(1, max(abs(lvec)))) {
    return(NULL)
  }
  list(theta = as.numeric(point), cond = kappa(qmat))
}
