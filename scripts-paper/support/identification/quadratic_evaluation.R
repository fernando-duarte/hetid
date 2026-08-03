# Canonical evaluation of the paper's quadratic inequality systems.

PAPER_QUADRATIC_CONTROL <- list(
  constraint_scale_floor_rtol = 1e-12,
  symmetry_rtol = 1e-8,
  solver_boxes = c(1e6, 1e9, 1e10),
  solver_xtol_rel = 1e-8,
  solver_maxeval = 1000L,
  feasibility_tolerance = 1e-4,
  admission_tolerance = 1e-10,
  crossing_range_rtol = 1e-8,
  point_identification_tolerance = 1e-8,
  bound_edge_rtol = 0.99,
  bound_stability_rtol = 1e-3,
  unbounded_growth_factor = 5,
  polish_blow_factor = 5,
  # rounds of the news-box multistart (theta_box_multistart.R): round one solves
  # the axis pool, round two re-seeds from its argmaxes and is the one that
  # recovers the missed branch, and later rounds have never moved an endpoint
  box_multistart_rounds = 4L,
  # significant digits at which two multistart argmaxes count as the same start.
  # Two solves that reach one vertex from different starts agree to about the
  # solver's own xtol_rel (1e-8) and so differ in the ninth digit; deduplicating
  # at full precision would leave the round-two queue full of copies and make
  # the widening cost hundreds of redundant solves
  box_multistart_dedup_digits = 6L,
  # a polished log-variance endpoint attained this far outside the news box it
  # was searched against, relative to the box's own span, proves the box is not
  # the outer screen it is contracted to be (engine/polish_support.R). Held at
  # the feasibility tolerance the endpoint itself was accepted under: judging a
  # boundary point against a tighter box criterion than its own admission test
  # would report solver noise as a contract breach
  box_escape_rtol = 1e-4
)

quadratic_constraint_values <- function(
  theta,
  quadratic,
  omega = NULL
) {
  n_constraints <- length(quadratic$A_i)
  if (is.null(omega)) {
    omega <- rep(1, n_constraints)
  }
  stopifnot(length(omega) == n_constraints)
  if (is.null(dim(theta))) {
    theta <- as.numeric(theta)
    return(vapply(seq_len(n_constraints), function(index) {
      (
        drop(
          t(theta) %*% quadratic$A_i[[index]] %*% theta
        ) +
          sum(quadratic$b_i[[index]] * theta) +
          quadratic$c_i[index]
      ) / omega[index]
    }, numeric(1)))
  }
  points <- as.matrix(theta)
  values <- vapply(seq_len(n_constraints), function(index) {
    (
      rowSums(
        (points %*% quadratic$A_i[[index]]) * points
      ) +
        drop(points %*% quadratic$b_i[[index]]) +
        quadratic$c_i[index]
    ) / omega[index]
  }, numeric(nrow(points)))
  matrix(
    values,
    nrow = nrow(points),
    ncol = n_constraints
  )
}

quadratic_constraint_jacobian <- function(
  theta,
  quadratic,
  omega = NULL,
  theta_scale = 1
) {
  theta <- as.numeric(theta)
  n_constraints <- length(quadratic$A_i)
  if (is.null(omega)) {
    omega <- rep(1, n_constraints)
  }
  stopifnot(length(omega) == n_constraints)
  # vapply with FUN.VALUE = numeric(1) drops dim, so t() returns a
  # 1 x n_constraints row for a scalar theta where slsqp's hinjac needs
  # n_constraints x 1. as.vector of the vapply result is constraint-major in
  # every shape, so byrow = TRUE rebuilds the contract shape uniformly.
  jacobian <- vapply(seq_len(n_constraints), function(index) {
    theta_scale * (
      2 * drop(quadratic$A_i[[index]] %*% theta) +
        quadratic$b_i[[index]]
    ) / omega[index]
  }, numeric(length(theta)))
  matrix(
    jacobian,
    nrow = n_constraints,
    ncol = length(theta),
    byrow = TRUE
  )
}

quadratic_constraint_residual <- function(
  theta,
  quadratic,
  omega = NULL
) {
  max(quadratic_constraint_values(theta, quadratic, omega))
}

# membership at unit constraint weights: coordinates non-missing (an infinite
# coordinate is decided by the residual, not by this guard) and the
# most-binding constraint satisfied. Stricter than logvar_feasible_grid's
# admission test, which normalizes by .derive_constraint_scales and admits at a
# roundoff tolerance -- this certifies a point the pipeline publishes or seeds
# a search from
quadratic_point_feasible <- function(quadratic, theta) {
  !anyNA(theta) && quadratic_constraint_residual(theta, quadratic) <= 0
}

assert_quadratic_symmetric <- function(
  quadratic,
  tolerance = PAPER_QUADRATIC_CONTROL$symmetry_rtol
) {
  for (matrix_i in quadratic$A_i) {
    scale <- max(1, max(abs(matrix_i)))
    if (max(abs(matrix_i - t(matrix_i))) > tolerance * scale) {
      stop(
        "A_i must be symmetric; symmetrize as (A+t(A))/2 before solving -- ",
        "the analytic SLSQP Jacobian assumes symmetry",
        call. = FALSE
      )
    }
  }
  invisible(quadratic)
}
