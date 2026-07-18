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
  polish_blow_factor = 5
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
  t(vapply(seq_len(n_constraints), function(index) {
    theta_scale * (
      2 * drop(quadratic$A_i[[index]] %*% theta) +
        quadratic$b_i[[index]]
    ) / omega[index]
  }, numeric(length(theta))))
}

quadratic_constraint_residual <- function(
  theta,
  quadratic,
  omega = NULL
) {
  max(quadratic_constraint_values(theta, quadratic, omega))
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
