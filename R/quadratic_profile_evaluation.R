profile_constraint_values <- function(theta, quadratic, omega) {
  n_constraints <- length(quadratic$A_i)
  assert_dimension_ok(length(omega) == n_constraints, "omega has wrong length")
  if (is.null(dim(theta))) {
    theta <- as.numeric(theta)
    return(vapply(seq_len(n_constraints), function(index) {
      (drop(t(theta) %*% quadratic$A_i[[index]] %*% theta) +
        sum(quadratic$b_i[[index]] * theta) + quadratic$c_i[index]) / omega[index]
    }, numeric(1)))
  }
  point_rows <- as.matrix(theta)
  values <- vapply(seq_len(n_constraints), function(index) {
    (rowSums((point_rows %*% quadratic$A_i[[index]]) * point_rows) +
      drop(point_rows %*% quadratic$b_i[[index]]) + quadratic$c_i[index]) / omega[index]
  }, numeric(nrow(point_rows)))
  matrix(values, nrow = nrow(point_rows), ncol = n_constraints)
}

profile_constraint_jacobian <- function(theta, quadratic, omega, theta_scale) {
  theta <- as.numeric(theta)
  n_constraints <- length(quadratic$A_i)
  assert_dimension_ok(length(omega) == n_constraints, "omega has wrong length")
  jacobian <- vapply(seq_len(n_constraints), function(index) {
    theta_scale * (2 * drop(quadratic$A_i[[index]] %*% theta) +
      quadratic$b_i[[index]]) / omega[index]
  }, numeric(length(theta)))
  matrix(jacobian, nrow = n_constraints, ncol = length(theta), byrow = TRUE)
}

profile_residual <- function(quadratic, theta, omega) {
  max(profile_constraint_values(theta, quadratic, omega))
}
