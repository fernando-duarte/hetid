# Independent value and derivative oracles for shared quadratic helpers.

quadratic_oracle <- function(theta, quadratic, index, omega = 1) {
  (
    sum(theta * drop(quadratic$A_i[[index]] %*% theta)) +
      sum(quadratic$b_i[[index]] * theta) +
      quadratic$c_i[[index]]
  ) / omega
}

quadratic_fixture <- list(
  A_i = list(
    matrix(c(2, 0.5, 0.5, 1), nrow = 2L),
    matrix(c(-0.25, 0.1, 0.1, 1.5), nrow = 2L)
  ),
  b_i = list(c(-1, 0.5), c(0.25, -0.75)),
  c_i = c(-3, 0.4)
)
theta_fixture <- c(0.75, -1.25)
omega_fixture <- c(2, 0.5)
expected_vector <- vapply(
  seq_along(quadratic_fixture$A_i),
  function(index) {
    quadratic_oracle(
      theta_fixture,
      quadratic_fixture,
      index,
      omega_fixture[[index]]
    )
  },
  numeric(1)
)
check(
  "quadratic vector values match an independent scalar oracle",
  isTRUE(all.equal(
    quadratic_constraint_values(
      theta_fixture,
      quadratic_fixture,
      omega_fixture
    ),
    expected_vector
  ))
)

theta_rows <- rbind(theta_fixture, c(-0.5, 2), c(0, 0))
expected_matrix <- t(vapply(
  seq_len(nrow(theta_rows)),
  function(row) {
    vapply(
      seq_along(quadratic_fixture$A_i),
      function(index) {
        quadratic_oracle(
          theta_rows[row, ],
          quadratic_fixture,
          index,
          omega_fixture[[index]]
        )
      },
      numeric(1)
    )
  },
  numeric(length(quadratic_fixture$A_i))
))
check(
  "quadratic matrix values preserve rows and multiple constraints",
  isTRUE(all.equal(
    quadratic_constraint_values(
      theta_rows,
      quadratic_fixture,
      omega_fixture
    ),
    expected_matrix
  ))
)

one_constraint <- lapply(quadratic_fixture, function(field) field[1L])
one_vector <- quadratic_constraint_values(theta_fixture, one_constraint)
one_values <- quadratic_constraint_values(theta_rows, one_constraint)
check(
  "quadratic vector and matrix values retain one constraint",
  length(one_vector) == 1L &&
    isTRUE(all.equal(
      one_vector[[1L]],
      quadratic_oracle(theta_fixture, one_constraint, 1L)
    )) &&
    identical(dim(one_values), c(nrow(theta_rows), 1L)) &&
    isTRUE(all.equal(
      drop(one_values),
      unname(apply(
        theta_rows,
        1L,
        quadratic_oracle,
        one_constraint,
        1L
      ))
    ))
)

central_difference <- function(point, value_fn, step = 1e-6) {
  vapply(seq_along(point), function(index) {
    offset <- numeric(length(point))
    offset[[index]] <- step
    (value_fn(point + offset) - value_fn(point - offset)) / (2 * step)
  }, numeric(length(value_fn(point))))
}
numeric_jacobian <- central_difference(
  theta_fixture,
  function(theta) {
    quadratic_constraint_values(theta, quadratic_fixture, omega_fixture)
  }
)
check(
  "quadratic Jacobian matches independent central differences",
  isTRUE(all.equal(
    quadratic_constraint_jacobian(
      theta_fixture,
      quadratic_fixture,
      omega_fixture
    ),
    numeric_jacobian,
    tolerance = 1e-7
  ))
)

theta_scale <- 2.75
phi <- theta_fixture / theta_scale
scaled_jacobian <- central_difference(
  phi,
  function(value) {
    quadratic_constraint_values(
      theta_scale * value,
      quadratic_fixture,
      omega_fixture
    )
  }
)
check(
  "quadratic Jacobian applies the theta-scale chain rule",
  isTRUE(all.equal(
    quadratic_constraint_jacobian(
      theta_scale * phi,
      quadratic_fixture,
      omega_fixture,
      theta_scale = theta_scale
    ),
    scaled_jacobian,
    tolerance = 1e-7
  ))
)

unit_ball <- list(
  A_i = list(diag(2L)),
  b_i = list(c(0, 0)),
  c_i = -1
)
check(
  "quadratic feasibility uses the hin less-than-or-equal-to-zero sign",
  quadratic_constraint_residual(c(0, 0), unit_ball) < 0 &&
    quadratic_constraint_residual(c(2, 0), unit_ball) > 0
)

asymmetric <- unit_ball
asymmetric$A_i[[1L]][1L, 2L] <- 0.25
asymmetric_error <- tryCatch(
  {
    assert_quadratic_symmetric(asymmetric)
    ""
  },
  error = conditionMessage
)
check(
  "asymmetric quadratic matrices raise the exact shared-helper error",
  identical(
    asymmetric_error,
    paste0(
      "A_i must be symmetric; symmetrize as (A+t(A))/2 before solving -- ",
      "the analytic SLSQP Jacobian assumes symmetry"
    )
  )
)
