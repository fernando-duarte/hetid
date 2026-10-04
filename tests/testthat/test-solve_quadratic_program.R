test_that("SLSQP uses the joint constraint and admits an interior nonlinear optimum", {
  qs <- mean_profile_ball()
  linear <- solve_quadratic_program(
    qs, c(0, 0), function(x) -sum(x),
    function(x) c(-1, -1), c(-2, -2), c(2, 2)
  )
  expect_equal(linear$theta, rep(1 / sqrt(2), 2), tolerance = 1e-6)
  expect_lt(abs(linear$feasibility_residual), 1e-6)
  inside <- solve_quadratic_program(
    qs, c(2, 2), function(x) sum((x - 0.2)^2),
    function(x) 2 * (x - 0.2), c(-0.5, -0.5), c(0.5, 0.5)
  )
  expect_equal(inside$theta, c(0.2, 0.2), tolerance = 1e-7)
  expect_lt(inside$feasibility_residual, 0)
})

test_that("objective scaling retains the affine endpoint", {
  qs <- list(A_i = list(diag(2) / 100), b_i = list(c(0, 0)), c_i = -1)
  ordinary <- solve_quadratic_program(
    qs, c(0, 0), function(x) x[1],
    function(x) c(1, 0), c(-20, -20), c(20, 20)
  )
  scaled <- solve_quadratic_program(qs, c(0, 0), function(x) x[1],
    function(x) c(1, 0), c(-20, -20), c(20, 20),
    objective_scale = "variable"
  )
  expect_equal(ordinary$theta, c(-10, 0), tolerance = 1e-6)
  expect_equal(scaled$theta, c(-10, 0), tolerance = 1e-6)
  expect_equal(scaled$theta, profile_theta_scale(qs) * scaled$phi, tolerance = 0)
})

test_that("solver errors have explicit catch behavior", {
  qs <- mean_profile_ball()
  failed <- solve_quadratic_program(
    qs, c(0, 0), function(x) stop("callback failed"),
    function(x) c(1, 0), c(-2, -2), c(2, 2)
  )
  expect_identical(failed, list(
    theta = c(NA_real_, NA_real_),
    phi = c(NA_real_, NA_real_), feasibility_residual = NA_real_
  ))
  expect_error(
    solve_quadratic_program(qs, c(0, 0), function(x) stop("callback failed"),
      function(x) c(1, 0), c(-2, -2), c(2, 2),
      catch_errors = FALSE
    ),
    class = "hetid_error_solver"
  )
  original <- new_hetid_error("budget exhausted", "hetid_error_budget")
  got <- tryCatch(
    solve_quadratic_program(qs, c(0, 0), function(x) stop(original),
      function(x) c(1, 0), c(-2, -2), c(2, 2),
      catch_errors = FALSE
    ),
    hetid_error_budget = identity
  )
  expect_identical(got, original)
})

test_that("argument failures are outside the numerical catch", {
  qs <- mean_profile_ball()
  run <- function(...) {
    solve_quadratic_program(qs, ...,
      objective = sum,
      gradient = function(x) rep(1, length(x))
    )
  }
  expect_error(run(x0 = 0, lower = c(-1, -1), upper = c(1, 1)),
    class = "hetid_error_dimension_mismatch"
  )
  expect_error(run(x0 = c(0, 0), lower = c(1, 1), upper = c(-1, -1)),
    class = "hetid_error_bad_argument"
  )
  expect_error(run(x0 = c(0, 0), lower = c(-Inf, -1), upper = c(1, 1)),
    class = "hetid_error_bad_argument"
  )
  expect_error(run(
    x0 = c(0, 0), lower = c(-1, -1), upper = c(1, 1),
    objective_scale = "invalid"
  ), class = "hetid_error_bad_argument")
  broken <- QUADRATIC_PROFILE_CONTROL
  broken$SOLVER_MAXEVAL <- 0L
  expect_error(run(x0 = c(0, 0), lower = c(-1, -1), upper = c(1, 1), control = broken),
    class = "hetid_error_bad_argument"
  )
})

test_that("normalized constraint derivatives match finite differences", {
  qs <- list(
    A_i = list(matrix(c(2, 0.3, 0.3, 1), 2), diag(c(1, 4))),
    b_i = list(c(0.5, -1), c(-0.1, 0.2)), c_i = c(-2, -3)
  )
  delta <- profile_theta_scale(qs)
  omega <- profile_constraint_scales(qs, delta, QUADRATIC_PROFILE_CONTROL)
  phi <- c(0.1, 0.2)
  step <- 1e-6
  finite <- sapply(seq_along(phi), function(j) {
    d <- numeric(length(phi))
    d[j] <- step
    (profile_constraint_values(delta * (phi + d), qs, omega) -
      profile_constraint_values(delta * (phi - d), qs, omega)) / (2 * step)
  })
  expect_equal(profile_constraint_jacobian(delta * phi, qs, omega, delta),
    finite,
    tolerance = 1e-8
  )
  point_rows <- rbind(delta * phi, c(0, 0))
  expect_equal(
    profile_constraint_values(point_rows, qs, omega),
    t(apply(point_rows, 1, profile_constraint_values, quadratic = qs, omega = omega))
  )
})
