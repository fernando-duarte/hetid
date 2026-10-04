test_that("finite scaling overflow follows the numerical catch contract", {
  qs <- list(A_i = list(matrix(.Machine$double.xmax)), b_i = list(0), c_i = -1)
  got <- solve_quadratic_program(qs, 0, sum, function(x) 1, -1, 1)
  expect_identical(got, list(
    theta = NA_real_, phi = NA_real_,
    feasibility_residual = NA_real_
  ))
  error <- tryCatch(solve_quadratic_program(qs, 0, sum, function(x) 1,
    -1, 1,
    catch_errors = FALSE
  ), error = identity)
  expect_s3_class(error, "hetid_error_solver")
  expect_s3_class(error$parent, "error")
  expect_match(conditionMessage(error$parent), "infinite or missing")
  expect_error(profile_theta_scale(qs), class = "hetid_error_solver")
  expect_error(profile_constraint_scales(qs, 1, QUADRATIC_PROFILE_CONTROL),
    class = "hetid_error_solver"
  )
})

test_that("finite non-success parameters remain candidates", {
  testthat::local_mocked_bindings(slsqp = function(...) {
    list(par = c(0.2, 0.1), convergence = -4L, message = "roundoff limited")
  }, .package = "nloptr")
  got <- solve_quadratic_program(
    mean_profile_ball(), c(0, 0), sum,
    function(x) c(1, 1), c(-1, -1), c(1, 1)
  )
  expect_identical(got$phi, c(0.2, 0.1))
  expect_identical(got$theta, c(0.2, 0.1))
  expect_equal(got$feasibility_residual, -0.95, tolerance = 1e-15)
})

test_that("finite backend parameters with overflowing theta return missing candidates", {
  delta <- 1.0681910382076414
  qs <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -delta^2)
  seen <- NULL
  callback_calls <- 0L
  callback <- function(x) {
    callback_calls <<- callback_calls + 1L
    0
  }
  testthat::local_mocked_bindings(slsqp = function(x0, fn, gr, lower, upper, ...) {
    seen <<- list(par = upper, lower = lower, upper = upper)
    list(par = upper, convergence = -4L, message = "roundoff limited")
  }, .package = "nloptr")
  for (catching in c(TRUE, FALSE)) {
    got <- solve_quadratic_program(qs, 0, callback, callback,
      -.Machine$double.xmax, .Machine$double.xmax,
      catch_errors = catching
    )
    expect_true(is.finite(seen$par))
    expect_true(seen$par >= seen$lower && seen$par <= seen$upper)
    expect_false(is.finite(profile_theta_scale(qs) * seen$par))
    expect_identical(got, list(
      theta = NA_real_, phi = NA_real_,
      feasibility_residual = NA_real_
    ))
    expect_identical(callback_calls, 0L)
  }
})

test_that("an actual multistart round aborts on its first backend failure", {
  calls <- 0L
  original <- new_hetid_error("failed start", "hetid_error_test_start")
  testthat::local_mocked_bindings(slsqp = function(...) {
    calls <<- calls + 1L
    stop(original)
  }, .package = "nloptr")
  qs <- mean_profile_ball()
  evidence <- profile_evidence(qs, diag(2), matrix(0, 1, 2))
  got <- tryCatch(profile_multistart(
    qs, list(c(0, 0)), evidence,
    QUADRATIC_PROFILE_CONTROL
  ), error = identity)
  expect_identical(got, original)
  expect_identical(calls, 1L)
})

test_that("derived profile overflow is numerical failure rather than bad caller input", {
  extreme <- list(A_i = list(matrix(1e-200)), b_i = list(0), c_i = -1e200)
  expect_identical(profile_theta_scale(extreme), Inf)
  low_level <- solve_quadratic_program(extreme, 0, identity, function(x) 1, -1e300, 1e300)
  expect_identical(low_level, list(
    theta = NA_real_, phi = NA_real_,
    feasibility_residual = NA_real_
  ))
  finite_scale <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -4)
  expect_identical(profile_theta_scale(finite_scale), 2)
  overflowing_boxes <- QUADRATIC_PROFILE_CONTROL
  overflowing_boxes$SOLVER_BOXES <- rep(.Machine$double.xmax, 3L)
  cases <- list(
    list(quadratic = extreme, control = QUADRATIC_PROFILE_CONTROL),
    list(quadratic = finite_scale, control = overflowing_boxes)
  )
  calls <- 0L
  testthat::local_mocked_bindings(solve_quadratic_program = function(...) {
    calls <<- calls + 1L
    stop_hetid("Unrepresentable bounds reached the solver")
  }, .package = "hetid")
  for (case in cases) {
    qs <- case$quadratic
    control <- case$control
    evidence <- profile_evidence(qs, matrix(1), matrix(0))
    endpoint <- profile_linear_bound(qs, 1, "min", evidence, 1L, control)
    expect_identical(endpoint, list(bound = NA_real_, bounded = FALSE, valid = FALSE))
    expect_error(profile_multistart(qs, list(0), evidence, control),
      class = "hetid_error_solver"
    )
    error <- tryCatch(profile_quadratic_coefficients(qs, c(intercept = 2),
      matrix(0, 1, 1, dimnames = list("theta", "intercept")),
      points = matrix(0), warm = list(0), control = control
    ), error = identity)
    expect_s3_class(error, "hetid_error_solver")
    expect_false(inherits(error, "hetid_error_bad_argument"))
  }
  expect_identical(calls, 0L)
})
