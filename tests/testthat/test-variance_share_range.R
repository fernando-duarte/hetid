test_that("block ranges use the joint set and retain interior minima", {
  control <- VARIANCE_SHARE_CONTROL
  control$grid_points_per_axis <- 11L
  ball <- mean_profile_ball()
  tab <- variance_share_ball_table()
  share <- variance_share_quadratic(diag(2), c(0, 0), 0)
  got <- variance_share_range(tab, ball, share, control)
  expect_true(all(abs(got - c(0, 1)) <= 1e-6))
  expect_lt(got[2], sum(tab$outer_upper^2))
  tab$status <- "unreliable"
  tab$set_lower <- tab$set_upper <- NA_real_
  expect_equal(variance_share_range(tab, ball, share, control), got, tolerance = 0)
  tab$outer_upper[1] <- NA_real_
  expect_identical(variance_share_range(tab, ball, share, control), c(NA_real_, NA_real_))
  interior <- variance_share_quadratic(matrix(1), -.4, .04)
  expect_true(all(abs(variance_share_range(
    variance_share_ball_table(1), mean_profile_ball(1),
    interior, control
  ) - c(0, 1.44)) <= 1e-7))
})

test_that("enclosure, grid admission and mandatory polishing failures stay explicit", {
  tab <- variance_share_ball_table()
  ball <- mean_profile_ball()
  share <- variance_share_quadratic(diag(2), c(0, 0), 0)
  control <- VARIANCE_SHARE_CONTROL
  control$grid_points_per_axis <- 2L
  expect_error(variance_share_range(tab, ball, share, control), "No feasible grid point",
    class = "hetid_error"
  )
  control$grid_points_per_axis <- 11L
  expect_error(variance_share_range(within(tab, outer_lower[1] <- 2), ball, share, control),
    "exceeds",
    class = "hetid_error"
  )
  expect_error(variance_share_range(within(tab, outer_upper[1] <- Inf), ball, share, control),
    "lacks finite",
    class = "hetid_error"
  )
  expect_error(variance_share_range(within(tab, set_upper[1] <- 2), ball, share, control),
    "outside",
    class = "hetid_error"
  )
  control$grid_points_limit <- 100
  expect_error(variance_share_range(tab, ball, share, control), "too many points")
  control$grid_points_limit <- 2e6
  local_mocked_bindings(solve_quadratic_program = function(...) {
    list(theta = c(NA_real_, NA_real_), feasibility_residual = NA_real_)
  })
  expect_error(variance_share_range(tab, ball, share, control), "No polished share",
    class = "hetid_error"
  )
})

test_that("finite pre-clamp residuals through tolerance pass without a convergence gate", {
  control <- VARIANCE_SHARE_CONTROL
  control$grid_points_per_axis <- 3L
  residual <- -1
  theta <- 1.2
  starts <- list()
  local_mocked_bindings(solve_quadratic_program = function(
    quadratic, x0, objective,
    gradient, lower, upper, objective_scale, control, ...
  ) {
    starts[[length(starts) + 1L]] <<- x0
    expect_identical(objective_scale, "none")
    list(theta = theta, feasibility_residual = residual, status = -1L)
  })
  share <- variance_share_quadratic(matrix(1), 0, 0)
  evaluate <- function() {
    variance_share_range(
      variance_share_ball_table(1),
      mean_profile_ball(1), share, control
    )
  }
  for (value in c(-1, 5e-5, 1e-4)) {
    residual <- value
    expect_identical(evaluate(), c(0, 1))
  }
  expect_identical(starts[1:6], lapply(
    c(0, -1, 1, 0, -1, 1),
    function(x) setNames(x, "Var1")
  ))
  for (value in c(1.01e-4, NA_real_, NaN, Inf, -Inf)) {
    residual <- value
    expect_error(evaluate(), "No polished share", class = "hetid_error")
  }
  residual <- -1
  theta <- NA_real_
  expect_error(evaluate(), "No polished share", class = "hetid_error")
})
