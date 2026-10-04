profile_retry_tables <- function(retry) {
  quadratic <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -1)
  evidence_points <- list()
  table_evidence <- list()
  state_summary <- data.frame(lower_state = "bounded", upper_state = "bounded")
  theta_table <- function(status, lower) {
    data.frame(
      coef = "theta", set_lower = lower, set_upper = 1,
      lower_status = status, upper_status = "bounded"
    )
  }
  testthat::local_mocked_bindings(
    profile_evidence = function(quadratic, objectives, points = NULL) {
      evidence_points[[length(evidence_points) + 1L]] <<- points
      list(summary = state_summary, objectives = objectives, generation = length(evidence_points))
    },
    profile_interval_tables = function(quadratic, beta1r, beta2r, evidence, control) {
      table_evidence[[length(table_evidence) + 1L]] <<- evidence
      first <- retry && evidence$generation == 1L
      list(
        theta = theta_table(if (first) "unreliable" else "bounded", if (first) NA_real_ else -1),
        beta1 = data.frame(
          coef = "intercept", set_lower = -1, set_upper = 1,
          lower_status = "bounded", upper_status = "bounded"
        )
      )
    },
    profile_multistart = function(quadratic, warm, evidence, control) {
      list(points = list(0.5), evidence = evidence)
    },
    .package = "hetid"
  )
  tables <- profile_tables_widened(
    quadratic, c(intercept = 0),
    matrix(1, 1L, 1L, dimnames = list("theta", "intercept")),
    matrix(0), list(), QUADRATIC_PROFILE_CONTROL
  )
  list(tables = tables, evidence_points = evidence_points, table_evidence = table_evidence)
}

test_that("an unreliable endpoint is rebuilt from the widened points", {
  run <- profile_retry_tables(retry = TRUE)
  expect_length(run$evidence_points, 2L)
  expect_identical(run$evidence_points[[2L]], profile_point_matrix(list(0.5), 1L))
  expect_length(run$table_evidence, 2L)
  expect_identical(run$table_evidence[[2L]]$generation, 2L)
  expect_identical(run$tables$theta$lower_status, "bounded")
  expect_identical(run$tables$theta$set_lower, -1)
  expect_identical(attr(run$tables, "profile_points"), list(0.5))
})

test_that("reliable tables with unchanged evidence are built once", {
  run <- profile_retry_tables(retry = FALSE)
  expect_length(run$evidence_points, 1L)
  expect_length(run$table_evidence, 1L)
  expect_identical(attr(run$tables, "profile_points"), list(0.5))
})

test_that("multistart rechecks geometry from accepted points when unbounded", {
  quadratic <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -1)
  seen <- list()
  refreshed <- list(refreshed = TRUE)
  testthat::local_mocked_bindings(
    profile_multistart_round = function(quadratic, queue, evidence, delta, search_box,
                                        control) {
      list(0.5)
    },
    profile_evidence = function(quadratic, objectives, points, directions) {
      seen[[length(seen) + 1L]] <<- list(points = points, directions = directions)
      refreshed
    },
    .package = "hetid"
  )
  evidence <- list(
    feasible_points = matrix(0.25, 1L),
    check_point = function(point) all(is.finite(point)) && all(abs(point) <= 1),
    strict_direction = NULL, boundedness = NULL, objectives = matrix(1)
  )
  control <- QUADRATIC_PROFILE_CONTROL
  control$MULTISTART_ROUNDS <- 1L
  out <- profile_multistart(quadratic, list(0.75), evidence, control)
  accepted <- matrix(c(0.75, 0.25, 0.5), ncol = 1L)
  expect_identical(seen, list(list(points = accepted, directions = accepted)))
  expect_identical(out$evidence, refreshed)
  # a boundedness certificate makes the recheck unnecessary
  evidence$boundedness <- list()
  expect_identical(profile_multistart(quadratic, list(0.75), evidence, control)$evidence, evidence)
  expect_length(seen, 1L)
})
