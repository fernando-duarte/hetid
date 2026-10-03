test_that("strict interior distinguishes boundary, missing and outside centers", {
  qs <- mean_profile_ball()
  inside <- check_identified_set_center(qs, c(0, 0))
  expect_identical(inside, list(interior = TRUE, reason = "interior", slack = -1))
  for (point in list(c(1, 0), c(2, 0))) {
    result <- check_identified_set_center(qs, point)
    expect_false(result$interior)
    expect_identical(result$reason, "not_strictly_inside")
  }
  expect_identical(
    check_identified_set_center(qs, NULL),
    list(interior = FALSE, reason = "missing_center", slack = NULL)
  )
  qs$A_i$second <- diag(2)
  qs$b_i$second <- c(0, 0)
  qs$c_i <- c(-1, 0)
  expect_false(check_identified_set_center(qs, c(0, 0))$interior)
})

test_that("finite inputs with overflowing slacks are numerical infeasibility", {
  qs <- list(A_i = list(matrix(.Machine$double.xmax)), b_i = list(0), c_i = -1)
  result <- check_identified_set_center(qs, 2)
  expect_false(result$interior)
  expect_identical(result$reason, "nonfinite_constraints")
  expect_true(is.infinite(result$slack))
})

test_that("malformed centers and systems remain structured argument failures", {
  qs <- mean_profile_ball()
  expect_error(check_identified_set_center(qs, NA_real_), class = "hetid_error_bad_argument")
  expect_error(check_identified_set_center(qs, c(0, 0, 0)),
    class = "hetid_error_dimension_mismatch"
  )
  expect_error(check_identified_set_center(qs, matrix(0, 1, 2)), class = "hetid_error")
  expect_error(check_identified_set_center(list(), c(0, 0)), class = "hetid_error")
})
