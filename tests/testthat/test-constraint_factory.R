# Tests for make_constraint_checker

test_that("checker value matches the quadratic form computed directly", {
  a <- matrix(c(2, 0.5, 0.5, 1), 2, 2)
  b <- c(-1, 2)
  c_val <- 0.25
  theta <- c(0.3, -0.7)

  check <- make_constraint_checker(a, b, c_val)
  expected <- as.numeric(t(theta) %*% a %*% theta) +
    sum(b * theta) + c_val

  expect_equal(check(theta), expected, tolerance = 1e-12)
})

test_that("sign convention is negative inside the set, positive outside", {
  # Unit disk: theta' I theta - 1 <= 0
  check <- make_constraint_checker(diag(2), c(0, 0), -1)

  expect_lt(check(c(0.5, 0)), 0)
  expect_gt(check(c(2, 0)), 0)
  expect_equal(check(c(1, 0)), 0)
})

test_that("works for one-dimensional theta", {
  # 4 theta^2 + 2 theta - 6 has roots 1 and -1.5
  check <- make_constraint_checker(matrix(4), 2, -6)

  expect_equal(check(1), 0)
  expect_equal(check(-1.5), 0)
  expect_lt(check(0), 0)
  expect_gt(check(2), 0)
})

test_that("closure captures arguments at creation time", {
  a <- diag(2)
  b <- c(0, 0)
  c_val <- -1
  check <- make_constraint_checker(a, b, c_val)

  a[1, 1] <- 100
  b <- c(50, 50)
  c_val <- 99

  expect_equal(check(c(1, 0)), 0)
})

test_that("returns a scalar numeric for vector theta", {
  check <- make_constraint_checker(diag(3), c(1, 1, 1), 0)
  value <- check(c(0.1, 0.2, 0.3))

  expect_type(value, "double")
  expect_length(value, 1)
})

test_that("factory rejects non-matrix or non-square A_i", {
  expect_error(
    make_constraint_checker("not a matrix", c(1, 2), 0),
    "A_i must be a square numeric matrix",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    make_constraint_checker(matrix(1:6, 2, 3), c(1, 2), 0),
    "A_i must be a square numeric matrix",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    make_constraint_checker(matrix("a", 2, 2), c(1, 2), 0),
    "A_i must be a square numeric matrix",
    class = "hetid_error_bad_argument"
  )
})

test_that("factory rejects b_i whose length mismatches A_i", {
  expect_error(
    make_constraint_checker(diag(2), c(1, 1, 1, 1), 0),
    "b_i must have length nrow(A_i) = 2; got length 4",
    fixed = TRUE,
    class = "hetid_error_dimension_mismatch"
  )
  expect_error(
    make_constraint_checker(diag(2), "not numeric", 0),
    "b_i must be a numeric vector",
    class = "hetid_error_bad_argument"
  )
})

test_that("factory rejects non-scalar c_i", {
  expect_error(
    make_constraint_checker(diag(2), c(1, 1), c(0, 1)),
    "c_i must be a numeric scalar",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    make_constraint_checker(diag(2), c(1, 1), "zero"),
    "c_i must be a numeric scalar",
    class = "hetid_error_bad_argument"
  )
})

test_that("checker built from validated inputs still evaluates correctly", {
  a <- diag(2)
  b <- c(1, 1)
  check <- make_constraint_checker(a, b, -1)
  theta <- c(0.5, 0.25)
  expected <- as.numeric(t(theta) %*% a %*% theta) + sum(b * theta) - 1

  expect_equal(check(theta), expected, tolerance = 1e-12)
})
