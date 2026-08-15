# Container validation for the structural (beta1) block of hetid_theta_box:
# the same one-rejection-per-branch coverage the theta block has.

test_that("validation rejects a beta1 frame with the wrong columns", {
  box <- build_box()
  names(box$beta1_bounds) <- c("coef", "lo", "hi")
  expect_error(
    validate_hetid_theta_box(box),
    "beta1_bounds",
    class = "hetid_error_bad_argument"
  )
})

test_that("validation rejects non-numeric beta1 bounds", {
  box <- build_box()
  box$beta1_bounds$lower <- as.character(box$beta1_bounds$lower)
  expect_error(
    validate_hetid_theta_box(box),
    "numeric",
    class = "hetid_error_bad_argument"
  )
})

test_that("validation rejects a missing beta1 bound", {
  # the search is seeded from a feasible center, so a bound is finite or
  # infinite and never missing; a missing one would also dodge the witness rule
  box <- build_box()
  box$beta1_bounds$upper[2L] <- NA_real_
  expect_error(
    validate_hetid_theta_box(box),
    "non-missing",
    class = "hetid_error_bad_argument"
  )
})

test_that("validation rejects a crossed beta1 interval", {
  box <- build_box()
  box$beta1_bounds$lower[2L] <- 0.5
  expect_error(
    validate_hetid_theta_box(box),
    "lower <= upper",
    class = "hetid_error_bad_argument"
  )
})

test_that("validation rejects a crossed infinite interval", {
  box <- build_box()
  box$beta1_bounds$lower[2L] <- Inf
  box$beta1_bounds$upper[2L] <- -Inf
  box$beta1_arg_lower[2L, ] <- NA_real_
  box$beta1_arg_upper[2L, ] <- NA_real_
  expect_error(
    validate_hetid_theta_box(box),
    "lower <= upper",
    class = "hetid_error_bad_argument"
  )
})

test_that("validation rejects beta1 witnesses whose columns are not n_components", {
  box <- build_box()
  box$beta1_arg_lower <- matrix(0, 2L, 3L)
  expect_error(
    validate_hetid_theta_box(box),
    "beta1_arg_lower",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("validation rejects beta1 witnesses whose rows do not match the frame", {
  box <- build_box()
  box$beta1_arg_upper <- matrix(0, 3L, 2L)
  expect_error(
    validate_hetid_theta_box(box),
    "beta1_arg_upper",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("validation rejects a finite beta1 bound with no witness", {
  box <- build_box()
  box$beta1_arg_upper[2L, ] <- NA_real_
  expect_error(
    validate_hetid_theta_box(box),
    "attaining it",
    class = "hetid_error_bad_argument"
  )
})

test_that("validation rejects an infinite witness beside a finite beta1 bound", {
  box <- build_box()
  box$beta1_arg_upper[2L, 1L] <- Inf
  expect_error(
    validate_hetid_theta_box(box),
    "beta1_arg_upper",
    class = "hetid_error_bad_argument"
  )
})

test_that("an infinite beta1 bound needs no witness", {
  box <- build_box()
  box$beta1_bounds$lower[2L] <- -Inf
  box$beta1_arg_lower[2L, ] <- NA_real_
  expect_invisible(validate_hetid_theta_box(box))
})

test_that("validation rejects a null_loading flag not named by the beta1 rows", {
  box <- build_box()
  attr(box, "null_loading") <- c(TRUE, FALSE)
  expect_error(
    validate_hetid_theta_box(box),
    "null_loading",
    class = "hetid_error_bad_argument"
  )
  attr(box, "null_loading") <- c(x1 = FALSE, "(Intercept)" = TRUE)
  expect_error(
    validate_hetid_theta_box(box),
    "null_loading",
    class = "hetid_error_bad_argument"
  )
})

test_that("print reports the beta1 rows, their unbounded sides and the zero loadings", {
  box <- build_box()
  box$beta1_bounds$lower[2L] <- -Inf
  box$beta1_arg_lower[2L, ] <- NA_real_
  expect_output(print(box), "structural coefficients \\(beta1 axis\\): 2")
  expect_output(print(box), "unbounded beta1 sides: 1")
  expect_output(print(box), "unbounded sides: 0")
  expect_output(print(box), "loadings treated as zero: \\(Intercept\\)")
})
