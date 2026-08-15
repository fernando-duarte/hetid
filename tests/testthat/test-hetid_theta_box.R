# Container validation for hetid_theta_box: one rejection per branch, so a
# malformed box cannot reach a downstream profile.

make_box_parts <- function(n_components = 2L, n_obs = 40L) {
  set.seed(42)
  w2 <- matrix(rnorm(n_obs * n_components), n_obs, n_components)
  colnames(w2) <- paste0("news", seq_len(n_components))
  list(
    bounds = data.frame(
      coef = colnames(w2),
      lower = rep(-1, n_components),
      upper = rep(1, n_components),
      row.names = NULL
    ),
    arg_lower = matrix(0, n_components, n_components),
    arg_upper = matrix(0, n_components, n_components),
    w1 = rnorm(n_obs),
    w2 = w2,
    quadratic = list(
      A_i = list(diag(n_components)),
      b_i = list(rep(0, n_components)),
      c_i = -1
    ),
    tau = 0.05,
    n_grid = 21L,
    n_obs = n_obs
  )
}

build_box <- function(...) {
  parts <- utils::modifyList(make_box_parts(), list(...))
  do.call(new_hetid_theta_box, parts)
}

test_that("constructor returns a validated container", {
  box <- build_box()
  expect_s3_class(box, "hetid_theta_box")
  expect_identical(attr(box, "n_components"), 2L)
  expect_identical(attr(box, "n_grid"), 21L)
  expect_identical(attr(box, "n_obs"), 40L)
  expect_invisible(validate_hetid_theta_box(box))
})

test_that("a non-finite tau is rejected at construction", {
  expect_error(build_box(tau = NA_real_), class = "hetid_error_bad_argument")
})

test_that("a grid below two points is rejected at construction", {
  expect_error(build_box(n_grid = 1L), class = "hetid_error_bad_argument")
})

test_that("validation rejects a non-box", {
  expect_error(
    validate_hetid_theta_box(list(bounds = NULL)),
    class = "hetid_error_bad_argument"
  )
})

test_that("validation rejects a bounds frame with the wrong columns", {
  box <- build_box()
  names(box$bounds) <- c("coef", "lo", "hi")
  expect_error(
    validate_hetid_theta_box(box),
    class = "hetid_error_bad_argument"
  )
})

test_that("validation rejects bounds with the wrong row count", {
  box <- build_box()
  box$bounds <- box$bounds[1, , drop = FALSE]
  expect_error(
    validate_hetid_theta_box(box),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("validation rejects duplicated component labels", {
  box <- build_box()
  box$bounds$coef <- c("news1", "news1")
  expect_error(
    validate_hetid_theta_box(box),
    class = "hetid_error_bad_argument"
  )
})

test_that("validation rejects a crossed interval", {
  box <- build_box()
  box$bounds$lower <- c(2, -1)
  expect_error(
    validate_hetid_theta_box(box),
    "lower <= upper",
    class = "hetid_error_bad_argument"
  )
})

test_that("validation rejects witnesses of the wrong shape", {
  box <- build_box()
  box$arg_lower <- matrix(0, 3, 3)
  expect_error(
    validate_hetid_theta_box(box),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("validation rejects a finite bound with no witness", {
  box <- build_box()
  box$arg_upper[1, ] <- NA_real_
  expect_error(
    validate_hetid_theta_box(box),
    "attaining it",
    class = "hetid_error_bad_argument"
  )
})

test_that("an infinite bound needs no witness", {
  box <- build_box()
  box$bounds$upper[1] <- Inf
  box$arg_upper[1, ] <- NA_real_
  expect_invisible(validate_hetid_theta_box(box))
})

test_that("validation rejects sources that do not match the box", {
  box <- build_box()
  box$w1 <- box$w1[-1]
  expect_error(
    validate_hetid_theta_box(box),
    "share rows",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("validation rejects a quadratic missing its pieces", {
  box <- build_box()
  box$quadratic$c_i <- NULL
  expect_error(
    validate_hetid_theta_box(box),
    class = "hetid_error_bad_argument"
  )
})

test_that("the assertion names the argument it guards", {
  expect_error(
    assert_hetid_theta_box(list(), arg = "box"),
    "box must be a hetid_theta_box",
    class = "hetid_error_bad_argument"
  )
  expect_invisible(assert_hetid_theta_box(build_box()))
})

test_that("print reports the slack and the unbounded side count", {
  box <- build_box()
  box$bounds$upper[1] <- Inf
  box$arg_upper[1, ] <- NA_real_
  expect_output(print(box), "hetid_theta_box")
  expect_output(print(box), "unbounded sides: 1")
  expect_invisible(print(box))
})
