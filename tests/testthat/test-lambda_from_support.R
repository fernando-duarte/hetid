test_that("weights are zero-padded onto the union axis", {
  out <- lambda_from_support(
    list(c(1L, 3L), 2L),
    list(matrix(c(5, 7), 2, 1), matrix(3, 1, 1)),
    j_total = 4
  )
  expect_identical(out[[1]], matrix(c(5, 0, 7, 0), 4, 1))
  expect_identical(out[[2]], matrix(c(0, 3, 0, 0), 4, 1))
})

test_that("multi-column weights pad every combination", {
  out <- lambda_from_support(
    list(c(2L, 4L)),
    list(matrix(c(1, 2, 3, 4), 2, 2)),
    j_total = 5
  )
  expect_identical(
    out[[1]],
    matrix(c(0, 1, 0, 2, 0, 0, 3, 0, 4, 0), 5, 2)
  )
})

test_that("support columns left NULL produce NULL lambda entries", {
  out <- lambda_from_support(
    list(NULL, 1:2),
    list(NULL, matrix(c(1, 1), 2, 1)),
    j_total = 3
  )
  expect_null(out[[1]])
  expect_identical(out[[2]], matrix(c(1, 1, 0), 3, 1))
})

test_that("padded output feeds the general builder unchanged", {
  sys <- make_general_test_system(i_dim = 2, j_dim = 4)
  lambda <- lambda_from_support(
    list(c(1L, 2L), c(3L, 4L)),
    list(matrix(c(0.6, 0.8), 2, 1), matrix(c(1, 1), 2, 1)),
    j_total = 4
  )
  manual <- list(
    matrix(c(0.6, 0.8, 0, 0), 4, 1),
    matrix(c(0, 0, 1, 1), 4, 1)
  )
  expect_identical(lambda, manual)
  qs <- build_general_quadratic_system(lambda, 0.2, sys$moments)
  qs_manual <- build_general_quadratic_system(manual, 0.2, sys$moments)
  expect_identical(qs$quadratic, qs_manual$quadratic)
})

test_that("support and weights NULL patterns must match", {
  expect_error(
    lambda_from_support(
      list(1:2, NULL), list(NULL, matrix(1, 1, 1)), 3
    ),
    class = "hetid_error_bad_argument"
  )
})

test_that("weight row counts must match support sizes", {
  expect_error(
    lambda_from_support(list(1:2), list(matrix(1, 3, 1)), 4),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("malformed support entries are rejected", {
  weights <- list(matrix(1, 2, 1))
  expect_error(
    lambda_from_support(list(c(1, 5)), weights, 4),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    lambda_from_support(list(c(1, 1)), weights, 4),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    lambda_from_support(list(c(1.5, 2)), weights, 4),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    lambda_from_support(list(integer(0)), list(matrix(1, 0, 1)), 4),
    class = "hetid_error_bad_argument"
  )
})

test_that("entirely NULL supports and bad j_total are rejected", {
  expect_error(
    lambda_from_support(list(NULL), list(NULL), 3),
    class = "hetid_error_bad_argument"
  )
  weights <- list(matrix(1, 1, 1))
  expect_error(
    lambda_from_support(list(1L), weights, 0),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    lambda_from_support(list(1L), weights, 2.5),
    class = "hetid_error_bad_argument"
  )
})

test_that("non-numeric or non-finite weights are rejected", {
  expect_error(
    lambda_from_support(list(1:2), list(matrix(c(1, NA), 2, 1)), 3),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    lambda_from_support(
      list(1:2), list(matrix(letters[1:2], 2, 1)), 3
    ),
    class = "hetid_error_bad_argument"
  )
})

test_that("all-zero padded columns defer to the builder rejection", {
  sys <- make_general_test_system(i_dim = 1, j_dim = 4)
  lambda <- lambda_from_support(
    list(1:2), list(matrix(0, 2, 1)),
    j_total = 4
  )
  expect_identical(lambda[[1]], matrix(0, 4, 1))
  expect_error(
    build_general_quadratic_system(lambda, 0.2, sys$moments),
    class = "hetid_error_bad_argument"
  )
})
