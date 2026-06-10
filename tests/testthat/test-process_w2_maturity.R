# Tests for the skip-and-warn guards in process_w2_maturity

make_w2_guard_inputs <- function(n = 30, seed = 99) {
  set.seed(seed)
  list(
    yields = data.frame(y1 = rnorm(n, 2), y2 = rnorm(n, 2.5)),
    term_premia = data.frame(tp1 = rnorm(n, 0.5), tp2 = rnorm(n, 0.6)),
    pcs = matrix(rnorm(n * 4), ncol = 4)
  )
}

test_that("missing yield column warns and returns NULL", {
  inputs <- make_w2_guard_inputs()
  yields_no_y1 <- inputs$yields[, "y2", drop = FALSE]

  expect_warning(
    result <- process_w2_maturity(
      1, yields_no_y1, inputs$term_premia, inputs$pcs,
      n_pcs = 4
    ),
    "Yield column y1 not found"
  )
  expect_null(result)
})

test_that("missing term premium column warns and returns NULL", {
  inputs <- make_w2_guard_inputs()
  tp_no_tp1 <- inputs$term_premia[, "tp2", drop = FALSE]

  expect_warning(
    result <- process_w2_maturity(
      1, inputs$yields, tp_no_tp1, inputs$pcs,
      n_pcs = 4
    ),
    "Term premium column tp1 not found"
  )
  expect_null(result)
})

test_that("single-row PCs warn as insufficient and return NULL", {
  inputs <- make_w2_guard_inputs()
  pcs_one_row <- inputs$pcs[1, , drop = FALSE]

  expect_warning(
    result <- process_w2_maturity(
      1, inputs$yields, inputs$term_premia, pcs_one_row,
      n_pcs = 4
    ),
    "Insufficient data for maturity 1"
  )
  expect_null(result)
})

test_that("too few complete observations warn and return NULL", {
  # Five observations give four SDF innovations, below the
  # n_pcs + 2 = 6 needed for the regression
  inputs <- make_w2_guard_inputs(n = 5)

  expect_warning(
    result <- process_w2_maturity(
      1, inputs$yields, inputs$term_premia, inputs$pcs,
      n_pcs = 4
    ),
    "Insufficient data for maturity"
  )
  expect_null(result)
})

test_that("well-formed inputs process without warnings", {
  inputs <- make_w2_guard_inputs()

  result <- process_w2_maturity(
    1, inputs$yields, inputs$term_premia, inputs$pcs,
    n_pcs = 4
  )
  expect_type(result, "list")
  expect_true(result$r_squared >= 0 && result$r_squared <= 1)
  expect_equal(length(result$residuals), result$n_obs)
})
