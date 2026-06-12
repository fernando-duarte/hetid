# Tests for the skip-and-warn guards in process_w2_maturity

make_w2_guard_inputs <- function(n = 30, seed = 99) {
  set.seed(seed)
  list(
    yields = data.frame(y12 = rnorm(n, 2), y24 = rnorm(n, 2.5)),
    term_premia = data.frame(tp12 = rnorm(n, 0.5), tp24 = rnorm(n, 0.6)),
    pcs = matrix(rnorm(n * 4), ncol = 4)
  )
}

test_that("missing yield column warns and returns NULL", {
  inputs <- make_w2_guard_inputs()
  yields_no_y12 <- inputs$yields[, "y24", drop = FALSE]

  expect_warning(
    result <- process_w2_maturity(
      12, yields_no_y12, inputs$term_premia, inputs$pcs,
      n_pcs = 4
    ),
    "y12.*[Ss]kipping maturity 12"
  )
  expect_null(result)
})

test_that("missing term premium column warns and returns NULL", {
  inputs <- make_w2_guard_inputs()
  tp_no_tp12 <- inputs$term_premia[, "tp24", drop = FALSE]

  expect_warning(
    result <- process_w2_maturity(
      12, inputs$yields, tp_no_tp12, inputs$pcs,
      n_pcs = 4
    ),
    "tp12.*[Ss]kipping maturity 12"
  )
  expect_null(result)
})

test_that("maturity equal to highest available column warns and returns NULL", {
  # Maturity 24 needs y36/tp36 via compute_n_hat, which the
  # two-column inputs lack; must skip, not hard-error
  inputs <- make_w2_guard_inputs()

  expect_warning(
    result <- process_w2_maturity(
      24, inputs$yields, inputs$term_premia, inputs$pcs,
      n_pcs = 4
    ),
    "y36.*[Ss]kipping maturity 24"
  )
  expect_null(result)
})

test_that("missing previous-maturity column warns and returns NULL", {
  # Maturity 24 also needs y12/tp12 via compute_n_hat_previous
  inputs <- make_w2_guard_inputs()
  yields <- inputs$yields
  yields$y36 <- yields$y24 + 0.1
  term_premia <- inputs$term_premia
  term_premia$tp36 <- term_premia$tp24 + 0.01
  yields_no_y12 <- yields[, c("y24", "y36")]
  tp_no_tp12 <- term_premia[, c("tp24", "tp36")]

  expect_warning(
    result <- process_w2_maturity(
      24, yields_no_y12, tp_no_tp12, inputs$pcs,
      n_pcs = 4
    ),
    "y12.*[Ss]kipping maturity 24"
  )
  expect_null(result)
})

test_that("non-contiguous columns covering the maturity process cleanly", {
  # Maturity 60 needs only y48/y60/y72 and tp48/tp60/tp72; the
  # y12/tp12 columns are present but irrelevant
  set.seed(99)
  n <- 30
  yields <- data.frame(
    y12 = rnorm(n, 2), y48 = rnorm(n, 2.5),
    y60 = rnorm(n, 2.7), y72 = rnorm(n, 2.9)
  )
  term_premia <- data.frame(
    tp12 = rnorm(n, 0.5), tp48 = rnorm(n, 0.6),
    tp60 = rnorm(n, 0.7), tp72 = rnorm(n, 0.8)
  )
  pcs <- matrix(rnorm(n * 4), ncol = 4)

  result <- process_w2_maturity(60, yields, term_premia, pcs, n_pcs = 4)
  expect_type(result, "list")
  expect_true(result$r_squared >= 0 && result$r_squared <= 1)
})

test_that("single-row PCs warn as insufficient and return NULL", {
  inputs <- make_w2_guard_inputs()
  pcs_one_row <- inputs$pcs[1, , drop = FALSE]

  expect_warning(
    result <- process_w2_maturity(
      12, inputs$yields, inputs$term_premia, pcs_one_row,
      n_pcs = 4
    ),
    "Insufficient data for maturity 12"
  )
  expect_null(result)
})

test_that("too few complete observations warn and return NULL", {
  # Five observations give four SDF innovations, below the
  # n_pcs + 2 = 6 needed for the regression
  inputs <- make_w2_guard_inputs(n = 5)

  expect_warning(
    result <- process_w2_maturity(
      12, inputs$yields, inputs$term_premia, inputs$pcs,
      n_pcs = 4
    ),
    "Insufficient data for maturity"
  )
  expect_null(result)
})

test_that("well-formed inputs process without warnings", {
  inputs <- make_w2_guard_inputs()

  result <- process_w2_maturity(
    12, inputs$yields, inputs$term_premia, inputs$pcs,
    n_pcs = 4
  )
  expect_type(result, "list")
  expect_true(result$r_squared >= 0 && result$r_squared <= 1)
  expect_equal(length(result$residuals), result$n_obs)
})
