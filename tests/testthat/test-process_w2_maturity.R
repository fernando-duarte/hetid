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
    "y1.*[Ss]kipping maturity 1"
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
    "tp1.*[Ss]kipping maturity 1"
  )
  expect_null(result)
})

test_that("maturity equal to highest available column warns and returns NULL", {
  # Maturity 2 needs y3/tp3 via compute_n_hat, which the
  # two-column inputs lack; must skip, not hard-error
  inputs <- make_w2_guard_inputs()

  expect_warning(
    result <- process_w2_maturity(
      2, inputs$yields, inputs$term_premia, inputs$pcs,
      n_pcs = 4
    ),
    "y3.*[Ss]kipping maturity 2"
  )
  expect_null(result)
})

test_that("missing previous-maturity column warns and returns NULL", {
  # Maturity 2 also needs y1/tp1 via compute_n_hat_previous
  inputs <- make_w2_guard_inputs()
  yields <- inputs$yields
  yields$y3 <- yields$y2 + 0.1
  term_premia <- inputs$term_premia
  term_premia$tp3 <- term_premia$tp2 + 0.01
  yields_no_y1 <- yields[, c("y2", "y3")]
  tp_no_tp1 <- term_premia[, c("tp2", "tp3")]

  expect_warning(
    result <- process_w2_maturity(
      2, yields_no_y1, tp_no_tp1, inputs$pcs,
      n_pcs = 4
    ),
    "y1.*[Ss]kipping maturity 2"
  )
  expect_null(result)
})

test_that("non-contiguous columns covering the maturity process cleanly", {
  # Maturity 5 needs only y4/y5/y6 and tp4/tp5/tp6; the y1/tp1
  # columns are present but irrelevant
  set.seed(99)
  n <- 30
  yields <- data.frame(
    y1 = rnorm(n, 2), y4 = rnorm(n, 2.5),
    y5 = rnorm(n, 2.7), y6 = rnorm(n, 2.9)
  )
  term_premia <- data.frame(
    tp1 = rnorm(n, 0.5), tp4 = rnorm(n, 0.6),
    tp5 = rnorm(n, 0.7), tp6 = rnorm(n, 0.8)
  )
  pcs <- matrix(rnorm(n * 4), ncol = 4)

  result <- process_w2_maturity(5, yields, term_premia, pcs, n_pcs = 4)
  expect_type(result, "list")
  expect_true(result$r_squared >= 0 && result$r_squared <= 1)
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
