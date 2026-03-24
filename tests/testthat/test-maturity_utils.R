# --- resolve_maturities ---

test_that("resolve_maturities infers from named inputs", {
  nms <- paste0("maturity_", c(2, 4, 6))
  stats_list <- list(
    L_i = setNames(1:3, nms),
    V_i = setNames(4:6, nms)
  )
  result <- resolve_maturities(
    NULL, stats_list,
    n_components = 6
  )
  expect_equal(result, c(2L, 4L, 6L))
})

test_that(
  "resolve_maturities validates names against explicit",
  {
    nms_wrong <- paste0("maturity_", c(1, 3, 5))
    stats_list <- list(
      L_i = setNames(1:3, nms_wrong)
    )
    expect_error(
      resolve_maturities(
        c(2, 4, 6), stats_list,
        n_components = 6
      ),
      "do not match maturities"
    )
  }
)

test_that("resolve_maturities errors on unnamed subsets", {
  stats_list <- list(L_i = 1:3)
  expect_error(
    resolve_maturities(
      NULL, stats_list,
      n_components = 6
    ),
    "Cannot infer maturities"
  )
})

test_that("resolve_maturities allows unnamed full-size", {
  stats_list <- list(L_i = 1:6)
  result <- resolve_maturities(
    NULL, stats_list,
    n_components = 6
  )
  expect_equal(result, 1:6)
})

test_that("resolve_maturities rejects duplicate maturities", {
  nms <- paste0("maturity_", c(2, 2, 6))
  stats_list <- list(L_i = setNames(1:3, nms))
  expect_error(
    resolve_maturities(
      NULL, stats_list,
      n_components = 6
    ),
    "Duplicate"
  )
})

test_that("resolve_maturities rejects mixed named/unnamed", {
  nms <- paste0("maturity_", c(2, 4, 6))
  stats_list <- list(
    L_i = setNames(1:3, nms),
    V_i = 4:6
  )
  expect_error(
    resolve_maturities(
      NULL, stats_list,
      n_components = 6
    ),
    "mix named and unnamed"
  )
})

test_that("resolve_maturities passes through explicit", {
  stats_list <- list(L_i = 1:3)
  result <- resolve_maturities(
    c(2, 4, 6), stats_list,
    n_components = 6
  )
  expect_equal(result, c(2, 4, 6))
})
