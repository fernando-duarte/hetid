# Tests for the bundled-PC fallback branches in load_w2_pcs

test_that("bundled fallback errors when PC columns are missing", {
  data("variables", package = "hetid")
  only_two_pcs <- variables[, setdiff(names(variables), c("pc3", "pc4", "pc5", "pc6"))]
  local_mocked_bindings(get_bundled_variables = function() only_two_pcs)

  expect_error(
    expect_warning(
      load_w2_pcs(NULL, n_pcs = 4, n_obs = 100),
      "bundled"
    ),
    "Missing PC columns",
    class = "hetid_error_bad_argument"
  )
})

test_that("bundled fallback returns NULL dates without a date column", {
  data("variables", package = "hetid")
  no_date <- variables[, setdiff(names(variables), "date")]
  local_mocked_bindings(get_bundled_variables = function() no_date)

  expect_warning(
    result <- load_w2_pcs(NULL, n_pcs = 2, n_obs = 50),
    "bundled"
  )
  expect_null(result$dates)
  expect_equal(ncol(result$pcs), 2)
})
