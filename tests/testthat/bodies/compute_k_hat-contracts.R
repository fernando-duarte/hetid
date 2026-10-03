{
  test_env <- setup_standard_test_env()
  decimal_yields <- test_env$yields / 100

  count_unit_warnings <- function(expr) {
    n <- 0L
    withCallingHandlers(
      force(expr),
      hetid_warning_unit_scale = function(w) {
        n <<- n + 1L
        invokeRestart("muffleWarning")
      }
    )
    n
  }

  # boundary i == step warns via the compute_n_hat_previous direct branch
  expect_identical(
    count_unit_warnings(
      compute_k_hat(decimal_yields, test_env$term_premia, i = 12)
    ),
    1L
  )
  # non-boundary i > step warns via n_hat_series, never twice
  expect_identical(
    count_unit_warnings(
      compute_k_hat(decimal_yields, test_env$term_premia, i = 24)
    ),
    1L
  )
  # percentage-point yields raise no warning
  expect_identical(
    count_unit_warnings(
      compute_k_hat(test_env$yields, test_env$term_premia, i = 12)
    ),
    0L
  )
}
