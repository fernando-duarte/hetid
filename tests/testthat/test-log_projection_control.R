test_that("log-projection controls hold their documented values", {
  expect_identical(LOG_PROJECTION_CONTROL$METHODS, "log")
  expect_identical(LOG_PROJECTION_CONTROL$MULTIPLIER, 1)
  expect_identical(LOG_PROJECTION_CONTROL$SENSITIVITY_MULTIPLIERS, c(0.5, 1, 2))
  expect_identical(LOG_PROJECTION_CONTROL$RANK_TOLERANCE, 1e-10)
  expect_identical(LOG_PROJECTION_CONTROL$MEAN_ZERO_TOLERANCE, 1e-8)
  expect_identical(LOG_PROJECTION_CONTROL$SCALE_TOLERANCE, 1e-12)
  expect_identical(
    names(LOG_PROJECTION_CONTROL),
    c(
      "METHODS", "MULTIPLIER", "SENSITIVITY_MULTIPLIERS", "RANK_TOLERANCE",
      "MEAN_ZERO_TOLERANCE", "SCALE_TOLERANCE"
    )
  )
})
