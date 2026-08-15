test_that("LOG_VARIANCE_CONTROL carries the ratified numerical controls", {
  expect_identical(LOG_VARIANCE_CONTROL$GLM_EPSILON, 1e-10)
  expect_identical(LOG_VARIANCE_CONTROL$GLM_MAXIT, 100L)
  expect_identical(LOG_VARIANCE_CONTROL$SCORE_TOLERANCE, 1e-8)
  expect_identical(LOG_VARIANCE_CONTROL$RANK_TOLERANCE, 1e-10)
  expect_identical(LOG_VARIANCE_CONTROL$RCOND_TOLERANCE, 1e-10)
  expect_identical(LOG_VARIANCE_CONTROL$HAC_LAGS, 4L)
  expect_identical(LOG_VARIANCE_CONTROL$SE_TYPES, c("naive", "hc0", "hc1", "hac"))
})

test_that("LOG_VARIANCE_HARVEY_CONTROL carries the paper's Harvey controls", {
  expect_identical(LOG_VARIANCE_HARVEY_CONTROL$SCORE_TOLERANCE, 1e-8)
  expect_identical(LOG_VARIANCE_HARVEY_CONTROL$RANK_TOLERANCE, 1e-8)
  expect_identical(LOG_VARIANCE_HARVEY_CONTROL$RCOND_TOLERANCE, 1e-10)
  expect_identical(LOG_VARIANCE_HARVEY_CONTROL$NEWTON_RCOND_TOLERANCE, 1e-12)
  expect_identical(LOG_VARIANCE_HARVEY_CONTROL$LINE_SEARCH_HALVINGS, 30L)
  expect_identical(LOG_VARIANCE_HARVEY_CONTROL$Q_NOISE_MULTIPLIER, 4)
  expect_identical(LOG_VARIANCE_HARVEY_CONTROL$SCORE_PROGRESS_MULTIPLIER, 10)
  expect_identical(LOG_VARIANCE_HARVEY_CONTROL$MAXIT, 1000L)
  expect_identical(LOG_VARIANCE_HARVEY_CONTROL$REL_CHANGE_TOLERANCE, 1e-10)
  expect_identical(
    LOG_VARIANCE_HARVEY_CONTROL$SE_TYPES,
    c("expected", "observed", "opg", "robust", "hac")
  )
})

test_that("HETID_CONSTANTS carries the tau=0 point tolerance", {
  expect_identical(HETID_CONSTANTS$TAU0_POINT_TOLERANCE, 1e-8)
})
