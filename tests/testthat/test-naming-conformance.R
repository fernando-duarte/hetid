# Locks the migrated outcome-lag naming grammar so a regression to the legacy
# `y1_lag<h>` column names is caught. This is a narrow conformance check, not a
# general naming validator.

test_that("lag_grammar_names follows default-unit drop-first-digit grammar", {
  expect_identical(lag_grammar_names("y1", 1L), "l.y1")
  expect_identical(lag_grammar_names("y1", 3L), c("l.y1", "l2.y1", "l3.y1"))
  expect_identical(lag_grammar_names("y1", 0L), character(0))
})

test_that("build_y1_lag_columns emits grammar names, never legacy y1_lag*", {
  nms <- colnames(build_y1_lag_columns(seq_len(10), 2L))
  expect_identical(nms, c("l.y1", "l2.y1"))
  expect_false(any(grepl("y1_lag[0-9]", nms)))
})

test_that("W1 design coefficients carry grammar lag names, not legacy names", {
  res <- compute_w1_residuals(n_pcs = 3, data = variables, y1_lags = 2L)
  cn <- names(res$coefficients)
  expect_true(all(c("l.y1", "l2.y1") %in% cn))
  expect_false(any(grepl("y1_lag[0-9]", cn)))
})
