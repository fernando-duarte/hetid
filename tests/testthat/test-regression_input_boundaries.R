test_that("W1 rejects nonnumeric and retained infinite outcomes with metadata", {
  d <- residual_chain_fixture()$aligned
  outcome <- HETID_CONSTANTS$CONSUMPTION_GROWTH_COL
  old <- options(warn = 2)
  on.exit(options(old))
  for (value in list(as.character(d[[outcome]]), replace(d[[outcome]], 3L, Inf))) {
    bad <- d
    bad[[outcome]] <- value
    err <- tryCatch(compute_w1_residuals(1L, bad), error = identity)
    expect_s3_class(err, "hetid_error_bad_argument")
    expect_identical(err$arg, "y")
  }
  bad <- d
  bad$pc1[3L] <- Inf
  err <- tryCatch(compute_w1_residuals(1L, bad), error = identity)
  expect_s3_class(err, "hetid_error_bad_argument")
  expect_identical(err$arg, "pcs")
  bad$pc1 <- as.character(d$pc1)
  err <- tryCatch(compute_w1_residuals(1L, bad), error = identity)
  expect_s3_class(err, "hetid_error_bad_argument")
  expect_identical(err$arg, "data")
})

test_that("empty exog is rejected before assigning default names", {
  d <- residual_chain_fixture()$aligned
  for (exog in list(matrix(numeric(), nrow(d), 0L), d[, FALSE, drop = FALSE])) {
    err <- tryCatch(compute_w1_residuals(data = d, exog = exog), error = identity)
    expect_s3_class(err, "hetid_error_bad_argument")
    expect_identical(err$arg, "exog")
  }
})

test_that("shared regression validates selected types and complete rows only", {
  old <- options(warn = 2)
  on.exit(options(old))
  d <- residual_chain_fixture()$aligned
  y <- d[[HETID_CONSTANTS$CONSUMPTION_GROWTH_COL]]
  x <- as.matrix(d[, c("pc1", "pc2")])
  for (bad_y in list(as.character(y), factor(y))) {
    err <- tryCatch(run_pc_regression(bad_y, x, 1L), error = identity)
    expect_s3_class(err, "hetid_error_bad_argument")
    expect_identical(err$arg, "y")
  }
  mixed <- data.frame(pc1 = x[, 1L], unused = letters[seq_len(nrow(x)) %% 26L + 1L])
  expect_equal(
    run_pc_regression(y, mixed, 1L)$coefficients,
    run_pc_regression(y, x, 1L)$coefficients
  )
  err <- tryCatch(run_pc_regression(y, mixed, 2L), error = identity)
  expect_s3_class(err, "hetid_error_bad_argument")
  expect_identical(err$arg, "pcs")
  y[3L] <- NA_real_
  x[3L, 1L] <- Inf
  x[4L, 2L] <- Inf
  fit <- run_pc_regression(y, x, 1L)
  oracle <- residual_chain_ols(y, x[, 1L, drop = FALSE])
  expect_identical(fit$complete_idx, oracle$keep)
  expect_equal(unname(fit$residuals), unname(oracle$residuals))
})

test_that("W1 retains missingness, alignment, output shape and unused-row policies", {
  d <- residual_chain_fixture()$aligned
  outcome <- HETID_CONSTANTS$CONSUMPTION_GROWTH_COL
  d[[outcome]][1L] <- Inf
  d$pc1[nrow(d)] <- Inf
  d[[outcome]][4L] <- NaN
  d$pc1[3L] <- Inf
  fit <- withVisible(compute_w1_residuals(1L, d))
  expect_true(fit$visible)
  y <- d[[outcome]][-1L]
  x <- as.matrix(d[-nrow(d), "pc1", drop = FALSE])
  oracle <- residual_chain_ols(y, x)
  expect_identical(fit$value$kept_idx, oracle$keep)
  expect_identical(fit$value$dates, d$date[-1L][oracle$keep])
  expect_equal(unname(fit$value$residuals), unname(oracle$residuals))
  expect_identical(names(fit$value$coefficients), c("(Intercept)", "pc1"))
  frame <- compute_w1_residuals(1L, d, return_df = TRUE)
  expect_named(frame, c("date", "residuals", "fitted"))
  expect_identical(frame$date, fit$value$dates)
  expect_equal(frame$residuals, unname(fit$value$residuals))
  expect_error(compute_w1_residuals(1L, d[1:3, ]),
    class = "hetid_error_insufficient_data"
  )
  exog <- matrix(seq_len(nrow(d)), ncol = 1L)
  exog[nrow(d), 1L] <- Inf
  err <- tryCatch(compute_w1_residuals(data = d, exog = exog), error = identity)
  expect_s3_class(err, "hetid_error_bad_argument")
  expect_identical(err$arg, "exog")
})

test_that("unnamed finite exog keeps default labels and PC-count exclusivity", {
  d <- residual_chain_fixture()$aligned
  exog <- matrix(seq_len(nrow(d)), ncol = 1L)
  fit <- compute_w1_residuals(data = d, exog = exog)
  expect_identical(names(fit$coefficients), c("(Intercept)", "z1"))
  err <- tryCatch(compute_w1_residuals(1L, d, exog = exog), error = identity)
  expect_s3_class(err, "hetid_error_bad_argument")
  expect_identical(err$arg, "n_pcs")
})
