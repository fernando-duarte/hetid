test_that("reserved W1 labels preserve OLS results and coherent lm methods", {
  d <- residual_chain_fixture()$aligned
  d$pc1[is.na(d$pc1)] <- 0
  outcome <- HETID_CONSTANTS$CONSUMPTION_GROWTH_COL
  exog <- as.matrix(d[, c("pc1", "pc2")])
  control <- compute_w1_residuals(data = d, exog = exog)
  manual <- residual_chain_ols(d[[outcome]][-1L], exog[-nrow(d), , drop = FALSE])
  for (labels in list(
    c("...", "..1"), c("...", ".hetid_x1"),
    c(".hetid_x2", "..1"), c("..2", "..3")
  )) {
    colnames(exog) <- labels
    fit <- compute_w1_residuals(data = d, exog = exog)
    expect_identical(names(fit$coefficients), c("(Intercept)", labels))
    expect_equal(unname(fit$coefficients), unname(manual$coefficients))
    expect_equal(fit$residuals, control$residuals)
    expect_equal(fit$fitted, control$fitted)
    expect_equal(fit$r_squared, control$r_squared)
    expect_identical(fit$dates, control$dates)
    expect_identical(fit$kept_idx, control$kept_idx)
    model <- fit$model
    expect_identical(class(model), "lm")
    mapping <- attr(model, "hetid_regressor_names")
    expect_identical(unname(mapping), labels)
    expect_false(anyDuplicated(names(mapping)) > 0L)
    expect_identical(names(coef(model)), c("(Intercept)", names(mapping)))
    expect_identical(colnames(model.matrix(model)), names(coef(model)))
    expect_identical(colnames(model$qr$qr), names(coef(model)))
    expect_identical(attr(terms(model), "term.labels"), names(mapping))
    expect_identical(names(model.frame(model)), c("y", names(mapping)))
    expect_equal(unname(coef(model)), unname(fit$coefficients))
    expect_equal(fitted(model), fit$fitted)
    expect_equal(residuals(model), fit$residuals)
    expect_equal(summary(model)$r.squared, fit$r_squared)
    newdata <- as.data.frame(exog[-nrow(d), , drop = FALSE])
    names(newdata) <- names(mapping)
    expect_equal(
      unname(predict(model, newdata = newdata)[fit$kept_idx]),
      unname(fit$fitted)
    )
  }
})

test_that("ordinary labels, sanitization, fallback and design errors are preserved", {
  d <- residual_chain_fixture()$aligned
  d$pc1[is.na(d$pc1)] <- 0
  y <- d[[HETID_CONSTANTS$CONSUMPTION_GROWTH_COL]]
  x <- as.matrix(d[, c("pc1", "pc2")])
  fit <- run_pc_regression(y, x, 2L)
  keep <- complete.cases(y, x)
  ordinary <- lm(y ~ pc1 + pc2, data = data.frame(y = y[keep], x[keep, ]))
  expect_null(attr(fit$model, "hetid_regressor_names"))
  expect_identical(fit$coefficients, coef(ordinary))
  expect_equal(lapply(fit$model$model, identity), lapply(model.frame(ordinary), identity))
  expect_equal(model.matrix(fit$model), model.matrix(ordinary))
  expect_identical(class(fit$model), "lm")
  for (labels in list(c("a b", "a-b"), c("", "pc2"), c(NA_character_, "pc2"))) {
    colnames(x) <- labels
    fit <- run_pc_regression(y, x, 2L)
    expected <- if (anyNA(labels) || any(!nzchar(labels))) {
      get_pc_column_names(2L)
    } else {
      make.names(labels, unique = TRUE)
    }
    expect_identical(names(fit$coefficients), c("(Intercept)", expected))
    expect_equal(unname(fit$coefficients), unname(coef(ordinary)))
  }
  colnames(x) <- c("y", "pc2")
  err <- tryCatch(compute_w1_residuals(data = d, exog = x), error = identity)
  expect_s3_class(err, "hetid_error_bad_argument")
  expect_identical(err$arg, "pcs")
  x[, 2L] <- x[, 1L]
  colnames(x) <- c("...", "..1")
  expect_error(compute_w1_residuals(data = d, exog = x),
    "Rank-deficient",
    class = "hetid_error"
  )
})

test_that("tau-zero coefficient axes remain aligned for reserved conditioning names", {
  d <- simulate_tau0_dgp(t_obs = 150L)
  control <- compute_tau0_system(d$y1, d$y2, d$x, d$z)
  colnames(d$x) <- c("...", "..1")
  for (impose_null in c(FALSE, TRUE)) {
    ordinary <- if (impose_null) {
      compute_tau0_system(d$y1, d$y2, setNames(as.data.frame(d$x), c("x1", "x2")),
        d$z,
        impose_null = TRUE
      )
    } else {
      control
    }
    fit <- compute_tau0_system(d$y1, d$y2, d$x, d$z, impose_null = impose_null)
    expect_identical(names(fit$beta1r), c("(Intercept)", "...", "..1"))
    expect_identical(colnames(fit$beta2r), names(fit$beta1r))
    expect_equal(unname(fit$beta1r), unname(ordinary$beta1r))
    expect_equal(unname(fit$beta2r), unname(ordinary$beta2r))
    expect_equal(fit$w1, ordinary$w1)
    expect_equal(fit$w2, ordinary$w2)
    expect_equal(fit$moments, ordinary$moments)
    expect_equal(fit$point, ordinary$point)
  }
})
