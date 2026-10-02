test_that("log coefficients match an independent least-squares fit", {
  fx <- lp_fixture()
  prep <- lp_prep(fx)
  fit <- evaluate_log_projection(prep, fx$b, "log")
  e <- prep$w1 - drop(prep$w2 %*% fx$b)
  g <- 2 * log(abs(e))
  v <- lp_design(prep)
  expect_equal(unname(fit$coef), unname(stats::lm.fit(v, g)$coefficients),
    tolerance = 1e-10
  )
  expect_identical(names(fit$coef), c("(Intercept)", "pc1", "pc2"))
  expect_lt(max(abs(crossprod(v, g - v %*% fit$coef))), 1e-10 * sum(abs(g)))
  expect_identical(fit$status, "ok")
})

test_that("batch evaluation equals single evaluations", {
  fx <- lp_fixture()
  prep <- lp_prep(fx)
  cand <- rbind(fx$b, fx$b / 2, -fx$b, c(0.1, -0.3), c(1, 1))
  batch <- evaluate_log_projection(prep, cand, "log", jacobian = FALSE)
  for (i in seq_len(nrow(cand))) {
    single <- evaluate_log_projection(prep, cand[i, ], "log", jacobian = FALSE)
    expect_equal(batch$coef[, i], single$coef, tolerance = 1e-14)
    expect_identical(batch$status[i], single$status)
  }
  expect_error(
    evaluate_log_projection(prep, cand, "log", jacobian = TRUE),
    class = "hetid_error_bad_argument"
  )
})

test_that("an exact zero residual is a log domain failure", {
  zf <- lp_zero_fixture()
  prep <- prepare_log_projection(zf$w1, zf$w2, zf$x_var, zf$mean_ids, zf$vol_ids)
  fit <- evaluate_log_projection(prep, 0, "log")
  expect_identical(fit$status, "domain_failure")
  expect_false(all(is.finite(fit$coef)))
  expect_null(fit$jacobian)
})

test_that("failures in one batch column never touch another", {
  zf <- lp_zero_fixture()
  prep <- prepare_log_projection(
    zf$w1, zf$w2 * 2, zf$x_var, zf$mean_ids, zf$vol_ids
  )
  cand <- matrix(c(0.3, 0, .Machine$double.xmax), ncol = 1L)
  batch <- evaluate_log_projection(prep, cand, "log", jacobian = FALSE)
  expect_identical(batch$status, c("ok", "domain_failure", "numerical_failure"))
  single <- evaluate_log_projection(prep, 0.3, "log", jacobian = FALSE)
  expect_equal(batch$coef[, 1], single$coef, tolerance = 1e-14)
  expect_true(all(is.na(batch$coef[, 3])))
})

test_that("outcome units shift the intercept and scale the Jacobian", {
  fx <- lp_fixture()
  base <- evaluate_log_projection(lp_prep(fx), fx$b, "log")
  for (a in c(3, -0.5, 1e-150, 1e150)) {
    scaled <- fx
    scaled$w1 <- fx$w1 * a
    fit <- evaluate_log_projection(lp_prep(scaled), a * fx$b, "log")
    expect_equal(fit$coef[1], base$coef[1] + 2 * log(abs(a)), tolerance = 1e-9)
    expect_equal(fit$coef[-1], base$coef[-1], tolerance = 1e-9)
    expect_equal(a * fit$jacobian, base$jacobian, tolerance = 1e-9)
  }
})

test_that("a Jacobian overflow clears coefficients and status together", {
  fx <- lp_fixture()
  tiny <- fx
  tiny$w1 <- fx$w1 * 1e-310
  fit <- evaluate_log_projection(lp_prep(tiny), fx$b * 1e-310, "log")
  expect_identical(fit$status, "numerical_failure")
  expect_true(all(is.na(fit$coef)))
  expect_null(fit$jacobian)
})

test_that("evaluation guards raise their documented conditions", {
  fx <- lp_fixture()
  prep <- lp_prep(fx)
  expect_error(evaluate_log_projection(prep, fx$b, "log_plus"),
    class = "hetid_error_bad_argument"
  )
  for (m in c(0, -1, NA, Inf)) {
    expect_error(evaluate_log_projection(prep, fx$b, "log", multiplier = m),
      class = "hetid_error_bad_argument"
    )
  }
  expect_error(evaluate_log_projection(list(), fx$b, "log"),
    class = "hetid_error_bad_argument"
  )
  expect_error(evaluate_log_projection(prep, c(Inf, 1), "log"),
    class = "hetid_error_bad_argument"
  )
  expect_error(evaluate_log_projection(prep, numeric(3), "log"),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("named candidates must follow the news column order", {
  fx <- lp_fixture()
  prep <- lp_prep(fx)
  expect_error(evaluate_log_projection(prep, c(n2 = 0.4, n1 = 0.4), "log"),
    class = "hetid_error_bad_argument"
  )
  cand <- rbind(fx$b, fx$b)
  colnames(cand) <- c("n2", "n1")
  expect_error(evaluate_log_projection(prep, cand, "log", jacobian = FALSE),
    class = "hetid_error_bad_argument"
  )
  ok <- evaluate_log_projection(prep, c(n1 = 0.4, n2 = 0.4), "log")
  expect_identical(ok$status, "ok")
  expect_identical(evaluate_log_projection(prep, unname(fx$b), "log")$status, "ok")
})
