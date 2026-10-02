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
  expect_error(evaluate_log_projection(prep, fx$b, "log_minus"),
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

# Direct two-pass Fuller computation with plain log/exp, as an oracle
lp_fuller_oracle <- function(prep, b, m) {
  e <- prep$w1 - drop(prep$w2 %*% b)
  e_mean <- prep$w1_mean - drop(prep$w2_mean %*% b)
  n_vol <- length(e)
  c_t <- m^2 / n_vol
  delta0 <- c_t * mean(e_mean^2)
  f0 <- log(e^2 + delta0) - delta0 / (e^2 + delta0)
  v <- lp_design(prep)
  theta0 <- stats::lm.fit(v, f0)$coefficients
  eta <- drop(prep$x_centered %*% theta0[-1])
  delta <- delta0 * exp(eta) / mean(exp(eta))
  f1 <- log(e^2 + delta) - delta / (e^2 + delta)
  unname(stats::lm.fit(v, f1)$coefficients)
}

test_that("regularized methods match independent oracles", {
  fx <- lp_fixture()
  prep <- lp_prep(fx)
  e <- prep$w1 - drop(prep$w2 %*% fx$b)
  h2 <- mean(fx$w1^2) / length(e)
  plus <- evaluate_log_projection(prep, fx$b, "log_plus")
  expect_equal(unname(plus$coef),
    unname(stats::lm.fit(lp_design(prep), log(e^2 + h2))$coefficients),
    tolerance = 1e-10
  )
  expect_equal(plus$diagnostics$share_small, mean(abs(e) < sqrt(h2)))
  fuller <- evaluate_log_projection(prep, fx$b, "log_fuller", multiplier = 1.5)
  expect_equal(unname(fuller$coef), lp_fuller_oracle(prep, fx$b, 1.5),
    tolerance = 1e-10
  )
})

test_that("individual exact zeros are valid for the regularized methods", {
  zf <- lp_zero_fixture()
  prep <- prepare_log_projection(zf$w1, zf$w2, zf$x_var, zf$mean_ids, zf$vol_ids)
  for (method in c("log_plus", "log_fuller")) {
    fit <- evaluate_log_projection(prep, 0, method)
    expect_identical(fit$status, "ok")
    expect_true(all(is.finite(fit$coef)) && all(is.finite(fit$jacobian)))
  }
})

test_that("the Fuller transform satisfies its identities", {
  tr <- hetid:::log_projection_fuller_transform
  expect_equal(tr(-Inf, log(0.3))$value, log(0.3) - 1)
  x <- 0.7
  d <- 0.2
  for (a in c(3, -0.5)) {
    expect_equal(
      tr(log(a^2 * x), log(a^2 * d))$value,
      tr(log(x), log(d))$value + 2 * log(abs(a))
    )
  }
})

test_that("zero scales are domain failures with NA coefficients", {
  fx <- lp_fixture()
  zero <- fx
  zero$w1 <- 0 * fx$w1
  fit <- evaluate_log_projection(lp_prep(zero), fx$b, "log_plus")
  expect_identical(fit$status, "domain_failure")
  expect_true(all(is.na(fit$coef)))
  exact <- fx
  exact$w1 <- 2 * fx$w2[, 1]
  fit <- evaluate_log_projection(lp_prep(exact), c(2, 0), "log_fuller")
  expect_identical(fit$status, "domain_failure")
  expect_true(all(is.na(fit$coef)))
})

test_that("a mixed Fuller batch isolates its failures", {
  fx <- lp_fixture()
  doubled <- fx
  doubled$w2 <- 2 * fx$w2
  doubled$w1 <- 2 * doubled$w2[, 1]
  prep <- lp_prep(doubled)
  cand <- rbind(c(0.3, 0), c(2, 0), rep(.Machine$double.xmax, 2))
  batch <- evaluate_log_projection(prep, cand, "log_fuller", jacobian = FALSE)
  expect_identical(batch$status, c("ok", "domain_failure", "numerical_failure"))
  single <- evaluate_log_projection(prep, c(0.3, 0), "log_fuller", jacobian = FALSE)
  expect_equal(batch$coef[, 1], single$coef, tolerance = 1e-14)
})

test_that("regularized methods are equivariant to outcome units", {
  fx <- lp_fixture()
  for (method in c("log_plus", "log_fuller")) {
    base <- evaluate_log_projection(lp_prep(fx), fx$b, method)
    for (a in c(3, -0.5, 1e-150, 1e150, 1e300)) {
      scaled <- fx
      scaled$w1 <- fx$w1 * a
      fit <- evaluate_log_projection(lp_prep(scaled), a * fx$b, method)
      expect_equal(fit$coef[1], base$coef[1] + 2 * log(abs(a)), tolerance = 1e-9)
      expect_equal(fit$coef[-1], base$coef[-1], tolerance = 1e-9)
      expect_equal(a * fit$jacobian, base$jacobian, tolerance = 1e-9)
    }
  }
})

test_that("small multipliers approach the ordinary log projection", {
  fx <- lp_fixture()
  prep <- lp_prep(fx)
  ref <- evaluate_log_projection(prep, fx$b, "log")$coef
  for (method in c("log_plus", "log_fuller")) {
    fit <- evaluate_log_projection(prep, fx$b, method, multiplier = 1e-6)
    expect_equal(fit$coef, ref, tolerance = 1e-6)
  }
})

test_that("the Fuller profile averages to the candidate scale", {
  fx <- lp_fixture()
  prep <- lp_prep(fx)
  b_mat <- matrix(fx$b, nrow = 1L)
  e <- prep$w1 - prep$w2 %*% t(b_mat)
  e_mean <- prep$w1_mean - prep$w2_mean %*% t(b_mat)
  pass <- hetid:::log_projection_fuller(prep, e_mean, e, 2 * log(abs(e)), 1)
  expect_equal(mean(exp(pass$work$log_ratio[, 1])), 1, tolerance = 1e-12)
  set.seed(9)
  conc <- fx
  pc1 <- fx$x_var[, 1] - mean(fx$x_var[, 1])
  tail_rows <- utils::tail(seq_along(fx$w1), length(pc1))
  conc$w1[tail_rows] <- fx$w1[tail_rows] * exp(3 * pc1)
  conc$w1 <- conc$w1 - mean(conc$w1)
  fit <- evaluate_log_projection(lp_prep(conc), c(0, 0), "log_fuller")
  expect_identical(fit$status, "ok")
  base <- evaluate_log_projection(lp_prep(fx), c(0, 0), "log_fuller")
  expect_gt(
    fit$diagnostics$profile_log_ratio_max,
    base$diagnostics$profile_log_ratio_max + 0.5
  )
  expect_lte(fit$diagnostics$profile_log_ratio_max, log(length(pc1)))
})
