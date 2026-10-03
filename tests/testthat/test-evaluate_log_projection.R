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
  eval(parse("bodies/evaluate_log_projection-contracts.R", encoding = "UTF-8")[[1]], environment())
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
  f1 <- lp_fuller_response(prep, b, m)
  unname(stats::lm.fit(lp_design(prep), f1)$coefficients)
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
  eval(parse("bodies/evaluate_log_projection-contracts.R", encoding = "UTF-8")[[2]], environment())
})
