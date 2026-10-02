# Independent OLS sandwich: explicit bread, residuals and Bartlett loop
lp_vcov_oracle <- function(prep, z, lags) {
  v <- lp_design(prep)
  bread <- solve(crossprod(v))
  r <- drop(z - v %*% (bread %*% crossprod(v, z)))
  u <- v * r
  meat <- crossprod(u)
  hac <- meat
  for (l in seq_len(min(lags, nrow(u) - 1L))) {
    g <- crossprod(
      u[(l + 1):nrow(u), , drop = FALSE], u[1:(nrow(u) - l), , drop = FALSE]
    )
    hac <- hac + (1 - l / (lags + 1)) * (g + t(g))
  }
  n <- nrow(v)
  p <- ncol(v)
  list(
    naive = sum(r^2) / (n - p) * bread, hc0 = bread %*% meat %*% bread,
    hc1 = n / (n - p) * bread %*% meat %*% bread, hac = bread %*% hac %*% bread
  )
}

lp_vcov_responses <- function(prep, fx) {
  e <- prep$w1 - drop(prep$w2 %*% fx$b)
  list(
    log = 2 * log(abs(e)),
    log_plus = log(e^2 + mean(fx$w1^2) / length(e)),
    log_fuller = lp_fuller_response(prep, fx$b, 1)
  )
}

test_that("log projection covariances match an independent OLS sandwich", {
  for (d_r in c(2L, 0L)) {
    fx <- lp_fixture(d_r = d_r)
    prep <- lp_prep(fx)
    responses <- lp_vcov_responses(prep, fx)
    for (method in names(responses)) {
      for (lags in c(3L, 500L)) {
        got <- compute_log_projection_vcov(prep, fx$b, method, hac_lags = lags)
        want <- lp_vcov_oracle(prep, responses[[method]], lags)
        expect_identical(names(got), LOG_VARIANCE_CONTROL$SE_TYPES)
        for (k in names(want)) {
          expect_equal(unname(got[[k]]), unname(want[[k]]), tolerance = 1e-9)
        }
        expect_identical(
          dimnames(got$hac), rep(list(rownames(prep$projection)), 2L)
        )
      }
    }
  }
})

test_that("zero lags reduce HAC to HC0; failures and overflow are all-NA", {
  fx <- lp_fixture()
  prep <- lp_prep(fx)
  v0 <- compute_log_projection_vcov(prep, fx$b, "log_plus", hac_lags = 0L)
  expect_identical(v0$hac, v0$hc0)
  all_na <- function(v) all(vapply(v, function(m) all(is.na(m)), logical(1)))
  zf <- lp_zero_fixture()
  zp <- lp_prep(zf)
  zero_fit <- compute_log_projection_vcov(zp, 0, "log")
  expect_true(all_na(zero_fit))
  expect_identical(names(zero_fit), LOG_VARIANCE_CONTROL$SE_TYPES)
  tiny <- prepare_log_projection(
    fx$w1, fx$w2, fx$x_var * 1e-200, fx$mean_ids, fx$vol_ids
  )
  vt <- compute_log_projection_vcov(tiny, fx$b, "log_plus")
  expect_true(all(vapply(vt, function(m) {
    all(is.finite(m)) || all(is.na(m))
  }, logical(1))))
})

test_that("covariance guards raise their documented conditions", {
  fx <- lp_fixture()
  prep <- lp_prep(fx)
  expect_error(
    compute_log_projection_vcov(prep, rbind(fx$b, fx$b), "log_plus"),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_log_projection_vcov(prep, fx$b, "log_plus", hac_lags = -1),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_log_projection_vcov(prep, fx$b, "ols"),
    class = "hetid_error_bad_argument"
  )
})

test_that("oracle responses reproduce the evaluator's regularized coefficients", {
  fx <- lp_fixture()
  prep <- lp_prep(fx)
  responses <- lp_vcov_responses(prep, fx)
  for (method in c("log_plus", "log_fuller")) {
    fit <- evaluate_log_projection(prep, fx$b, method, jacobian = FALSE)
    expect_equal(
      unname(fit$coef),
      unname(drop(prep$projection %*% responses[[method]])),
      tolerance = 1e-10
    )
  }
})
