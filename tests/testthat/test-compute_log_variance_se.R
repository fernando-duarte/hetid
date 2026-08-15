# Tests for compute_log_variance_se(): the term/coef/SE frame assembled over
# compute_log_variance_vcov(), plus the NA-not-zero diagonal rule on the
# internal frame-assembly helper.

test_that("the frame has the right shape and default row names", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x)
  se <- compute_log_variance_se(fit)
  labels <- attr(fit, "coef_labels")
  spec <- hetid:::log_variance_estimator(attr(fit, "estimator"))
  expect_identical(names(se), c("term", "coef", spec$se_types))
  expect_identical(row.names(se), as.character(seq_along(labels)))
  expect_identical(se$term, labels)
})

test_that("se matches sqrt(diag(vcov)) for every type on a clean fit", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x)
  se <- compute_log_variance_se(fit)
  vc <- compute_log_variance_vcov(fit)
  for (type in names(vc)) {
    expect_equal(se[[type]], unname(sqrt(diag(vc[[type]]))), tolerance = 1e-12)
  }
  expect_equal(se$coef, unname(fit$coef), tolerance = 1e-12)
})

test_that("hac_lags is threaded through to the same vcov call", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x)
  se <- compute_log_variance_se(fit, hac_lags = 0L)
  vc <- compute_log_variance_vcov(fit, hac_lags = 0L)
  expect_equal(se$hac, unname(sqrt(diag(vc$hac))), tolerance = 1e-12)
})

test_that("a failed fit renders the fully-NA frame with no warning", {
  d <- simulate_logvar_data()
  failed <- fit_log_variance(rep(0, nrow(d$x)), d$x)
  labels <- attr(failed, "coef_labels")
  se <- expect_no_warning(compute_log_variance_se(failed))
  expect_identical(se$term, labels)
  expect_true(all(is.na(se$coef)))
  spec <- hetid:::log_variance_estimator(attr(failed, "estimator"))
  for (type in spec$se_types) {
    expect_true(all(is.na(se[[type]])))
  }
  expect_identical(nrow(se), length(labels))
})

test_that("the frame-assembly helper renders NA, not 0, for a negative diagonal", {
  vcov_list <- list(
    naive = matrix(c(-1e-10, 0, 0, 4), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  )
  se <- expect_no_warning(
    hetid:::log_variance_se_frame(c(1, 2), c("a", "b"), vcov_list)
  )
  expect_identical(se$naive[1], NA_real_)
  expect_identical(se$naive[2], 2)
})

test_that("malformed arguments error loudly", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x)
  expect_error(compute_log_variance_se(fit$coef), class = "hetid_error_bad_argument")
  expect_error(compute_log_variance_se(fit, hac_lags = -1L),
    class = "hetid_error_bad_argument"
  )
})
