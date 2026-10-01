public_tau0_fixture <- function(impose_null = FALSE) {
  d <- simulate_tau0_dgp(t_obs = 150)
  list(data = d, fit = compute_tau0_system(d$y1, d$y2, d$x, d$z,
    impose_null = impose_null
  ))
}

test_that("tau-zero validator rejects empty or malformed nested moments", {
  fit <- public_tau0_fixture()$fit
  bad <- fit
  bad$moments <- structure(list(), class = "hetid_moments")
  expect_error(validate_hetid_tau0_fit(bad), class = "hetid_error_bad_argument")
  for (field in c("r_i_0", "p_i_0", "r_i_1", "s_i_2")) {
    bad <- fit
    if (is.list(bad$moments[[field]])) {
      storage.mode(bad$moments[[field]][[1]]) <- "character"
    } else {
      storage.mode(bad$moments[[field]]) <- "character"
    }
    err <- tryCatch(validate_hetid_tau0_fit(bad), error = identity)
    expect_s3_class(err, "hetid_error_bad_argument")
    expect_identical(err$arg, field)
  }
  bad <- fit
  bad$moments$s_i_1[[1]] <- matrix(bad$moments$s_i_1[[1]], 1)
  expect_error(validate_hetid_tau0_fit(bad), class = "hetid_error_dimension_mismatch")
})

test_that("nested component, instrument, and observation counts match the fit", {
  fit <- public_tau0_fixture()$fit
  bad <- fit
  extra_w2 <- cbind(fit$w2, extra = fit$w2[, 1] + 2 * fit$w2[, 2])
  bad$moments <- compute_identification_moments(fit$w1, extra_w2, fit$z)
  expect_identical(validate_hetid_moments(bad$moments), bad$moments)
  expect_error(validate_hetid_tau0_fit(bad), "n_components",
    class = "hetid_error_dimension_mismatch"
  )

  bad <- fit
  attr(bad$moments, "n_instruments") <- 2L
  expect_error(validate_hetid_tau0_fit(bad), "n_instruments",
    class = "hetid_error_dimension_mismatch"
  )

  bad <- fit
  bad$z <- cbind(fit$z, extra = 2 * fit$z[, 1])
  bad$gamma <- rbind(fit$gamma, extra = rep(1, ncol(fit$w2)))
  expect_error(validate_hetid_tau0_fit(bad), "n_instruments",
    class = "hetid_error_dimension_mismatch"
  )

  bad <- fit
  extra_z <- cbind(fit$z, extra = 2 * fit$z[, 1])
  bad$moments <- compute_identification_moments(fit$w1, fit$w2, extra_z)
  attr(bad$moments, "n_instruments") <- 1L
  expect_identical(validate_hetid_moments(bad$moments), bad$moments)
  expect_error(validate_hetid_tau0_fit(bad), "r_i_0",
    class = "hetid_error_dimension_mismatch"
  )

  bad <- fit
  attr(bad$moments, "n_obs") <- 151L
  expect_error(validate_hetid_tau0_fit(bad), "n_obs",
    class = "hetid_error_dimension_mismatch"
  )
  expect_identical(validate_hetid_tau0_fit(fit), fit)
})

test_that("residuals, instruments, and point theta must be finite", {
  fit <- public_tau0_fixture()$fit
  expect_false(is.null(fit$point))
  old <- options(warn = 2)
  on.exit(options(old))
  for (field in c("w1", "w2", "z", "point")) {
    for (value in c(NA_real_, NaN, Inf, -Inf)) {
      bad <- fit
      if (field == "point") {
        bad$point$theta[1] <- value
      } else {
        bad[[field]][1] <- value
      }
      err <- tryCatch(validate_hetid_tau0_fit(bad), error = identity)
      expect_s3_class(err, "hetid_error_bad_argument")
      expect_identical(err$arg, field)
    }
  }
})

test_that("structural coefficient labels match reduced-form labels exactly", {
  fit <- public_tau0_fixture()$fit
  bad <- fit
  names(bad$beta1) <- rev(names(bad$beta1))
  err <- tryCatch(validate_hetid_tau0_fit(bad), error = identity)
  expect_s3_class(err, "hetid_error_bad_argument")
  expect_identical(err$arg, "beta1")
  expect_identical(names(fit$beta1), names(fit$beta1r))
})

test_that("validation preserves valid fitted values, moments, and visibility", {
  fixture <- public_tau0_fixture()
  fit <- fixture$fit
  d <- fixture$data
  before <- fit
  checked <- withVisible(validate_hetid_tau0_fit(fit))
  expect_false(checked$visible)
  expect_identical(checked$value, fit)
  y1_fit <- stats::lm(d$y1 ~ d$x)
  expect_equal(unname(fit$beta1r), unname(stats::coef(y1_fit)))
  expect_equal(unname(fit$w1), unname(stats::residuals(y1_fit)))
  expect_equal(unname(d$y1 - fit$w1), unname(stats::fitted(y1_fit)))
  for (i in seq_len(ncol(d$y2))) {
    y2_fit <- stats::lm(d$y2[, i] ~ d$x)
    expect_equal(unname(fit$beta2r[i, ]), unname(stats::coef(y2_fit)))
    expect_equal(unname(fit$w2[, i]), unname(stats::residuals(y2_fit)))
    expect_equal(d$y2[, i] - fit$w2[, i], unname(stats::fitted(y2_fit)))
  }
  expect_identical(fit, before)
  expect_identical(fit$moments, before$moments)

  fit$moments <- compute_identification_moments(fit$w1, fit$w2, fit$z, c(2, 1))
  expect_identical(validate_hetid_tau0_fit(fit), fit)
  fit$moments <- compute_identification_moments(fit$w1, fit$w2, fit$z, 2)
  expect_identical(validate_hetid_tau0_fit(fit), fit)
  colnames(fit$w2) <- NULL
  colnames(fit$z) <- NULL
  expect_identical(validate_hetid_tau0_fit(fit), fit)
  fit[c("point", "beta1")] <- list(NULL, NULL)
  expect_null(fit[["point"]])
  expect_null(fit[["beta1"]])
  expect_identical(validate_hetid_tau0_fit(fit), fit)
})

test_that("impose_null retains its coefficients and residuals", {
  fixture <- public_tau0_fixture(impose_null = TRUE)
  fit <- fixture$fit
  expect_identical(attr(fit, "impose_null"), TRUE)
  expect_identical(fit$w2, fixture$data$y2)
  expect_true(all(fit$beta2r == 0))
  expect_identical(validate_hetid_tau0_fit(fit), fit)
  expect_invisible(validate_hetid_tau0_fit(fit))
})

test_that("gamma and coefficient value contracts remain unchanged", {
  fit <- public_tau0_fixture()$fit
  for (field in c("gamma", "beta1r", "beta2r", "beta1")) {
    supplied <- fit
    supplied[[field]][1] <- Inf
    expect_identical(validate_hetid_tau0_fit(supplied), supplied)
    expect_invisible(validate_hetid_tau0_fit(supplied))
  }
})
