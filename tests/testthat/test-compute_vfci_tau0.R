test_that("dated VFCI composes the unchanged mean and PPML estimators", {
  observations <- vfci_tau0_frame()
  original <- observations
  out <- vfci_tau0_test_fit(observations)
  direct <- compute_tau0_system(
    observations$y, as.matrix(observations[c("news1", "news2")]),
    as.matrix(observations[c("x1", "x2")]), observations$z
  )
  rows <- complete.cases(observations[c("h1", "h2")])
  centered <- scale(as.matrix(observations[c("h1", "h2")])[rows, ], center = TRUE, scale = FALSE)
  theta <- setNames(direct$point$theta, colnames(direct$w2))
  logvar <- fit_log_variance_at_b(theta, direct$w1[rows], direct$w2[rows, ], centered)

  expect_identical(names(out), c("ts", "tau0", "logvar", "masks", "call"))
  expect_identical(class(out$ts), "data.frame")
  expect_identical(names(out$ts), c("date", "vfci", "mu", "epsilon"))
  expect_identical(out$ts$date, observations$date)
  expect_identical(out$tau0, direct)
  expect_identical(out$logvar, logvar)
  expect_equal(out$ts$epsilon, drop(direct$w1 - direct$w2 %*% theta), tolerance = 1e-10)
  expect_equal(out$ts$mu + out$ts$epsilon, observations$y, tolerance = 1e-10)
  expect_equal(out$logvar$y, out$ts$epsilon[rows]^2, tolerance = 1e-10)
  expect_equal(out$ts$vfci[rows], drop(centered %*% logvar$coef[-1]) / 2,
    tolerance = 1e-10
  )
  expect_lt(abs(mean(out$ts$vfci, na.rm = TRUE)), 1e-10)
  expect_true(all(abs(out$tau0$point$theta - c(0.8, -0.5)) <= 0.15))
  expect_identical(out$masks, list(
    window = rep(TRUE, 300), mean = rep(TRUE, 300),
    variance = rows
  ))
  expect_true(is.call(out$call))
  expect_identical(observations, original)
})

test_that("equation masks keep original positions and mean-only residuals", {
  observations <- vfci_tau0_frame()
  observations$y[101] <- NA_real_
  observations$h2[120] <- NaN
  out <- vfci_tau0_test_fit(observations)
  mean_mask <- seq_len(nrow(observations)) != 101L
  variance_mask <- mean_mask & !seq_len(nrow(observations)) %in% c(1L, 120L)
  expect_identical(out$masks$mean, mean_mask)
  expect_identical(out$masks$variance, variance_mask)
  expect_identical(out$ts$date, observations$date[mean_mask])
  expect_identical(is.na(out$ts$vfci), !variance_mask[mean_mask])
  expect_true(all(is.finite(out$ts$epsilon)))
  expect_true(all(is.finite(out$ts$mu)))
  expect_length(out$logvar$y, sum(variance_mask))
  expect_equal(out$ts$mu + out$ts$epsilon, observations$y[mean_mask], tolerance = 1e-10)
  centered <- scale(as.matrix(observations[variance_mask, c("h1", "h2")]),
    center = TRUE, scale = FALSE
  )
  expect_equal(out$logvar$x_design[, -1], centered, ignore_attr = TRUE, tolerance = 1e-10)
})

test_that("quarter windowing and date relabeling preserve forecast origins", {
  observations <- vfci_tau0_frame()
  out <- vfci_tau0_test_fit(observations,
    date_begin = as.Date("1960-01-01"),
    date_end = as.Date("1969-12-31")
  )
  rows <- observations$date >= as.Date("1960-03-31") & observations$date <= as.Date("1969-12-31")
  expect_identical(out$masks$window, rows)
  expect_identical(out$masks$mean, rows)
  expect_identical(out$masks$variance, rows)
  expect_identical(out$ts$date, observations$date[rows])
  expect_equal(nrow(out$ts), 40)
  starts <- observations
  starts$date <- seq(as.Date("1950-01-01"), by = "3 months", length.out = nrow(observations))
  relabeled <- vfci_tau0_test_fit(starts,
    date_begin = as.Date("1960-02-10"),
    date_end = as.Date("1969-11-10")
  )
  expect_identical(relabeled$ts, out$ts)
  expect_identical(relabeled$masks, out$masks)
  gapped <- observations[-100, ]
  expect_identical(vfci_tau0_test_fit(gapped)$ts$date, gapped$date)
})

test_that("centering uses only volatility rows and excludes the intercept", {
  observations <- vfci_tau0_frame()
  observations$h2[120] <- NA_real_
  out <- vfci_tau0_test_fit(observations)
  shifted <- observations
  shifted$h1 <- shifted$h1 + 100
  shifted$h2 <- shifted$h2 - 30
  expect_equal(vfci_tau0_test_fit(shifted)$ts$vfci, out$ts$vfci, tolerance = 1e-10)
  rows <- out$masks$variance
  eta <- drop(out$logvar$x_design %*% out$logvar$coef)
  expect_equal(out$ts$vfci[rows], (eta - out$logvar$coef[1]) / 2,
    tolerance = 1e-10, ignore_attr = TRUE
  )
  expect_gt(abs(mean(eta / 2) - mean(out$ts$vfci[rows])), 1e-4)
  changed <- observations
  changed$h2[1] <- 1e100
  expect_identical(vfci_tau0_test_fit(changed)$ts, out$ts)
})

test_that("news order and single-column designs preserve axis identity", {
  observations <- vfci_tau0_frame()
  out <- vfci_tau0_test_fit(observations)
  reordered <- vfci_tau0_test_fit(observations, y2 = c("news2", "news1"))
  expect_identical(colnames(reordered$tau0$w2), c("news2", "news1"))
  expect_equal(reordered$tau0$point$theta, rev(out$tau0$point$theta), tolerance = 1e-10)
  expect_equal(reordered$ts$epsilon, out$ts$epsilon, tolerance = 1e-10)
  single_fit <- vfci_tau0_test_fit(observations, x = "x1", y2 = "news1", het = "h1")
  expect_identical(colnames(single_fit$tau0$w2), "news1")
  expect_identical(names(single_fit$logvar$coef), c("(Intercept)", "h1"))
  expect_lt(abs(mean(single_fit$ts$vfci, na.rm = TRUE)), 1e-10)
})

test_that("VFCI fits preserve present and absent caller RNG state", {
  observations <- vfci_tau0_frame()
  withr::local_preserve_seed()
  set.seed(81)
  kind <- RNGkind()
  seed <- .Random.seed
  vfci_tau0_test_fit(observations)
  expect_identical(RNGkind(), kind)
  expect_identical(.Random.seed, seed)
  rm(".Random.seed", envir = .GlobalEnv)
  vfci_tau0_test_fit(observations)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_identical(RNGkind(), kind)
})
