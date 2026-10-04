test_that("the lean PPML solve reproduces an uneventful glm.fit solve bit for bit", {
  control <- log_variance_fit_control("ppml")
  set.seed(11)
  x <- cbind("(Intercept)" = 1, a = rnorm(80), b = rnorm(80))
  for (k in 1:25) {
    y <- rexp(80) * exp(drop(x %*% c(0.2, rnorm(2, sd = 0.3))))
    if (k %% 5 == 0) y[1:6] <- 0
    start <- stats::setNames(rnorm(3, sd = 0.5), colnames(x))
    lean <- ppml_irls(x, y, start, control$GLM_EPSILON, control$GLM_MAXIT)
    ref <- stats::glm.fit(x, y,
      family = stats::quasipoisson(link = "log"), start = start,
      control = stats::glm.control(control$GLM_EPSILON, control$GLM_MAXIT)
    )
    expect_identical(lean$coefficients, ref$coefficients)
    expect_identical(lean$iter, ref$iter)
    expect_true(ref$converged && !ref$boundary)
  }
})

test_that("an overflowing working weight leaves the rung to glm.fit", {
  control <- log_variance_fit_control("ppml")
  x <- cbind("(Intercept)" = 1, a = seq(-1, 1, length.out = 20))
  start <- c("(Intercept)" = 360, a = 0)
  expect_null(ppml_irls(x, rep(1, 20), start, control$GLM_EPSILON, control$GLM_MAXIT))
})
