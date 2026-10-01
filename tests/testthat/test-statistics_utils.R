# Tests for the variance positivity diagnostic

make_diagnostic_inputs <- function(n = 200, seed = 123) {
  set.seed(seed)
  list(
    w1 = rnorm(n),
    w2 = matrix(rnorm(n * 2), ncol = 2),
    pcs = matrix(rnorm(n * 2), ncol = 2)
  )
}

test_that("centered_var equals the diagonal of centered_cov", {
  x <- rnorm(50)
  expect_identical(centered_var(x), centered_cov(x, x)[1, 1])
})

test_that("centered_cov preserves vector spread at large offsets", {
  x <- 2^40 + c(-1, 0, 1)
  y <- -2^40 + c(1, 0, -1)

  expect_equal(centered_cov(x, x), matrix(2 / 3), tolerance = 1e-15)
  expect_equal(centered_cov(x, y), matrix(-2 / 3), tolerance = 1e-15)
})

test_that("centered_cov preserves unequal column covariances at large offsets", {
  a <- cbind(
    2^40 + c(-1, 0, 1),
    -2^41 + c(1, -2, 1)
  )
  b <- cbind(
    2^42 + c(-2, 0, 2),
    -2^43 + c(3, 0, -3),
    2^44 + c(1, -2, 1)
  )
  expected <- matrix(c(4 / 3, -2, 0, 0, 0, 2), nrow = 2, byrow = TRUE)

  expect_equal(centered_cov(a, b), expected, tolerance = 1e-15)
  expect_equal(centered_cov(b, a), t(expected), tolerance = 1e-15)
})

test_that("centered_var preserves divisor-T variance at large offsets", {
  x <- 2^40 + c(-1, 0, 1)

  expect_equal(centered_var(x), 2 / 3, tolerance = 1e-15)
  expect_equal(centered_var(rep(2^40, 3)), 0)
})

test_that("well-conditioned residuals produce no warning", {
  inputs <- make_diagnostic_inputs()

  expect_no_warning(
    compute_identification_moments(inputs$w1, inputs$w2, inputs$pcs)
  )
})

test_that("two-point omega2 residual triggers the var(omega2^2) diagnostic", {
  inputs <- make_diagnostic_inputs()
  n <- length(inputs$w1)
  inputs$w2[, 1] <- sample(c(-1, 1), n, replace = TRUE)

  expect_warning(
    compute_identification_moments(inputs$w1, inputs$w2, inputs$pcs),
    "var\\(omega2\\^2\\) is numerically degenerate for maturity 1"
  )
})

test_that("exact outcome equation triggers the product-variance diagnostic", {
  inputs <- make_diagnostic_inputs(seed = 7)
  # w1 * w2_1 = 2 * w2_1^2 exactly, so the residual variance of the
  # product on w2^2 is zero and the second condition fails
  inputs$w1 <- 2 * inputs$w2[, 1]

  expect_warning(
    compute_identification_moments(inputs$w1, inputs$w2, inputs$pcs),
    "var\\(omega1\\*omega2 - gamma\\*omega2\\^2\\) is numerically degenerate for maturity 1"
  )
})

test_that("diagnostic only checks the requested maturities", {
  inputs <- make_diagnostic_inputs()
  n <- length(inputs$w1)
  inputs$w2[, 1] <- sample(c(-1, 1), n, replace = TRUE)

  expect_no_warning(
    compute_identification_moments(
      inputs$w1, inputs$w2, inputs$pcs,
      maturities = 2
    )
  )
})

test_that("degeneracy warning carries the hetid warning classes", {
  inputs <- make_diagnostic_inputs()
  n <- length(inputs$w1)
  inputs$w2[, 1] <- sample(c(-1, 1), n, replace = TRUE)

  expect_warning(
    compute_identification_moments(inputs$w1, inputs$w2, inputs$pcs),
    class = "hetid_warning_degenerate_variance"
  )

  caught <- NULL
  withCallingHandlers(
    compute_identification_moments(inputs$w1, inputs$w2, inputs$pcs),
    hetid_warning_degenerate_variance = function(w) {
      caught <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(caught, "hetid_warning_degenerate_variance")
  expect_s3_class(caught, "hetid_warning")
  expect_s3_class(caught, "warning")
  expect_match(
    conditionMessage(caught),
    "Variance positivity diagnostic"
  )
})
