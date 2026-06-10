# Tests for the variance positivity diagnostic

make_diagnostic_inputs <- function(n = 200, seed = 123) {
  set.seed(seed)
  list(
    w1 = rnorm(n),
    w2 = matrix(rnorm(n * 2), ncol = 2),
    pcs = matrix(rnorm(n * 2), ncol = 2)
  )
}

test_that("well-conditioned residuals produce no warning", {
  inputs <- make_diagnostic_inputs()

  expect_no_warning(
    compute_identification_moments(inputs$w1, inputs$w2, inputs$pcs)
  )
})

test_that("two-point W2 residual triggers the var(W2^2) diagnostic", {
  inputs <- make_diagnostic_inputs()
  n <- length(inputs$w1)
  inputs$w2[, 1] <- sample(c(-1, 1), n, replace = TRUE)

  expect_warning(
    compute_identification_moments(inputs$w1, inputs$w2, inputs$pcs),
    "var\\(W2\\^2\\) is numerically degenerate for maturity 1"
  )
})

test_that("exact outcome equation triggers the product-variance diagnostic", {
  inputs <- make_diagnostic_inputs(seed = 7)
  # w1 * w2_1 = 2 * w2_1^2 exactly, so the residual variance of the
  # product on w2^2 is zero and the second condition fails
  inputs$w1 <- 2 * inputs$w2[, 1]

  expect_warning(
    compute_identification_moments(inputs$w1, inputs$w2, inputs$pcs),
    "var\\(W1\\*W2 - gamma\\*W2\\^2\\) is numerically degenerate for maturity 1"
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
