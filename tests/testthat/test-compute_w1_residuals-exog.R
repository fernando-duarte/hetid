test_that("exog regressors replace the bundled PCs in the W1 regression", {
  set.seed(5)
  n <- 40
  df <- data.frame(
    date = seq(as.Date("1990-03-31"), by = "quarter", length.out = n),
    gr1.pcecc96 = rnorm(n),
    junk = rnorm(n)
  )
  exog <- matrix(rnorm(n * 2), nrow = n, dimnames = list(NULL, c("f1", "f2")))

  res <- compute_w1_residuals(data = df, exog = exog)

  manual <- lm(
    df$gr1.pcecc96[2:n] ~ exog[1:(n - 1), "f1"] + exog[1:(n - 1), "f2"]
  )
  expect_equal(unname(res$residuals), unname(residuals(manual)))
  expect_identical(names(res$coefficients), c("(Intercept)", "f1", "f2"))
})

test_that("non-syntactic exog names are sanitized in coefficient labels", {
  set.seed(6)
  df <- data.frame(
    date = seq(as.Date("1990-03-31"), by = "quarter", length.out = 25),
    gr1.pcecc96 = rnorm(25)
  )
  exog <- matrix(
    rnorm(50),
    nrow = 25, dimnames = list(NULL, c("10y rate", "f2"))
  )
  res <- compute_w1_residuals(data = df, exog = exog)
  expect_identical(
    names(res$coefficients),
    c("(Intercept)", "X10y.rate", "f2")
  )
})

test_that("exog combined with an explicit n_pcs is rejected", {
  df <- data.frame(gr1.pcecc96 = rnorm(20))
  exog <- matrix(rnorm(40), nrow = 20)
  expect_error(
    compute_w1_residuals(n_pcs = 3, data = df, exog = exog),
    class = "hetid_error_bad_argument"
  )
})

test_that("exog with mismatched rows is rejected", {
  df <- data.frame(
    date = seq(as.Date("1990-03-31"), by = "quarter", length.out = 20),
    gr1.pcecc96 = rnorm(20)
  )
  exog <- matrix(rnorm(30), nrow = 15)
  expect_error(
    compute_w1_residuals(data = df, exog = exog),
    class = "hetid_error_dimension_mismatch"
  )
})
