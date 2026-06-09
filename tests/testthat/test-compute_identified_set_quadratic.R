test_that("compute_identified_set_quadratic validates inputs correctly", {
  inputs <- setup_quadratic_test_inputs()

  expect_error(
    compute_identified_set_quadratic(
      "not numeric", inputs$components, inputs$moments
    ),
    "tau must be a numeric vector"
  )

  expect_error(
    compute_identified_set_quadratic(
      c(1, -1, 1, 1), inputs$components, inputs$moments
    ),
    "All elements of tau must be nonnegative"
  )

  expect_error(
    compute_identified_set_quadratic(
      c(1, 1, 1), inputs$components, inputs$moments
    ),
    "tau must have length I"
  )

  expect_error(
    compute_identified_set_quadratic(
      inputs$tau, unclass(inputs$components), inputs$moments
    ),
    "hetid_components object",
    class = "hetid_error_bad_argument"
  )

  expect_error(
    compute_identified_set_quadratic(
      inputs$tau, inputs$components, unclass(inputs$moments)
    ),
    "hetid_moments object",
    class = "hetid_error_bad_argument"
  )

  broken <- inputs
  broken$components$L_i <- "not numeric"
  expect_error(
    compute_identified_set_quadratic(
      broken$tau, broken$components, broken$moments
    ),
    "L_i must be a numeric vector"
  )

  broken <- inputs
  broken$components$Q_i <- "not list"
  expect_error(
    compute_identified_set_quadratic(
      broken$tau, broken$components, broken$moments
    ),
    "Q_i must be a list"
  )
})

test_that("rejects components and moments from different subsets", {
  inputs <- setup_quadratic_test_inputs(
    n_rows = 3, n_maturities = 3, n_components = 6, maturities = c(2, 4, 5)
  )
  other <- setup_quadratic_test_inputs(
    n_rows = 3, n_maturities = 3, n_components = 6, maturities = c(1, 4, 5)
  )

  expect_error(
    compute_identified_set_quadratic(
      inputs$tau, other$components, inputs$moments
    ),
    "different maturities",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("rejects components and moments from different systems", {
  inputs <- setup_quadratic_test_inputs(
    n_rows = 3, n_maturities = 3, n_components = 6, maturities = c(2, 4, 5)
  )
  other <- setup_quadratic_test_inputs(
    n_rows = 3, n_maturities = 3, n_components = 7, maturities = c(2, 4, 5)
  )

  expect_error(
    compute_identified_set_quadratic(
      inputs$tau, other$components, inputs$moments
    ),
    "different n_components",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("rejects hand-built components with mismatched lengths", {
  inputs <- setup_quadratic_test_inputs()
  inputs$components$L_i <- inputs$components$L_i[1:3]

  expect_error(
    compute_identified_set_quadratic(
      inputs$tau, inputs$components, inputs$moments
    ),
    "All inputs must have length"
  )
})

test_that("errors on zero sigma_i_sq (no heteroskedasticity)", {
  inputs <- setup_quadratic_test_inputs()
  inputs$moments$sigma_i_sq[2] <- 0

  expect_error(
    compute_identified_set_quadratic(
      inputs$tau, inputs$components, inputs$moments
    ),
    "maturity/maturities 2"
  )
})

test_that("errors on NA/NaN/Inf sigma_i_sq", {
  bad_values <- c(NA, NaN, Inf, -0.5)
  for (bad in bad_values) {
    inputs <- setup_quadratic_test_inputs()
    inputs$moments$sigma_i_sq[2] <- bad
    expect_error(
      compute_identified_set_quadratic(
        inputs$tau, inputs$components, inputs$moments
      ),
      "non-positive, non-finite, or NA"
    )
  }
})

test_that("reports all bad sigma_i_sq maturities at once", {
  inputs <- setup_quadratic_test_inputs()
  inputs$moments$sigma_i_sq[1] <- 0
  inputs$moments$sigma_i_sq[3] <- -1
  inputs$moments$sigma_i_sq[4] <- NA

  expect_error(
    compute_identified_set_quadratic(
      inputs$tau, inputs$components, inputs$moments
    ),
    "maturity/maturities 1, 3, 4"
  )
})

test_that("errors on zero sigma_i_sq with maturities subset", {
  inputs <- setup_quadratic_test_inputs(
    n_rows = 3, n_maturities = 3, n_components = 6, maturities = c(2, 4, 5)
  )
  inputs$moments$sigma_i_sq[2] <- 0

  expect_error(
    compute_identified_set_quadratic(
      inputs$tau, inputs$components, inputs$moments
    ),
    "maturity/maturities 4"
  )
})

test_that("accepts small but positive sigma_i_sq", {
  inputs <- setup_quadratic_test_inputs()
  inputs$moments$sigma_i_sq[2] <- 1e-20

  result <- compute_identified_set_quadratic(
    inputs$tau, inputs$components, inputs$moments
  )
  expect_type(result, "list")
  expect_true(all(is.finite(result$d_i)))
})

test_that("accepts exact zero tau and returns the point-id benchmark form", {
  inputs <- setup_quadratic_test_inputs()
  tau <- rep(0, length(inputs$tau))

  result <- compute_identified_set_quadratic(
    tau, inputs$components, inputs$moments
  )

  expect_type(result, "list")
  expect_equal(unname(result$d_i), rep(0, length(inputs$components$L_i)))

  for (i in seq_along(inputs$components$Q_i)) {
    expect_equal(
      unname(result$A_i[[i]]),
      tcrossprod(inputs$components$Q_i[[i]])
    )
    expect_equal(
      unname(result$b_i[[i]]),
      -2 * inputs$components$L_i[[i]] * inputs$components$Q_i[[i]]
    )
    expect_equal(unname(result$c_i[i]), inputs$components$L_i[[i]]^2)
  }
})

test_that("errors on d_i overflow from tiny sigma_i_sq", {
  inputs <- setup_quadratic_test_inputs()
  inputs$moments$sigma_i_sq[2] <- 1e-309

  expect_error(
    compute_identified_set_quadratic(
      inputs$tau, inputs$components, inputs$moments
    ),
    "d_i overflowed to non-finite"
  )
})

test_that(
  "compute_identified_set_quadratic returns correct structure",
  {
    set.seed(123)
    n_obs <- 80
    J <- 3
    I <- 4

    w1 <- rnorm(n_obs)
    w2 <- matrix(rnorm(n_obs * I), n_obs, I)
    pcs <- matrix(rnorm(n_obs * J), n_obs, J)
    gamma <- matrix(rnorm(J * I), J, I)
    tau <- runif(I, 0.5, 2)

    moments <- compute_identification_moments(w1, w2, pcs)
    components <- compute_identified_set_components(gamma, moments)
    result <- compute_identified_set_quadratic(tau, components, moments)

    expect_type(result, "list")
    expect_named(result, c("d_i", "A_i", "b_i", "c_i"))

    mat_names <- maturity_names(1:I)

    expect_type(result$d_i, "double")
    expect_length(result$d_i, I)
    expect_named(result$d_i, mat_names)

    expect_type(result$A_i, "list")
    expect_length(result$A_i, I)
    expect_named(result$A_i, mat_names)

    expect_type(result$b_i, "list")
    expect_length(result$b_i, I)
    expect_named(result$b_i, mat_names)

    expect_type(result$c_i, "double")
    expect_length(result$c_i, I)
    expect_named(result$c_i, mat_names)

    for (i in 1:I) {
      expect_true(is.matrix(result$A_i[[i]]))
      expect_equal(dim(result$A_i[[i]]), c(I, I))
      expect_type(result$b_i[[i]], "double")
      expect_length(result$b_i[[i]], I)
      expect_named(result$b_i[[i]], mat_names)
    }
  }
)

test_that("container construction rejects maturities beyond the system", {
  expect_error(
    setup_quadratic_test_inputs(
      n_rows = 3, n_maturities = 2, n_components = 4, maturities = c(3, 7)
    ),
    "between 1 and n_components",
    class = "hetid_error_bad_argument"
  )

  expect_error(
    setup_quadratic_test_inputs(
      n_rows = 3, n_maturities = 2, n_components = 4, maturities = c(0, 2)
    ),
    "between 1 and n_components",
    class = "hetid_error_bad_argument"
  )
})
