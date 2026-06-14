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
      c(0.5, -1, 0.5, 0.5), inputs$components, inputs$moments
    ),
    "All elements of tau must be in [0, 1)",
    fixed = TRUE
  )

  expect_error(
    compute_identified_set_quadratic(
      c(0.5, 0.5, 0.5), inputs$components, inputs$moments
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

test_that("d_i overflow error reports the actual offending values", {
  inputs <- setup_quadratic_test_inputs()
  inputs$moments$sigma_i_sq[2] <- 1e-309

  err <- tryCatch(
    compute_identified_set_quadratic(
      inputs$tau, inputs$components, inputs$moments
    ),
    error = function(e) e
  )

  expect_s3_class(err, "hetid_error")
  expect_match(
    conditionMessage(err), "non-finite for maturity 2",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "tau_i = 0.5", fixed = TRUE)
  expect_match(conditionMessage(err), "V_i = 1", fixed = TRUE)
  expect_match(
    conditionMessage(err), "sigma_i_sq = 1e-309",
    fixed = TRUE
  )
})

test_that("rejects tau at or above one with a structured error", {
  inputs <- setup_quadratic_test_inputs()
  for (bad in c(1, 1.5, 100)) {
    tau <- inputs$tau
    tau[2] <- bad

    err <- tryCatch(
      compute_identified_set_quadratic(
        tau, inputs$components, inputs$moments
      ),
      error = function(e) e
    )

    expect_s3_class(err, "hetid_error_bad_argument")
    expect_match(
      conditionMessage(err), "All elements of tau must be in [0, 1)",
      fixed = TRUE
    )
    expect_identical(err$arg, "tau")
  }
})

test_that("rejects non-finite tau with a structured error naming tau", {
  inputs <- setup_quadratic_test_inputs()
  for (bad in c(Inf, NA_real_, NaN)) {
    tau <- inputs$tau
    tau[2] <- bad

    err <- tryCatch(
      compute_identified_set_quadratic(
        tau, inputs$components, inputs$moments
      ),
      error = function(e) e
    )

    expect_s3_class(err, "hetid_error_bad_argument")
    expect_match(
      conditionMessage(err), "tau must be finite",
      fixed = TRUE
    )
    expect_identical(err$arg, "tau")
  }
})

test_that("NA planted in moments s_i_0 raises a finiteness error", {
  inputs <- setup_quadratic_test_inputs()
  inputs$moments$s_i_0[3] <- NA_real_

  err <- tryCatch(
    compute_identified_set_quadratic(
      inputs$tau, inputs$components, inputs$moments
    ),
    error = function(e) e
  )

  expect_s3_class(err, "hetid_error_bad_argument")
  expect_match(
    conditionMessage(err),
    "s_i_0 contains non-finite (NA/NaN/Inf) values for maturity/maturities 3",
    fixed = TRUE
  )
  expect_identical(err$arg, "s_i_0")
})

test_that("NA in moments at zero tau does not blame sigma_i_sq", {
  inputs <- setup_quadratic_test_inputs()
  inputs$moments$s_i_0[3] <- NA_real_

  err <- tryCatch(
    compute_identified_set_quadratic(
      rep(0, length(inputs$tau)), inputs$components, inputs$moments
    ),
    error = function(e) e
  )

  expect_s3_class(err, "hetid_error_bad_argument")
  expect_match(conditionMessage(err), "s_i_0", fixed = TRUE)
  expect_false(grepl("sigma_i_sq", conditionMessage(err), fixed = TRUE))
})

test_that("NA planted in components L_i raises a finiteness error", {
  inputs <- setup_quadratic_test_inputs()
  inputs$components$L_i[1] <- NA_real_

  err <- tryCatch(
    compute_identified_set_quadratic(
      inputs$tau, inputs$components, inputs$moments
    ),
    error = function(e) e
  )

  expect_s3_class(err, "hetid_error_bad_argument")
  expect_match(
    conditionMessage(err),
    "L_i contains non-finite (NA/NaN/Inf) values for maturity/maturities 1",
    fixed = TRUE
  )
  expect_identical(err$arg, "L_i")
})

test_that("Inf planted in moments s_i_2 raises a finiteness error", {
  inputs <- setup_quadratic_test_inputs()
  inputs$moments$s_i_2[[4]][1, 2] <- Inf

  err <- tryCatch(
    compute_identified_set_quadratic(
      inputs$tau, inputs$components, inputs$moments
    ),
    error = function(e) e
  )

  expect_s3_class(err, "hetid_error_bad_argument")
  expect_match(
    conditionMessage(err),
    "s_i_2 contains non-finite (NA/NaN/Inf) values for maturity/maturities 4",
    fixed = TRUE
  )
  expect_identical(err$arg, "s_i_2")
})

test_that("assembled A_i matrices are exactly symmetric", {
  set.seed(11)
  n_obs <- 60
  J <- 3
  I <- 4
  w1 <- rnorm(n_obs)
  w2 <- matrix(rnorm(n_obs * I), n_obs, I)
  pcs <- matrix(rnorm(n_obs * J), n_obs, J)
  gamma <- matrix(rnorm(J * I), J, I)
  tau <- runif(I, 0.1, 0.9)

  moments <- compute_identification_moments(w1, w2, pcs)
  components <- compute_identified_set_components(gamma, moments)
  result <- compute_identified_set_quadratic(tau, components, moments)

  for (a in result$A_i) {
    expect_identical(a, t(a))
  }
})

test_that("assembly symmetrizes an asymmetric hand-built s_i_2 exactly", {
  asym <- matrix(c(1, 0.25, 0.75, 1), 2, 2)

  result <- quadratic_from_components(
    tau = c(0.5, 0.5),
    L_i = c(1, 1), V_i = c(1, 1),
    Q_i = list(c(1, 2), c(3, 4)),
    s_i_0 = c(1, 1),
    s_i_1 = list(c(0, 0), c(0, 0)),
    s_i_2 = list(asym, asym),
    sigma_i_sq = c(1, 1),
    maturities = 1:2, n_components = 2
  )

  for (a in result$A_i) {
    expect_identical(a, t(a))
  }
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
    tau <- runif(I, 0.1, 0.9)

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

test_that("assembly guard fires when components yield a non-finite form", {
  # d_i stays finite (tau_i^2 * V_i / sigma_i_sq), but an Inf in Q_i makes
  # the assembled A_i non-finite, tripping the belt-and-braces guard.
  err <- tryCatch(
    quadratic_from_components(
      tau = c(0.5, 0.5),
      L_i = c(1, 1), V_i = c(1, 1),
      Q_i = list(c(Inf, 2), c(3, 4)),
      s_i_0 = c(1, 1),
      s_i_1 = list(c(0, 0), c(0, 0)),
      s_i_2 = list(diag(2), diag(2)),
      sigma_i_sq = c(1, 1),
      maturities = 1:2, n_components = 2
    ),
    error = function(e) e
  )

  expect_s3_class(err, "hetid_error")
  expect_match(
    conditionMessage(err),
    "Assembled quadratic form contains non-finite values",
    fixed = TRUE
  )
})

test_that("a dim-carrying Q_i element is rejected as not a numeric vector", {
  inputs <- setup_quadratic_test_inputs(n_maturities = 2)
  # A 1 x I row matrix has the same length and values but is not a vector;
  # the tightened is_numeric_vector_dim guard must reject it.
  inputs$components$Q_i[[1]] <- matrix(inputs$components$Q_i[[1]], nrow = 1)
  expect_error(
    compute_identified_set_quadratic(
      inputs$tau, inputs$components, inputs$moments
    ),
    class = "hetid_error_dimension_mismatch"
  )
})
