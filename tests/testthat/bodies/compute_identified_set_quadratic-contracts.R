{
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
}

{
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
}

{
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
}

{
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
}

{
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
}

{
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
}

{
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
}

{
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
}
