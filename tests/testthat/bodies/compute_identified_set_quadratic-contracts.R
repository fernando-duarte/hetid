{
  inputs <- setup_quadratic_test_inputs()

  expect_error(
    build_quadratic_system(inputs$gamma, "not numeric", inputs$moments)$quadratic,
    "tau must be a numeric vector"
  )

  expect_error(
    build_quadratic_system(inputs$gamma, c(0.5, -1, 0.5, 0.5), inputs$moments)$quadratic,
    "All elements of tau must be in [0, 1)",
    fixed = TRUE
  )

  expect_error(
    build_quadratic_system(inputs$gamma, c(0.5, 0.5, 0.5), inputs$moments)$quadratic,
    "tau must have length I"
  )

  expect_error(
    build_quadratic_system(inputs$gamma, inputs$tau, unclass(inputs$moments)),
    "hetid_moments object",
    class = "hetid_error_bad_argument"
  )
}

{
  bad_values <- c(NA, NaN, Inf, -0.5)
  for (bad in bad_values) {
    inputs <- setup_quadratic_test_inputs()
    inputs$moments$sigma_i_sq[2] <- bad
    expect_error(
      build_quadratic_system(inputs$gamma, inputs$tau, inputs$moments)$quadratic,
      "non-positive, non-finite, or NA"
    )
  }
}

{
  inputs <- setup_quadratic_test_inputs()
  tau <- rep(0, length(inputs$tau))

  result <- build_quadratic_system(inputs$gamma, tau, inputs$moments)$quadratic

  components <- compute_identified_set_components(inputs$gamma, inputs$moments)

  expect_type(result, "list")
  expect_equal(unname(result$d_i), rep(0, length(components$L_i)))

  for (i in seq_along(components$Q_i)) {
    expect_equal(
      unname(result$A_i[[i]]),
      unname(tcrossprod(components$Q_i[[i]]))
    )
    expect_equal(
      unname(result$b_i[[i]]),
      unname(-2 * components$L_i[[i]] * components$Q_i[[i]])
    )
    expect_equal(unname(result$c_i[i]), components$L_i[[i]]^2)
  }
}

{
  inputs <- setup_quadratic_test_inputs()
  inputs$moments$sigma_i_sq[2] <- 1e-309

  err <- tryCatch(
    build_quadratic_system(inputs$gamma, inputs$tau, inputs$moments)$quadratic,
    error = function(e) e
  )

  expect_s3_class(err, "hetid_error")
  expect_match(
    conditionMessage(err), "non-finite for maturity_2",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "tau_i = 0.5", fixed = TRUE)
  expect_match(conditionMessage(err), "V_i = 9", fixed = TRUE)
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
      build_quadratic_system(inputs$gamma, tau, inputs$moments)$quadratic,
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
      build_quadratic_system(inputs$gamma, tau, inputs$moments)$quadratic,
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
