{
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
}

{
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
}

{
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
}

{
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
}

{
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
}

{
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
}

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

{
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
}
