test_that("compute_identified_set_quadratic validates inputs correctly", {
  inputs <- setup_quadratic_test_inputs()

  expect_error(
    do.call(
      compute_identified_set_quadratic,
      modifyList(inputs, list(gamma = "not matrix"))
    ),
    "gamma must be a matrix"
  )

  expect_error(
    do.call(
      compute_identified_set_quadratic,
      modifyList(inputs, list(tau = "not numeric"))
    ),
    "tau must be a numeric vector"
  )

  expect_error(
    do.call(
      compute_identified_set_quadratic,
      modifyList(inputs, list(tau = c(1, -1, 1, 1)))
    ),
    "All elements of tau must be nonnegative"
  )

  expect_error(
    do.call(
      compute_identified_set_quadratic,
      modifyList(inputs, list(tau = c(1, 1, 1)))
    ),
    "tau must have length I"
  )

  expect_error(
    do.call(
      compute_identified_set_quadratic,
      modifyList(inputs, list(L_i = "not numeric"))
    ),
    "L_i and V_i must be numeric vectors"
  )

  expect_error(
    do.call(
      compute_identified_set_quadratic,
      modifyList(inputs, list(Q_i = "not list"))
    ),
    "Q_i must be a list"
  )

  expect_error(
    do.call(
      compute_identified_set_quadratic,
      modifyList(inputs, list(s_i_1 = "not list"))
    ),
    "s_i_1 and s_i_2 must be lists"
  )
})

test_that("errors on zero sigma_i_sq (no heteroskedasticity)", {
  inputs <- setup_quadratic_test_inputs()
  inputs$sigma_i_sq <- c(1, 0, 1, 1)

  expect_error(
    do.call(compute_identified_set_quadratic, inputs),
    "maturity/maturities 2"
  )
})

test_that("errors on NA/NaN/Inf sigma_i_sq", {
  inputs <- setup_quadratic_test_inputs()

  # NA case
  inputs$sigma_i_sq <- c(1, NA, 1, 1)
  expect_error(
    do.call(compute_identified_set_quadratic, inputs),
    "non-positive, non-finite, or NA"
  )

  # NaN case
  inputs$sigma_i_sq <- c(1, NaN, 1, 1)
  expect_error(
    do.call(compute_identified_set_quadratic, inputs),
    "non-positive, non-finite, or NA"
  )

  # Inf case
  inputs$sigma_i_sq <- c(1, Inf, 1, 1)
  expect_error(
    do.call(compute_identified_set_quadratic, inputs),
    "non-positive, non-finite, or NA"
  )

  # Negative case
  inputs$sigma_i_sq <- c(1, -0.5, 1, 1)
  expect_error(
    do.call(compute_identified_set_quadratic, inputs),
    "non-positive, non-finite, or NA"
  )
})

test_that("reports all bad sigma_i_sq maturities at once", {
  inputs <- setup_quadratic_test_inputs()
  inputs$sigma_i_sq <- c(0, 1, -1, NA)

  expect_error(
    do.call(compute_identified_set_quadratic, inputs),
    "maturity/maturities 1, 3, 4"
  )
})

test_that("errors on zero sigma_i_sq with maturities subset", {
  inputs <- setup_quadratic_test_inputs(
    n_rows = 3, n_maturities = 3, n_components = 6
  )
  inputs$sigma_i_sq <- c(1, 0, 1)

  expect_error(
    do.call(
      compute_identified_set_quadratic,
      c(inputs, list(maturities = c(2, 4, 5)))
    ),
    "maturity/maturities 4"
  )
})

test_that("accepts small but positive sigma_i_sq", {
  inputs <- setup_quadratic_test_inputs()
  inputs$sigma_i_sq <- c(1, 1e-20, 1, 1)

  result <- do.call(compute_identified_set_quadratic, inputs)
  expect_type(result, "list")
  expect_true(all(is.finite(result$d_i)))
})

test_that("accepts exact zero tau and returns the point-id benchmark form", {
  inputs <- setup_quadratic_test_inputs()
  inputs$tau <- rep(0, length(inputs$tau))

  result <- do.call(compute_identified_set_quadratic, inputs)

  expect_type(result, "list")
  expect_equal(unname(result$d_i), rep(0, length(inputs$L_i)))

  for (i in seq_along(inputs$Q_i)) {
    expect_equal(unname(result$A_i[[i]]), tcrossprod(inputs$Q_i[[i]]))
    expect_equal(
      unname(result$b_i[[i]]),
      -2 * inputs$L_i[i] * inputs$Q_i[[i]]
    )
    expect_equal(unname(result$c_i[i]), inputs$L_i[i]^2)
  }
})

test_that("errors on d_i overflow from tiny sigma_i_sq", {
  inputs <- setup_quadratic_test_inputs()
  inputs$sigma_i_sq <- c(1, 1e-309, 1, 1)

  expect_error(
    do.call(compute_identified_set_quadratic, inputs),
    "d_i overflowed to non-finite"
  )
})

test_that(
  "compute_identified_set_quadratic returns correct structure",
  {
    set.seed(123)
    J <- 3
    I <- 4

    gamma <- matrix(rnorm(J * I), J, I)
    tau <- runif(I, 0.5, 2)
    l_i <- runif(I)
    v_i <- runif(I)
    q_i <- lapply(1:I, function(i) rnorm(I))
    s_i_0 <- runif(I)
    s_i_1 <- lapply(1:I, function(i) rnorm(I))
    # Symmetric s_i_2 needed for valid quadratic form
    s_i_2 <- lapply(1:I, function(i) {
      M <- matrix(rnorm(I * I), I, I)
      M %*% t(M)
    })
    sigma_i_sq <- runif(I, 0.1, 1)

    result <- compute_identified_set_quadratic(
      gamma, tau, l_i, v_i, q_i,
      s_i_0, s_i_1, s_i_2, sigma_i_sq
    )

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

test_that("errors when maturities exceed ncol(gamma)", {
  inputs <- setup_quadratic_test_inputs(
    n_rows = 3, n_maturities = 2, n_components = 4
  )

  expect_error(
    do.call(
      compute_identified_set_quadratic,
      c(inputs, list(maturities = c(3, 7)))
    ),
    "between 1 and ncol\\(gamma\\)"
  )
})

test_that("errors when maturities include zero or negative", {
  inputs <- setup_quadratic_test_inputs(
    n_rows = 3, n_maturities = 2, n_components = 4
  )

  expect_error(
    do.call(
      compute_identified_set_quadratic,
      c(inputs, list(maturities = c(0, 2)))
    ),
    "between 1 and ncol\\(gamma\\)"
  )
})

test_that("rejects mismatched input lengths with subset", {
  inputs <- setup_quadratic_test_inputs(
    n_rows = 3, n_maturities = 6, n_components = 6
  )
  maturities <- c(1, 3, 5)

  # Full-length s_i_1 with subsetted L_i should error
  expect_error(
    do.call(compute_identified_set_quadratic, c(
      list(
        gamma = inputs$gamma,
        tau = inputs$tau,
        L_i = inputs$L_i[maturities],
        V_i = inputs$V_i[maturities],
        Q_i = inputs$Q_i[maturities],
        s_i_0 = inputs$s_i_0[maturities],
        s_i_1 = inputs$s_i_1,
        s_i_2 = inputs$s_i_2,
        sigma_i_sq = inputs$sigma_i_sq[maturities]
      ),
      list(maturities = maturities)
    )),
    "length matching maturities"
  )
})
