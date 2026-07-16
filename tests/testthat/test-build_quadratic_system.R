# End-to-end tests for the sanctioned moments -> components -> quadratic chain

make_system_inputs <- function(n_obs = 100, n_components = 4, j = 3,
                               seed = 42) {
  set.seed(seed)
  list(
    w1 = rnorm(n_obs),
    w2 = matrix(rnorm(n_obs * n_components), nrow = n_obs),
    pcs = matrix(rnorm(n_obs * j), nrow = n_obs),
    gamma = matrix(rnorm(j * n_components), nrow = j),
    tau = rep(0.2, n_components)
  )
}

test_that("build_quadratic_system chains components and quadratic", {
  inp <- make_system_inputs()
  moments <- compute_identification_moments(inp$w1, inp$w2, inp$pcs)

  system <- build_quadratic_system(inp$gamma, inp$tau, moments)

  expect_named(system, c("components", "quadratic"))
  expect_s3_class(system$components, "hetid_components")
  expect_named(system$quadratic, c("d_i", "A_i", "b_i", "c_i"))

  components <- compute_identified_set_components(inp$gamma, moments)
  quadratic <- compute_identified_set_quadratic(inp$tau, components, moments)
  expect_identical(system$components$L_i, components$L_i)
  expect_identical(system$quadratic, quadratic)
})

test_that("subset system equals the matching full-system constraints", {
  inp <- make_system_inputs(n_components = 6)
  maturities <- c(2, 4, 5)

  full_moments <- compute_identification_moments(inp$w1, inp$w2, inp$pcs)
  full_tau <- rep(0.2, 6)
  full_system <- build_quadratic_system(inp$gamma, full_tau, full_moments)

  subset_moments <- compute_identification_moments(
    inp$w1, inp$w2, inp$pcs,
    maturities = maturities
  )
  subset_system <- build_quadratic_system(inp$gamma, full_tau, subset_moments)

  nms <- paste0("maturity_", maturities)
  expect_identical(
    subset_system$quadratic$d_i,
    full_system$quadratic$d_i[nms]
  )
  expect_identical(
    subset_system$quadratic$A_i,
    full_system$quadratic$A_i[nms]
  )
  expect_identical(
    subset_system$quadratic$c_i,
    full_system$quadratic$c_i[nms]
  )
})

test_that("constraint checker preserves the hin <= 0 sign convention", {
  inp <- make_system_inputs()
  moments <- compute_identification_moments(inp$w1, inp$w2, inp$pcs)
  system <- build_quadratic_system(inp$gamma, rep(0, 4), moments)

  # At tau = 0 the constraint is (Q_i' theta - L_i)^2 <= 0, so theta*
  # solves Q_i' theta = L_i and anything else evaluates strictly positive
  quadratic <- system$quadratic
  check <- make_constraint_checker(
    quadratic$A_i[[1]], quadratic$b_i[[1]], quadratic$c_i[[1]]
  )

  q_vec <- system$components$Q_i[[1]]
  l_val <- system$components$L_i[[1]]
  theta_on <- l_val / sum(q_vec) * rep(1, length(q_vec))
  expect_equal(check(theta_on), 0, tolerance = 1e-10)
  expect_gt(check(theta_on + 1), 0)
})

test_that("rejects gamma from a different system", {
  inp <- make_system_inputs()
  moments <- compute_identification_moments(inp$w1, inp$w2, inp$pcs)

  expect_error(
    build_quadratic_system(inp$gamma[, 1:3], inp$tau, moments),
    "n_components",
    class = "hetid_error_dimension_mismatch"
  )
})
