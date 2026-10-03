test_that("compute_identified_set_quadratic validates inputs correctly", {
  eval(
    parse("bodies/compute_identified_set_quadratic-contracts.R", encoding = "UTF-8")[[1]],
    environment()
  )
})

test_that("rejects components and moments from different subsets", {
  eval(
    parse("bodies/compute_identified_set_quadratic-contracts.R", encoding = "UTF-8")[[2]],
    environment()
  )
})

test_that("rejects components and moments from different systems", {
  eval(
    parse("bodies/compute_identified_set_quadratic-contracts.R", encoding = "UTF-8")[[3]],
    environment()
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
  eval(
    parse("bodies/compute_identified_set_quadratic-contracts.R", encoding = "UTF-8")[[4]],
    environment()
  )
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
  eval(
    parse("bodies/compute_identified_set_quadratic-contracts.R", encoding = "UTF-8")[[5]],
    environment()
  )
})

test_that("d_i overflow error reports the actual offending values", {
  eval(
    parse("bodies/compute_identified_set_quadratic-contracts.R", encoding = "UTF-8")[[6]],
    environment()
  )
})

test_that("rejects tau at or above one with a structured error", {
  eval(
    parse("bodies/compute_identified_set_quadratic-contracts.R", encoding = "UTF-8")[[7]],
    environment()
  )
})

test_that("rejects non-finite tau with a structured error naming tau", {
  eval(
    parse("bodies/compute_identified_set_quadratic-contracts.R", encoding = "UTF-8")[[8]],
    environment()
  )
})

test_that("NA planted in moments s_i_0 raises a finiteness error", {
  eval(
    parse("bodies/compute_identified_set_quadratic-inputs.R", encoding = "UTF-8")[[1]],
    environment()
  )
})

test_that("NA in moments at zero tau does not blame sigma_i_sq", {
  eval(
    parse("bodies/compute_identified_set_quadratic-inputs.R", encoding = "UTF-8")[[2]],
    environment()
  )
})

test_that("NA planted in components L_i raises a finiteness error", {
  eval(
    parse("bodies/compute_identified_set_quadratic-inputs.R", encoding = "UTF-8")[[3]],
    environment()
  )
})

test_that("Inf planted in moments s_i_2 raises a finiteness error", {
  eval(
    parse("bodies/compute_identified_set_quadratic-inputs.R", encoding = "UTF-8")[[4]],
    environment()
  )
})

test_that("assembled A_i matrices are exactly symmetric", {
  eval(
    parse("bodies/compute_identified_set_quadratic-inputs.R", encoding = "UTF-8")[[5]],
    environment()
  )
})

test_that("assembly symmetrizes an asymmetric hand-built s_i_2 exactly", {
  eval(
    parse("bodies/compute_identified_set_quadratic-inputs.R", encoding = "UTF-8")[[6]],
    environment()
  )
})

test_that("compute_identified_set_quadratic returns correct structure", {
  eval(
    parse("bodies/compute_identified_set_quadratic-inputs.R", encoding = "UTF-8")[[7]],
    environment()
  )
})

test_that("container construction rejects maturities beyond the system", {
  eval(
    parse("bodies/compute_identified_set_quadratic-inputs.R", encoding = "UTF-8")[[8]],
    environment()
  )
})

test_that("assembly guard fires when components yield a non-finite form", {
  eval(
    parse("bodies/compute_identified_set_quadratic-validation.R", encoding = "UTF-8")[[1]],
    environment()
  )
})

test_that("a dim-carrying Q_i element is rejected as not a numeric vector", {
  inputs <- setup_quadratic_test_inputs(n_maturities = 2)
  # A 1 x I row matrix has the same length and values but is not a vector;
  # the tightened is_numeric_vector_dim guard must reject it
  inputs$components$Q_i[[1]] <- matrix(inputs$components$Q_i[[1]], nrow = 1)
  expect_error(
    compute_identified_set_quadratic(
      inputs$tau, inputs$components, inputs$moments
    ),
    class = "hetid_error_dimension_mismatch"
  )
})
