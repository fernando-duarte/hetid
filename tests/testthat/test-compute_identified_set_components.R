# Tests for compute_identified_set_components with the moments container

make_components_moments <- function(r_i_0, r_i_1, p_i_0,
                                    maturities, n_components) {
  n <- length(maturities)
  nms <- paste0("maturity_", maturities)
  colnames(r_i_0) <- nms
  colnames(p_i_0) <- nms
  names(r_i_1) <- nms
  new_hetid_moments(
    list(
      s_i_0 = setNames(rep(1, n), nms),
      sigma_i_sq = setNames(rep(1, n), nms),
      r_i_0 = r_i_0,
      r_i_1 = r_i_1,
      p_i_0 = p_i_0,
      s_i_1 = setNames(
        lapply(seq_len(n), function(k) numeric(n_components)), nms
      ),
      s_i_2 = setNames(
        lapply(seq_len(n), function(k) diag(n_components)), nms
      )
    ),
    maturities = maturities,
    n_components = n_components,
    n_obs = 50
  )
}

test_that("components validates gamma and moments arguments", {
  eval(
    parse("bodies/compute_identified_set_components-contracts.R", encoding = "UTF-8")[[1]],
    environment()
  )
})

test_that("components rejects non-numeric and non-finite gamma", {
  eval(
    parse("bodies/compute_identified_set_components-contracts.R", encoding = "UTF-8")[[2]],
    environment()
  )
})

test_that("compute_identified_set_components returns correct structure", {
  eval(
    parse("bodies/compute_identified_set_components-contracts.R", encoding = "UTF-8")[[3]],
    environment()
  )
})

test_that("an all-zero gamma column outside maturities is accepted", {
  J <- 2
  I <- 2

  gamma <- matrix(c(1, 1, 0, 0), J, I) # column 2 is all zero
  moments <- make_components_moments(
    r_i_0 = matrix(c(2, 3), J, 1),
    r_i_1 = list(matrix(c(1, 2, 3, 4), J, I)),
    p_i_0 = matrix(c(1, 2), J, 1),
    maturities = 1L, n_components = I
  )

  # the guard is scoped to constrained columns, matching as_lambda_list();
  # column 2 is never read, so a stricter check would over-reject
  expect_s3_class(
    compute_identified_set_components(gamma, moments), "hetid_components"
  )
})

test_that("compute_identified_set_components computes values correctly", {
  eval(
    parse("bodies/compute_identified_set_components-contracts.R", encoding = "UTF-8")[[4]],
    environment()
  )
})

test_that("components aligns a maturity subset with the gamma columns", {
  eval(
    parse("bodies/compute_identified_set_components-contracts.R", encoding = "UTF-8")[[5]],
    environment()
  )
})

test_that("pipeline: subset moments match full-system components", {
  eval(
    parse("bodies/compute_identified_set_components-inputs.R", encoding = "UTF-8")[[1]],
    environment()
  )
})

test_that("print method summarizes both axes", {
  set.seed(123)
  j_pcs <- 3
  i_sys <- 4
  maturities <- c(2, 4)
  n_mat <- length(maturities)
  gamma <- matrix(rnorm(j_pcs * i_sys), j_pcs, i_sys)
  moments <- make_components_moments(
    r_i_0 = matrix(rnorm(j_pcs * n_mat), j_pcs, n_mat),
    r_i_1 = lapply(seq_len(n_mat), function(k) {
      matrix(rnorm(j_pcs * i_sys), j_pcs, i_sys)
    }),
    p_i_0 = matrix(rnorm(j_pcs * n_mat), j_pcs, n_mat),
    maturities = maturities, n_components = i_sys
  )
  # Construct the hetid_components object (not the moments) before printing,
  # so dispatch hits print.hetid_components
  components <- compute_identified_set_components(gamma, moments)
  expect_s3_class(components, "hetid_components")

  printed <- capture.output(returned <- print(components))
  expect_true(any(grepl("hetid_components", printed)))
  expect_true(any(grepl("components \\(theta axis\\): 4", printed)))
  expect_true(any(grepl("maturities \\(constraint axis\\): 2, 4", printed)))
  expect_identical(returned, components)
})

test_that("constructor rejects wrong component types and lengths", {
  nms <- paste0("maturity_", 1:2)
  l_ok <- stats::setNames(c(1, 2), nms)
  q_ok <- stats::setNames(list(c(1, 2), c(3, 4)), nms)

  expect_error(
    new_hetid_components(
      L_i = "a", V_i = l_ok, Q_i = q_ok,
      maturities = 1:2, n_components = 2
    ),
    "L_i must be a numeric vector",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    new_hetid_components(
      L_i = l_ok, V_i = l_ok[1], Q_i = q_ok,
      maturities = 1:2, n_components = 2
    ),
    "V_i must be a numeric vector",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    new_hetid_components(
      L_i = l_ok, V_i = l_ok, Q_i = c(1, 2),
      maturities = 1:2, n_components = 2
    ),
    "Q_i must be a list",
    class = "hetid_error_bad_argument"
  )
})

test_that("new_hetid_components rejects non-integer maturities before coercing", {
  nms <- paste0("maturity_", 1:2)
  l_ok <- stats::setNames(c(1, 2), nms)
  q_ok <- stats::setNames(list(c(1, 2), c(3, 4)), nms)

  expect_error(
    new_hetid_components(
      L_i = l_ok, V_i = l_ok, Q_i = q_ok,
      maturities = c(1, 2.9), n_components = 2
    ),
    regexp = "maturities must be finite integer values",
    class = "hetid_error_bad_argument"
  )
})

test_that("validate_hetid_components returns a valid object invisibly", {
  inputs <- setup_quadratic_test_inputs(n_maturities = 2)
  expect_invisible(validate_hetid_components(inputs$components))
  expect_identical(
    validate_hetid_components(inputs$components), inputs$components
  )

  renamed <- inputs$components
  names(renamed$L_i) <- c("a", "b")
  expect_error(
    validate_hetid_components(renamed),
    "names must equal maturity_N",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    validate_hetid_components(unclass(inputs$components)),
    class = "hetid_error_bad_argument"
  )
})
