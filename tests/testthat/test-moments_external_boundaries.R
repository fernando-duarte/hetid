moments_boundary_calls <- function(gamma, moments) {
  tau <- rep(0.2, ncol(gamma))
  list(
    validator = function(x) validate_hetid_moments(x),
    components = function(x) compute_identified_set_components(gamma, x),
    standard = function(x) build_quadratic_system(gamma, tau, x),
    general = function(x) build_general_quadratic_system(gamma, tau, x),
    separate = function(x) separate_instruments_lambda(x)
  )
}

test_that("external moment boundaries reject each nonnumeric matrix", {
  fixture <- make_general_test_system(i_dim = 3, j_dim = 2)
  moments <- fixture$moments
  gamma <- matrix(seq_len(6), 2, 3)
  calls <- moments_boundary_calls(gamma, moments)
  old <- options(warn = 2)
  on.exit(options(old))

  for (field in c("r_i_0", "p_i_0", "r_i_1", "s_i_2")) {
    indices <- if (is.list(moments[[field]])) seq_along(moments[[field]]) else 1L
    for (k in indices) {
      bad <- moments
      if (is.list(bad[[field]])) {
        storage.mode(bad[[field]][[k]]) <- "character"
      } else {
        storage.mode(bad[[field]]) <- "character"
      }
      for (call in calls) {
        err <- tryCatch(call(bad), error = identity)
        expect_s3_class(err, "hetid_error_bad_argument")
        expect_identical(err$arg, field)
      }
      expect_identical(assert_hetid_moments(bad), invisible(TRUE))
    }
  }
  expect_identical(validate_hetid_moments(moments), moments)
})

test_that("row and column s_i_1 matrices fail before quadratic assembly", {
  fixture <- make_general_test_system(i_dim = 3, j_dim = 2)
  moments <- fixture$moments
  calls <- moments_boundary_calls(matrix(seq_len(6), 2, 3), moments)
  old <- options(warn = 2)
  on.exit(options(old))

  for (k in seq_along(moments$s_i_1)) {
    for (n_rows in c(1L, 3L)) {
      bad <- moments
      bad$s_i_1[[k]] <- matrix(bad$s_i_1[[k]], nrow = n_rows)
      for (call in calls) {
        expect_error(call(bad), "s_i_1", class = "hetid_error_dimension_mismatch")
      }
    }
  }
})

test_that("valid reordered moment subsets preserve both axes and values", {
  fixture <- make_general_test_system(i_dim = 3, j_dim = 2)
  full <- fixture$moments
  subset <- compute_identification_moments(
    fixture$w1, fixture$w2, fixture$z,
    maturities = c(3, 1)
  )
  gamma <- matrix(seq_len(6), 2, 3)
  tau <- c(0.1, 0.2, 0.3)
  before <- subset
  checked <- withVisible(validate_hetid_moments(subset))
  expect_false(checked$visible)
  expect_identical(checked$value, subset)
  expect_identical(attr(subset, "maturities"), c(3L, 1L))
  expect_identical(attr(subset, "n_components"), 3L)
  expected <- c("maturity_3", "maturity_1")
  for (field in names(subset)) {
    full_part <- if (is.matrix(full[[field]])) {
      full[[field]][, expected, drop = FALSE]
    } else {
      full[[field]][expected]
    }
    expect_identical(subset[[field]], full_part)
  }

  standard <- build_quadratic_system(gamma, tau, subset)
  general <- build_general_quadratic_system(gamma, tau, subset)
  expect_identical(general$components, unclass(standard$components)[c("L_i", "V_i", "Q_i")])
  expect_identical(general$quadratic, standard$quadratic)
  direct <- standard$quadratic
  all_quadratic <- build_quadratic_system(gamma, tau, full)$quadratic
  for (field in names(direct)) {
    expect_identical(direct[[field]], all_quadratic[[field]][expected])
  }
  for (b in direct$b_i) {
    expect_type(b, "double")
    expect_null(dim(b))
    expect_length(b, 3L)
  }
  basis <- separate_instruments_lambda(subset)
  expect_null(basis[[2]])
  expect_identical(basis[[1]], basis[[3]])
  expect_identical(rownames(basis[[3]]), colnames(fixture$z))
  expect_identical(subset, before)
})

test_that("full moment validation retains its existing nonfinite value policy", {
  moments <- make_general_test_system(i_dim = 2, j_dim = 2)$moments
  moments$r_i_0[1] <- Inf
  moments$s_i_2[[1]][1] <- NA_real_
  expect_identical(validate_hetid_moments(moments), moments)
  expect_invisible(validate_hetid_moments(moments))
})
