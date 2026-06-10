# Tests for the hetid_moments container: constructor, validator, methods

make_moments_inputs <- function(n_obs = 60, n_components = 4, j = 3) {
  set.seed(42)
  list(
    w1 = rnorm(n_obs),
    w2 = matrix(rnorm(n_obs * n_components), nrow = n_obs),
    pcs = matrix(rnorm(n_obs * j), nrow = n_obs)
  )
}

test_that("constructor returns a validated container for all maturities", {
  inp <- make_moments_inputs()
  moments <- compute_identification_moments(inp$w1, inp$w2, inp$pcs)

  expect_s3_class(moments, "hetid_moments")
  expect_identical(attr(moments, "maturities"), 1:4)
  expect_identical(attr(moments, "n_components"), 4L)
  expect_identical(attr(moments, "n_obs"), 60L)

  scalar_stats <- compute_scalar_statistics(inp$w1, inp$w2)
  vector_stats <- compute_vector_statistics(inp$w1, inp$w2, inp$pcs)
  matrix_stats <- compute_matrix_statistics(inp$w1, inp$w2)
  expect_identical(moments$s_i_0, scalar_stats$s_i_0)
  expect_identical(moments$sigma_i_sq, scalar_stats$sigma_i_sq)
  expect_identical(moments$r_i_0, vector_stats$r_i_0)
  expect_identical(moments$r_i_1, vector_stats$r_i_1)
  expect_identical(moments$p_i_0, vector_stats$p_i_0)
  expect_identical(moments$s_i_1, matrix_stats$s_i_1)
  expect_identical(moments$s_i_2, matrix_stats$s_i_2)
})

test_that("constructor handles a nonconsecutive maturity subset", {
  inp <- make_moments_inputs()
  moments <- compute_identification_moments(
    inp$w1, inp$w2, inp$pcs,
    maturities = c(2, 4)
  )

  expect_identical(attr(moments, "maturities"), c(2L, 4L))
  expect_identical(attr(moments, "n_components"), 4L)
  expect_named(moments$s_i_0, c("maturity_2", "maturity_4"))
  expect_named(moments$r_i_1, c("maturity_2", "maturity_4"))
  expect_identical(colnames(moments$r_i_0), c("maturity_2", "maturity_4"))

  full <- compute_identification_moments(inp$w1, inp$w2, inp$pcs)
  expect_identical(moments$s_i_0, full$s_i_0[c(2, 4)])
  expect_identical(moments$s_i_2, full$s_i_2[c(2, 4)])
})

test_that("constructor rejects out-of-range maturities", {
  inp <- make_moments_inputs()
  expect_error(
    compute_identification_moments(
      inp$w1, inp$w2, inp$pcs,
      maturities = c(2, 5)
    ),
    class = "hetid_error_bad_argument"
  )
})

test_that("validator rejects missing statistics elements", {
  inp <- make_moments_inputs()
  moments <- compute_identification_moments(inp$w1, inp$w2, inp$pcs)
  stats <- unclass(moments)

  expect_error(
    new_hetid_moments(
      stats[setdiff(names(stats), "s_i_2")],
      maturities = 1:4, n_components = 4, n_obs = 60
    ),
    "must be a list containing",
    class = "hetid_error_bad_argument"
  )
})

test_that("validator rejects misaligned outer shapes and names", {
  inp <- make_moments_inputs()
  moments <- compute_identification_moments(inp$w1, inp$w2, inp$pcs)
  stats <- unclass(moments)

  truncated <- stats
  truncated$s_i_0 <- truncated$s_i_0[1:3]
  expect_error(
    new_hetid_moments(truncated, 1:4, 4, 60),
    class = "hetid_error_dimension_mismatch"
  )

  renamed <- stats
  names(renamed$sigma_i_sq) <- paste0("m", 1:4)
  expect_error(
    new_hetid_moments(renamed, 1:4, 4, 60),
    "names must equal maturity_N",
    class = "hetid_error_bad_argument"
  )

  expect_error(
    new_hetid_moments(stats, c(1, 2, 3, 3), 4, 60),
    "duplicates",
    class = "hetid_error_bad_argument"
  )
})

test_that("validator rejects wrong inner theta-axis dimensions", {
  inp <- make_moments_inputs()
  moments <- compute_identification_moments(inp$w1, inp$w2, inp$pcs)
  stats <- unclass(moments)

  trimmed <- stats
  trimmed$r_i_1[[2]] <- trimmed$r_i_1[[2]][, 1:3]
  expect_error(
    new_hetid_moments(trimmed, 1:4, 4, 60),
    "r_i_1 for maturity 2",
    class = "hetid_error_dimension_mismatch"
  )

  shortened <- stats
  shortened$s_i_1[[1]] <- shortened$s_i_1[[1]][1:2]
  expect_error(
    new_hetid_moments(shortened, 1:4, 4, 60),
    "s_i_1 for maturity 1",
    class = "hetid_error_dimension_mismatch"
  )

  shrunk <- stats
  shrunk$s_i_2[[3]] <- shrunk$s_i_2[[3]][1:2, 1:2]
  expect_error(
    new_hetid_moments(shrunk, 1:4, 4, 60),
    "s_i_2 for maturity 3",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("assert_hetid_moments rejects plain lists", {
  inp <- make_moments_inputs()
  moments <- compute_identification_moments(inp$w1, inp$w2, inp$pcs)
  expect_error(
    assert_hetid_moments(unclass(moments)),
    "hetid_moments object",
    class = "hetid_error_bad_argument"
  )
  expect_true(assert_hetid_moments(moments))
})

test_that("non-integer maturities error instead of truncating", {
  inp <- make_moments_inputs()
  expect_error(
    compute_identification_moments(
      inp$w1, inp$w2, inp$pcs,
      maturities = c(1.5, 2)
    ),
    "finite integer values",
    class = "hetid_error_bad_argument"
  )

  moments <- compute_identification_moments(inp$w1, inp$w2, inp$pcs)
  expect_error(
    new_hetid_moments(
      unclass(moments), 1.5,
      attr(moments, "n_components"), attr(moments, "n_obs")
    ),
    "finite integer values",
    class = "hetid_error_bad_argument"
  )
})

test_that("print method summarizes both axes", {
  inp <- make_moments_inputs()
  moments <- compute_identification_moments(
    inp$w1, inp$w2, inp$pcs,
    maturities = c(2, 4)
  )
  printed <- capture.output(print(moments))
  expect_true(any(grepl("hetid_moments", printed)))
  expect_true(any(grepl("components \\(theta axis\\): 4", printed)))
  expect_true(any(grepl("maturities \\(constraint axis\\): 2, 4", printed)))
})
