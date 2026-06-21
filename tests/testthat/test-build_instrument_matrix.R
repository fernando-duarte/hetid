test_that("transform outputs are bound next to the originals", {
  set.seed(7)
  z <- matrix(rnorm(60), nrow = 20, dimnames = list(NULL, c("a", "b", "c")))
  out <- build_instrument_matrix(
    z,
    transforms = list(sq_a = function(z) z[, "a"]^2)
  )
  expect_identical(out, cbind(z, sq_a = z[, "a"]^2))
})

test_that("a bare function transform and matrix-valued transforms work", {
  set.seed(7)
  z <- matrix(rnorm(40), nrow = 20, dimnames = list(NULL, c("a", "b")))
  out <- build_instrument_matrix(
    z,
    transforms = function(z) z^2
  )
  expect_identical(colnames(out), c("a", "b", "h1_1", "h1_2"))
  expect_identical(unname(out[, 3:4]), unname(z^2))
})

test_that("partially named transform lists backfill names by position", {
  z <- matrix(rnorm(40), nrow = 20, dimnames = list(NULL, c("a", "b")))
  out <- build_instrument_matrix(
    z,
    transforms = list(
      sq_a = function(z) z[, "a"]^2,
      function(z) z[, "b"]^2,
      function(z) z[, "a"] * z[, "b"]
    )
  )
  expect_identical(colnames(out), c("a", "b", "sq_a", "h2", "h3"))
  expect_identical(unname(out[, "h3"]), unname(z[, "a"] * z[, "b"]))
})

test_that("originals can be excluded when transforms are supplied", {
  z <- matrix(rnorm(20), nrow = 10, dimnames = list(NULL, c("a", "b")))
  out <- build_instrument_matrix(
    z,
    transforms = list(prod_ab = function(z) z[, "a"] * z[, "b"]),
    include_original = FALSE
  )
  expect_identical(colnames(out), "prod_ab")
})

test_that("excluding originals with no transforms is rejected", {
  z <- matrix(rnorm(20), nrow = 10)
  expect_error(
    build_instrument_matrix(z, include_original = FALSE),
    class = "hetid_error_bad_argument"
  )
})

test_that("non-finite transform output is rejected", {
  z <- matrix(rnorm(20), nrow = 10, dimnames = list(NULL, c("a", "b")))
  expect_error(
    build_instrument_matrix(
      z,
      transforms = list(bad = function(z) z[, "a"] / 0)
    ),
    class = "hetid_error_bad_argument"
  )
})

test_that("duplicate instrument names are rejected", {
  z <- matrix(rnorm(20), nrow = 10, dimnames = list(NULL, c("a", "b")))
  expect_error(
    build_instrument_matrix(
      z,
      transforms = list(a = function(z) z[, "b"]^2)
    ),
    class = "hetid_error_bad_argument"
  )
})

test_that("NA or empty primitive column names are rejected", {
  z <- matrix(rnorm(20), nrow = 10)
  colnames(z) <- c("a", NA)
  expect_error(
    build_instrument_matrix(z),
    class = "hetid_error_bad_argument"
  )
  colnames(z) <- c("a", "")
  expect_error(
    build_instrument_matrix(z),
    class = "hetid_error_bad_argument"
  )
})

test_that("zero-column transform output is rejected", {
  z <- matrix(rnorm(20), nrow = 10, dimnames = list(NULL, c("a", "b")))
  expect_error(
    build_instrument_matrix(
      z,
      transforms = list(none = function(z) z[, integer(0), drop = FALSE])
    ),
    class = "hetid_error_bad_argument"
  )
})

test_that("wrong-length transform output is rejected", {
  z <- matrix(rnorm(20), nrow = 10, dimnames = list(NULL, c("a", "b")))
  expect_error(
    build_instrument_matrix(
      z,
      transforms = list(short = function(z) z[-1, "a"])
    ),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("the result feeds the moments computation directly", {
  set.seed(11)
  w1 <- rnorm(30)
  w2 <- matrix(rnorm(60), nrow = 30)
  z <- matrix(rnorm(60), nrow = 30, dimnames = list(NULL, c("a", "b")))
  instruments <- build_instrument_matrix(
    z,
    transforms = list(sq_a = function(z) z[, "a"]^2)
  )
  moments <- compute_identification_moments(w1, w2, instruments)
  expect_identical(rownames(moments$r_i_0), c("a", "b", "sq_a"))
})

test_that("the default (no transforms) returns the originals unchanged", {
  set.seed(7)
  z <- matrix(rnorm(40), nrow = 20, dimnames = list(NULL, c("a", "b")))
  # transforms = NULL, include_original = TRUE: blocks is exactly list(z),
  # so the result is z up to cbind's attribute handling.
  expect_equal(build_instrument_matrix(z), z)
})
