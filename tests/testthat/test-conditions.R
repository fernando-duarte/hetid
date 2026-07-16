test_that("assert_bad_argument_ok passes on TRUE", {
  expect_invisible(assert_bad_argument_ok(TRUE, "msg"))
  expect_true(assert_bad_argument_ok(TRUE, "msg"))
})

test_that("assert_bad_argument_ok errors on FALSE", {
  expect_error(
    assert_bad_argument_ok(FALSE, "bad arg", arg = "x"),
    class = "hetid_error_bad_argument"
  )
})

test_that("assert_bad_argument_ok errors on NA", {
  expect_error(
    assert_bad_argument_ok(NA, "na input"),
    class = "hetid_error_bad_argument"
  )
})

test_that(
  "assert_bad_argument_ok errors on logical(0)",
  {
    expect_error(
      assert_bad_argument_ok(logical(0), "empty"),
      class = "hetid_error_bad_argument"
    )
  }
)

test_that(
  "assert_bad_argument_ok errors on non-scalar",
  {
    expect_error(
      assert_bad_argument_ok(c(TRUE, TRUE), "vector"),
      class = "hetid_error_bad_argument"
    )
  }
)

test_that(
  "assert_bad_argument_ok condition carries arg",
  {
    err <- tryCatch(
      assert_bad_argument_ok(
        FALSE, "msg",
        arg = "myarg"
      ),
      hetid_error_bad_argument = function(e) e
    )
    expect_equal(err$arg, "myarg")
  }
)

test_that("assert_dimension_ok passes on TRUE", {
  expect_true(assert_dimension_ok(TRUE, "msg"))
})

test_that("assert_dimension_ok errors on FALSE", {
  expect_error(
    assert_dimension_ok(FALSE, "dim mismatch"),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("assert_dimension_ok errors on NA", {
  expect_error(
    assert_dimension_ok(NA, "na"),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that(
  "assert_insufficient_data_ok passes on TRUE",
  {
    expect_true(
      assert_insufficient_data_ok(TRUE, "msg")
    )
  }
)

test_that(
  "assert_insufficient_data_ok errors on FALSE",
  {
    expect_error(
      assert_insufficient_data_ok(FALSE, "no data"),
      class = "hetid_error_insufficient_data"
    )
  }
)

test_that(
  "assert_insufficient_data_ok errors on NA",
  {
    expect_error(
      assert_insufficient_data_ok(NA, "na"),
      class = "hetid_error_insufficient_data"
    )
  }
)

test_that("assertion helpers have expected formals", {
  expect_equal(
    names(formals(assert_bad_argument_ok)),
    c("ok", "message", "arg")
  )
  expect_equal(
    names(formals(assert_dimension_ok)),
    c("ok", "message")
  )
  expect_equal(
    names(formals(assert_insufficient_data_ok)),
    c("ok", "message")
  )
})

test_that("assert_flag accepts a single TRUE/FALSE and rejects everything else", {
  expect_true(assert_flag(TRUE, "x"))
  expect_true(assert_flag(FALSE, "x"))

  for (bad in list(NA, "yes", c(TRUE, TRUE), 1, logical(0))) {
    expect_error(
      assert_flag(bad, "x"),
      class = "hetid_error_bad_argument"
    )
  }

  err <- tryCatch(assert_flag(NA, "paired"), error = function(e) e)
  expect_match(conditionMessage(err), "paired must be TRUE or FALSE", fixed = TRUE)
  expect_identical(err$arg, "paired")
})

test_that("exported logical flags are guarded at the boundary", {
  expect_error(
    compute_price_news(return_yield_news = NA),
    regexp = "return_yield_news must be TRUE or FALSE",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    build_instrument_matrix(matrix(rnorm(20), nrow = 10), include_original = NA),
    regexp = "include_original must be TRUE or FALSE",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_w1_residuals(return_df = NA),
    regexp = "return_df must be TRUE or FALSE",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_w2_residuals(return_df = NA),
    regexp = "return_df must be TRUE or FALSE",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_w2_residuals(impose_b_zero = NA),
    regexp = "impose_b_zero must be TRUE or FALSE",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    download_term_premia(force = NA),
    regexp = "force must be TRUE or FALSE",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    download_term_premia(quiet = NA),
    regexp = "quiet must be TRUE or FALSE",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    load_term_premia(auto_download = NA),
    regexp = "auto_download must be TRUE or FALSE",
    class = "hetid_error_bad_argument"
  )
})
