# --- assert_bad_argument_ok ---

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

# --- assert_dimension_ok ---

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

# --- assert_insufficient_data_ok ---

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

# --- Runtime signature stability ---

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
