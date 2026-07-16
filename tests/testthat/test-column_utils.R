test_that("acm_column_name builds schema-driven column names", {
  expect_identical(acm_column_name("yields", 5), "y5")
  expect_identical(acm_column_name("term_premia", 1:2), c("tp1", "tp2"))
})

test_that("acm_column_name rejects unknown data types", {
  err <- tryCatch(
    acm_column_name("not_a_type", 1),
    error = function(e) e
  )

  expect_s3_class(err, "hetid_error_bad_argument")
  expect_match(
    conditionMessage(err), "Unknown data type: 'not_a_type'",
    fixed = TRUE
  )
})

test_that("acm_raw_column_name rejects unknown data types with its own arg", {
  err <- tryCatch(
    acm_raw_column_name("not_a_type", 12),
    error = function(e) e
  )

  expect_s3_class(err, "hetid_error_bad_argument")
  expect_match(
    conditionMessage(err), "Unknown data type: 'not_a_type'",
    fixed = TRUE
  )
  expect_identical(err$arg, "data_types")
})

test_that("assert_columns_exist names the context when columns are missing", {
  err <- tryCatch(
    assert_columns_exist(
      data.frame(a = 1), c("a", "b"),
      context = "input frame"
    ),
    error = function(e) e
  )

  expect_s3_class(err, "hetid_error_bad_argument")
  expect_match(
    conditionMessage(err), "Missing required columns: b",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "in input frame", fixed = TRUE)
})

test_that("assert_columns_exist keys on colnames for matrix input", {
  # bug-trigger regression: a matrix carrying every required column in colnames()
  # must pass. The old names()-based code reported all columns missing here
  # (names(m) is NULL), so this fixture fails on the buggy code and passes on the
  # fix, mirroring require_column's matrix branch
  m <- matrix(
    1:4,
    nrow = 2,
    dimnames = list(NULL, c("date", "pc1"))
  )
  expect_true(assert_columns_exist(m, c("date", "pc1")))

  # error-path coverage: a matrix with no column names still reports the columns
  # absent (colnames(m) is NULL -> every required column is missing)
  m_unnamed <- matrix(1:4, nrow = 2)
  err <- tryCatch(
    assert_columns_exist(m_unnamed, c("date", "pc1")),
    error = function(e) e
  )
  expect_s3_class(err, "hetid_error_bad_argument")
  expect_match(
    conditionMessage(err), "Missing required columns: date, pc1",
    fixed = TRUE
  )
})

test_that("assert_acm_data_types validates a non-empty vector of schema keys", {
  expect_true(assert_acm_data_types(c("yields", "term_premia")))

  err <- tryCatch(
    assert_acm_data_types(c("yields", "not_a_type")),
    error = function(e) e
  )
  expect_s3_class(err, "hetid_error_bad_argument")
  expect_match(
    conditionMessage(err), "Invalid data_types. Must be one or more of:",
    fixed = TRUE
  )
  expect_identical(err$arg, "data_types")

  # Empty and non-character inputs are rejected too
  expect_error(
    assert_acm_data_types(character(0)),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    assert_acm_data_types(1L),
    class = "hetid_error_bad_argument"
  )

  # Duplicates are rejected (mirrors validate_maturities)
  dup_err <- tryCatch(
    assert_acm_data_types(c("yields", "yields")),
    error = function(e) e
  )
  expect_s3_class(dup_err, "hetid_error_bad_argument")
  expect_match(
    conditionMessage(dup_err), "must not contain duplicates",
    fixed = TRUE
  )
})

test_that("require_column extracts from data frames and matrices", {
  expect_identical(require_column(data.frame(y12 = 3), "y12"), 3)

  # matrix column extraction returns the same unnamed shape as a data frame
  m <- matrix(1:2, nrow = 1, dimnames = list(NULL, c("y12", "y24")))
  expect_identical(require_column(m, "y24"), 2L)

  # missing column raises the structured error on both shapes
  expect_error(
    require_column(data.frame(a = 1), "y12"),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    require_column(matrix(1, 1, 1, dimnames = list(NULL, "a")), "y12"),
    class = "hetid_error_bad_argument"
  )
})
