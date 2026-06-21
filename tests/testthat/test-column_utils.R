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
