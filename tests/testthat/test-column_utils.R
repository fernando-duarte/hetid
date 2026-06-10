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
