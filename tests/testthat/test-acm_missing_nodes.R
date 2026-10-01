test_that("mixed ACM nodes identify only missing requested maturities", {
  withr::local_options(warn = 2)
  acm <- data.frame(
    date = as.Date(c("2020-01-31", "2020-02-29")),
    ACMY003M = c(2, NA_real_), ACMY01 = c(3, 4)
  )
  local_mocked_bindings(load_term_premia = function(...) acm, .package = "hetid")
  for (maturities in list(c(3, 6), c(6, 3))) {
    err <- tryCatch(extract_acm_data("yields", maturities), error = identity)
    expect_s3_class(err, "hetid_error_insufficient_data")
    expect_s3_class(err, "hetid_error")
    message <- conditionMessage(err)
    expect_match(message, "ACMY006M", fixed = TRUE)
    expect_match(message, "months: 6", fixed = TRUE)
    expect_false(grepl("ACMY003M", message, fixed = TRUE))
    expect_false(grepl("only annual", message, fixed = TRUE))
  }
  result <- withVisible(extract_acm_data("yields", c(3, 12)))
  expect_true(result$visible)
  expect_identical(result$value, data.frame(
    date = acm$date, y3 = acm$ACMY003M,
    y12 = acm$ACMY01
  ))
})

test_that("annual-only ACM sources retain their truthful diagnosis", {
  acm <- data.frame(date = as.Date("2020-01-31"), ACMY01 = 3, ACMY02 = 4)
  local_mocked_bindings(load_term_premia = function(...) acm, .package = "hetid")
  err <- tryCatch(extract_acm_data("yields", c(12, 18)), error = identity)
  expect_s3_class(err, "hetid_error_insufficient_data")
  expect_match(conditionMessage(err), "only annual maturities", fixed = TRUE)
  expect_match(conditionMessage(err), "ACMY018M", fixed = TRUE)
  expect_match(conditionMessage(err), "months: 18", fixed = TRUE)
  expect_match(conditionMessage(err), "GitHub source", fixed = TRUE)
  expect_identical(
    extract_acm_data("yields", c(24, 12)),
    data.frame(date = acm$date, y24 = acm$ACMY02, y12 = acm$ACMY01)
  )
})

test_that("annual-only diagnosis requires observed annual maturity columns", {
  fixtures <- list(
    data.frame(date = as.Date("2020-01-31")),
    data.frame(date = as.Date("2020-01-31"), ACMTP003M = 0.5, ACMY01 = 3)
  )
  for (acm in fixtures) {
    local_mocked_bindings(load_term_premia = function(...) acm, .package = "hetid")
    err <- tryCatch(extract_acm_data("yields", 6), error = identity)
    expect_s3_class(err, "hetid_error_insufficient_data")
    expect_match(conditionMessage(err), "ACMY006M", fixed = TRUE)
    expect_false(grepl("only annual", conditionMessage(err), fixed = TRUE))
  }
})
