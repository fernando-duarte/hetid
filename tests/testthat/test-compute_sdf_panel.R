sdf_panel_fixture <- function() {
  dates <- as.Date(c(
    "2022-03-31", "2022-06-30", "2022-09-30", "2022-12-31",
    "2023-03-31", "2023-06-30", "2023-09-30", "2023-12-31"
  ))
  yields <- as.data.frame(outer(seq_along(dates), 1:12, function(t, m) 3 + t / 7 + m / 13))
  tp <- as.data.frame(outer(seq_along(dates), 1:12, function(t, m) t / 21 + m / 31))
  names(yields) <- paste0("y", 1:12)
  names(tp) <- paste0("tp", 1:12)
  list(yields = cbind(date = dates, yields), term_premia = cbind(date = dates, tp))
}

test_that("expected panels preserve explicit horizon order and zero's exact price", {
  x <- sdf_panel_fixture()
  horizons <- c(long = 6L, now = 0L, short = 3L)
  expect_warning(result <- compute_sdf_panel(x$yields, x$term_premia, horizons, step = 3L),
    class = "hetid_warning_horizon_zero"
  )
  expect_identical(names(result), c("data", "horizons", "step", "type", "paired"))
  expect_identical(names(result$data), c(
    "date", "expected_sdf_m6", "expected_sdf_m0",
    "expected_sdf_m3"
  ))
  expect_identical(result$data$date, x$yields$date)
  expect_identical(result$horizons, unname(horizons))
  expect_identical(result$step, 3L)
  expect_identical(result$type, "expected")
  expect_false(result$paired)
  expect_identical(result$data$expected_sdf_m0, exp(-(3 / 12) * x$yields$y3 / 100))
  for (i in c(6L, 3L)) {
    expected <- compute_expected_sdf(x$yields[-1], x$term_premia[-1], i,
      x$yields$date,
      step = 3L
    )
    expect_identical(result$data[[paste0("expected_sdf_m", i)]], expected$expected_sdf)
  }
})

test_that("news accepts nonmultiple horizons and keeps realization dates and missing masks", {
  x <- sdf_panel_fixture()
  x$yields$y7[4] <- NA_real_
  result <- compute_sdf_panel(x$yields, x$term_premia, c(4L, 3L), step = 3L, type = "news")
  expect_identical(names(result$data), c("date", "sdf_news_m4", "sdf_news_m3"))
  expect_identical(result$data$date, x$yields$date)
  expect_equal(nrow(result$data), 8L)
  for (i in c(4L, 3L)) {
    expected <- compute_sdf_innovations(x$yields[-1], x$term_premia[-1], i,
      x$yields$date,
      step = 3L
    )
    expect_identical(result$data[[paste0("sdf_news_m", i)]], expected$sdf_innovations)
    expect_true(is.na(result$data[[paste0("sdf_news_m", i)]][1]))
  }
  expect_false(identical(is.na(result$data$sdf_news_m4), is.na(result$data$sdf_news_m3)))
})

test_that("paired expected panels retain the estimator choice and horizon restriction", {
  x <- sdf_panel_fixture()
  paired <- compute_sdf_panel(x$yields, x$term_premia, c(3L, 6L), step = 3L, paired = TRUE)
  expect_true(paired$paired)
  expect_warning(zero <- compute_sdf_panel(x$yields, x$term_premia, 0L,
    step = 3L, paired = TRUE
  ), class = "hetid_warning_horizon_zero")
  expect_true(zero$paired)
  expect_identical(zero$data$expected_sdf_m0, exp(-(3 / 12) * x$yields$y3 / 100))
  for (i in c(3L, 6L)) {
    expected <- compute_expected_sdf(x$yields[-1], x$term_premia[-1], i,
      x$yields$date,
      step = 3L, paired = TRUE
    )
    expect_identical(paired$data[[paste0("expected_sdf_m", i)]], expected$expected_sdf)
  }
  expect_error(compute_sdf_panel(x$yields, x$term_premia, 4L, step = 3L, paired = TRUE),
    "multiple of step",
    class = "hetid_error_bad_argument"
  )
  unpaired <- compute_sdf_panel(x$yields, x$term_premia, 4L, step = 3L)
  expect_identical(unpaired$horizons, 4L)
  expect_error(
    compute_sdf_panel(x$yields, x$term_premia, 3L,
      step = 3L,
      type = "news", paired = TRUE
    ),
    class = "hetid_error_bad_argument"
  )
})
