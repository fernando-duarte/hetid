sdf_input_fixture <- function() {
  dates <- as.Date(c("2022-03-31", "2022-06-30", "2022-09-30", "2022-12-31"))
  list(
    yields = data.frame(date = dates, y3 = 3:6, y6 = 4:7),
    term_premia = data.frame(date = dates, tp3 = rep(0.5, 4), tp6 = rep(0.8, 4))
  )
}

test_that("panel assembly rejects unequal or ambiguously ordered date keys", {
  x <- sdf_input_fixture()
  for (date in list(
    rev(x$yields$date), x$yields$date[c(1, 1, 3, 4)],
    as.Date(c("2022-03-31", "2022-09-30", "2022-12-31", "2023-03-31")),
    x$yields$date - 1L
  )) {
    y <- x$yields
    tp <- x$term_premia
    y$date <- date
    tp$date <- date
    expect_error(compute_sdf_panel(y, tp, 3, step = 3), class = "hetid_error_bad_argument")
  }
  tp <- x$term_premia
  tp$date <- rev(tp$date)
  expect_error(compute_sdf_panel(x$yields, tp, 3, step = 3), "identical date keys",
    class = "hetid_error_bad_argument"
  )
  expect_error(compute_sdf_panel(x$yields, tp[-1, ], 3, step = 3),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("invalid date types and malformed dated frames fail before computation", {
  x <- sdf_input_fixture()
  for (date in list(
    as.character(x$yields$date), as.numeric(x$yields$date),
    as.Date(c(NA, "2022-06-30", "2022-09-30", "2022-12-31")),
    structure(c(Inf, as.numeric(x$yields$date[-1])), class = "Date"),
    structure(as.numeric(x$yields$date), dim = c(4L, 1L), class = "Date")
  )) {
    y <- x$yields
    y$date <- date
    expect_error(compute_sdf_panel(y, x$term_premia, 3, step = 3),
      class = "hetid_error_bad_argument"
    )
  }
  y <- x$yields
  y$y3 <- as.character(y$y3)
  expect_error(compute_sdf_panel(y, x$term_premia, 3, step = 3),
    class = "hetid_error_bad_argument"
  )
  expect_error(compute_sdf_panel(x$yields[-1], x$term_premia, 3, step = 3),
    class = "hetid_error_bad_argument"
  )
  expect_error(compute_sdf_panel(as.matrix(x$yields[-1]), x$term_premia, 3, step = 3),
    class = "hetid_error_bad_argument"
  )
  expect_error(compute_sdf_panel(x$yields[0, ], x$term_premia[0, ], 3, step = 3),
    class = "hetid_error_insufficient_data"
  )
})

test_that("horizons and scalar choices retain structured failures", {
  x <- sdf_input_fixture()
  for (h in list(numeric(), c(3, 3), NA_real_, Inf, 3.5, -1, 118, matrix(3), "3")) {
    expect_error(compute_sdf_panel(x$yields, x$term_premia, h, step = 3),
      class = "hetid_error_bad_argument"
    )
  }
  expect_error(compute_sdf_panel(x$yields, x$term_premia, 0, step = 3, type = "news"),
    class = "hetid_error_bad_argument"
  )
  expect_error(compute_sdf_panel(x$yields, x$term_premia, 2, step = 3, type = "news"),
    class = "hetid_error_bad_argument"
  )
  expect_error(compute_sdf_panel(x$yields, x$term_premia, 3, step = 0),
    class = "hetid_error_bad_argument"
  )
  expect_error(compute_sdf_panel(x$yields, x$term_premia, 3, step = 3, type = "other"),
    class = "hetid_error_bad_argument"
  )
  expect_error(compute_sdf_panel(x$yields, x$term_premia, 3, step = 3, paired = NA),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_sdf_panel(x$yields[1, ], x$term_premia[1, ], 3,
      step = 3, type = "news"
    ),
    class = "hetid_error_insufficient_data"
  )
  expect_error(compute_sdf_panel(x$yields["date"], x$term_premia, 3, step = 3),
    class = "hetid_error_bad_argument"
  )
  expect_error(compute_sdf_panel(x$yields[c("date", "y3")], x$term_premia, 3, step = 3),
    "y6",
    class = "hetid_error_bad_argument"
  )
})
