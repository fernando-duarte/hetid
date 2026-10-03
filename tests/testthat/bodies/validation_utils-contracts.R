{
  expect_error(
    validate_maturity_index("a"), "single finite numeric"
  )
  expect_error(
    validate_maturity_index(NA), "single finite numeric"
  )
  expect_error(
    validate_maturity_index(Inf), "single finite numeric"
  )
  expect_error(
    validate_maturity_index(c(1, 2)), "single finite numeric"
  )
  expect_error(
    validate_maturity_index(NULL), "single finite numeric"
  )
}

{
  expect_error(
    validate_n_pcs(0),
    "n_pcs must be between 1 and"
  )
  expect_error(
    validate_n_pcs(-1),
    "n_pcs must be between 1 and"
  )
  expect_error(
    validate_n_pcs(7),
    "n_pcs must be between 1 and 6"
  )
}

{
  expect_error(
    validate_n_pcs("a"),
    "single finite numeric"
  )
  expect_error(
    validate_n_pcs(NA),
    "single finite numeric"
  )
  expect_error(
    validate_n_pcs(Inf),
    "single finite numeric"
  )
  expect_error(
    validate_n_pcs(c(1, 2)),
    "single finite numeric"
  )
  expect_error(
    validate_n_pcs(NULL),
    "single finite numeric"
  )
}

{
  expect_error(
    validate_numeric_inputs(x = "text"),
    "x must be a numeric vector"
  )
  expect_error(
    validate_numeric_inputs(
      good = c(1, 2), bad = "text"
    ),
    "bad must be a numeric vector"
  )
}

{
  expect_error(
    validate_numeric_inputs("text"),
    "input_1 must be a numeric vector"
  )
  expect_error(
    validate_numeric_inputs(c(1, 2), "text"),
    "input_2 must be a numeric vector"
  )
}

{
  expect_error(
    validate_numeric_inputs(x = c(1, 2), "text"),
    "input_2 must be a numeric vector"
  )
  expect_error(
    validate_numeric_inputs("text", y = c(1, 2)),
    "input_1 must be a numeric vector"
  )
  expect_error(
    validate_numeric_inputs(c(1, 2), bad = "text"),
    "bad must be a numeric vector"
  )
}

{
  expect_true(
    validate_time_series_lengths(
      c(1, 2, 3), c(4, 5, 6)
    )
  )
  expect_true(
    validate_time_series_lengths(
      c(1, 2), c(3, 4), c(5, 6)
    )
  )
}

{
  expect_error(
    validate_time_series_lengths(
      c(1, 2, 3), c(4, 5)
    ),
    "same length"
  )
  expect_error(
    validate_time_series_lengths(
      c(1, 2, 3), c(4, 5)
    ),
    "Got lengths: 3, 2"
  )
}

{
  expect_error(
    validate_time_series_lengths(c(1, 2, 3)),
    "At least two inputs required"
  )
  expect_error(
    validate_time_series_lengths(),
    "At least two inputs required"
  )
}

{
  expect_true(
    validate_time_series_lengths(
      integer(0), integer(0)
    )
  )
  expect_error(
    validate_time_series_lengths(
      integer(0), c(1, 2)
    ),
    "same length"
  )
}

{
  y <- matrix(1:20, nrow = 10, ncol = 2)
  tp <- matrix(1:10, nrow = 5, ncol = 2)
  expect_error(
    validate_row_alignment(y, tp),
    "same number of observations",
    class = "hetid_error_dimension_mismatch"
  )
  expect_error(
    validate_row_alignment(y, tp),
    "10 vs 5 rows"
  )
}

{
  y <- matrix(1:20, nrow = 10, ncol = 2)
  tp <- matrix(1:14, nrow = 7, ncol = 2)
  expect_error(
    validate_data_dimensions(y, tp),
    "same number of observations"
  )
  expect_error(
    validate_data_dimensions(y, tp),
    "10 vs 7 rows"
  )
}

{
  y <- matrix(1:30, nrow = 10, ncol = 3)
  tp <- matrix(1:20, nrow = 10, ncol = 2)
  expect_error(
    validate_data_dimensions(y, tp),
    "same number of maturities"
  )
  expect_error(
    validate_data_dimensions(y, tp),
    "3 vs 2 columns"
  )
}
