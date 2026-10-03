{
  expect_error(
    assert_scalar_finite(Inf, "test_param"),
    "test_param must be a single finite numeric value"
  )
  expect_error(
    assert_scalar_finite(NA_real_, "test_param"),
    "test_param must be a single finite numeric value"
  )
  expect_error(
    assert_scalar_finite(NaN, "test_param"),
    "test_param must be a single finite numeric value"
  )
}
