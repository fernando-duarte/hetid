{
  inp <- make_moments_inputs()
  moments <- compute_identification_moments(inp$w1, inp$w2, inp$pcs)
  stats <- unclass(moments)

  truncated <- stats
  truncated$s_i_0 <- truncated$s_i_0[1:3]
  expect_error(
    validate_hetid_moments(new_hetid_moments(truncated, 1:4, 4, 60)),
    class = "hetid_error_dimension_mismatch"
  )

  renamed <- stats
  names(renamed$sigma_i_sq) <- paste0("m", 1:4)
  expect_error(
    validate_hetid_moments(new_hetid_moments(renamed, 1:4, 4, 60)),
    "names must equal maturity_N",
    class = "hetid_error_bad_argument"
  )

  expect_error(
    new_hetid_moments(stats, c(1, 2, 3, 3), 4, 60),
    "duplicates",
    class = "hetid_error_bad_argument"
  )
}

{
  inp <- make_moments_inputs()
  moments <- compute_identification_moments(inp$w1, inp$w2, inp$pcs)
  stats <- unclass(moments)

  trimmed <- stats
  trimmed$r_i_1[[2]] <- trimmed$r_i_1[[2]][, 1:3]
  expect_error(
    validate_hetid_moments(new_hetid_moments(trimmed, 1:4, 4, 60)),
    "r_i_1 for maturity 2",
    class = "hetid_error_dimension_mismatch"
  )

  shortened <- stats
  shortened$s_i_1[[1]] <- shortened$s_i_1[[1]][1:2]
  expect_error(
    validate_hetid_moments(new_hetid_moments(shortened, 1:4, 4, 60)),
    "s_i_1 for maturity 1",
    class = "hetid_error_dimension_mismatch"
  )

  shrunk <- stats
  shrunk$s_i_2[[3]] <- shrunk$s_i_2[[3]][1:2, 1:2]
  expect_error(
    validate_hetid_moments(new_hetid_moments(shrunk, 1:4, 4, 60)),
    "s_i_2 for maturity 3",
    class = "hetid_error_dimension_mismatch"
  )
}

{
  inp <- make_moments_inputs()

  w1_na <- replace(inp$w1, 5, NA)
  expect_error(
    compute_identification_moments(w1_na, inp$w2, inp$pcs),
    "w1 must not contain NA, NaN, or infinite values",
    class = "hetid_error_bad_argument"
  )

  w2_na <- inp$w2
  w2_na[3, 2] <- NA
  expect_error(
    compute_identification_moments(inp$w1, w2_na, inp$pcs),
    "w2 must not contain NA, NaN, or infinite values",
    class = "hetid_error_bad_argument"
  )

  pcs_bad <- inp$pcs
  pcs_bad[7, 1] <- Inf
  expect_error(
    compute_identification_moments(inp$w1, inp$w2, pcs_bad),
    "pcs must not contain NA, NaN, or infinite values",
    class = "hetid_error_bad_argument"
  )
}
