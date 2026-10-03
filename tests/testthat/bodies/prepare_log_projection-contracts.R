{
  fx <- lp_fixture()
  shifted <- fx
  shifted$w2[, 1] <- shifted$w2[, 1] + 1
  prep <- prepare_log_projection(
    shifted$w1, shifted$w2, shifted$x_var, shifted$mean_ids, shifted$vol_ids,
    impose_null = TRUE
  )
  expect_identical(prep$w2_mean, shifted$w2)
  expect_identical(evaluate_log_projection(prep, fx$b, "log_fuller")$status, "ok")
  expect_identical(
    unclass(prepare_log_projection(
      fx$w1, fx$w2, fx$x_var, fx$mean_ids, fx$vol_ids,
      impose_null = TRUE
    )),
    unclass(lp_prep(fx))
  )
  bad_w1 <- shifted
  bad_w1$w1 <- bad_w1$w1 + 1
  expect_error(
    prepare_log_projection(
      bad_w1$w1, bad_w1$w2, bad_w1$x_var, bad_w1$mean_ids, bad_w1$vol_ids,
      impose_null = TRUE
    ),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    prepare_log_projection(
      fx$w1, fx$w2, fx$x_var, fx$mean_ids, fx$vol_ids,
      impose_null = NA
    ),
    class = "hetid_error_bad_argument"
  )
}
