test_that("full synthetic paths retain every independent historical hex pin", {
  pins <- utils::read.csv(test_path("fixtures", "variance-share-pins.csv"),
    colClasses = c(
      "character", "numeric", "character", "integer", "character",
      "character", "character"
    ), na.strings = character()
  )
  pins$exact <- suppressWarnings(as.numeric(pins$bits))
  scenarios <- list(
    synthetic_seed3 = c(.05, .2, .5),
    synthetic_seed26 = c(.05, .2, .5, .65), synthetic_seed34 = c(.05, .2)
  )
  for (scenario in names(scenarios)) {
    control <- VARIANCE_SHARE_CONTROL
    control$taus <- scenarios[[scenario]]
    seed <- as.integer(sub("synthetic_seed", "", scenario))
    prepared <- variance_share_fixture(seed)
    result <- compute_variance_shares(prepared, control)
    got <- variance_share_flatten(result, scenario)
    want <- pins[pins$scenario == scenario, ]
    expect_identical(got$field, want$field)
    expect_identical(got$row, want$row)
    expect_identical(got$tau, want$tau)
    repaired <- vapply(seq_len(nrow(got)), function(i) {
      got$row[i] %in% repaired_pin_rows(scenario, got$tau[i], got$field[i])
    }, logical(1))
    expect_identical(got$status[!repaired], want$status[!repaired])
    expect_oracle_equal(
      got$value[!repaired], want$exact[!repaired], ORACLE_TOLERANCE[["solver"]]
    )
    if (any(repaired)) {
      fixed <- got[repaired, ]
      expect_true(all(is.finite(fixed$value)))
      expect_true(all(fixed$status[fixed$field != "share_hi"] == "bounded"))
      expect_true(all(fixed$value[fixed$field == "share_lo"] <=
        fixed$value[fixed$field == "share_hi"]))
      upper <- !is.na(got$tau) & got$tau == 0.05 & got$field == "beta1_upper"
      expect_beta1_upper_within_outer(
        mean_profile_fixture(seed), 0.05, fixed$row[fixed$field == "beta1_upper"],
        got$value[upper]
      )
    }
    expect_identical(names(result), c(
      "rows", "ols", "point", "set_cols", "sets",
      "news_row", "combined_row", "sd_c", "n_obs", "taus"
    ))
    expect_identical(result$rows, c(
      "expected_block", colnames(prepared$x),
      "news_block", colnames(prepared$y2), "combined"
    ))
    expect_identical(result$news_row, 5L)
    expect_identical(result$combined_row, 9L)
    expect_identical(result$n_obs, 220L)
    expect_identical(names(result$sets), sprintf("%.17g", control$taus))
    expect_identical(names(result$set_cols), names(result$sets))
    expect_null(attr(result$sets, "profile_points"))
    expect_equal(sum(result$point[2:4]), result$point[1], tolerance = 1e-12)
    expect_equal(sum(result$point[6:8]), result$point[5], tolerance = 1e-12)
    for (cc in result$set_cols) {
      expect_true(all(cc$lo <= result$point + 1e-9 | is.na(cc$lo)))
      expect_true(all(result$point <= cc$hi + 1e-9 | is.na(cc$hi)))
    }
    if (seed == 3L) {
      control$taus <- rev(control$taus)
      prepared$variance <- rep(FALSE, length(prepared$y))
      prepared <- c(prepared, setNames(list(1, 2), c("extra", "extra")))
      reversed <- compute_variance_shares(prepared, control)
      expect_identical(reversed$set_cols, rev(result$set_cols))
      expect_identical(reversed$sets, rev(result$sets))
      expect_identical(reversed$point, result$point)
      expect_identical(reversed$taus, rev(result$taus))
    }
  }
})
