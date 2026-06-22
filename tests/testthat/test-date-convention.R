# Canonical regression lock for the date convention (presence + filtration
# alignment) across the whole time-series surface; per-function tests do details.

acm <- extract_acm_data(data_types = c("yields", "term_premia"))
yld <- acm[, paste0("y", seq(12, 120, 12))]
tpm <- acm[, paste0("tp", seq(12, 120, 12))]
dts <- acm$date
n <- nrow(yld)

# The four SDF time-series functions and a representative call for each.
sdf_calls <- list(
  compute_n_hat = function(dates) compute_n_hat(yld, tpm, i = 60, dates = dates),
  compute_expected_sdf = function(dates) {
    compute_expected_sdf(yld, tpm, i = 60, dates = dates)
  },
  compute_price_news = function(dates) {
    compute_price_news(yld, tpm, i = 60, dates = dates)
  },
  compute_sdf_innovations = function(dates) {
    compute_sdf_innovations(yld, tpm, i = 60, dates = dates)
  }
)

test_that("date presence: SDF series cannot be emitted without a real Date index", {
  for (nm in names(sdf_calls)) {
    f <- sdf_calls[[nm]]
    expect_error(f(NULL), class = "hetid_error_bad_argument", info = nm)
    # fabricated integer index (the old fallback) is rejected
    expect_error(f(seq_len(n)), class = "hetid_error_bad_argument", info = nm)
    # character dates are rejected (no silent coercion)
    expect_error(
      f(as.character(dts)),
      class = "hetid_error_bad_argument", info = nm
    )
    expect_error(f(dts[-1]), class = "hetid_error_dimension_mismatch", info = nm)
    # valid: a dated data frame whose first column is a Date named `date`
    out <- f(dts)
    expect_s3_class(out, "data.frame")
    expect_identical(names(out)[1], "date", info = nm)
    expect_s3_class(out$date, "Date")
    expect_identical(out$date, dts, info = nm)
  }
})

test_that("migration guard: an old positional return_df=TRUE call now errors", {
  # The 4th positional slot of compute_n_hat is now `dates`; the legacy
  # `compute_n_hat(y, tp, i, TRUE)` binds TRUE into dates and is rejected.
  expect_error(
    compute_n_hat(yld, tpm, 60, TRUE),
    class = "hetid_error_bad_argument"
  )
})

test_that("filtration alignment: levels are dated at t, news at t+1", {
  # Level: n_hat(i, t) is F_t-measurable -> dated t (1:1 with the input dates).
  nh <- compute_n_hat(yld, tpm, i = 60, dates = dts)
  expect_identical(nh$date, dts)
  expect_equal(nh$n_hat, n_hat_series(yld, tpm, i = 60))

  # News: delta_{t+1} p is realized at t+1 -> the (T-1) news values are
  # NA-prepended so value k sits on date[k+1], and row 1 is NA.
  pn <- compute_price_news(yld, tpm, i = 60, dates = dts)
  expect_identical(pn$date, dts)
  expect_true(is.na(pn$price_news[1]))
  expect_equal(
    pn$price_news[-1],
    compute_news_components(yld, tpm, i = 60)$delta_p
  )

  si <- compute_sdf_innovations(yld, tpm, i = 60, dates = dts)
  expect_true(is.na(si$sdf_innovations[1]))
  expect_equal(si$sdf_innovations[-1], sdf_innovations_series(yld, tpm, i = 60))
})

test_that("presence + filtration: W1/W2 residuals carry their t+1 realization dates", {
  data("variables", package = "hetid")
  mats <- c(12, 24, 36, 48)
  # suppressWarnings: the quarterly extract emits an incomplete-quarter notice
  # for the trailing partial quarter -- a data property, not part of this test.
  acm_q <- suppressWarnings(extract_acm_data(
    data_types = c("yields", "term_premia"),
    maturities = mats, frequency = "quarterly"
  ))
  merged <- merge(
    variables[, c("date", paste0("pc", 1:4))],
    acm_q[, c("date", paste0("y", mats), paste0("tp", mats))],
    by = "date"
  )

  # W1: data must carry a date column; residual W_{1,t+1} is dated at t+1.
  w1 <- compute_w1_residuals(n_pcs = 4, data = variables)
  expect_s3_class(w1$dates, "Date")
  expect_false(anyNA(w1$dates))
  expect_length(w1$dates, length(w1$residuals))
  expect_true(all(w1$dates %in% variables$date[-1])) # lead (t+1) dates only

  # missing date column is an error (cannot emit a dateless series)
  expect_error(
    compute_w1_residuals(n_pcs = 4, data = variables[, setdiff(names(variables), "date")]),
    class = "hetid_error"
  )

  # W2: dates are full-T (one per yield row); residual W_{2,t+1} dated at t+1.
  w2 <- compute_w2_residuals(
    merged[, paste0("y", mats)], merged[, paste0("tp", mats)],
    maturities = c(24, 36), n_pcs = 4,
    pcs = as.matrix(merged[, paste0("pc", 1:4)]),
    dates = merged$date
  )
  for (k in names(w2$dates)) {
    expect_s3_class(w2$dates[[k]], "Date")
    expect_true(all(w2$dates[[k]] %in% merged$date[-1])) # lead (t+1) dates only
  }

  # missing dates is an error
  expect_error(
    compute_w2_residuals(
      merged[, paste0("y", mats)], merged[, paste0("tp", mats)],
      maturities = c(24, 36), n_pcs = 4,
      pcs = as.matrix(merged[, paste0("pc", 1:4)])
    ),
    class = "hetid_error_bad_argument"
  )
})
