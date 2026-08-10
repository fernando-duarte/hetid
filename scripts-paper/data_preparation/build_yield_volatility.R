# Realized quarterly volatility of bond yields: within each quarter, the
# square root of the sum of squared business-day yield changes (realized
# volatility), one column per ACM maturity (y1_vol .. y120_vol), in the
# yields' percentage-point units, not annualized. Also adds y60_vol_log
# (the natural log of the five-year column) and y60_ret_vol (the realized
# volatility of the duration-approximated one-day return on the five-year
# bond), both alternate instrument choices in config/instrument_choices.R.
# Run via run_pipeline.R, which defines the shared maturity grids. The daily ACM
# asset (~40 MB) is cache-only; the first run downloads it from GitHub.

# The asset is pinned by release tag and digest (config/analysis.R) and verified
# by paper_verify_frozen_inputs() before the run creates or cleans anything, so
# the extract below finds a known-good cache and the contract's auto_download
# never reaches the network.

yield_volatility_input <-
  PAPER_ANALYSIS_CONTRACT$input$yield_volatility
acm_daily <- hetid::extract_acm_data(
  data_types = yield_volatility_input$data_types,
  maturities = all_mats,
  frequency = yield_volatility_input$frequency,
  auto_download = yield_volatility_input$auto_download,
  source = yield_volatility_input$source
)

yield_vol <- acm_daily |>
  dplyr::mutate(
    qtr = tsibble::yearquarter(date),
    dplyr::across(dplyr::starts_with("y"), \(y) (y - dplyr::lag(y))^2)
  ) |>
  dplyr::summarise(
    # sum(NA, na.rm = TRUE) is 0, which would enter the instrument as a
    # legitimate extreme-low volatility instead of a missing quarter
    dplyr::across(
      dplyr::starts_with("y"),
      \(d2) if (all(is.na(d2))) NA_real_ else sqrt(sum(d2, na.rm = TRUE))
    ),
    .by = qtr
  ) |>
  dplyr::rename_with(\(x) paste0(x, PAPER_YIELD_VOL_SUFFIX), !qtr)

# the log-transformed five-year column, for the y60_vol_log instrument
# choice (config/instrument_choices.R). log(0) is -Inf, not NA, so it would
# survive drop_na() downstream and silently corrupt the demeaning; assert
# the precondition instead of guessing a floor.
stopifnot(
  "y60_vol must be positive wherever observed for its log to be defined" =
    all(yield_vol$y60_vol[!is.na(yield_vol$y60_vol)] > 0)
)
yield_vol$y60_vol_log <- log(yield_vol$y60_vol)

# realized volatility of the duration-approximated one-day return on the
# five-year bond, for the y60_ret_vol instrument choice
# (config/instrument_choices.R). The exact one-day holding-period return
# r_t = p_t^(n-1) - p_{t-1}^(n) needs a yield at "n minus one day," which
# is not on this maturity-monthly grid; a first-order Taylor expansion
# around the fixed five-year point gives the standard duration
# approximation instead: r_t ~= y_{t-1}/tdays - duration*(y_t - y_{t-1}),
# a one-day carry term minus duration (= maturity, for a zero-coupon bond)
# times the yield change already used for y60_vol. Computed from the raw
# acm_daily level (not yield_vol, which has already squared and summed
# the changes) and joined back on qtr, never row position.
trading_days_per_year <- 252
y60_duration_years <- 60L / hetid::HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
y60_ret_vol <- acm_daily |>
  dplyr::transmute(
    qtr = tsibble::yearquarter(date),
    ret = dplyr::lag(y60) / trading_days_per_year -
      y60_duration_years * (y60 - dplyr::lag(y60))
  ) |>
  dplyr::summarise(
    y60_ret_vol = if (all(is.na(ret))) NA_real_ else sqrt(sum(ret^2, na.rm = TRUE)),
    .by = qtr
  )
stopifnot(
  "y60_ret_vol must cover exactly yield_vol's quarters, no more, no fewer" =
    setequal(y60_ret_vol$qtr, yield_vol$qtr)
)
yield_vol <- dplyr::left_join(yield_vol, y60_ret_vol, by = "qtr")

rm(acm_daily, yield_volatility_input, trading_days_per_year, y60_duration_years, y60_ret_vol)
