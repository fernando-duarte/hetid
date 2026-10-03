#' Validate a Dated Numeric Panel
#'
#' @param data A data frame with a Date column and numeric value columns.
#' @param arg The argument name stored in structured errors.
#' @return Invisible \code{TRUE} if valid.
#' @noRd
validate_sdf_dated_frame <- function(data, arg) {
  assert_bad_argument_ok(is.data.frame(data), paste0(arg, " must be a data frame"), arg)
  column_names <- names(data)
  assert_bad_argument_ok(
    !anyNA(column_names) && all(nzchar(column_names)) && !anyDuplicated(column_names) &&
      "date" %in% column_names && length(column_names) > 1L,
    paste0(arg, " must have unique column names, date, and numeric value columns"), arg
  )
  validate_dates_vector(data$date, nrow(data), paste0(arg, "$date"))
  values <- data[setdiff(column_names, "date")]
  assert_bad_argument_ok(
    all(vapply(values, function(x) is.numeric(x) && is.null(dim(x)), logical(1))),
    paste0(arg, " value columns must be numeric vectors"), arg
  )
  invisible(TRUE)
}

#' Validate Aligned SDF Panels and Their Horizons
#'
#' @inheritParams compute_sdf_panel
#' @return Invisible \code{TRUE} when dates and horizons satisfy the panel contract.
#' @noRd
validate_sdf_panel_inputs <- function(yields, term_premia, horizons, step, type, paired) {
  validate_step(step)
  assert_flag(paired, "paired")
  assert_bad_argument_ok(
    type != "news" || !paired, "paired = TRUE is only available for expected SDF", "paired"
  )
  validate_sdf_dated_frame(yields, "yields")
  validate_sdf_dated_frame(term_premia, "term_premia")
  validate_row_alignment(yields, term_premia)
  assert_insufficient_data_ok(nrow(yields) > 0L, "SDF panels must contain observations")
  assert_bad_argument_ok(
    identical(yields$date, term_premia$date),
    "yields and term_premia must have identical date keys", "term_premia"
  )
  dates <- yields$date
  assert_bad_argument_ok(
    !anyDuplicated(dates) && !is.unsorted(dates, strictly = TRUE),
    "SDF panel dates must be unique and strictly ordered", "dates"
  )
  date_frequency <- if (step == HETID_CONSTANTS$MONTHS_PER_QUARTER) {
    "quarterly"
  } else if (step == HETID_CONSTANTS$MONTHS_PER_YEAR) {
    "annual"
  } else {
    "monthly"
  }
  assert_bad_argument_ok(
    identical(dates, to_period_end(dates, date_frequency)),
    "SDF panel dates must be calendar period ends", "dates"
  )
  month_index <- as.integer(format(dates, HETID_CONSTANTS$YEAR_FORMAT)) *
    HETID_CONSTANTS$MONTHS_PER_YEAR +
    as.integer(format(dates, HETID_CONSTANTS$MONTH_FORMAT))
  assert_bad_argument_ok(
    all(diff(month_index) == step),
    "Consecutive SDF panel dates must be exactly step months apart", "dates"
  )
  validate_maturities(
    horizons, effective_max_maturity(step),
    arg = "horizons",
    min_value = if (type == "expected") 0L else HETID_CONSTANTS$MIN_MATURITY
  )
  if (type == "news") {
    assert_news_contract_ok(
      horizons, step, "horizons", "Horizons", "horizon",
      include_invalid = TRUE
    )
  } else if (paired) {
    for (i in horizons[horizons > 0L]) {
      validate_step_multiple(i, step, "paired expected SDF leads whole news periods")
    }
  }
  invisible(TRUE)
}
