# Canonical validated quarterly ACM inputs used by paper computations.

paper_load_quarterly_acm <- function(
  maturities,
  contract = PAPER_ANALYSIS_CONTRACT$input$acm
) {
  stopifnot(
    is.numeric(maturities),
    length(maturities) > 0L,
    !anyNA(maturities),
    !anyDuplicated(maturities)
  )
  acm <- hetid::extract_acm_data(
    data_types = contract$data_types,
    maturities = maturities,
    frequency = contract$frequency,
    auto_download = contract$auto_download,
    source = contract$source
  )
  yield_cols <- hetid::acm_column_name("yields", maturities)
  term_premium_cols <- hetid::acm_column_name("term_premia", maturities)
  required <- c("date", yield_cols, term_premium_cols)
  missing <- setdiff(required, names(acm))
  if (length(missing)) {
    stop(
      sprintf(
        "Quarterly ACM input is missing: %s",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  dates <- as.Date(acm$date)
  if (anyNA(dates) || anyDuplicated(dates) || is.unsorted(dates)) {
    stop(
      "Quarterly ACM dates must be non-missing, unique, and sorted",
      call. = FALSE
    )
  }
  list(
    data = acm,
    dates = dates,
    yields = as.matrix(acm[yield_cols]),
    term_premia = as.matrix(acm[term_premium_cols]),
    maturities = maturities,
    contract = contract
  )
}
