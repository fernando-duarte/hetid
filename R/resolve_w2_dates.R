#' Resolve the W2 observation date index
#'
#' Internal helper shared by \code{compute_w2_residuals} (list mode) and
#' \code{format_w2_dataframe} (data-frame mode) so both derive the W2 date
#' index identically. Resolution order: user-supplied \code{dates}, else the
#' bundled-PC dates, else row indices. A user-supplied vector is validated to
#' have \code{nrow(yields) - 1} elements (W2 is a one-step news series, so it
#' carries one fewer observation than the yield panel).
#'
#' @param dates Optional user-supplied date vector (may be NULL).
#' @param bundled_dates Date vector shipped with bundled PCs (may be NULL).
#' @param n_yield_rows Integer, the number of rows in the yields data frame.
#'
#' @return A vector (dates or row indices) indexing the W2 observations, before
#'   any per-maturity complete-cases subsetting.
#'
#' @keywords internal
#' @noRd
resolve_w2_dates <- function(dates, bundled_dates, n_yield_rows) {
  user_supplied_dates <- !is.null(dates)

  if (is.null(dates)) {
    if (!is.null(bundled_dates)) {
      dates <- bundled_dates
    } else {
      # User provided custom PCs -- use row indices
      # per package convention (prepare_return_data)
      dates <- seq_len(n_yield_rows - 1)
    }
  }

  if (user_supplied_dates) {
    expected_len <- n_yield_rows - 1
    assert_dimension_ok(
      length(dates) == expected_len,
      paste0(
        "dates has ", length(dates),
        " elements but nrow(yields) - 1 = ",
        expected_len
      )
    )
  }

  dates
}
