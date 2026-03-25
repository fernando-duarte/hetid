#' Format W2 residuals as a tidy data frame
#'
#' Internal helper that converts the list-based output of
#' \code{compute_w2_residuals} into a long-format data frame
#' with one row per maturity-date observation.
#'
#' @param residuals_list Named list of residual vectors,
#'   keyed by \code{"maturity_i"}.
#' @param fitted_list Named list of fitted-value vectors,
#'   keyed by \code{"maturity_i"}.
#' @param kept_idx_list Named list of logical vectors
#'   indicating which rows survived complete-cases filtering.
#' @param maturities Integer vector of maturities processed.
#' @param dates Optional user-supplied date vector.
#' @param bundled_dates Date vector shipped with bundled PCs
#'   (may be NULL).
#' @param n_yield_rows Integer, the number of rows in the
#'   original yields data frame.
#'
#' @return A data frame with columns \code{date},
#'   \code{maturity}, \code{residuals}, and \code{fitted}.
#'
#' @keywords internal
#' @noRd
format_w2_dataframe <- function(
  residuals_list, fitted_list, kept_idx_list,
  maturities, dates, bundled_dates, n_yield_rows
) {
  df_list <- list()

  # Track whether user supplied dates
  user_supplied_dates <- !is.null(dates)

  # Get dates from user, bundled data, or indices
  if (is.null(dates)) {
    if (!is.null(bundled_dates)) {
      dates <- bundled_dates
    } else {
      # User provided custom PCs -- use row indices
      # per package convention (prepare_return_data)
      dates <- seq_len(n_yield_rows - 1)
    }
  }

  # Validate user-supplied dates length
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

  # Build data frame for each maturity
  for (idx in seq_along(maturities)) {
    i <- maturities[idx]
    mat_key <- maturity_names(i)

    if (mat_key %in% names(residuals_list)) {
      kept <- kept_idx_list[[mat_key]]
      mat_dates <- dates[which(kept)]
      df_list[[idx]] <- data.frame(
        date = mat_dates,
        maturity = i,
        residuals = residuals_list[[mat_key]],
        fitted = fitted_list[[mat_key]],
        stringsAsFactors = FALSE
      )
    }
  }

  # Combine all maturities
  do.call(rbind, df_list)
}
