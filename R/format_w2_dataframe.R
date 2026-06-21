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
#' @param dates The already-resolved t+1 realization \code{Date} index (length
#'   \code{nrow(yields) - 1}), as produced by \code{resolve_w2_dates}.
#'
#' @return A data frame with columns \code{date},
#'   \code{maturity}, \code{residuals}, and \code{fitted}.
#'
#' @keywords internal
#' @noRd
format_w2_dataframe <- function(
  residuals_list, fitted_list, kept_idx_list,
  maturities, dates
) {
  df_list <- list()

  # `dates` is the already-resolved t+1 realization index (length T-1), shared
  # with the list-mode return of compute_w2_residuals.

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

  # Combine all maturities; when every maturity was skipped,
  # return a zero-row frame with the documented columns instead
  # of the NULL that rbind on an empty list produces
  combined <- do.call(rbind, df_list)
  if (is.null(combined)) {
    combined <- data.frame(
      date = dates[0],
      maturity = maturities[0],
      residuals = numeric(0),
      fitted = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  combined
}
