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
#' @param dates_list Named list of per-maturity t+1 realization \code{Date}
#'   vectors (the resolved index subset by each maturity's \code{kept_idx}),
#'   keyed by \code{"maturity_i"} and parallel to \code{residuals_list}.
#' @param maturities Integer vector of maturities processed.
#'
#' @return A data frame with columns \code{date},
#'   \code{maturity}, \code{residuals}, and \code{fitted}.
#'
#' @keywords internal
#' @noRd
format_w2_dataframe <- function(
  residuals_list, fitted_list, dates_list, maturities
) {
  df_list <- list()

  for (idx in seq_along(maturities)) {
    i <- maturities[idx]
    mat_key <- maturity_names(i)

    if (mat_key %in% names(residuals_list)) {
      df_list[[idx]] <- data.frame(
        date = dates_list[[mat_key]],
        maturity = i,
        residuals = residuals_list[[mat_key]],
        fitted = fitted_list[[mat_key]],
        stringsAsFactors = FALSE
      )
    }
  }

  # rbind on an empty list returns NULL; return a typed zero-row frame instead
  combined <- do.call(rbind, df_list)
  if (is.null(combined)) {
    combined <- data.frame(
      date = as.Date(character(0)),
      maturity = maturities[0],
      residuals = numeric(0),
      fitted = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  combined
}
