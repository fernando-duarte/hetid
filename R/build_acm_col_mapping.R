#' Build a Raw ACM Column Name from a Month Maturity
#'
#' Single source of truth for the raw file's dual naming convention:
#' whole-year maturities keep the official padded-year names
#' (\code{ACMY01}..\code{ACMY10}), maturities that are not whole years use the
#' three-digit month form (\code{ACMY001M}..\code{ACMY119M}).
#' Vectorized over \code{maturity_months}.
#'
#' Maturities are coerced to integers but are not checked against the
#' available maturity grid. Missing maturities are retained as missing names.
#' Unknown data types raise a \code{hetid_error_bad_argument} condition.
#'
#' @param data_type Character scalar schema key: \code{"yields"},
#'   \code{"term_premia"}, or \code{"risk_neutral_yields"}.
#' @param maturity_months Integer vector of maturities in months.
#'
#' @return A character vector of raw column names in maturity order, with
#'   \code{NA_character_} for missing maturities. An empty maturity vector
#'   returns \code{logical(0)}.
#' @keywords internal
acm_raw_column_name <- function(data_type, maturity_months) {
  assert_acm_data_type(data_type, arg = "data_types")
  rule <- HETID_ACM_SCHEMA[[data_type]]
  m <- as.integer(maturity_months)
  ifelse(
    m %% HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR == 0L,
    sprintf(
      HETID_CONSTANTS$COL_FORMAT_PADDED, rule$prefix_old,
      m %/% HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
    ),
    sprintf(HETID_CONSTANTS$COL_FORMAT_MONTHLY, rule$prefix_old, m)
  )
}

#' Build Column Mapping for ACM Data
#'
#' Internal function to build mapping between raw and package column
#' names for the requested data types and maturities (months).
#'
#' Maturity validation is the caller's responsibility. Missing maturities
#' produce missing raw names with package keys ending in \code{NA}.
#' Unknown data types raise a \code{hetid_error_bad_argument} condition.
#'
#' @param data_types Character vector of schema keys: \code{"yields"},
#'   \code{"term_premia"}, or \code{"risk_neutral_yields"}.
#' @param maturities Numeric vector of integer-valued maturities in months.
#'
#' @return A named list of scalar raw column names, keyed by package column
#'   names (for example, \code{y12} maps to \code{"ACMY01"}). Entries follow
#'   data-type order, then maturity order within each type; names on
#'   \code{data_types} do not prefix the keys. Empty maturities yield an empty
#'   list; empty \code{data_types} yields \code{NULL}.
#' @importFrom stats setNames
#' @keywords internal
build_acm_col_mapping <- function(data_types, maturities) {
  mappings <- lapply(data_types, function(dtype) {
    old_cols <- acm_raw_column_name(dtype, maturities)
    new_cols <- sprintf(
      HETID_CONSTANTS$COL_FORMAT_SIMPLE,
      HETID_ACM_SCHEMA[[dtype]]$prefix_new, maturities
    )
    setNames(as.list(old_cols), new_cols)
  })
  do.call(c, unname(mappings))
}
