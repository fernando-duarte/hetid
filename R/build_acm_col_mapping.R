#' Build a Raw ACM Column Name from a Month Maturity
#'
#' Single source of truth for the raw file's dual naming convention:
#' whole-year maturities keep the official padded-year names
#' (\code{ACMY01}..\code{ACMY10}), sub-annual months use the
#' three-digit month form (\code{ACMY003M}..\code{ACMY119M}).
#' Vectorized over \code{maturity_months}.
#'
#' @param data_type Schema key from \code{HETID_ACM_SCHEMA}
#' @param maturity_months Integer vector of maturities in months
#'
#' @return Character vector of raw column names
#' @keywords internal
acm_raw_column_name <- function(data_type, maturity_months) {
  if (!(data_type %in% names(HETID_ACM_SCHEMA))) {
    stop_bad_argument(
      paste0(
        "Unknown data type: '", data_type, "'. Must be one of: ",
        paste(names(HETID_ACM_SCHEMA), collapse = ", ")
      ),
      arg = "data_types"
    )
  }
  rule <- HETID_ACM_SCHEMA[[data_type]]
  m <- as.integer(maturity_months)
  ifelse(
    m %% 12L == 0L,
    sprintf(HETID_CONSTANTS$COL_FORMAT_PADDED, rule$prefix_old, m %/% 12L),
    sprintf(HETID_CONSTANTS$COL_FORMAT_MONTHLY, rule$prefix_old, m)
  )
}

#' Build Column Mapping for ACM Data
#'
#' Internal function to build mapping between raw and package column
#' names for the requested data types and maturities (months).
#'
#' @param data_types Character vector of data types
#' @param maturities Numeric vector of maturities in months
#'
#' @return List mapping new names to old names
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
  # do.call(c, unname(...)) flattens one level keeping the inner (new-name) keys
  # without prefixing them, even if data_types is ever passed as a named vector.
  do.call(c, unname(mappings))
}
