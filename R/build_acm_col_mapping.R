#' Build Column Mapping for ACM Data
#'
#' Internal function to build mapping between old and new column names
#'
#' @param data_types Character vector of data types
#' @param maturities Numeric vector of maturities
#'
#' @return List mapping new names to old names
#' @keywords internal
build_acm_col_mapping <- function(data_types, maturities) {
  col_mapping <- list()

  # Build mapping for each data type
  for (dtype in data_types) {
    if (dtype %in% names(HETID_ACM_SCHEMA)) {
      rule <- HETID_ACM_SCHEMA[[dtype]]
      for (mat in maturities) {
        old_col <- sprintf(HETID_CONSTANTS$COL_FORMAT_PADDED, rule$prefix_old, mat)
        new_col <- sprintf(HETID_CONSTANTS$COL_FORMAT_SIMPLE, rule$prefix_new, mat)
        col_mapping[[new_col]] <- old_col
      }
    }
  }

  col_mapping
}
