#' Build Column Mapping for ACM Data
#'
#' Internal function to build mapping between old and new column names
#'
#' @param data_types Character vector of data types
#' @param maturities Numeric vector of maturities
#'
#' @return List mapping new names to old names
#' @importFrom stats setNames
#' @keywords internal
build_acm_col_mapping <- function(data_types, maturities) {
  mappings <- lapply(data_types, function(dtype) {
    if (!(dtype %in% names(HETID_ACM_SCHEMA))) {
      return(NULL)
    }
    rule <- HETID_ACM_SCHEMA[[dtype]]
    old_cols <- sprintf(
      HETID_CONSTANTS$COL_FORMAT_PADDED,
      rule$prefix_old, maturities
    )
    new_cols <- sprintf(
      HETID_CONSTANTS$COL_FORMAT_SIMPLE,
      rule$prefix_new, maturities
    )
    setNames(as.list(old_cols), new_cols)
  })
  unlist(mappings, recursive = FALSE)
}
