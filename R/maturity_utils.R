#' Maturity Utilities
#'
#' Helpers for naming maturity-indexed objects.
#'
#' @name maturity_utils
#' @keywords internal
NULL

#' Build Maturity Names
#'
#' Generates standard maturity label vector from indices.
#'
#' @param maturities Integer vector of maturity indices
#' @return Character vector, e.g. c("maturity_1", "maturity_2")
#' @keywords internal
maturity_names <- function(maturities) {
  paste0(HETID_CONSTANTS$MATURITY_PREFIX, maturities)
}
