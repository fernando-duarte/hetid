#' Maturity Utilities
#'
#' Helpers for naming maturity-indexed objects.
#'
#' @name maturity_utils
#' @keywords internal
NULL

#' Build Maturity Names
#'
#' Generates a standard maturity label vector from indices.
#'
#' The indices are used as supplied, without validation. Missing values
#' become labels ending in \code{"NA"}.
#'
#' @param maturities Integer vector of maturity indices. In the identification
#'   layer, these are column positions in \code{w2}, not bond maturities in months.
#' @return A character vector of the same length and order as \code{maturities},
#'   with each index prefixed by \code{HETID_CONSTANTS$MATURITY_PREFIX}, such as
#'   \code{c("maturity_1", "maturity_2")}. Returns \code{character(0)} for a
#'   zero-length input.
#' @keywords internal
maturity_names <- function(maturities) {
  # paste0 against integer(0) returns "maturity_", not character(0)
  if (length(maturities) == 0L) {
    return(character(0))
  }
  paste0(HETID_CONSTANTS$MATURITY_PREFIX, maturities)
}

#' Effective Maximum Maturity for a News Step
#'
#' Largest maturity index usable by functions that need data at
#' maturity \code{i + step} (e.g. \code{\link{compute_n_hat}}).
#'
#' Maturity indices and \code{step} are measured in months. The step must be
#' a single finite numeric value with no fractional part, between \code{1} and
#' \code{HETID_CONSTANTS$MAX_MATURITY %/% 2}, inclusive. Missing values and
#' invalid steps raise a \code{hetid_error_bad_argument} condition.
#'
#' @template param-step
#' @return An integer scalar giving the maximum usable bond maturity in months,
#'   equal to \code{HETID_CONSTANTS$MAX_MATURITY - step}.
#' @examples
#' effective_max_maturity()
#' effective_max_maturity(step = 6)
#' @export
effective_max_maturity <- function(step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_step(step)
  HETID_CONSTANTS$MAX_MATURITY - as.integer(step)
}
