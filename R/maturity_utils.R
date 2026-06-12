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

#' Effective Maximum Maturity for a News Step
#'
#' Largest maturity index usable by functions that need data at
#' maturity \code{i + step} (e.g. \code{compute_n_hat}). Replaces the
#' former fixed \code{EFFECTIVE_MAX_MATURITY} constant with a
#' step-dependent bound.
#'
#' @template param-step
#' @return Integer scalar, \code{MAX_MATURITY - step}
#' @export
effective_max_maturity <- function(step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_step(step)
  HETID_CONSTANTS$MAX_MATURITY - as.integer(step)
}
