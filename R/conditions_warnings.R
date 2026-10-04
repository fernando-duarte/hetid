#' Custom Warning Conditions for hetid
#'
#' Classed warning constructors mirroring the error constructors in
#' \code{\link{conditions}}, so callers can \code{withCallingHandlers}/
#' \code{tryCatch}-dispatch on recoverable states by class. Every
#' constructor carries a specific subclass and the shared
#' \code{hetid_warning} parent.
#'
#' @name conditions_warnings
#' @keywords internal
NULL

#' Signal a Classed hetid Warning
#'
#' Generic helper behind the specific \code{warn_*} constructors:
#' raises a \code{warningCondition} whose class vector is the given
#' subclass followed by the shared \code{hetid_warning} parent.
#'
#' @details Warning display and conversion to an error follow
#'   \code{\link[base:warning]{warning}} and the \code{warn} option.
#'
#' @param message A character string containing the warning message.
#' @param subclass A character string naming the specific warning subclass.
#' @param call A call object to include in the condition, or \code{NULL} (the
#'   default) to omit the call.
#' @return The warning message is returned invisibly as a character string.
#'   The function is called for its warning side effect.
#' @keywords internal
warn_hetid <- function(message, subclass, call = NULL) {
  cnd <- warningCondition(
    message,
    class = c(subclass, "hetid_warning"),
    call = call
  )
  warning(cnd)
}

#' Signal a Horizon-Zero Expected-SDF Warning
#'
#' Classed warning raised when \code{\link{compute_expected_sdf}} or
#' \code{\link{compute_expected_sdf_variance_bound}} is called with \code{i = 0}: the
#' horizon-zero expected SDF is the realized one-period price, returned exactly, and its
#' approximation-error variance bound is identically zero. Callers can dispatch on class
#' \code{hetid_warning_horizon_zero}.
#'
#' @param message A character string containing the warning message.
#' @param call A call object to include in the condition, or \code{NULL} (the
#'   default) to omit the call.
#' @return The warning message is returned invisibly as a character string.
#'   The function is called for its warning side effect.
#' @keywords internal
warn_horizon_zero <- function(message, call = NULL) {
  warn_hetid(message, "hetid_warning_horizon_zero", call = call)
}
