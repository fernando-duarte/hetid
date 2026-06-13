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
#' @param message Warning message string
#' @param subclass Specific warning subclass string
#' @param call The call (default NULL)
#' @keywords internal
warn_hetid <- function(message, subclass, call = NULL) {
  cnd <- warningCondition(
    message,
    class = c(subclass, "hetid_warning"),
    call = call
  )
  warning(cnd)
}

#' Signal a Degenerate Variance Warning
#'
#' Classed warning for the variance positivity diagnostic so callers
#' can dispatch on it (e.g. \code{withCallingHandlers} with class
#' \code{hetid_warning_degenerate_variance}).
#'
#' @param message Warning message string
#' @param call The call (default NULL)
#' @keywords internal
warn_degenerate_variance <- function(message, call = NULL) {
  warn_hetid(message, "hetid_warning_degenerate_variance", call = call)
}

#' Signal an Incomplete Quarter Warning
#'
#' Classed warning raised when quarterly conversion encounters quarters
#' whose last available observation is not in the terminal month, so
#' callers can dispatch on class
#' \code{hetid_warning_incomplete_quarter}.
#'
#' @param message Warning message string
#' @param call The call (default NULL)
#' @keywords internal
warn_incomplete_quarter <- function(message, call = NULL) {
  warn_hetid(message, "hetid_warning_incomplete_quarter", call = call)
}

#' Signal a Dropped NA-Date Warning
#'
#' Classed warning raised when quarterly conversion removes rows whose
#' date is NA (the monthly path keeps them, but the quarterly aggregate
#' would drop them silently), so callers can dispatch on class
#' \code{hetid_warning_dropped_na_dates}.
#'
#' @param message Warning message string
#' @param call The call (default NULL)
#' @keywords internal
warn_dropped_na_dates <- function(message, call = NULL) {
  warn_hetid(message, "hetid_warning_dropped_na_dates", call = call)
}

#' Signal an Unparsed-Dates Warning
#'
#' Classed warning raised when date parsing leaves some previously
#' non-NA values as NA, so callers can dispatch on class
#' \code{hetid_warning_unparsed_dates}.
#'
#' @param message Warning message string
#' @param call The call (default NULL)
#' @keywords internal
warn_unparsed_dates <- function(message, call = NULL) {
  warn_hetid(message, "hetid_warning_unparsed_dates", call = call)
}

#' Signal a Skipped-Maturity Warning
#'
#' Classed warning raised when a maturity is skipped during W2
#' processing (missing columns or too few observations), so callers can
#' dispatch on class \code{hetid_warning_skipped_maturity}.
#'
#' @param message Warning message string
#' @param call The call (default NULL)
#' @keywords internal
warn_skipped_maturity <- function(message, call = NULL) {
  warn_hetid(message, "hetid_warning_skipped_maturity", call = call)
}
