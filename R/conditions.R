#' Custom Condition Classes for hetid
#'
#' Structured condition constructors for programmatic error
#' handling. Enables tryCatch with class-based dispatch.
#'
#' @name conditions
#' @keywords internal
NULL

#' Construct a Structured hetid Condition
#'
#' Single source of the \code{hetid_error} class vector and condition
#' layout shared by every \code{stop_*} constructor (the error-side
#' mirror of \code{warn_hetid}). \code{subclass} prepends the specific
#' error class; \code{...} carries any extra condition fields (e.g.
#' \code{arg}).
#'
#' @param message Error message string
#' @param subclass Optional specific condition class, prepended
#' @param call The call (default NULL)
#' @param ... Extra named fields stored on the condition
#' @return A condition object (not signalled)
#' @keywords internal
new_hetid_error <- function(message, subclass = NULL, call = NULL, ...) {
  structure(
    class = c(subclass, "hetid_error", "error", "condition"),
    list(message = message, call = call, ...)
  )
}

#' Signal a Bad Argument Error
#'
#' @param message Error message string
#' @param arg Argument name (optional)
#' @param call The call (default NULL)
#' @keywords internal
stop_bad_argument <- function(message, arg = NULL,
                              call = NULL) {
  stop(new_hetid_error(
    message, "hetid_error_bad_argument", call,
    arg = arg
  ))
}

#' Signal a Dimension Mismatch Error
#'
#' @param message Error message string
#' @param call The call (default NULL)
#' @keywords internal
stop_dimension_mismatch <- function(message,
                                    call = NULL) {
  stop(new_hetid_error(message, "hetid_error_dimension_mismatch", call))
}

#' Signal an Insufficient Data Error
#'
#' @param message Error message string
#' @param call The call (default NULL)
#' @keywords internal
stop_insufficient_data <- function(message,
                                   call = NULL) {
  stop(new_hetid_error(message, "hetid_error_insufficient_data", call))
}

#' Signal a Generic hetid Error
#'
#' @param message Error message string
#' @param call The call (default NULL)
#' @keywords internal
stop_hetid <- function(message, call = NULL) {
  stop(new_hetid_error(message, call = call))
}

#' Assert Bad Argument Invariant
#'
#' @param ok Logical scalar; if not TRUE, signals error
#' @param message Error message string
#' @param arg Optional argument name
#'
#' @return Invisible TRUE when validation passes
#' @noRd
assert_bad_argument_ok <- function(ok, message,
                                   arg = NULL) {
  if (!isTRUE(ok)) {
    stop_bad_argument(message, arg = arg)
  }
  invisible(TRUE)
}

#' Assert a Value Is a Single TRUE/FALSE Flag
#'
#' @param x Value to check
#' @param arg Argument name, used in both the message and the condition
#'
#' @return Invisible TRUE when valid
#' @noRd
assert_flag <- function(x, arg) {
  assert_bad_argument_ok(
    isTRUE(x) || isFALSE(x),
    paste0(arg, " must be TRUE or FALSE"),
    arg = arg
  )
}

#' Assert Dimension Invariant
#'
#' @param ok Logical scalar; if not TRUE, signals error
#' @param message Error message string
#'
#' @return Invisible TRUE when validation passes
#' @noRd
assert_dimension_ok <- function(ok, message) {
  if (!isTRUE(ok)) {
    stop_dimension_mismatch(message)
  }
  invisible(TRUE)
}

#' Assert Data Availability Invariant
#'
#' @param ok Logical scalar; if not TRUE, signals error
#' @param message Error message string
#'
#' @return Invisible TRUE when validation passes
#' @noRd
assert_insufficient_data_ok <- function(ok, message) {
  if (!isTRUE(ok)) {
    stop_insufficient_data(message)
  }
  invisible(TRUE)
}
