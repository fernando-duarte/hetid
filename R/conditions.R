#' Custom Condition Classes for hetid
#'
#' Structured condition constructors for programmatic error
#' handling. These conditions support \code{tryCatch()} with class-based dispatch.
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
#' @param message A character scalar containing the error message.
#' @param subclass A character vector of specific condition classes to prepend,
#'   or \code{NULL} (the default) for no additional classes.
#' @param call A call object to store on the condition, or \code{NULL}
#'   (the default) to omit the call from the displayed error.
#' @param ... Additional named fields stored on the condition without modification.
#' @return A list with \code{message}, \code{call}, and any additional fields.
#'   Its classes are \code{subclass}, when supplied, followed by
#'   \code{hetid_error}, \code{error}, and \code{condition}.
#'   The condition is returned without being signaled.
#' @keywords internal
new_hetid_error <- function(message, subclass = NULL, call = NULL, ...) {
  structure(
    class = c(subclass, "hetid_error", "error", "condition"),
    list(message = message, call = call, ...)
  )
}

#' Signal a Bad Argument Error
#'
#' @param message A character scalar containing the error message.
#' @param arg A character scalar naming the invalid argument, or \code{NULL}
#'   (the default) when no argument name is supplied.
#' @param call A call object to store on the condition, or \code{NULL}
#'   (the default) to omit the call from the displayed error.
#' @return Never returns normally; signals a \code{hetid_error_bad_argument}
#'   condition inheriting from \code{hetid_error}, with an \code{arg} field.
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
#' @param message A character scalar containing the error message.
#' @param call A call object to store on the condition, or \code{NULL}
#'   (the default) to omit the call from the displayed error.
#' @return Never returns; signals a \code{hetid_error_dimension_mismatch}
#'   condition inheriting from \code{hetid_error}.
#' @keywords internal
stop_dimension_mismatch <- function(message,
                                    call = NULL) {
  stop(new_hetid_error(message, "hetid_error_dimension_mismatch", call))
}

#' Signal an Insufficient Data Error
#'
#' @param message A character scalar containing the error message.
#' @param call A call object to store on the condition, or \code{NULL}
#'   (the default) to omit the call from the displayed error.
#' @return Never returns; signals a \code{hetid_error_insufficient_data}
#'   condition inheriting from \code{hetid_error}.
#' @keywords internal
stop_insufficient_data <- function(message,
                                   call = NULL) {
  stop(new_hetid_error(message, "hetid_error_insufficient_data", call))
}

#' Signal a Generic hetid Error
#'
#' @param message A character scalar containing the error message.
#' @param call A call object to store on the condition, or \code{NULL}
#'   (the default) to omit the call from the displayed error.
#' @return Never returns normally; signals a \code{hetid_error} condition.
#' @keywords internal
stop_hetid <- function(message, call = NULL) {
  stop(new_hetid_error(message, call = call))
}

#' Assert Bad Argument Invariant
#'
#' @param ok A logical scalar. Any value other than a single nonmissing
#'   \code{TRUE} signals an error.
#' @param message A character scalar containing the error message.
#' @param arg A character scalar naming the invalid argument, or \code{NULL}
#'   (the default) when no argument name is supplied.
#'
#' @return Invisible \code{TRUE} when validation passes.
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
#' @param x A logical scalar to check. Missing, empty, nonscalar, and
#'   nonlogical inputs signal a bad argument error.
#' @param arg A character scalar naming the argument, used in both the message
#'   and the condition.
#'
#' @return Invisible \code{TRUE} when valid.
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
#' @param ok A logical scalar. Any value other than a single nonmissing
#'   \code{TRUE} signals an error.
#' @param message A character scalar containing the error message.
#'
#' @return Invisible \code{TRUE} when validation passes.
#' @noRd
assert_dimension_ok <- function(ok, message) {
  if (!isTRUE(ok)) {
    stop_dimension_mismatch(message)
  }
  invisible(TRUE)
}

#' Assert Data Availability Invariant
#'
#' @param ok A logical scalar. Any value other than a single nonmissing
#'   \code{TRUE} signals an error.
#' @param message A character scalar containing the error message.
#'
#' @return Invisible \code{TRUE} when validation passes.
#' @noRd
assert_insufficient_data_ok <- function(ok, message) {
  if (!isTRUE(ok)) {
    stop_insufficient_data(message)
  }
  invisible(TRUE)
}
