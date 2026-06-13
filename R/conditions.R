#' Custom Condition Classes for hetid
#'
#' Structured condition constructors for programmatic error
#' handling. Enables tryCatch with class-based dispatch.
#'
#' @name conditions
#' @keywords internal
NULL

#' Signal a Bad Argument Error
#'
#' @param message Error message string
#' @param arg Argument name (optional)
#' @param call The call (default NULL)
#' @keywords internal
stop_bad_argument <- function(message, arg = NULL,
                              call = NULL) {
  cnd <- structure(
    class = c(
      "hetid_error_bad_argument",
      "hetid_error", "error", "condition"
    ),
    list(message = message, call = call, arg = arg)
  )
  stop(cnd)
}

#' Signal a Dimension Mismatch Error
#'
#' @param message Error message string
#' @param call The call (default NULL)
#' @keywords internal
stop_dimension_mismatch <- function(message,
                                    call = NULL) {
  cnd <- structure(
    class = c(
      "hetid_error_dimension_mismatch",
      "hetid_error", "error", "condition"
    ),
    list(message = message, call = call)
  )
  stop(cnd)
}

#' Signal an Insufficient Data Error
#'
#' @param message Error message string
#' @param call The call (default NULL)
#' @keywords internal
stop_insufficient_data <- function(message,
                                   call = NULL) {
  cnd <- structure(
    class = c(
      "hetid_error_insufficient_data",
      "hetid_error", "error", "condition"
    ),
    list(message = message, call = call)
  )
  stop(cnd)
}

#' Signal a Generic hetid Error
#'
#' @param message Error message string
#' @param call The call (default NULL)
#' @keywords internal
stop_hetid <- function(message, call = NULL) {
  cnd <- structure(
    class = c(
      "hetid_error", "error", "condition"
    ),
    list(message = message, call = call)
  )
  stop(cnd)
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
