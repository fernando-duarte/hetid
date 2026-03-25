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
