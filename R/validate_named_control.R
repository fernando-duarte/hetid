#' Assert a Named List of Supported Controls
#'
#' @param control List of control overrides to validate.
#' @param supported Character vector of supported control names.
#' @param message Single error message for invalid controls.
#' @return Invisible \code{TRUE} when valid.
#' @noRd
assert_named_control <- function(control, supported, message) {
  assert_bad_argument_ok(
    is.list(control) && (length(control) == 0L ||
      (!is.null(names(control)) && !anyNA(names(control)) &&
        !anyDuplicated(names(control)) && all(names(control) %in% supported))),
    message,
    arg = "control"
  )
}
