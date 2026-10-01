#' Assert a Named List of Supported Controls
#'
#' @param control List of control overrides. An empty list is allowed; otherwise
#'   names must be nonmissing, unique, and present in \code{supported}.
#'   Control values are not checked.
#' @param supported Character vector of supported control names.
#' @param message Single error message for invalid controls.
#' @return Invisible \code{TRUE} when valid. Otherwise signals a
#'   \code{hetid_error_bad_argument} condition with \code{arg = "control"}.
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
