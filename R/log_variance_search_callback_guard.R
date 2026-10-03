lv_set_guard_callback <- function(callback, state, gradient_labels) {
  function(b) {
    missing_value <- if (is.null(gradient_labels)) NaN else rep(NaN, length(gradient_labels))
    if (!is.null(state$condition) || (is.numeric(b) && any(!is.finite(b)))) {
      return(missing_value)
    }
    retain <- function(condition) {
      state$condition <- condition
      missing_value
    }
    tryCatch(
      {
        value <- callback(b)
        if (is.null(gradient_labels)) {
          assert_bad_argument_ok(
            is.numeric(value) && is.null(dim(value)) && length(value) == 1L,
            "objective must return a numeric scalar", "estimator"
          )
        } else {
          lv_set_axis(value, gradient_labels, "objective gradient", finite = FALSE)
        }
        value
      },
      hetid_error_log_variance_budget = retain,
      hetid_error_bad_argument = retain
    )
  }
}
