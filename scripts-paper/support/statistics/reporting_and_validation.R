# Data-validation functions.

#' Test data for missing values and report
#' @param data data frame or list
#' @param stop_on_na whether to stop execution if NA found
#' @return logical indicating if data is complete
check_data_completeness <- function(data, stop_on_na = TRUE) {
  if (is.data.frame(data)) {
    na_check <- colSums(is.na(data))
    has_na <- any(na_check > 0)
  } else if (is.list(data)) {
    na_check <- vapply(
      data, function(x) sum(is.na(x)), integer(1)
    )
    has_na <- any(na_check > 0)
  } else {
    has_na <- any(is.na(data))
  }

  if (has_na) {
    if (is.data.frame(data) || is.list(data)) {
      na_vars <- names(na_check)[na_check > 0]
      message <- paste("Missing values found in:", paste(na_vars, collapse = ", "))
    } else {
      message <- "Missing values found in data"
    }

    if (stop_on_na) {
      stop(message)
    } else {
      cli::cli_alert_warning(message)
    }
  }

  !has_na
}
