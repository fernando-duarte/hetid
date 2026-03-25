#' Column Validation Utilities
#'
#' Helpers for validating and extracting columns from data frames and matrices.
#'
#' @name column_utils
#' @keywords internal
NULL

#' Extract and Validate a Required Column
#'
#' @param x Data frame or list to extract from
#' @param col_name Column name to extract
#' @param context Description for error message
#' @return The extracted column value
#' @keywords internal
require_column <- function(x, col_name, context = NULL) {
  val <- x[[col_name]]
  if (is.null(val)) {
    msg <- paste0(col_name, " column not found")
    if (!is.null(context)) {
      msg <- paste0(msg, " in ", context)
    }
    stop(msg, call. = FALSE)
  }
  val
}

#' Assert Input is Tabular
#'
#' @param x Object to check
#' @param name Argument name for error message
#' @keywords internal
assert_tabular <- function(x, name) {
  if (!is.data.frame(x) && !is.matrix(x)) {
    stop(
      name, " must be a matrix or data frame",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Assert Required Columns Exist
#'
#' @param df Data frame or matrix
#' @param required_cols Character vector of required column names
#' @param context Description for error message
#' @keywords internal
assert_columns_exist <- function(df, required_cols,
                                 context = NULL) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    msg <- paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    )
    if (!is.null(context)) {
      msg <- paste0(msg, " in ", context)
    }
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}
