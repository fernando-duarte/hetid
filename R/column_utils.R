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
  msg <- paste0(col_name, " column not found")
  if (!is.null(context)) {
    msg <- paste0(msg, " in ", context)
  }
  assert_bad_argument_ok(
    !is.null(val),
    msg,
    arg = col_name
  )
  val
}

#' Build an ACM Column Name from the Schema
#'
#' Single source of truth for reshaped ACM column names: routes the
#' prefix through \code{HETID_ACM_SCHEMA} and the format through
#' \code{HETID_CONSTANTS$COL_FORMAT_SIMPLE} instead of hard-coding
#' literals like \code{paste0("y", i)} at call sites.
#'
#' @param data_type Schema key: \code{"yields"}, \code{"term_premia"},
#'   or \code{"risk_neutral_yields"}
#' @param maturity Maturity index (or vector of indices)
#' @return Character vector of column names, e.g. \code{"y60"}
#' @keywords internal
acm_column_name <- function(data_type, maturity) {
  assert_bad_argument_ok(
    data_type %in% names(HETID_ACM_SCHEMA),
    paste0(
      "Unknown data type: '", data_type, "'. Must be one of: ",
      paste(names(HETID_ACM_SCHEMA), collapse = ", ")
    ),
    arg = "data_type"
  )
  sprintf(
    HETID_CONSTANTS$COL_FORMAT_SIMPLE,
    HETID_ACM_SCHEMA[[data_type]]$prefix_new,
    maturity
  )
}

#' Assert Input is Tabular
#'
#' @param x Object to check
#' @param name Argument name for error message
#' @keywords internal
assert_tabular <- function(x, name) {
  assert_bad_argument_ok(
    is.data.frame(x) || is.matrix(x),
    paste0(name, " must be a matrix or data frame"),
    arg = name
  )
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
  msg <- paste(
    "Missing required columns:",
    paste(missing_cols, collapse = ", ")
  )
  if (!is.null(context)) {
    msg <- paste0(msg, " in ", context)
  }
  assert_bad_argument_ok(
    length(missing_cols) == 0,
    msg
  )
  invisible(TRUE)
}
