#' Column Validation Utilities
#'
#' Helpers for validating and extracting columns from data frames and matrices.
#'
#' @name column_utils
#' @keywords internal
NULL

#' Column Labels of a Matrix or Data Frame
#'
#' @param x A matrix, data frame, or named list.
#' @return A character vector of column or element names, or \code{NULL}
#'   when no names are present.
#' @noRd
column_labels <- function(x) {
  if (is.matrix(x)) colnames(x) else names(x)
}

#' Append an Optional Context Clause to a Message
#'
#' @param msg A character string containing the base message.
#' @param context An optional character string, or \code{NULL} (the default).
#' @return A character string with \code{" in <context>"} appended when context
#'   is supplied; otherwise, \code{msg} unchanged.
#' @noRd
with_context <- function(msg, context = NULL) {
  if (!is.null(context)) paste0(msg, " in ", context) else msg
}

#' Extract and Validate a Required Column
#'
#' @param x A data frame, matrix, or named list to extract from.
#' @param col_name A character string naming the required column or element.
#' @param context An optional character string for the error message;
#'   \code{NULL} (the default) adds no context.
#' @return The column or element, with missing values unchanged. Matrix results
#'   have names removed; data-frame and list elements retain their attributes.
#'   A missing name signals a \code{hetid_error_bad_argument} with
#'   \code{arg = col_name}.
#' @keywords internal
require_column <- function(x, col_name, context = NULL) {
  has_col <- col_name %in% column_labels(x)
  msg <- with_context(paste0(col_name, " column not found"), context)
  assert_bad_argument_ok(has_col, msg, arg = col_name)
  if (is.matrix(x)) unname(x[, col_name, drop = TRUE]) else x[[col_name]]
}

#' Assert a Data Type Is a Known ACM Schema Key
#'
#' Scalar key guard shared by \code{acm_column_name} and
#' \code{acm_raw_column_name}; \code{arg} preserves each call site's
#' condition field (\code{"data_type"} vs \code{"data_types"}).
#'
#' @param data_type A character string naming a key in \code{HETID_ACM_SCHEMA}.
#' @param arg A character string naming the condition argument;
#'   defaults to \code{"data_type"}.
#' @return Invisible \code{TRUE} for a valid key; otherwise signals a
#'   \code{hetid_error_bad_argument} with the supplied \code{arg}.
#' @noRd
assert_acm_data_type <- function(data_type, arg = "data_type") {
  assert_bad_argument_ok(
    data_type %in% names(HETID_ACM_SCHEMA),
    paste0(
      "Unknown data type: '", data_type, "'. Must be one of: ",
      paste(names(HETID_ACM_SCHEMA), collapse = ", ")
    ),
    arg = arg
  )
}

#' Assert Data Types Are Known ACM Schema Keys (Vector Form)
#'
#' Vector sibling of \code{assert_acm_data_type}: the non-empty
#' character-vector contract for \code{validate_acm_extract_inputs}.
#' Shares the schema-key source of truth (\code{names(HETID_ACM_SCHEMA)});
#' keeps the vector message distinct from the scalar one.
#'
#' @param data_types A non-empty character vector of distinct schema keys from
#'   \code{names(HETID_ACM_SCHEMA)}; missing values are not allowed.
#' @param arg A character string naming the condition argument;
#'   defaults to \code{"data_types"}.
#' @return Invisible \code{TRUE} for valid keys; otherwise signals a
#'   \code{hetid_error_bad_argument} with the supplied \code{arg}.
#' @noRd
assert_acm_data_types <- function(data_types, arg = "data_types") {
  assert_bad_argument_ok(
    is.character(data_types) && length(data_types) >= 1 &&
      all(data_types %in% names(HETID_ACM_SCHEMA)),
    paste0(
      "Invalid data_types. Must be one or more of: ",
      paste(names(HETID_ACM_SCHEMA), collapse = ", ")
    ),
    arg = arg
  )
  assert_bad_argument_ok(
    anyDuplicated(data_types) == 0L,
    paste0(
      "data_types must not contain duplicates; got: ",
      paste(unique(data_types[duplicated(data_types)]), collapse = ", ")
    ),
    arg = arg
  )
}

#' Build an ACM Column Name from the Schema
#'
#' Constructs names for reshaped ACM data, such as \code{"y12"} for a 12-month
#' yield. Prefixes follow \code{HETID_ACM_SCHEMA}, and maturity formatting follows
#' \code{HETID_CONSTANTS$COL_FORMAT_SIMPLE}.
#'
#' @details Only \code{data_type} is validated. Maturities are formatted directly
#'   without range or missing-value checks.
#' @param data_type A character string: \code{"yields"}, \code{"term_premia"},
#'   or \code{"risk_neutral_yields"}.
#' @param maturity An integer or integer-valued numeric vector of maturities in months.
#' @return An unnamed character vector with one column name per maturity, in input
#'   order, such as \code{"y60"}. Empty input returns \code{character(0)};
#'   missing maturities produce names ending in \code{"NA"}. An invalid schema
#'   key signals a \code{hetid_error_bad_argument} with \code{arg = "data_type"}.
#' @examples
#' acm_column_name("yields", c(12, 60))
#' acm_column_name("term_premia", 12)
#' acm_column_name("risk_neutral_yields", 12)
#' @export
acm_column_name <- function(data_type, maturity) {
  assert_acm_data_type(data_type, arg = "data_type")
  sprintf(
    HETID_CONSTANTS$COL_FORMAT_SIMPLE,
    HETID_ACM_SCHEMA[[data_type]]$prefix_new,
    maturity
  )
}

#' Fetch an ACM Column by Schema Type and Maturity
#'
#' Retrieves a reshaped ACM column using its data type and maturity, without
#' altering its observations.
#'
#' @param data An ACM data frame or matrix.
#' @param data_type A character string: \code{"yields"}, \code{"term_premia"},
#'   or \code{"risk_neutral_yields"}.
#' @param maturity A single integer or integer-valued numeric maturity in months.
#' @return The requested column, with values and extraction attributes as described
#'   in \code{\link{require_column}}. An invalid schema key or missing column
#'   signals a \code{hetid_error_bad_argument}.
#' @keywords internal
require_acm_col <- function(data, data_type, maturity) {
  require_column(data, acm_column_name(data_type, maturity), data_type)
}

#' Assert Input Is Tabular
#'
#' @param x An object to check; column contents are not inspected.
#' @param name A character string naming the argument in the error and its \code{arg} field.
#' @return Invisible \code{TRUE} for a matrix or data frame; otherwise signals a
#'   \code{hetid_error_bad_argument}.
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
#' @param df A data frame or matrix; column contents are not inspected.
#' @param required_cols A character vector of required column names. Empty input
#'   imposes no requirements; repeated names are checked only once.
#' @param context An optional character string for the error message;
#'   \code{NULL} (the default) adds no context.
#' @param arg A character string naming the condition argument; defaults to \code{"df"}.
#' @return Invisible \code{TRUE} when all required names exist; otherwise signals
#'   a \code{hetid_error_bad_argument} listing the missing names.
#' @keywords internal
assert_columns_exist <- function(df, required_cols,
                                 context = NULL, arg = "df") {
  cols <- column_labels(df)
  missing_cols <- setdiff(required_cols, cols)
  msg <- paste(
    "Missing required columns:",
    paste(missing_cols, collapse = ", ")
  )
  msg <- with_context(msg, context)
  assert_bad_argument_ok(
    length(missing_cols) == 0,
    msg,
    arg = arg
  )
  invisible(TRUE)
}
