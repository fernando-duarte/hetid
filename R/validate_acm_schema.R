#' Validate the ACM Data Schema After Reading
#'
#' Checks the column schema of data read from disk to catch stale or
#' corrupt cache files. Requires a date column, at least one ACM yield
#' column, and numeric columns in every ACM family. The check is deliberately lenient
#' about which maturities are present so both the GitHub
#' (monthly-maturity) and NY Fed (annual-only) sources pass, as do
#' reduced test fixtures.
#'
#' @details
#' The date column must be named \code{DATE} or \code{date}; its
#' values are not parsed or validated here. ACM families are identified
#' by the raw prefixes in \code{\link{HETID_ACM_SCHEMA}}. Numeric
#' columns may contain missing or nonfinite values, and zero-row data
#' frames are allowed. Other columns are ignored. This function does
#' not read or modify the source file or change the supplied data frame.
#'
#' @param acm_data A data frame as read from disk, with raw ACM column names.
#' @param path A character string naming the source file in error messages.
#' @return The logical scalar \code{TRUE}, invisibly, when validation passes.
#'   Otherwise, signals a \code{hetid_error} condition naming the source file
#'   and the first failed requirement.
#' @keywords internal
validate_acm_schema <- function(acm_data, path) {
  # Derive patterns from HETID_ACM_SCHEMA so a rename there propagates here
  prefixes <- vapply(HETID_ACM_SCHEMA, `[[`, character(1), "prefix_old")
  family_pattern <- paste0("^(", paste(prefixes, collapse = "|"), ")")

  has_date <- any(c("DATE", "date") %in% names(acm_data))
  yield_cols <- grep(
    paste0("^", prefixes[["yields"]]), names(acm_data),
    value = TRUE
  )
  family_cols <- grep(family_pattern, names(acm_data), value = TRUE)
  non_numeric <- family_cols[
    !vapply(acm_data[family_cols], is.numeric, logical(1))
  ]

  ok <- has_date && length(yield_cols) > 0 && length(non_numeric) == 0
  if (!ok) {
    detail <- if (!has_date) {
      "no DATE column"
    } else if (length(yield_cols) == 0) {
      "no ACMY yield columns"
    } else {
      paste0(
        "non-numeric columns: ", paste(non_numeric, collapse = ", ")
      )
    }
    stop_hetid(paste0(
      "ACM data at ", path, " failed schema validation (", detail,
      "). The file may be stale or corrupt; delete it or re-run ",
      "download_term_premia(force = TRUE)."
    ))
  }
  invisible(TRUE)
}
