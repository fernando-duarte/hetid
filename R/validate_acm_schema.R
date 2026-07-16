#' Validate the ACM Data Schema After Reading
#'
#' Guards against stale or corrupt cache files that the download-time
#' digest check cannot see (the digest only covers fresh downloads):
#' requires a date column, at least one ACM yield column, and numeric
#' values in every ACM family column. The check is deliberately lenient
#' about which maturities are present so both the GitHub
#' (monthly-maturity) and NY Fed (annual-only) sources pass, as do
#' reduced test fixtures.
#'
#' @param acm_data Data frame as read from disk
#' @param path Source file path, named in error messages
#' @return Invisible TRUE
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
