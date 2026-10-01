#' Validation for Instrument Support Index Lists
#'
#' Internal validators shared by \code{\link{lambda_from_support}} and
#' \code{\link{align_instrument_sets}}. A support is a list with one
#' integer vector of free instrument rows per constrained system
#' column and \code{NULL} at every unconstrained column -- the same
#' positional convention as the lambda weight lists themselves.
#'
#' @name validate_instrument_support
#' @keywords internal
NULL

#' A Value Is a Single Positive Integer Count
#'
#' Tests whether a dimensionless numeric input is a single finite positive
#' whole number. Invalid or missing inputs return \code{FALSE}.
#'
#' @param x Candidate numeric scalar count.
#' @return Logical scalar, \code{TRUE} only for a finite positive whole number.
#' @noRd
positive_count_ok <- function(x) {
  if (!is.numeric(x) || !is.null(dim(x))) {
    return(FALSE)
  }
  isTRUE(all(c(
    length(x) == 1,
    is.finite(x),
    x %% 1 == 0,
    x >= 1
  )))
}

#' One Support Entry Is a Valid Free-Row Index Vector
#'
#' @param s_i Candidate numeric vector of instrument row indices.
#' @param j_total Validated positive integer count of instrument rows on the union axis.
#' @return Logical scalar, \code{TRUE} only for a nonempty vector of unique,
#'   finite whole-number indices in \code{1..j_total}.
#' @noRd
support_entry_ok <- function(s_i, j_total) {
  if (!is.numeric(s_i) || !is.null(dim(s_i))) {
    return(FALSE)
  }
  isTRUE(all(c(
    length(s_i) >= 1,
    is.finite(s_i),
    s_i %% 1 == 0,
    anyDuplicated(s_i) == 0,
    s_i >= 1,
    s_i <= j_total
  )))
}

#' Coerce and Validate a Support List
#'
#' Enforces the support contract against an explicit constrained-
#' column set: NULL exactly at unconstrained columns, and at every
#' constrained column a vector of unique integer row indices in
#' \code{1..j_total} with at least one entry. A fully masked column
#' could never satisfy the builder's nonzero-direction rule, so it is
#' rejected here, before any downstream work.
#'
#' @param support Candidate list with one entry per system column.
#' @param j_total Validated positive integer count of instrument rows.
#' @param n_components Validated system width on the theta axis (required list length).
#' @param maturities Validated integer vector of constrained system column indices.
#' @return List of length \code{n_components}, with constrained entries coerced
#'   to integer and \code{NULL} at unconstrained columns. Invalid support signals
#'   a \code{hetid_error_bad_argument} condition.
#' @noRd
assert_support_list <- function(support, j_total, n_components,
                                maturities) {
  assert_bad_argument_ok(
    is.list(support) && length(support) == n_components,
    paste0(
      "support must be a list of length n_components (",
      n_components, ") with an integer vector of free instrument ",
      "rows at every constrained system column and NULL elsewhere"
    ),
    arg = "support"
  )
  assert_null_at_unconstrained(
    support, n_components, maturities, "support"
  )
  for (i in maturities) {
    assert_bad_argument_ok(
      support_entry_ok(support[[i]], j_total),
      paste0(
        "support[[", i, "]] must be unique integer row indices in ",
        "1..", j_total, " with at least one free row (a fully ",
        "masked column can never satisfy the nonzero-direction rule)"
      ),
      arg = "support"
    )
    support[[i]] <- as.integer(support[[i]])
  }
  support
}
