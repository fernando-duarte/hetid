#' News-Contract Predicate
#'
#' Vectorized test of the news contract: a horizon's previous-period
#' index is either the boundary case (\code{maturity == step}) or stays
#' at or above \code{MIN_MATURITY}. Single source of truth shared by
#' the scalar validator, the W2 vector validator, and the default-grid
#' builder.
#'
#' @param maturities Numeric vector of maturity indices
#' @param step Integer number of maturity-index units per news period.
#' @return Logical vector, TRUE where the contract holds
#' @keywords internal
news_contract_ok <- function(maturities, step) {
  maturities == step |
    maturities - step >= HETID_CONSTANTS$MIN_MATURITY
}

#' Assert the News Contract, Owning the Shared Message
#'
#' Single source of the news-contract failure message for both the
#' scalar index check (\code{validate_news_maturity_index}) and the
#' vector check in \code{validate_w2_inputs}. \code{subject} and
#' \code{offset_label} adapt the wording to each call site;
#' \code{include_invalid} appends the offending values (used by the
#' vector path).
#'
#' @param maturities Scalar or vector of maturity indices
#' @param step Integer number of maturity-index units per news period.
#' @param arg Condition argument name
#' @param subject,offset_label Wording for the subject and the
#'   \code{<x> - step} offset in the message
#' @param include_invalid Whether to append the invalid values
#' @return Invisible TRUE if valid, stops otherwise
#' @keywords internal
assert_news_contract_ok <- function(maturities, step, arg,
                                    subject, offset_label, include_invalid) {
  bad <- maturities[!news_contract_ok(maturities, step)]
  msg <- paste0(
    subject, " must equal step (", step, ") or satisfy ",
    offset_label, " - step >= ", HETID_CONSTANTS$MIN_MATURITY
  )
  if (include_invalid && length(bad) > 0L) {
    msg <- paste0(msg, "; invalid: ", paste(bad, collapse = ", "))
  }
  assert_bad_argument_ok(length(bad) == 0L, msg, arg = arg)
}

#' Validate That a Maturity Index Is a Positive Multiple of the Step
#'
#' Single source of the guard shared by compute_k_hat / compute_k2_hat,
#' whose news-period arithmetic shifts whole steps; \code{reason} adapts the
#' trailing clause to each call site. Stops with hetid_error_bad_argument.
#'
#' @param i Maturity index to check.
#' @param step Step size the index must be a positive multiple of.
#' @param reason Trailing clause naming why the caller needs the multiple.
#' @noRd
validate_step_multiple <- function(i, step, reason) {
  assert_bad_argument_ok(
    i >= step && i %% step == 0,
    paste0(
      "Maturity index i must be a positive multiple of step (", step,
      "): ", reason
    ),
    arg = "i"
  )
  invisible(TRUE)
}
