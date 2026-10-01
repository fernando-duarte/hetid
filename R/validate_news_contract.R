#' News-Contract Predicate
#'
#' Vectorized test of the news contract: a horizon equals \code{step}
#' (the boundary case) or its previous-period index \code{maturities - step}
#' stays at or above \code{HETID_CONSTANTS$MIN_MATURITY}. Shared by
#' the scalar validator, the \eqn{\omega_2} vector validator, and the default-grid
#' builder.
#'
#' @details This predicate does not validate input types or ranges.
#' @param maturities Numeric vector of maturity indices in months.
#' @param step Positive integer scalar giving the number of months per news period.
#' @return A logical vector of length \code{length(maturities)}, with
#'   \code{TRUE} where the news contract holds and \code{FALSE} otherwise.
#'   Missing maturity indices produce \code{NA}; an empty input returns
#'   \code{logical(0)}.
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
#' @details This guard checks only the news contract, not input types,
#'   ranges, or the validity of \code{step}.
#' @param maturities Numeric scalar or vector of maturity indices in months.
#' @param step Positive integer scalar giving the number of months per news period.
#' @param arg Character scalar naming the argument stored in the error condition.
#' @param subject,offset_label Character scalars giving the subject and the
#'   \code{<x> - step} offset label in the error message.
#' @param include_invalid Logical scalar indicating whether to append invalid values.
#' @return Invisible \code{TRUE} when every index satisfies the news contract,
#'   including when the input is empty. Otherwise, signals a
#'   \code{hetid_error_bad_argument} condition with the supplied \code{arg}.
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
#' @param i Finite integer scalar maturity index in months, validated by the caller.
#' @param step Positive integer scalar number of months per news period,
#'   validated by the caller.
#' @param reason Character scalar naming why the caller needs the multiple.
#' @return Invisible \code{TRUE} when \code{i} is a positive multiple of \code{step}.
#'   Otherwise, signals a \code{hetid_error_bad_argument} condition for \code{i}.
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
