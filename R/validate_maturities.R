#' Maturity Validators
#'
#' Validators for maturity indices: the scalar \code{validate_maturity_index}
#' and the vector \code{validate_maturities}. These enforce the maturity
#' contract; the maturity identity itself is carried by
#' \code{hetid_moments} containers (see
#' \code{\link{compute_identification_moments}}).
#'
#' @name validate_maturities_doc
#' @keywords internal
NULL

#' Validate Maturity Index
#'
#' Checks that a maturity index is a finite integer between
#' \code{HETID_CONSTANTS$MIN_MATURITY} and \code{max_maturity}, inclusive.
#'
#' @param i Numeric value of length one giving an integer maturity in months.
#'   Missing and non-finite values are rejected.
#' @param max_maturity Numeric value of length one giving the inclusive upper
#'   bound in months, defaulting to \code{HETID_CONSTANTS$MAX_MATURITY}.
#'
#' @return Invisible \code{TRUE} if valid; otherwise signals a
#'   \code{hetid_error_bad_argument} condition.
#' @keywords internal
validate_maturity_index <- function(i, max_maturity = HETID_CONSTANTS$MAX_MATURITY) {
  assert_scalar_integer_in_range(
    i, "Maturity index i", HETID_CONSTANTS$MIN_MATURITY, max_maturity,
    arg = "i"
  )
}

#' Validate a News Step
#'
#' Validates the number of maturity-index units per news period: a
#' positive integer no larger than half the maximum maturity, so that
#' at least one news horizon (\code{i = step}, needing maturity
#' \code{i + step}) fits inside the data.
#'
#' @param step Numeric value of length one giving a positive integer number
#'   of months per news period, at most half of \code{HETID_CONSTANTS$MAX_MATURITY}.
#'   Missing and non-finite values are rejected.
#' @return Invisible \code{TRUE} if valid; otherwise signals a
#'   \code{hetid_error_bad_argument} condition.
#' @keywords internal
validate_step <- function(step) {
  assert_scalar_integer_in_range(
    step, "step", 1L, HETID_CONSTANTS$MAX_MATURITY %/% 2L,
    arg = "step"
  )
}

#' Validate a News-Horizon Maturity Index
#'
#' Validates a maturity index used as a news horizon: the news at
#' horizon \code{i} differences \code{n_hat(i, t)} against
#' \code{n_hat(i - step, t + 1)}, so \code{i} must not exceed
#' \code{\link{effective_max_maturity}(step)}. The boundary case
#' \code{i == step} is allowed; otherwise \code{i - step} must be at least
#' \code{HETID_CONSTANTS$MIN_MATURITY}. The horizon need not be a multiple
#' of \code{step}.
#'
#' The step must satisfy \code{\link{validate_step}}.
#'
#' @param i Numeric value of length one giving an integer news horizon in
#'   months. Missing and non-finite values are rejected.
#' @template param-step
#' @return Invisible \code{TRUE} if valid; otherwise signals a
#'   \code{hetid_error_bad_argument} condition for an invalid horizon or step.
#' @keywords internal
validate_news_maturity_index <- function(i, step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_maturity_index(i, max_maturity = effective_max_maturity(step))
  assert_news_contract_ok(
    i, step,
    arg = "i", subject = "Maturity index i", offset_label = "i",
    include_invalid = FALSE
  )
  invisible(TRUE)
}

#' Validate a Vector of Maturity Indices
#'
#' Single source of truth for validating a numeric maturity vector:
#' non-empty, finite integers, within \code{[min_value, max_value]},
#' and free of duplicates. The scalar analog is
#' \code{\link{validate_maturity_index}}.
#'
#' The default \code{min_value = 1} serves the identification layer,
#' whose "maturities" are positional w2 column indices (1..n); callers
#' validating ACM bond maturities pass
#' \code{min_value = HETID_CONSTANTS$MIN_MATURITY} (months).
#'
#' The supplied order and names do not affect validity. This function only
#' checks the input; it does not sort, deduplicate, or convert it.
#' Callers supply valid scalar bounds and labels; these are not validated here.
#'
#' @param maturities Non-empty numeric vector of finite integer maturity
#'   indices with no dimensions or duplicates. Missing values are rejected.
#' @param max_value Numeric value of length one giving the inclusive upper
#'   bound (e.g. \code{ncol(gamma)}), in the same units as \code{maturities}.
#' @param max_label Character string or \code{NULL} (the default). An optional
#'   human label for the upper bound, shown label-first in the
#'   error message (e.g. \code{"ncol(gamma)"} renders as \code{ncol(gamma) (4)}).
#' @param arg Character string giving the argument name in error messages and
#'   the structured condition, defaulting to \code{"maturities"}.
#' @param min_value Numeric value of length one giving the inclusive lower
#'   bound in the same units as \code{maturities}. The default \code{1L} follows
#'   the positional column-index convention.
#'
#' @return Invisible \code{TRUE} if valid; otherwise signals a
#'   \code{hetid_error_bad_argument} condition with the supplied \code{arg}.
#' @seealso \code{\link{validate_maturity_index}}
#' @keywords internal
validate_maturities <- function(maturities, max_value,
                                max_label = NULL,
                                arg = "maturities",
                                min_value = 1L) {
  min_maturity <- min_value
  assert_bad_argument_ok(
    length(maturities) > 0,
    paste0(arg, " must not be empty"),
    arg = arg
  )
  assert_bad_argument_ok(
    is.numeric(maturities) &&
      is.null(dim(maturities)) &&
      all(is.finite(maturities)) &&
      all(maturities %% 1 == 0),
    paste0(arg, " must be finite integer values"),
    arg = arg
  )
  bound_desc <- if (is.null(max_label)) {
    as.character(max_value)
  } else {
    paste0(max_label, " (", max_value, ")")
  }
  bad <- maturities[maturities < min_maturity | maturities > max_value]
  assert_bad_argument_ok(
    length(bad) == 0,
    paste0(
      arg, " must be between ", min_maturity, " and ", bound_desc,
      "; invalid: ", paste(unique(bad), collapse = ", ")
    ),
    arg = arg
  )
  assert_bad_argument_ok(
    anyDuplicated(maturities) == 0,
    paste0(
      arg, " must not contain duplicates; got: ",
      paste(unique(maturities[duplicated(maturities)]), collapse = ", ")
    ),
    arg = arg
  )
  invisible(TRUE)
}
