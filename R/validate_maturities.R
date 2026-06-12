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
#' Validates maturity index against dataset constraints.
#'
#' @param i Integer maturity index to validate
#' @param max_maturity Maximum allowed maturity (default from ACM dataset limit)
#'
#' @return Invisible TRUE if valid, stops with informative error if invalid
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
#' @template param-step
#' @return Invisible TRUE if valid, stops with informative error otherwise
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
#' \code{effective_max_maturity(step)} and the previous-period index
#' must either be the boundary case (\code{i == step}) or stay at or
#' above \code{MIN_MATURITY}.
#'
#' @param i Integer maturity index to validate
#' @template param-step
#' @return Invisible TRUE if valid, stops with informative error otherwise
#' @keywords internal
validate_news_maturity_index <- function(i, step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_maturity_index(i, max_maturity = effective_max_maturity(step))
  assert_bad_argument_ok(
    i == step || i - step >= HETID_CONSTANTS$MIN_MATURITY,
    paste0(
      "Maturity index i must equal step (", step,
      ") or satisfy i - step >= ", HETID_CONSTANTS$MIN_MATURITY
    ),
    arg = "i"
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
#' @param maturities Numeric vector of maturity indices.
#' @param max_value Inclusive upper bound (e.g. \code{ncol(gamma)}).
#' @param max_label Optional human label for the bound, shown label-first in the
#'   error message (e.g. \code{"ncol(gamma)"} renders as \code{ncol(gamma) (4)}).
#' @param arg Argument name for the structured error.
#' @param min_value Inclusive lower bound (default 1, the positional
#'   component-index convention).
#'
#' @return Invisible TRUE if valid, stops with informative error otherwise.
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
      paste(maturities, collapse = ", ")
    ),
    arg = arg
  )
  invisible(TRUE)
}
