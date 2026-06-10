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

#' Validate a Vector of Maturity Indices
#'
#' Single source of truth for validating a numeric maturity vector:
#' non-empty, finite integers, within \code{[MIN_MATURITY, max_value]},
#' and free of duplicates. The scalar analog is
#' \code{\link{validate_maturity_index}}.
#'
#' @param maturities Numeric vector of maturity indices.
#' @param max_value Inclusive upper bound (e.g. \code{ncol(gamma)}).
#' @param max_label Optional human label for the bound, shown label-first in the
#'   error message (e.g. \code{"ncol(gamma)"} renders as \code{ncol(gamma) (4)}).
#' @param arg Argument name for the structured error.
#'
#' @return Invisible TRUE if valid, stops with informative error otherwise.
#' @seealso \code{\link{validate_maturity_index}}
#' @keywords internal
validate_maturities <- function(maturities, max_value,
                                max_label = NULL,
                                arg = "maturities") {
  min_maturity <- HETID_CONSTANTS$MIN_MATURITY
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
