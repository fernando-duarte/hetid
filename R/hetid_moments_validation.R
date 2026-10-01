#' Shape Validation for hetid_moments Objects
#'
#' Internal helpers behind \code{validate_hetid_moments()}: every outer
#' (constraint-axis) shape and name is checked against
#' \code{maturity_names(maturities)} and every inner (theta-axis)
#' dimension against \code{n_components}.
#'
#' @name hetid_moments_validation
#' @keywords internal
NULL

#' Validate a hetid_moments Object
#'
#' Checks numeric types, constraint-axis names and shapes, and theta-axis dimensions
#' against the object's own attributes. Run by the public boundary
#' \code{\link{compute_identification_moments}} on every object it
#' returns; call it directly on containers assembled via
#' \code{\link{new_hetid_moments}} from parts that are not known-good.
#'
#' @param x A \code{hetid_moments} list with seven named statistics and
#'   \code{maturities} and \code{n_components} attributes, as created by
#'   \code{\link{new_hetid_moments}}.
#' @return The \code{hetid_moments} object \code{x}, returned invisibly
#'   and unchanged when validation succeeds.
#' @details
#' The \code{maturities} attribute must contain distinct, finite integer
#' w2 column indices between one and \code{n_components}. Outer names
#' must be \code{maturity_N}, in the order of these indices. Instrument
#' dimensions are checked against the row count of \code{x$r_i_0}.
#' Finiteness of moment values and inner dimension names are not checked.
#' Invalid classes, maturity indices, or outer names signal
#' \code{hetid_error_bad_argument}; shape mismatches signal
#' \code{hetid_error_dimension_mismatch}.
#' @keywords internal
validate_hetid_moments <- function(x) {
  assert_hetid_moments(x, arg = "x")
  n_components <- attr(x, "n_components")
  maturities <- attr(x, "maturities")
  validate_maturities(
    maturities,
    max_value = n_components, max_label = "n_components"
  )
  validate_moments_shapes(x, maturities, n_components)
  invisible(x)
}

#' Validate the Seven Moment Shapes
#'
#' @param stats The seven-element moment list under validation.
#' @param maturities Integer maturity vector defining the constraint axis.
#' @param n_components Integer width of the theta axis.
#' @noRd
validate_moments_shapes <- function(stats, maturities, n_components) {
  expected <- maturity_names(maturities)
  n <- length(maturities)
  j_rows <- nrow(stats$r_i_0)

  for (name in c("s_i_0", "sigma_i_sq")) {
    x <- stats[[name]]
    assert_dimension_ok(
      is.numeric(x) && is.null(dim(x)) && length(x) == n,
      paste0(name, " must be a numeric vector of length(maturities)")
    )
    assert_bad_argument_ok(
      identical(names(x), expected),
      paste0(name, " names must equal maturity_N for maturities"),
      arg = name
    )
  }

  for (name in c("r_i_0", "p_i_0")) {
    x <- stats[[name]]
    assert_bad_argument_ok(
      is.numeric(x), paste0(name, " must be numeric"),
      arg = name
    )
    assert_dimension_ok(
      is.matrix(x) && nrow(x) == j_rows && ncol(x) == n,
      paste0(name, " must be a J x length(maturities) matrix")
    )
    assert_bad_argument_ok(
      identical(colnames(x), expected),
      paste0(name, " column names must equal maturity_N for maturities"),
      arg = name
    )
  }

  for (name in c("r_i_1", "s_i_1", "s_i_2")) {
    x <- stats[[name]]
    assert_dimension_ok(
      is.list(x) && length(x) == n,
      paste0(name, " must be a list of length(maturities) elements")
    )
    assert_bad_argument_ok(
      identical(names(x), expected),
      paste0(name, " names must equal maturity_N for maturities"),
      arg = name
    )
  }

  validate_moments_inner_dims(stats, maturities, n_components, j_rows)
}

#' Validate the Theta-Axis Dimensions of Each Moment Element
#'
#' @param stats The seven-element moment list under validation.
#' @param maturities Integer maturity vector defining the constraint axis.
#' @param n_components Integer width of the theta axis.
#' @param j_rows Integer row count read off \code{stats$r_i_0}.
#' @noRd
validate_moments_inner_dims <- function(stats, maturities,
                                        n_components, j_rows) {
  for (k in seq_along(maturities)) {
    label <- paste0(" for maturity ", maturities[k])
    for (name in c("r_i_1", "s_i_2")) {
      assert_bad_argument_ok(
        is.numeric(stats[[name]][[k]]),
        paste0(name, label, " must be numeric"),
        arg = name
      )
    }
    assert_dimension_ok(
      is.matrix(stats$r_i_1[[k]]) &&
        nrow(stats$r_i_1[[k]]) == j_rows &&
        ncol(stats$r_i_1[[k]]) == n_components,
      paste0("r_i_1", label, " must be a J x n_components matrix")
    )
    assert_dimension_ok(
      is.numeric(stats$s_i_1[[k]]) && is.null(dim(stats$s_i_1[[k]])) &&
        length(stats$s_i_1[[k]]) == n_components,
      paste0("s_i_1", label, " must be a length n_components vector")
    )
    assert_dimension_ok(
      is.matrix(stats$s_i_2[[k]]) &&
        all(dim(stats$s_i_2[[k]]) == n_components),
      paste0("s_i_2", label, " must be n_components x n_components")
    )
  }

  invisible(TRUE)
}
