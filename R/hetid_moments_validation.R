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
#' Full structural-alignment gate for the \code{hetid_moments} class,
#' checked against the object's own attributes. Run by the public
#' boundary \code{compute_identification_moments()} on every object it
#' returns; call it directly on containers assembled via
#' \code{new_hetid_moments()} from parts that are not known-good.
#'
#' @param x A classed \code{hetid_moments} object
#' @return \code{x}, invisibly
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
#' @noRd
validate_moments_inner_dims <- function(stats, maturities,
                                        n_components, j_rows) {
  for (k in seq_along(maturities)) {
    label <- paste0(" for maturity ", maturities[k])
    assert_dimension_ok(
      is.matrix(stats$r_i_1[[k]]) &&
        nrow(stats$r_i_1[[k]]) == j_rows &&
        ncol(stats$r_i_1[[k]]) == n_components,
      paste0("r_i_1", label, " must be a J x n_components matrix")
    )
    assert_dimension_ok(
      is.numeric(stats$s_i_1[[k]]) &&
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
