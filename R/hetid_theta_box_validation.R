#' Validation of hetid_theta_box Objects
#'
#' Full shape sweep behind \code{validate_hetid_theta_box()}. The theta
#' block and the structural (beta1) block share one bounds check and one
#' witness check, run once per block, followed by the zero-loading flag
#' and the retained-source check.
#'
#' @name hetid_theta_box_validation
#' @keywords internal
NULL

#' Validate a hetid_theta_box Object
#'
#' @param x A \code{hetid_theta_box} object
#' @return \code{x}, invisibly
#' @keywords internal
validate_hetid_theta_box <- function(x) {
  assert_bad_argument_ok(
    inherits(x, "hetid_theta_box"),
    "x must be a hetid_theta_box object",
    arg = "x"
  )
  n_components <- attr(x, "n_components")
  validate_theta_box_bounds(x$bounds, "bounds")
  assert_dimension_ok(
    nrow(x$bounds) == n_components,
    paste0(
      "bounds must have one row per component: rows = ", nrow(x$bounds),
      "; n_components = ", n_components
    )
  )
  validate_theta_box_witnesses(
    x$bounds, x$arg_lower, x$arg_upper, n_components,
    c("arg_lower", "arg_upper")
  )
  validate_theta_box_bounds(x$beta1_bounds, "beta1_bounds")
  validate_theta_box_witnesses(
    x$beta1_bounds, x$beta1_arg_lower, x$beta1_arg_upper, n_components,
    c("beta1_arg_lower", "beta1_arg_upper")
  )
  null_loading <- attr(x, "null_loading")
  assert_bad_argument_ok(
    is.logical(null_loading) && !anyNA(null_loading) &&
      identical(names(null_loading), x$beta1_bounds$coef),
    "null_loading must be a non-missing logical named by the beta1 rows",
    arg = "null_loading"
  )
  validate_theta_box_sources(x, n_components)
  invisible(x)
}

#' Validate One Bounds Frame
#'
#' Bounds are extremes over feasible points, so a lower bound is finite or
#' \code{-Inf}, an upper bound finite or \code{Inf}, and neither is ever
#' missing: the search is seeded from a feasible center.
#'
#' @param bounds Data frame of bounds
#' @param arg Element name used in the error message
#' @return Invisibly TRUE
#' @noRd
validate_theta_box_bounds <- function(bounds, arg) {
  assert_bad_argument_ok(
    is.data.frame(bounds) &&
      identical(names(bounds), c("coef", "lower", "upper")),
    paste0(arg, " must be a data frame with columns coef, lower and upper"),
    arg = arg
  )
  assert_bad_argument_ok(
    is.numeric(bounds$lower) && is.numeric(bounds$upper) &&
      !anyNA(bounds$lower) && !anyNA(bounds$upper),
    paste0(arg, "$lower and ", arg, "$upper must be numeric and non-missing"),
    arg = arg
  )
  assert_bad_argument_ok(
    !anyNA(bounds$coef) && !anyDuplicated(bounds$coef),
    paste0(arg, "$coef must be non-missing and unique"),
    arg = arg
  )
  assert_bad_argument_ok(
    all(bounds$lower <= bounds$upper) &&
      all(bounds$lower < Inf) && all(bounds$upper > -Inf),
    paste0(
      "every ", arg, " row must satisfy lower <= upper with lower below Inf ",
      "and upper above -Inf"
    ),
    arg = arg
  )
  invisible(TRUE)
}

#' Validate the Attaining Witnesses of One Block
#'
#' A finite bound must name the theta that attains it, because a box
#' corner is generally not a member of the set and cannot stand in.
#'
#' @param bounds The block's bounds frame
#' @param arg_lower,arg_upper The block's witness matrices
#' @param n_components Theta-axis dimension
#' @param args Character length two, the element names used in messages
#' @return Invisibly TRUE
#' @noRd
validate_theta_box_witnesses <- function(bounds, arg_lower, arg_upper,
                                         n_components, args) {
  n_rows <- nrow(bounds)
  witnesses <- list(arg_lower, arg_upper)
  finite <- list(is.finite(bounds$lower), is.finite(bounds$upper))
  for (k in 1:2) {
    assert_dimension_ok(
      is.matrix(witnesses[[k]]) && is.numeric(witnesses[[k]]) &&
        identical(dim(witnesses[[k]]), c(n_rows, n_components)),
      paste0(
        args[k], " must be a numeric ", n_rows, " x ", n_components,
        " matrix: got ", paste(dim(witnesses[[k]]), collapse = " x ")
      )
    )
    assert_bad_argument_ok(
      all(is.finite(witnesses[[k]][finite[[k]], , drop = FALSE])),
      paste0(
        "every finite bound must carry the theta attaining it: ", args[k],
        " has a missing or infinite row beside a finite bound"
      ),
      arg = args[k]
    )
  }
  invisible(TRUE)
}

#' Validate the Retained Reduced-Form Sources
#'
#' @param x A \code{hetid_theta_box} object
#' @param n_components Theta-axis dimension
#' @return Invisibly TRUE
#' @noRd
validate_theta_box_sources <- function(x, n_components) {
  assert_dimension_ok(
    is.matrix(x$w2) && ncol(x$w2) == n_components,
    paste0(
      "w2 must be a matrix with n_components columns: ncol = ",
      if (is.matrix(x$w2)) ncol(x$w2) else NA,
      "; n_components = ", n_components
    )
  )
  assert_dimension_ok(
    length(x$w1) == nrow(x$w2),
    paste0(
      "w1 and w2 must share rows: length(w1) = ", length(x$w1),
      "; nrow(w2) = ", nrow(x$w2)
    )
  )
  assert_bad_argument_ok(
    is.list(x$quadratic) &&
      all(c("A_i", "b_i", "c_i") %in% names(x$quadratic)),
    "quadratic must be a quadratic form list with A_i, b_i and c_i",
    arg = "quadratic"
  )
  invisible(TRUE)
}
