#' The hetid_theta_box Container
#'
#' Constructor and validator for the identified-set box at a slack
#' \eqn{\tau}. The box carries the reduced-form pieces it was built from,
#' so a downstream profile cannot be run against a different system by
#' accident.
#'
#' @name hetid_theta_box
#' @keywords internal
NULL

#' Construct a hetid_theta_box Object
#'
#' Low-level cheap constructor. It checks the identity attributes and
#' trusts the shapes of the bounds and witnesses themselves; the full
#' sweep lives in \code{validate_hetid_theta_box()}, which the public
#' boundary \code{compute_identified_set_box()} always runs. Hot paths
#' rebuilding a box from known-good parts may call this directly and skip
#' it.
#'
#' @param bounds Data frame with \code{coef}, \code{lower}, \code{upper}
#' @param arg_lower,arg_upper Numeric I x I matrices whose row k holds the
#'   theta that attains the bound for coordinate k, or \code{NA} when that
#'   bound is infinite or was never reached
#' @param w1,w2 Reduced-form pieces the box was built from
#' @param quadratic Quadratic form list at this slack
#' @param tau Scalar slack
#' @param n_grid Points per gridded coordinate used by the search
#' @param n_obs Observation count
#' @return A \code{hetid_theta_box} object
#' @keywords internal
new_hetid_theta_box <- function(bounds, arg_lower, arg_upper, w1, w2,
                                quadratic, tau, n_grid, n_obs) {
  assert_scalar_finite(tau, "tau")
  assert_scalar_integer_in_range(n_grid, "n_grid", 2, .Machine$integer.max)
  assert_scalar_integer_in_range(n_obs, "n_obs", 1, .Machine$integer.max)

  structure(
    list(
      bounds = bounds,
      arg_lower = arg_lower,
      arg_upper = arg_upper,
      w1 = w1,
      w2 = w2,
      quadratic = quadratic
    ),
    tau = tau,
    n_components = ncol(w2),
    n_grid = as.integer(n_grid),
    n_obs = as.integer(n_obs),
    class = "hetid_theta_box"
  )
}

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
  validate_theta_box_bounds(x$bounds, n_components)
  validate_theta_box_witnesses(x, n_components)
  validate_theta_box_sources(x, n_components)
  invisible(x)
}

#' Validate the Bounds Frame
#'
#' @param bounds Data frame of bounds
#' @param n_components Theta-axis dimension
#' @return Invisibly TRUE
#' @noRd
validate_theta_box_bounds <- function(bounds, n_components) {
  assert_bad_argument_ok(
    is.data.frame(bounds) &&
      identical(names(bounds), c("coef", "lower", "upper")),
    "bounds must be a data frame with columns coef, lower and upper",
    arg = "bounds"
  )
  assert_dimension_ok(
    nrow(bounds) == n_components,
    paste0(
      "bounds must have one row per component: rows = ", nrow(bounds),
      "; n_components = ", n_components
    )
  )
  assert_bad_argument_ok(
    !anyNA(bounds$coef) && !anyDuplicated(bounds$coef),
    "bounds$coef must be non-missing and unique",
    arg = "bounds"
  )
  finite_both <- is.finite(bounds$lower) & is.finite(bounds$upper)
  assert_bad_argument_ok(
    all(bounds$lower[finite_both] <= bounds$upper[finite_both]),
    "every finite bounds row must satisfy lower <= upper",
    arg = "bounds"
  )
  invisible(TRUE)
}

#' Validate the Attaining Witnesses
#'
#' A finite bound must name the theta that attains it, because a box
#' corner is generally not a member of the set and cannot stand in.
#'
#' @param x A \code{hetid_theta_box} object
#' @param n_components Theta-axis dimension
#' @return Invisibly TRUE
#' @noRd
validate_theta_box_witnesses <- function(x, n_components) {
  for (nm in c("arg_lower", "arg_upper")) {
    assert_dimension_ok(
      is.matrix(x[[nm]]) &&
        identical(dim(x[[nm]]), c(n_components, n_components)),
      paste0(
        nm, " must be an n_components x n_components matrix: got ",
        paste(dim(x[[nm]]), collapse = " x "),
        "; n_components = ", n_components
      )
    )
  }
  finite_lower <- is.finite(x$bounds$lower)
  finite_upper <- is.finite(x$bounds$upper)
  assert_bad_argument_ok(
    !anyNA(x$arg_lower[finite_lower, , drop = FALSE]) &&
      !anyNA(x$arg_upper[finite_upper, , drop = FALSE]),
    "every finite bound must carry the theta attaining it",
    arg = "arg_lower"
  )
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
