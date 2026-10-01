#' Inputs of the Identified-Set Box Search
#'
#' Internals of \code{compute_identified_set_box()}: the check on the fit
#' the structural block relies on, the search center, the linear
#' functionals the sweep bounds, and how one block of the sweep's result
#' is read back into a bounds frame.
#'
#' @name identified_set_box_inputs
#' @keywords internal
NULL

#' Check the Fit the Structural Block Relies On
#'
#' Runs \code{validate_hetid_tau0_fit()}, then checks that the reduced-form
#' coefficients are finite and the \code{beta2r} row names match the \code{w2}
#' column names. The structural map uses these coefficients in column order.
#'
#' @param fit A \code{hetid_tau0_fit} object with finite reduced-form
#'   coefficients and \code{beta2r} rows ordered as the \code{w2} columns.
#' @return \code{TRUE}, invisibly, when validation passes; otherwise a
#'   structured \code{hetid_error} is raised.
#' @noRd
validate_box_fit <- function(fit) {
  validate_hetid_tau0_fit(fit)
  assert_numeric_finite_values(fit$beta1r, "beta1r")
  assert_numeric_finite_values(fit$beta2r, "beta2r")
  assert_bad_argument_ok(
    identical(rownames(fit$beta2r), colnames(fit$w2)),
    "rownames(beta2r) must equal colnames(w2): the recovery map is positional",
    arg = "beta2r"
  )
  invisible(TRUE)
}

#' Resolve and Check the Search Center
#'
#' @param fit A \code{hetid_tau0_fit} object.
#' @param center A finite numeric vector of length \code{n_components},
#'   strictly inside every constraint, or \code{NULL} to use
#'   \code{fit$point$theta}. A missing fit point requires an explicit center.
#' @param quadratic A list with parallel \code{A_i}, \code{b_i}, and
#'   \code{c_i} elements at the requested slack.
#' @param n_components Number of theta components, equal to \code{ncol(fit$w2)}.
#' @return The numeric center of length \code{n_components}, with every
#'   constraint value strictly negative. Invalid centers raise a structured
#'   \code{hetid_error}; missing and non-finite values are not filtered.
#' @noRd
resolve_box_center <- function(fit, center, quadratic, n_components) {
  if (is.null(center)) {
    assert_bad_argument_ok(
      !is.null(fit$point),
      paste0(
        "fit carries no tau = 0 point to center the search on; ",
        "supply center explicitly"
      ),
      arg = "center"
    )
    center <- fit$point$theta
  }
  assert_dimension_ok(
    length(center) == n_components,
    paste0(
      "center must have one value per component: length = ", length(center),
      "; n_components = ", n_components
    )
  )
  assert_numeric_finite_values(center, "center")
  slack <- max(make_system_checker(quadratic)(center))
  assert_bad_argument_ok(
    slack < 0,
    paste0(
      "center is not strictly inside the set at this tau (largest ",
      "constraint value ", format(slack), "); supply a feasible center"
    ),
    arg = "center"
  )
  center
}

#' Linear Objectives of One Box Search
#'
#' The theta coordinates first, then the structural map
#' \eqn{\beta_1(\theta) = \beta_1^R - (\beta_2^R)'\theta}: its slope
#' columns are the objectives and the offset \eqn{\beta_1^R} is added back
#' when the bounds are read. Columns satisfying the relative loading threshold
#' are set to zero, so their structural coefficients are reported as constants.
#' The threshold uses each row's largest absolute loading; rescaling a Y2
#' component leaves the decision unchanged. Genuinely small loadings can also
#' be set to zero. Under \code{impose_null} every loading is already zero.
#'
#' @param fit A validated \code{hetid_tau0_fit} object.
#' @param n_components Number of theta components, equal to \code{ncol(fit$w2)}.
#' @param null_loading_rtol Numeric scalar in \code{[0, 1)}; a loading column with
#'   no entry above this fraction of its row's largest loading is snapped
#'   to zero, and \code{0} snaps only exact zeros.
#' @return An unnamed numeric matrix with \code{n_components} rows and
#'   \code{n_components + length(fit$beta1r)} columns. Theta-coordinate
#'   objectives precede structural-coefficient objectives in
#'   \code{names(fit$beta1r)} order; structural offsets are not included.
#' @noRd
identified_set_objectives <- function(fit, n_components, null_loading_rtol) {
  beta1_loadings <- -unname(fit$beta2r)
  row_scale <- apply(abs(beta1_loadings), 1, max) * null_loading_rtol
  null_col <- colSums(abs(beta1_loadings) > row_scale) == 0L
  beta1_loadings[, null_col] <- 0
  cbind(diag(n_components), beta1_loadings)
}

#' Bounds Frame for One Block of Objectives
#'
#' @param coef Character vector of labels, one per row of the block.
#' @param offset Finite numeric scalar or vector of length \code{length(rows)},
#'   added to the sweep's bounds (0 for theta,
#'   \code{beta1r} for the structural block); infinite bounds pass through
#'   untouched.
#' @param found Search-state list after \code{apply_recession_bounds()},
#'   with numeric \code{lower} and \code{upper} vectors.
#' @param rows Integer vector of indices of the block's objectives.
#' @return A data frame with character \code{coef} and numeric \code{lower}
#'   and \code{upper} columns, one row per selected objective in \code{rows}
#'   order, with automatic row names.
#' @noRd
identified_set_bounds_frame <- function(coef, offset, found, rows) {
  data.frame(
    coef = coef,
    lower = offset + found$lower[rows],
    upper = offset + found$upper[rows],
    row.names = NULL
  )
}
