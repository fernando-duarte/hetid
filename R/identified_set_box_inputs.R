#' Inputs of the Identified-Set Box Search
#'
#' Internals of \code{compute_identified_set_box()}: the check on the fit
#' the structural block relies on, the search center, the linear
#' functionals the sweep bounds, the component labels, and how one block
#' of the sweep's result is read back into a bounds frame.
#'
#' @name identified_set_box_inputs
#' @keywords internal
NULL

#' Check the Fit the Structural Block Relies On
#'
#' \code{assert_hetid_tau0_fit()} is a class check only. The structural
#' block indexes \code{names(beta1r)}, stacks \code{beta2r} beside the
#' identity and maps witnesses positionally through \code{beta2r}, so the
#' full container sweep runs here, plus the two facts it does not cover:
#' finite coefficients, and \code{beta2r} rows in the order of the
#' \code{w2} columns. A fit from \code{compute_tau0_system()} always
#' passes; a hand-modified one fails with a structured condition rather
#' than in a \code{data.frame()} call or, worse, by silently bounding the
#' wrong map.
#'
#' @param fit A \code{hetid_tau0_fit}
#' @return Invisibly TRUE
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
#' @param fit A \code{hetid_tau0_fit}
#' @param center Optional caller-supplied center
#' @param quadratic Quadratic form list at this slack
#' @param n_components Theta-axis dimension
#' @return Numeric length-I feasible center
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
#' when the bounds are read. A loading column that is zero up to rounding
#' is snapped to exact zero, so the coefficient it belongs to is reported
#' as the point it is (an intercept when both blocks are centered) rather
#' than as an interval of rounding width, or as unbounded when the set is.
#' The tolerance is relative to each row's own largest loading, since a
#' row of \eqn{\beta_2^R} is one regression's coefficient vector and that
#' is the noise floor of its zeros; rescaling a column of \eqn{Y_2} then
#' changes nothing. Under \code{impose_null} every loading is already zero.
#'
#' @param fit A \code{hetid_tau0_fit}
#' @param n_components Theta-axis dimension
#' @param null_loading_rtol Scalar in \code{[0, 1)}; a loading column with
#'   no entry above this fraction of its row's largest loading is snapped
#'   to zero, and \code{0} snaps only exact zeros
#' @return Numeric I x (I + p) matrix
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
#' @param coef Character labels, one per row of the block
#' @param offset Numeric, added to the sweep's bounds (0 for theta,
#'   \code{beta1r} for the structural block); infinite bounds pass through
#'   untouched
#' @param found Search state after \code{apply_recession_bounds()}
#' @param rows Integer indices of the block's objectives
#' @return Data frame with \code{coef}, \code{lower}, \code{upper}
#' @noRd
identified_set_bounds_frame <- function(coef, offset, found, rows) {
  data.frame(
    coef = coef,
    lower = offset + found$lower[rows],
    upper = offset + found$upper[rows],
    row.names = NULL
  )
}

#' Component Labels for the Bounds Frame
#'
#' @param w2 Reduced-form news matrix
#' @param n_components Theta-axis dimension
#' @return Character vector of component labels
#' @noRd
theta_box_labels <- function(w2, n_components) {
  coef_labels <- colnames(w2)
  if (is.null(coef_labels)) {
    coef_labels <- maturity_names(seq_len(n_components))
  }
  coef_labels
}
