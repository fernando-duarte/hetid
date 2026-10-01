#' Validate a hetid_tau0_fit Object
#'
#' Checks attributes, finite residuals and instruments, coefficient alignment,
#' nested moment shapes and counts, finite point theta, and point/beta1 pairing.
#' The public boundary \code{\link{compute_tau0_system}} always runs it; use it
#' on manually assembled containers too.
#'
#' @param x A classed \code{hetid_tau0_fit} object.
#' @return \code{x}, invisibly; failures signal a structured \code{hetid_error}.
#' @details Nested moments are fully validated. Their \code{n_components}
#' must match \code{ncol(w2)}, both \code{n_instruments} and the row count of
#' \code{r_i_0} must match \code{ncol(z)}, and \code{n_obs} must match the fit.
#' Reordered maturity subsets are allowed; moments are not recomputed.
#' @keywords internal
validate_hetid_tau0_fit <- function(x) {
  assert_hetid_tau0_fit(x, arg = "x")
  n_obs <- attr(x, "n_obs")
  assert_scalar_integer_in_range(n_obs, "n_obs", 1, .Machine$integer.max)
  assert_flag(attr(x, "impose_null"), "impose_null")
  tol <- attr(x, "tol")
  assert_scalar_finite(tol, "tol")
  assert_bad_argument_ok(tol > 0, "tol must be positive", arg = "tol")
  dims <- validate_tau0_fit_data_shapes(x, n_obs)
  validate_tau0_fit_betas(x, dims)
  validate_hetid_moments(x$moments)
  assert_dimension_ok(
    attr(x$moments, "n_components") == dims$i_dim,
    "moments n_components must equal ncol(w2)"
  )
  assert_dimension_ok(
    attr(x$moments, "n_instruments") == dims$j_dim,
    "moments n_instruments must equal ncol(z)"
  )
  assert_dimension_ok(
    nrow(x$moments$r_i_0) == dims$j_dim,
    "moments r_i_0 must have ncol(z) rows"
  )
  assert_dimension_ok(
    attr(x$moments, "n_obs") == n_obs,
    "moments n_obs must equal the fit n_obs"
  )
  validate_tau0_fit_point(x, dims)
  invisible(x)
}

#' Validate Finite w1, w2, z, and Their Shapes Against n_obs
#' @param x A classed \code{hetid_tau0_fit} object.
#' @param n_obs Number of observations the fit was computed from.
#' @return \code{list(i_dim, j_dim)} read off \code{w2} and \code{z}.
#' @noRd
validate_tau0_fit_data_shapes <- function(x, n_obs) {
  assert_bad_argument_ok(
    is.numeric(x$w1) && is.null(dim(x$w1)) && all(is.finite(x$w1)),
    "w1 must be a finite numeric vector",
    arg = "w1"
  )
  assert_dimension_ok(length(x$w1) == n_obs, "w1 must have length n_obs")
  assert_bad_argument_ok(
    is.matrix(x$w2) && is.numeric(x$w2) && all(is.finite(x$w2)),
    "w2 must be a finite numeric matrix",
    arg = "w2"
  )
  assert_dimension_ok(nrow(x$w2) == n_obs, "w2 must have n_obs rows")
  assert_bad_argument_ok(
    is.matrix(x$z) && is.numeric(x$z) && all(is.finite(x$z)),
    "z must be a finite numeric matrix",
    arg = "z"
  )
  assert_dimension_ok(nrow(x$z) == n_obs, "z must have n_obs rows")

  i_dim <- ncol(x$w2)
  j_dim <- ncol(x$z)
  assert_bad_argument_ok(
    is.matrix(x$gamma) && is.numeric(x$gamma), "gamma must be a numeric matrix",
    arg = "gamma"
  )
  assert_dimension_ok(
    nrow(x$gamma) == j_dim && ncol(x$gamma) == i_dim,
    "gamma must be a J x I matrix matching ncol(z) and ncol(w2)"
  )
  list(i_dim = i_dim, j_dim = j_dim)
}
