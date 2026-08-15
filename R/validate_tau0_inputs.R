#' Validate compute_tau0_system Inputs
#'
#' Internal validator for the tau = 0 mean-equation orchestrator: checks
#' shape and finiteness of every input, applies the axis-name contract to
#' \code{z} and \code{y2}, resolves \code{gamma}, and de-means \code{z}.
#' Never restates the \code{x} contract (no intercept column, no column
#' named "y") documented on \code{\link{compute_tau0_system}}: those are
#' caught for free downstream by \code{\link{run_pc_regression}}.
#'
#' @param y1 Numeric vector, the mean-equation outcome
#' @param y2 Numeric matrix or vector, the news/innovation variables
#' @param x Numeric matrix or vector, the common conditioning regressors
#' @param z Numeric matrix or vector, the instrument(s)
#' @param gamma NULL or a numeric matrix; see
#'   \code{\link{compute_tau0_system}} for the defaulting rule
#' @param impose_null Logical flag
#' @param tol Positive numeric scalar
#'
#' @return \code{list(y1, y2, x, z, gamma, n_obs)}: \code{y2}/\code{x}/\code{z}
#'   coerced to matrices, \code{z} de-meaned with normalized column names,
#'   \code{gamma} resolved to a \code{ncol(z) x ncol(y2)} matrix
#' @keywords internal
validate_tau0_inputs <- function(y1, y2, x, z, gamma, impose_null, tol) {
  assert_flag(impose_null, "impose_null")
  assert_scalar_finite(tol, "tol")
  assert_bad_argument_ok(tol > 0, "tol must be positive", arg = "tol")

  validate_numeric_inputs(y1 = y1)
  y2 <- as.matrix(y2)
  x <- as.matrix(x)
  z <- as.matrix(z)
  assert_bad_argument_ok(ncol(y2) >= 1, "y2 must have at least one column", arg = "y2")
  assert_bad_argument_ok(ncol(x) >= 1, "x must have at least one column", arg = "x")
  assert_bad_argument_ok(ncol(z) >= 1, "z must have at least one column", arg = "z")
  assert_numeric_finite_values(y1, "y1")
  assert_numeric_finite_values(y2, "y2")
  assert_numeric_finite_values(x, "x")
  assert_numeric_finite_values(z, "z")

  n_obs <- length(y1)
  assert_dimension_ok(nrow(y2) == n_obs, "y2 must have length(y1) rows")
  assert_dimension_ok(nrow(x) == n_obs, "x must have length(y1) rows")
  assert_dimension_ok(nrow(z) == n_obs, "z must have length(y1) rows")
  min_obs <- min_obs_for_pc_regression(ncol(x))
  assert_insufficient_data_ok(
    n_obs >= min_obs,
    paste0(
      "Insufficient observations for the tau=0 system: got ", n_obs,
      ", need at least ", min_obs, " (ncol(x) + 2)"
    )
  )

  z <- sweep(z, 2, colMeans(z))
  if (is.null(colnames(z))) {
    colnames(z) <- paste0("z", seq_len(ncol(z)))
  }
  assert_instrument_names(colnames(z), "z")
  assert_instrument_names(colnames(y2), "y2")

  gamma <- resolve_tau0_gamma(z, y2, gamma)

  list(y1 = y1, y2 = y2, x = x, z = z, gamma = gamma, n_obs = n_obs)
}

#' Resolve and Validate the gamma Argument
#'
#' @param z De-meaned, name-normalized instrument matrix
#' @param y2 Coerced y2 matrix
#' @param gamma NULL or a candidate \code{ncol(z) x ncol(y2)} matrix
#' @return The resolved gamma matrix
#' @noRd
resolve_tau0_gamma <- function(z, y2, gamma) {
  if (is.null(gamma)) {
    assert_bad_argument_ok(
      ncol(z) == 1,
      paste0(
        "gamma is required (not defaulted) when ncol(z) > 1: an implicit ",
        "equal-weight instrument direction is units-dependent and ",
        "silently changes the estimand"
      ),
      arg = "gamma"
    )
    return(matrix(1, 1, ncol(y2)))
  }
  assert_bad_argument_ok(
    is.matrix(gamma) && is.numeric(gamma), "gamma must be a numeric matrix",
    arg = "gamma"
  )
  assert_dimension_ok(
    nrow(gamma) == ncol(z) && ncol(gamma) == ncol(y2),
    "gamma must be a ncol(z) x ncol(y2) matrix"
  )
  gdn <- dimnames(gamma)
  if (!is.null(gdn)) {
    assert_bad_argument_ok(
      !is.null(gdn[[1]]) && !is.null(gdn[[2]]) &&
        identical(gdn[[1]], colnames(z)) && identical(gdn[[2]], colnames(y2)),
      paste0(
        "gamma dimnames, when present, must equal colnames(z) and ",
        "colnames(y2) exactly"
      ),
      arg = "gamma"
    )
  }
  gamma
}
