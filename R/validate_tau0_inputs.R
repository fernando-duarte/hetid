#' Validate Tau = 0 System Inputs
#'
#' Checks and prepares inputs for \code{\link{compute_tau0_system}}:
#' validates the observation arrays, column names, flag, and tolerance,
#' resolves the instrument weights, and de-means the instruments.
#'
#' @details
#' The observation arrays must contain only finite numeric values; missing
#' rows are rejected rather than omitted. Each of \code{y2}, \code{x}, and
#' \code{z} must have at least one column and \code{length(y1)} rows. At least
#' \code{min_obs_for_pc_regression(ncol(x))} observations are required.
#'
#' The \code{x} contract (no intercept column and no column named "y")
#' described in \code{\link{compute_tau0_system}} is enforced downstream
#' by \code{\link{run_pc_regression}}. Finiteness of supplied \code{gamma}
#' values is checked downstream by
#' \code{\link{compute_identified_set_components}}.
#'
#' @param y1 Numeric vector containing the mean-equation outcome.
#' @param y2 Numeric matrix or data frame containing the news/innovation
#'   variables, with unique, non-empty, non-missing column names.
#' @param x Numeric matrix, vector, or data frame containing the common
#'   conditioning regressors.
#' @param z Numeric matrix, vector, or data frame containing the instruments.
#'   Absent column names are assigned as \code{z1}, \code{z2}, and so on;
#'   supplied names must be unique, non-empty, and non-missing.
#' @param gamma A numeric \code{ncol(z) x ncol(y2)} matrix of instrument
#'   weights, or \code{NULL} to use unit weights when \code{z} has one column.
#'   Weights must be supplied for multiple instruments. If dimnames are
#'   present, both axes must match \code{colnames(z)} and
#'   \code{colnames(y2)} exactly, after instrument names are assigned.
#' @param impose_null Logical scalar, either \code{TRUE} or \code{FALSE}.
#'   Validated here; the reduced-form restriction is applied by the caller.
#' @param tol Positive, finite numeric scalar for the point-solve tolerance.
#'
#' @return A named list containing the prepared inputs:
#'   \describe{
#'     \item{y1}{The unchanged outcome vector.}
#'     \item{y2, x}{The news/innovation and conditioning-regressor matrices.}
#'     \item{z}{The de-meaned instrument matrix with validated column names.}
#'     \item{gamma}{The supplied weight matrix, or an unnamed one-row matrix
#'       of unit weights with one column per news/innovation variable.}
#'     \item{n_obs}{The number of observations, \code{length(y1)}.}
#'   }
#'   Validation failures signal structured \code{hetid_error} conditions.
#' @keywords internal
validate_tau0_inputs <- function(y1, y2, x, z, gamma, impose_null, tol) {
  assert_flag(impose_null, "impose_null")
  assert_scalar_finite(tol, "tol")
  assert_bad_argument_ok(tol > 0, "tol must be positive", arg = "tol")

  validate_numeric_inputs(y1 = y1)
  matrix_inputs <- list(y2 = y2, x = x, z = z)
  for (name in names(matrix_inputs)) {
    assert_bad_argument_ok(
      is.atomic(matrix_inputs[[name]]) || is.data.frame(matrix_inputs[[name]]),
      paste0(name, " must be a numeric matrix, vector, or data frame"),
      arg = name
    )
  }
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

  # Use mean() per instrument because colMeans() roundoff can disturb balanced deviations
  z_means <- vapply(seq_len(ncol(z)), function(j) mean(z[, j]), numeric(1))
  z <- sweep(z, 2, z_means)
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
#' @param z De-meaned instrument matrix with validated column names.
#' @param y2 News/innovation matrix with validated column names.
#' @param gamma A numeric \code{ncol(z) x ncol(y2)} weight matrix, or
#'   \code{NULL} when \code{z} has one column. Supplied dimnames must match
#'   both input axes exactly.
#' @return The supplied weight matrix, or an unnamed one-row matrix of unit
#'   weights with \code{ncol(y2)} columns. Invalid type, shape, or dimnames
#'   signal a structured \code{hetid_error} condition.
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
