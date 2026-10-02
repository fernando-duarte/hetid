#' Log-Projection OLS Operator
#'
#' Builds the fixed OLS operator \eqn{P = (V'V)^{-1} V'} of a regression on
#' an intercept and \code{x} from a thin, rank-revealing QR factorization of
#' the design, never from an explicit inverse of \eqn{V'V}.
#'
#' @param x Numeric matrix of finite regressors without an intercept, one
#'   row per observation. Zero columns give the intercept-only design.
#'   Unnamed columns are named by \code{\link{get_pc_column_names}}.
#' @param tol Positive rank tolerance passed to \code{\link[base]{qr}}.
#'
#' @return A plain \eqn{p \times T} numeric matrix with rows named by the
#'   design columns, \code{"(Intercept)"} first: a drop-in replacement for
#'   \code{solve(crossprod(V), t(V))}. A rank-deficient design signals
#'   \code{hetid_error_bad_argument}; fewer than \eqn{p + 1} rows signal
#'   \code{hetid_error_insufficient_data}.
#' @examples
#' x <- cbind(v1 = c(-1, 0, 1, 2), v2 = c(1, -1, 0, 1))
#' p <- log_projection_matrix(x)
#' drop(p %*% c(0.1, 0.4, 0.2, 0.3))
#' @export
log_projection_matrix <- function(x,
                                  tol = LOG_PROJECTION_CONTROL$RANK_TOLERANCE) {
  log_projection_factor(x, tol)$projection
}

# The operator and the reciprocal condition number of its triangular QR
# factor; prepare_log_projection records both
log_projection_factor <- function(x, tol) {
  x <- as.matrix(x)
  assert_numeric_finite_values(x, "x")
  assert_scalar_finite(tol, "tol")
  assert_bad_argument_ok(tol > 0, "tol must be positive", arg = "tol")
  design <- log_variance_design(x)
  assert_insufficient_data_ok(
    nrow(design) > ncol(design),
    sprintf("need more than %d observations for the volatility design", ncol(design))
  )
  qr_v <- qr(design, tol = tol)
  assert_bad_argument_ok(
    qr_v$rank == ncol(design),
    "the volatility design, including its intercept, must have full column rank",
    arg = "x"
  )
  r_factor <- qr.R(qr_v)
  projection <- matrix(NA_real_, ncol(design), nrow(design))
  projection[qr_v$pivot, ] <- backsolve(r_factor, t(qr.Q(qr_v)))
  dimnames(projection) <- list(colnames(design), NULL)
  list(projection = projection, rcond = rcond(r_factor, triangular = TRUE))
}
