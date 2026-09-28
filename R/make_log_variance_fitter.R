#' Prepare Repeated Log-Variance Fits on a Fixed Design
#'
#' Validates the regressors and fitting controls once and returns a function
#' that fits new responses on that design. Response values, response scaling,
#' starts and fitted results are checked on every call. Numerical fitting is
#' shared with \code{\link{fit_log_variance}}.
#'
#' @inheritParams fit_log_variance
#' @return A function with arguments \code{y}, \code{start = NULL},
#'   \code{fallback_starts = list()} and \code{response_scale = 1}, following
#'   the contracts of \code{\link{fit_log_variance}} and returning the same
#'   validated \code{hetid_log_variance_fit} container. The design and controls
#'   are captured when this function is created. Changing the caller's original
#'   objects afterward does not change the fitter.
#' @export
#' @examples
#' x <- matrix(seq(-1, 1, length.out = 20), ncol = 1)
#' fitter <- make_log_variance_fitter(x)
#' fitter(exp(0.2 + 0.3 * x[, 1]))$coef
make_log_variance_fitter <- function(x, estimator = "ppml", control = list()) {
  assert_tabular(x, "x")
  x <- as.matrix(x)
  assert_numeric_finite_values(x, "x")
  min_obs <- min_obs_for_pc_regression(ncol(x))
  assert_insufficient_data_ok(
    nrow(x) >= min_obs,
    paste0(
      "Insufficient observations for the log-variance fit: got ", nrow(x),
      ", need at least ", min_obs, " (ncol(x) + 2)"
    )
  )
  spec <- log_variance_estimator(estimator)
  control <- log_variance_fit_control(spec$id, control)
  x_mat <- log_variance_design(x)
  design <- log_variance_fixed_design(x_mat, spec$id, control)
  function(y, start = NULL, fallback_starts = list(), response_scale = 1) {
    validate_log_variance_response(y, design$n_obs, response_scale)
    validate_log_variance_starts(
      start, fallback_starts, design$labels,
      control$SKIP_NONFINITE_STARTS
    )
    fit <- spec$fit_response(
      y, x_mat, start, fallback_starts, response_scale,
      control, design
    )
    fit$diagnostics$fit_control <- control
    fit
  }
}

# Quantities determined only by the validated, fixed design and controls
log_variance_fixed_design <- function(x_mat, estimator, control) {
  harvey <- identical(estimator, "harvey")
  list(
    n_obs = nrow(x_mat), labels = colnames(x_mat),
    col_abs = colSums(abs(x_mat)),
    rank = if (harvey) {
      qr(x_mat, tol = control$RANK_TOLERANCE)$rank
    } else {
      ppml_pos_rank(rep(1, nrow(x_mat)), x_mat, control)
    },
    chol_xx = if (harvey) {
      tryCatch(chol(crossprod(x_mat)), error = function(cond) NULL)
    } else {
      NULL
    }
  )
}

validate_log_variance_response <- function(y, n_obs, response_scale) {
  validate_numeric_inputs(y = y)
  assert_numeric_finite_values(y, "y")
  assert_bad_argument_ok(all(y >= 0), "y must be nonnegative", arg = "y")
  assert_dimension_ok(length(y) == n_obs, "x must have length(y) rows")
  assert_scalar_finite(response_scale, "response_scale")
  assert_bad_argument_ok(response_scale > 0, "response_scale must be positive",
    arg = "response_scale"
  )
}

validate_log_variance_starts <- function(start, fallback_starts, labels, skip_nonfinite) {
  if (!is.null(start)) {
    assert_log_variance_start(start, length(labels), labels, "start", skip_nonfinite)
  }
  assert_bad_argument_ok(is.list(fallback_starts), "fallback_starts must be a list",
    arg = "fallback_starts"
  )
  for (i in seq_along(fallback_starts)) {
    assert_log_variance_start(
      fallback_starts[[i]], length(labels), labels,
      paste0("fallback_starts[[", i, "]]"), skip_nonfinite
    )
  }
}
