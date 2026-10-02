#' Prepare a Log Projection of Squared Residuals
#'
#' Fixes everything a log projection reuses across candidates: the mean and
#' volatility samples and their identifier row map, the volatility
#' regressors centered over the volatility sample, the QR-based OLS operator,
#' the common residual scale, and the unrestricted lower bound on the Fuller
#' scale.
#'
#' @param w1 Numeric vector of the outcome's auxiliary residuals on the mean
#'   sample (from a regression with an intercept, so mean zero there).
#' @param w2 Numeric matrix of the news regressors' auxiliary residuals on
#'   the mean sample, one column per news coefficient, same rows as \code{w1}.
#' @param x_var Numeric matrix or data frame of raw volatility regressors on
#'   the volatility sample, without an intercept; centered here.
#' @param mean_ids,volatility_ids Unique, non-missing observation identifiers
#'   of one atomic class (for example \code{Date} or
#'   \code{tsibble::yearquarter}), one per row of \code{w1} and of
#'   \code{x_var}. Every volatility identifier must occur in
#'   \code{mean_ids}; rows are matched by identifier, never by position.
#'   Identifiers are a within-call join key: a replay on resampled rows,
#'   which repeats quarters, passes positional integer identifiers
#'   (\code{seq_along}) on both sides after aligning the rows itself.
#'
#' @return A \code{hetid_log_projection_prep} object for
#'   \code{\link{evaluate_log_projection}}.
#' @details
#' The mean-sample residuals are not recentered or refitted on the
#' volatility sample. \code{log_scale_common} is \eqn{\log \hat s^2}, the log
#' mean square of \code{w1} over the mean sample (\code{-Inf} when \code{w1}
#' is zero). \code{log_scale_lower} is the log of the unrestricted
#' least-squares lower bound on every candidate's mean-sample scale, and
#' \code{scale_lower_certified} reports whether it is strictly positive
#' relative to \code{LOG_PROJECTION_CONTROL$SCALE_TOLERANCE}. Numerically
#' redundant news columns are accepted, but they make the bound zero
#' (\code{-Inf} on the log scale), so the scale is then uncertified.
#' @seealso \code{\link{evaluate_log_projection}},
#'   \code{\link{LOG_PROJECTION_CONTROL}}
#' @export
prepare_log_projection <- function(w1, w2, x_var, mean_ids, volatility_ids) {
  ctrl <- LOG_PROJECTION_CONTROL
  x <- validate_log_projection_inputs(w1, w2, x_var, mean_ids, volatility_ids)
  rows <- match(volatility_ids, mean_ids)
  center <- vapply(seq_len(ncol(x)), function(j) mean(x[, j]), numeric(1))
  names(center) <- colnames(x)
  x_centered <- sweep(x, 2L, center)
  # a rank-truncated QR minimizes over a smaller span, so its residual is not
  # a lower bound; certification then fails conservatively
  qr_w <- qr(w2, tol = ctrl$RANK_TOLERANCE)
  log_lower <- if (qr_w$rank == ncol(w2)) {
    log_mean_square_cols(qr.resid(qr_w, w1))
  } else {
    -Inf
  }
  log_common <- log_mean_square_cols(w1)
  qr_parts <- log_projection_factor(x_centered, ctrl$RANK_TOLERANCE)
  fields <- list(
    projection = qr_parts$projection, projection_rcond = qr_parts$rcond,
    x_centered = x_centered, x_center = center,
    w1 = w1[rows], w2 = w2[rows, , drop = FALSE], w1_mean = w1,
    w2_mean = w2, mean_ids = mean_ids, volatility_ids = volatility_ids,
    volatility_rows = rows, log_scale_common = log_common,
    log_scale_lower = log_lower,
    scale_lower_certified = is.finite(log_lower) && is.finite(log_common) &&
      log_lower - log_common > log(ctrl$SCALE_TOLERANCE),
    rank_tolerance = ctrl$RANK_TOLERANCE
  )
  validate_hetid_log_projection_prep(
    new_hetid_log_projection_prep(fields, length(w1), length(rows))
  )
}

# Validates the raw inputs and returns x_var as a named numeric matrix
validate_log_projection_inputs <- function(w1, w2, x_var, mean_ids,
                                           volatility_ids) {
  assert_bad_argument_ok(
    is.numeric(w1) && is.null(dim(w1)) && length(w1) >= 1L,
    "w1 must be a numeric vector",
    arg = "w1"
  )
  assert_numeric_finite_values(w1, "w1")
  assert_bad_argument_ok(
    is.matrix(w2) && is.numeric(w2) && ncol(w2) >= 1L,
    "w2 must be a numeric matrix with at least one column",
    arg = "w2"
  )
  assert_numeric_finite_values(w2, "w2")
  assert_dimension_ok(nrow(w2) == length(w1), "nrow(w2) must equal length(w1)")
  tol <- LOG_PROJECTION_CONTROL$MEAN_ZERO_TOLERANCE
  assert_bad_argument_ok(
    log_projection_mean_zero(w1, tol) && log_projection_mean_zero(w2, tol),
    "w1 and w2 must be intercept-regression residuals (mean zero on the mean sample)",
    arg = "w1"
  )
  x <- as.matrix(x_var)
  assert_numeric_finite_values(x, "x_var")
  if (is.null(colnames(x))) colnames(x) <- get_pc_column_names(ncol(x))
  validate_log_projection_ids(mean_ids, volatility_ids, length(w1), nrow(x))
  assert_insufficient_data_ok(
    nrow(x) >= min_obs_for_pc_regression(ncol(x)),
    sprintf(
      "the volatility sample needs at least %d rows",
      min_obs_for_pc_regression(ncol(x))
    )
  )
  x
}

validate_log_projection_ids <- function(mean_ids, volatility_ids, n_mean,
                                        n_vol) {
  ok_ids <- function(ids, n) {
    is.atomic(ids) && is.null(dim(ids)) && length(ids) == n &&
      !anyNA(ids) && !anyDuplicated(ids)
  }
  assert_bad_argument_ok(
    ok_ids(mean_ids, n_mean),
    "mean_ids must be unique non-missing identifiers, one per row of w1",
    arg = "mean_ids"
  )
  assert_bad_argument_ok(
    ok_ids(volatility_ids, n_vol),
    "volatility_ids must be unique non-missing identifiers, one per row of x_var",
    arg = "volatility_ids"
  )
  assert_bad_argument_ok(
    identical(class(mean_ids), class(volatility_ids)),
    "mean_ids and volatility_ids must have the same class",
    arg = "volatility_ids"
  )
  n_missing <- sum(is.na(match(volatility_ids, mean_ids)))
  assert_bad_argument_ok(
    n_missing == 0L,
    sprintf("%d volatility_ids do not occur in mean_ids", n_missing),
    arg = "volatility_ids"
  )
  invisible(TRUE)
}
