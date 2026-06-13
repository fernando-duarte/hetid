#' Build lagged-outcome regressor columns
#'
#' Constructs the predetermined lag block for the W1 reduced form. Column
#' \code{h} holds \eqn{Y_{1,t+1-h}} at predictor row \code{t}: the outcome
#' shifted down by \code{h - 1} rows with \code{h - 1} leading \code{NA}s, so
#' that under the one-period lag/lead convention of
#' \code{\link{compute_w1_residuals}} (regressor row \code{t} paired with
#' \eqn{Y_{1,t+1}}) the column lines up with \eqn{Y_{1,t+1-h}}.
#'
#' @param y1 Numeric outcome vector (length \eqn{n}).
#' @param n_lags Integer number of own-lags \eqn{H \ge 1}.
#'
#' @return An \eqn{n \times H} numeric matrix with columns
#'   \code{y1_lag1, ..., y1_lagH}. Column \code{h} has \code{h - 1} leading
#'   \code{NA}s, which the regression's complete-case filter drops, removing the
#'   first \eqn{H - 1} rows.
#' @keywords internal
build_y1_lag_columns <- function(y1, n_lags) {
  n <- length(y1)
  cols <- lapply(seq_len(n_lags), function(h) {
    c(rep(NA_real_, h - 1L), y1[seq_len(n - (h - 1L))])
  })
  mat <- do.call(cbind, cols)
  colnames(mat) <- paste0("y1_lag", seq_len(n_lags))
  mat
}

#' Append Y1 own-lag columns to a regressor matrix
#'
#' Sanitizes any blank/\code{NA} regressor label first (so
#' \code{run_pc_regression}'s name fallback cannot relabel the lag columns) with
#' a collision-proof scheme, then column-binds the lag block.
#'
#' @param reg_matrix Numeric regressor matrix (PCs or \code{exog}).
#' @param y1 Numeric outcome vector.
#' @param n_lags Integer number of own-lags \eqn{H \ge 1}.
#' @return \code{reg_matrix} with \code{n_lags} named lag columns appended.
#' @keywords internal
append_y1_lags <- function(reg_matrix, y1, n_lags) {
  nms <- colnames(reg_matrix)
  if (!is.null(nms)) {
    bad <- is.na(nms) | !nzchar(nms)
    if (any(bad)) {
      nms[bad] <- paste0(".exog", which(bad))
      colnames(reg_matrix) <- make.unique(nms)
    }
  }
  cbind(reg_matrix, build_y1_lag_columns(y1, n_lags))
}

#' Validate the y1_lags argument
#'
#' Type/range check only. The substantive sufficiency requirement (the
#' regression needs \code{n_reg + 2} complete observations, where
#' \code{n_reg = n_pcs + H}) is enforced by \code{run_pc_regression}, the single
#' source of truth for that bound; this guard only protects the lag builder's
#' index arithmetic.
#'
#' @param y1_lags Candidate number of own-lags.
#' @param n_obs Number of available observations.
#'
#' @return The validated value coerced to integer.
#' @keywords internal
validate_y1_lags <- function(y1_lags, n_obs) {
  assert_bad_argument_ok(
    is.numeric(y1_lags) && length(y1_lags) == 1L && !is.na(y1_lags) &&
      y1_lags >= 0 && y1_lags == as.integer(y1_lags),
    "y1_lags must be a single non-negative integer",
    arg = "y1_lags"
  )
  y1_lags <- as.integer(y1_lags)
  assert_insufficient_data_ok(
    y1_lags <= n_obs - 1L,
    paste0(
      "y1_lags = ", y1_lags, " exceeds the usable history (n = ", n_obs, ")"
    )
  )
  y1_lags
}
