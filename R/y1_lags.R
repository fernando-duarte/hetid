#' Build Lagged-Outcome Regressor Columns
#'
#' Constructs the predetermined lag block for the \eqn{\omega_1} reduced form. Column
#' \code{h} holds \eqn{Y_{1,t+1-h}} at predictor row \code{t}: the outcome
#' shifted down by \code{h - 1} rows with \code{h - 1} leading \code{NA}s, so
#' that under the one-period lag/lead convention of
#' \code{\link{compute_w1_residuals}} (regressor row \code{t} paired with
#' \eqn{Y_{1,t+1}}) the column lines up with \eqn{Y_{1,t+1-h}}.
#'
#' @param y1 Numeric outcome vector (length \eqn{n}), in time order.
#'   Missing values are retained in each shifted column.
#' @param n_lags Integer number of own-lags \eqn{1 \le H \le n}.
#'   The caller must validate this value before building the lag block.
#'
#' @return An \eqn{n \times H} numeric matrix with columns
#'   \code{l.y1}, \code{l2.y1}, and so on through lag \code{H}.
#'   Column \code{h} has \code{h - 1} leading \code{NA}s. No rows are removed
#'   here; the regression's complete-case filter drops the first \eqn{H - 1}
#'   rows and any other rows with missing responses or regressors.
#' @keywords internal
build_y1_lag_columns <- function(y1, n_lags) {
  n <- length(y1)
  cols <- lapply(seq_len(n_lags), function(h) {
    c(rep(NA_real_, h - 1L), y1[seq_len(n - (h - 1L))])
  })
  mat <- do.call(cbind, cols)
  colnames(mat) <- lag_grammar_names("y1", n_lags)
  mat
}

#' Append Y1 Own-Lag Columns to a Regressor Matrix
#'
#' Appends the predetermined outcome lag block to the supplied regressors.
#'
#' @details
#' If any supplied column name is blank or \code{NA}, those labels are replaced
#' with \code{.exog} plus their column index and all regressor names are made unique.
#' This avoids \code{\link{run_pc_regression}}'s blank-name fallback. An unnamed
#' matrix keeps blank labels on its original columns after binding; callers
#' must name those columns first to preserve the lag labels in the regression.
#' Existing names that match the appended lag names are not changed here.
#'
#' @param reg_matrix Numeric regressor matrix (PCs of nominal financial asset
#'   returns or \code{exog}), with one row per element of \code{y1} in the same
#'   time order. Missing values are retained.
#' @param y1 Numeric outcome vector. Missing values are retained in the lag block.
#' @param n_lags Integer number of own-lags, from 1 through \code{length(y1)}.
#'   The caller must validate this value before appending the lag block.
#' @return A numeric matrix with the same rows as \code{reg_matrix} and
#'   \code{n_lags} named lag columns appended. Existing column names are repaired
#'   only when at least one is blank or \code{NA}; the input is not modified.
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

#' Validate the y1_lags Argument
#'
#' Checks the lag count's type and range, protecting the lag builder's index arithmetic.
#' Regression requires \code{n_reg + 2} complete observations, where \code{n_reg}
#' counts PC or \code{exog} columns plus own-lags; see
#' \code{\link{min_obs_for_pc_regression}}. \code{\link{run_pc_regression}} errors
#' below this bound; \code{process_w2_maturity} skips the maturity, except that
#' its \code{impose_b_zero} path needs only two complete rows.
#'
#' @param y1_lags Numeric scalar giving a non-negative, integer-valued number of
#'   own-lags representable as an R integer. Missing values are not allowed;
#'   zero requests no lag block. Must not exceed \code{n_obs - 1}.
#' @param n_obs Number of available observations before lead/lag alignment.
#'   Supplied by the caller; this helper does not validate its type or count
#'   complete observations.
#'
#' @return The validated integer scalar. Invalid lag counts signal a
#'   \code{hetid_error_bad_argument}; counts beyond the available history signal
#'   a \code{hetid_error_insufficient_data}.
#' @keywords internal
validate_y1_lags <- function(y1_lags, n_obs) {
  assert_bad_argument_ok(
    is.numeric(y1_lags) && length(y1_lags) == 1L && is.finite(y1_lags) &&
      y1_lags >= 0 && y1_lags %% 1 == 0 && y1_lags <= .Machine$integer.max,
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
