#' Build the Common Conditioning Regressor Matrix X_t
#'
#' Constructs the shared conditioning block \eqn{X_t = (\mathrm{PC}_t, Y_{1,t},
#' \ldots, Y_{1,t+1-H})} used by both the consumption (W1) and news (W2)
#' reduced forms. The PC block is named first (so the lag append cannot trip
#' \code{\link{run_pc_regression}}'s blank-name fallback), then the \eqn{H}
#' predetermined own-lag columns of \code{y1} are appended via
#' \code{\link{append_y1_lags}}.
#'
#' @param pcs Numeric matrix of principal components (full \eqn{T} rows).
#' @param n_pcs Integer number of leading PC columns to keep.
#' @param y1 Numeric outcome vector (length \code{nrow(pcs)}), or NULL when
#'   \code{y1_lags == 0}.
#' @param y1_lags Integer number of own-lags \eqn{H \ge 0} to append.
#'
#' @return Numeric matrix with the \code{n_pcs} named PC columns and, when
#'   \code{y1_lags > 0}, \code{y1_lag1, ..., y1_lagH} appended.
#' @keywords internal
build_common_conditioning <- function(pcs, n_pcs, y1 = NULL, y1_lags = 0L) {
  reg_matrix <- pcs[, seq_len(n_pcs), drop = FALSE]

  # Ensure the PC block carries usable names BEFORE the lag append, so an
  # unnamed custom pcs cannot trigger run_pc_regression's "any blank name =>
  # relabel ALL columns pc1..pcN" fallback (which would rename lag columns).
  nms <- colnames(reg_matrix)
  if (is.null(nms) || anyNA(nms) || !all(nzchar(nms))) {
    colnames(reg_matrix) <- get_pc_column_names(n_pcs)
  }

  if (y1_lags > 0L) {
    assert_bad_argument_ok(
      !is.null(y1),
      "y1 must be supplied when y1_lags > 0",
      arg = "y1"
    )
    assert_dimension_ok(
      length(y1) == nrow(pcs),
      "y1 must have one element per row of pcs"
    )
    reg_matrix <- append_y1_lags(reg_matrix, y1, y1_lags)
  }

  reg_matrix
}
