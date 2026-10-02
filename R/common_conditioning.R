#' Build the Common Conditioning Regressor Matrix X_t
#'
#' Constructs the non-constant columns of the conditioning vector
#' \eqn{X_t = (1, \mathrm{PC}_t^\top, Y_{1,t}, \ldots, Y_{1,t+1-H})^\top} for the news
#' (\eqn{\omega_2}) reduced form; \code{\link{run_pc_regression}} adds the intercept.
#' \code{\link{compute_w1_residuals}} assembles the matching \eqn{\omega_1} block itself.
#'
#' @details
#' The PC block is named first (so the lag append cannot trip
#' \code{\link{run_pc_regression}}'s blank-name fallback), then the \eqn{H}
#' predetermined own-lag columns of \code{y1} are appended via
#' \code{\link{append_y1_lags}}.
#' Predictor row \eqn{t} is paired with the outcome at \eqn{t+1}; lag column
#' \eqn{h} contains \eqn{Y_{1,t+1-h}} with \eqn{h-1} leading \code{NA}s.
#' Missing input values and all rows are retained for downstream complete-case
#' filtering. Lag counts and PC inputs are validated by the calling workflow.
#' With positive \code{y1_lags}, \code{y1 = NULL} signals a
#' \code{hetid_error_bad_argument}; a length mismatch signals a
#' \code{hetid_error_dimension_mismatch}.
#'
#' @param pcs Numeric matrix of principal components of nominal financial
#'   asset returns (full \eqn{T} rows), in the same observation order as \code{y1}.
#' @param n_pcs Non-negative integer number of leading PC columns to keep,
#'   at most \code{ncol(pcs)}.
#' @param y1 Numeric outcome vector of length \code{nrow(pcs)}, required when
#'   \code{y1_lags > 0}. The default is \code{NULL}; ignored when \code{y1_lags == 0}.
#' @param y1_lags Integer number of own-lags \eqn{H \ge 0} to append.
#'   The default \code{0L} keeps only the PC block.
#'
#' @return Numeric matrix with \code{nrow(pcs)} rows and
#'   \code{n_pcs + y1_lags} columns. The first \code{n_pcs} columns retain their
#'   names unless any name is missing or blank, in which case all are replaced
#'   by the canonical PC names. When \code{y1_lags > 0}, the named lag columns
#'   \code{l.y1}, \code{l2.y1}, and so on are appended in increasing lag order.
#' @keywords internal
build_common_conditioning <- function(pcs, n_pcs, y1 = NULL, y1_lags = 0L) {
  reg_matrix <- pcs[, seq_len(n_pcs), drop = FALSE]

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
