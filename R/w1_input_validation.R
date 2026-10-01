#' Prepare Explicit W1 Regressors
#'
#' @param exog Numeric matrix or data frame with at least one column.
#' @param n_pcs_missing Whether the caller omitted the PC count.
#' @return Finite numeric matrix with default column labels when absent.
#' @noRd
prepare_w1_exog <- function(exog, n_pcs_missing) {
  assert_bad_argument_ok(
    n_pcs_missing,
    "supply either n_pcs (bundled PCs) or exog, not both",
    arg = "n_pcs"
  )
  assert_tabular(exog, "exog")
  assert_bad_argument_ok(
    ncol(exog) >= 1L,
    "exog must contain at least one column",
    arg = "exog"
  )
  exog <- as.matrix(exog)
  assert_numeric_finite_values(exog, "exog")
  if (is.null(colnames(exog))) {
    colnames(exog) <- paste0("z", seq_len(ncol(exog)))
  }
  exog
}
