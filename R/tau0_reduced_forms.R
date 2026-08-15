#' Compute the Tau = 0 Reduced Forms
#'
#' Regresses \code{y1} and each column of \code{y2} on \code{x} via
#' \code{\link{run_pc_regression}} (the aliased-coefficient hard error comes
#' for free), or freezes the \code{y2} reduced form at \eqn{B = 0} when
#' \code{impose_null} is set.
#'
#' @param y1 Numeric vector, validated and row-aligned
#' @param y2 Numeric matrix, validated and row-aligned, named columns
#' @param x Numeric matrix, validated and row-aligned
#' @param impose_null Logical; freeze the \code{y2} reduced form at
#'   \eqn{B = 0} instead of estimating it
#'
#' @return \code{list(beta1r, w1, beta2r, w2)}
#' @keywords internal
tau0_reduced_forms <- function(y1, y2, x, impose_null) {
  fit1 <- run_pc_regression(y1, x, ncol(x))
  beta1r <- fit1$coefficients
  w1 <- fit1$residuals

  if (impose_null) {
    w2 <- y2
    beta2r <- matrix(
      0, ncol(y2), ncol(x) + 1L,
      dimnames = list(colnames(y2), names(beta1r))
    )
    return(list(beta1r = beta1r, w1 = w1, beta2r = beta2r, w2 = w2))
  }

  coef_list <- vector("list", ncol(y2))
  w2 <- matrix(NA_real_, nrow(y2), ncol(y2), dimnames = dimnames(y2))
  for (idx in seq_len(ncol(y2))) {
    fit2 <- run_pc_regression(y2[, idx], x, ncol(x))
    coef_list[[idx]] <- fit2$coefficients
    w2[, idx] <- fit2$residuals
  }
  beta2r <- assemble_w2_coef_matrix(
    coef_list,
    row_names = colnames(y2), fallback_names = names(beta1r)
  )

  list(beta1r = beta1r, w1 = w1, beta2r = beta2r, w2 = w2)
}
