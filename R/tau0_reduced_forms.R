#' Compute the Tau = 0 Reduced Forms
#'
#' Regresses \code{y1} and each column of \code{y2} on \code{x} via
#' \code{\link{run_pc_regression}}, including an intercept. When
#' \code{impose_null = TRUE}, the entire \code{y2} coefficient matrix,
#' including the intercepts, is fixed at zero and \code{w2} equals \code{y2}.
#'
#' @param y1 Finite numeric outcome vector of length \code{T}.
#' @param y2 Finite numeric matrix of news/innovation variables with \code{T}
#'   rows and at least one column, with unique, non-blank column names.
#' @param x Finite numeric conditioning matrix with \code{T} rows and at least
#'   one column. Do not include an intercept column or a column named \code{"y"}.
#' @param impose_null Logical scalar without a missing value. If \code{TRUE},
#'   fix the \code{y2} coefficients at zero; if \code{FALSE}, estimate them.
#'
#' @details
#' Inputs must already be validated and row-aligned by the caller, as in
#' \code{\link{compute_tau0_system}}. Missing and non-finite values are outside
#' this helper's input contract. At least \code{ncol(x) + 2} observations are
#' required. Rank-deficient designs raise a structured \code{hetid_error};
#' too few complete observations raise \code{hetid_error_insufficient_data},
#' and a regressor named \code{"y"} raises \code{hetid_error_bad_argument}.
#' Coefficient names follow \code{\link{run_pc_regression}}.
#'
#' @return A named list with the following components:
#' \describe{
#'   \item{beta1r}{Named numeric vector of \code{ncol(x) + 1} outcome
#'     coefficients, with the intercept first.}
#'   \item{w1}{Numeric vector of \code{T} outcome residuals.}
#'   \item{beta2r}{Numeric \code{ncol(y2)} by \code{ncol(x) + 1} matrix.
#'     Rows follow \code{colnames(y2)}; columns follow \code{names(beta1r)}.
#'     All entries are zero when \code{impose_null = TRUE}.}
#'   \item{w2}{Numeric residual matrix with the dimensions and dimnames of
#'     \code{y2}, equal to \code{y2} when \code{impose_null = TRUE}.}
#' }
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
