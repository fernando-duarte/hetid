#' Process Single Maturity for \eqn{\omega_2}
#'
#' Computes SDF innovations for one bond maturity and either regresses them on
#' the common conditioning vector or imposes zero coefficients.
#'
#' @template param-maturity-index
#' @param yields_df Numeric yields data frame in annualized percentage points,
#'   with maturity columns named \code{y<N>} for maturity \code{N} in months.
#' @param term_premia_df Numeric term-premia data frame in annualized percentage
#'   points, with columns named \code{tp<N>} and the same date-aligned rows as
#'   \code{yields_df}.
#' @template param-pc-data
#' @param n_pcs Integer number of leading columns of \code{pcs} to use, from
#'   one to \code{ncol(pcs)}.
#' @template param-step
#' @param y1 Numeric outcome vector of length \code{nrow(pcs)}, aligned to the
#'   same dates, supplying the own-lag block of \eqn{X_t}. Required when
#'   \code{y1_lags > 0}; otherwise ignored and defaults to \code{NULL}.
#' @param y1_lags Integer number of own-lags \eqn{H \ge 0} to append, at most
#'   \code{nrow(pcs) - 1}. Defaults to zero.
#' @param impose_b_zero Logical; defaults to \code{FALSE}. If \code{TRUE},
#'   impose \eqn{\beta_2^R = 0} (no regression):
#'   the residual is the SDF innovation itself.
#'
#' @details
#' Called with inputs validated by \code{\link{compute_w2_residuals}}. Required
#' maturity columns are \code{i - step}, \code{i}, and \code{i + step}; at
#' \code{i == step}, only \code{i} and \code{i + step} are required.
#' The horizon must equal \code{step} or satisfy \code{i - step >= MIN_MATURITY},
#' and cannot exceed \code{MAX_MATURITY - step}. The positive integer \code{step}
#' cannot exceed \code{MAX_MATURITY \%/\% 2}.
#' Rows must already be aligned by calendar date; this helper does not join dates.
#' The SDF innovations use the convention of \code{\link{compute_sdf_innovations}}.
#'
#' Conditioning row \code{t} predicts news for \code{t + 1}. Only the common
#' leading news and conditioning rows are used. Rows with missing innovations
#' or conditioning values (including leading own-lag missing values) are dropped
#' in both paths. The fitted path needs \code{n_pcs + y1_lags + 2} complete rows;
#' the imposed path needs two. Infinities are not removed by complete-case filtering.
#'
#' Missing maturity columns or too few complete rows yield \code{NULL} and a
#' \code{hetid_warning_skipped_maturity} warning. Errors from the news kernel or
#' conditioning builder propagate, including \code{hetid_error_insufficient_data}
#' when no valid news remain. The fitted path also propagates structured errors
#' for a response-name collision or a rank-deficient design, and the news kernel
#' may signal \code{hetid_warning_unit_scale} for likely decimal yields.
#'
#' @return A named list with the following components, or \code{NULL} when skipped:
#'   \describe{
#'     \item{residuals, fitted}{Numeric vectors for the retained news rows.}
#'     \item{coefficients}{Named numeric vector of length
#'       \code{1 + n_pcs + y1_lags}, including \code{(Intercept)}. Fitted-path
#'       regressor names are made syntactically valid and unique.}
#'     \item{r_squared}{Numeric regression R-squared, or \code{NA_real_} when imposed.}
#'     \item{n_obs}{Number of retained complete rows.}
#'     \item{kept_idx}{Logical mask over the aligned news rows, before filtering.}
#'     \item{n_reg}{Number of conditioning columns, \code{n_pcs + y1_lags}.}
#'     \item{df_residual}{Residual degrees of freedom, or \code{NA_real_} when imposed.}
#'   }
#'   With \code{impose_b_zero = TRUE}, residuals are the retained innovations,
#'   fitted values and all coefficients are zero, and no model is fit.
#' @keywords internal
process_w2_maturity <- function(i, yields_df, term_premia_df, pcs, n_pcs,
                                step = HETID_CONSTANTS$DEFAULT_STEP,
                                y1 = NULL, y1_lags = 0L,
                                impose_b_zero = FALSE) {
  skip_maturity <- function(msg) {
    warn_skipped_maturity(msg)
    NULL
  }

  needed <- if (i > step) c(i - step, i, i + step) else c(i, i + step)
  missing_cols <- c(
    setdiff(acm_column_name("yields", needed), names(yields_df)),
    setdiff(acm_column_name("term_premia", needed), names(term_premia_df))
  )
  if (length(missing_cols) > 0) {
    return(skip_maturity(paste0(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      " - skipping maturity ", i
    )))
  }

  sdf_innov <- sdf_innovations_series(
    yields_df, term_premia_df,
    i = i, step = step
  )

  # Build own-lags before subsetting so y1 and pcs still have matching rows
  n_reg <- n_pcs + y1_lags
  reg_full <- build_common_conditioning(pcs, n_pcs, y1, y1_lags)
  n_sdf <- length(sdf_innov)
  n_reg_rows <- nrow(reg_full) - 1
  if (n_reg_rows < 1 || n_sdf < 1) {
    return(skip_maturity(paste0(
      "Insufficient data for maturity ", i, ". Skipping."
    )))
  }
  n_align <- min(n_reg_rows, n_sdf)
  reg_lagged <- reg_full[seq_len(n_align), , drop = FALSE]
  sdf_innov <- sdf_innov[seq_len(n_align)]

  complete_idx <- complete.cases(sdf_innov, reg_lagged)
  n_complete <- sum(complete_idx)
  min_required <- if (impose_b_zero) 2L else min_obs_for_pc_regression(n_reg)
  if (n_complete < min_required) {
    return(skip_maturity(paste0(
      "Insufficient data for maturity ", i, ". Skipping."
    )))
  }

  if (impose_b_zero) {
    return(impose_b_zero_result(sdf_innov, reg_lagged, complete_idx, n_complete))
  }

  reg <- run_pc_regression(sdf_innov, reg_lagged, n_reg)

  list(
    residuals = reg$residuals,
    fitted = reg$fitted,
    coefficients = reg$coefficients,
    r_squared = reg$r_squared,
    n_obs = n_complete,
    kept_idx = reg$complete_idx,
    n_reg = n_reg,
    df_residual = reg$df_residual
  )
}

#' Assemble the Imposed \eqn{\beta_2^R = 0} Result for One \eqn{\omega_2} Maturity
#'
#' Imposes \eqn{\beta_2^R = 0} literally: no regression is fit, the residual is the SDF
#' innovation itself, and the coefficient row is a full-width vector of
#' structural zeros, so that the
#' matrix assembled from these rows in \code{\link{compute_w2_residuals}} keeps its
#' \code{1 + n_pcs + y1_lags} column contract.
#'
#' @param sdf_innov Numeric SDF-innovation vector over the aligned news rows.
#' @param reg_lagged Numeric regressor matrix with one row per innovation and
#'   named columns for the conditioning variables.
#' @param complete_idx Logical complete-case mask of length \code{length(sdf_innov)}.
#' @param n_complete Integer complete-row count, equal to \code{sum(complete_idx)}.
#' @return Named list with the components described in
#'   \code{\link{process_w2_maturity}}. Residuals are \code{sdf_innov[complete_idx]},
#'   fitted values are a zero vector of length \code{n_complete}, and coefficients
#'   are zeros named \code{(Intercept)} followed by \code{colnames(reg_lagged)}.
#'   The \code{r_squared} and \code{df_residual} components are \code{NA_real_}.
#' @details Inputs are prepared by \code{\link{process_w2_maturity}}; this helper
#'   does not validate them. The returned mask is unchanged and
#'   \code{n_reg = ncol(reg_lagged)}.
#' @keywords internal
impose_b_zero_result <- function(sdf_innov, reg_lagged, complete_idx,
                                 n_complete) {
  coef_names <- c(HETID_CONSTANTS$INTERCEPT_LABEL, colnames(reg_lagged))
  zero_coefs <- stats::setNames(rep(0, length(coef_names)), coef_names)
  resid_vec <- sdf_innov[complete_idx]
  list(
    residuals = resid_vec,
    fitted = rep(0, n_complete),
    coefficients = zero_coefs,
    r_squared = NA_real_,
    n_obs = n_complete,
    kept_idx = complete_idx,
    n_reg = ncol(reg_lagged),
    df_residual = NA_real_
  )
}
