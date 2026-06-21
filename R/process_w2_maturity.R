#' Process Single Maturity for W2
#'
#' Internal function to process regression for a single maturity
#'
#' @param i Maturity index
#' @param yields_df Yields data frame
#' @param term_premia_df Term premia data frame
#' @param pcs Principal components matrix
#' @param n_pcs Number of PCs
#' @template param-step
#' @param y1 Optional outcome vector (length \code{nrow(pcs)}) supplying the
#'   own-lag block of the common conditioning vector \eqn{X_t}.
#' @param y1_lags Integer number of own-lags \eqn{H \ge 0} to append.
#' @param impose_b_zero Logical; if TRUE, impose \eqn{B = 0} (no regression):
#'   the residual is the SDF innovation itself.
#'
#' @return List with regression results or NULL if skipped
#' @keywords internal
process_w2_maturity <- function(i, yields_df, term_premia_df, pcs, n_pcs,
                                step = HETID_CONSTANTS$DEFAULT_STEP,
                                y1 = NULL, y1_lags = 0L,
                                impose_b_zero = FALSE) {
  # Emit a classed skip warning and return NULL so a guard can
  # `return(skip_maturity(...))` in one line.
  skip_maturity <- function(message) {
    warn_skipped_maturity(message)
    NULL
  }

  # Maturity i needs columns i and i+step via compute_n_hat, plus
  # column i-step via compute_n_hat_previous when i > step (for
  # i = step the previous-period n_hat uses only the step-maturity
  # yield, which is column i)
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

  # Compute SDF innovations for this maturity: Y_{2,t+1}^{(i)} is the
  # centered second-order approximation to the SDF news
  # E_{t+1}[SDF_{t+1+i}] - E_t[SDF_{t+1+i}] (see compute_sdf_innovations)
  sdf_innov <- sdf_innovations_series(
    yields_df, term_premia_df,
    i = i, step = step
  )

  # Build the common conditioning matrix X_t on the FULL-T series (so lag
  # columns line up before any news-row subsetting), then align to news rows.
  n_reg <- n_pcs + y1_lags
  reg_full <- build_common_conditioning(pcs, n_pcs, y1, y1_lags)

  # SDF innovations have length T-1; pair regressor row t with sdf_innov[t].
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

  # Completeness over (sdf_innov, regressors-including-lags); leading lag-NA
  # rows drop here consistently for both the fitted and imposed paths.
  complete_idx <- complete.cases(sdf_innov, reg_lagged)
  n_complete <- sum(complete_idx)
  min_obs_for_regression <- min_obs_for_pc_regression(n_reg)
  if (!impose_b_zero && n_complete < min_obs_for_regression) {
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

#' Assemble the imposed B = 0 result for one W2 maturity
#'
#' Imposes \eqn{B = 0} literally: no regression is fit, the residual is the SDF
#' innovation itself (already nonlinearly centered upstream), and the
#' coefficient row is a full-width vector of structural zeros so the API
#' contract \code{ncol(coefficients) == 1 + J + y1_lags} holds.
#'
#' @param sdf_innov News-row SDF innovations (length \code{n_align}).
#' @param reg_lagged News-row regressor matrix (for names / fitted length).
#' @param complete_idx Logical complete-case mask over the news rows.
#' @param n_complete Number of complete rows.
#' @return List mirroring the fitted-path result, with zero coefficients,
#'   zero fitted values, and \code{r_squared = NA}.
#' @keywords internal
impose_b_zero_result <- function(sdf_innov, reg_lagged, complete_idx,
                                 n_complete) {
  coef_names <- c("(Intercept)", colnames(reg_lagged))
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
