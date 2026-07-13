#' Compute Reduced Form Residuals for Y2 Variables
#'
#' Computes residuals W_\{2,t+1\}^\{(i)\} from regressing Y_\{2,t+1\}^\{(i)\} variables
#' (SDF innovations) on the common conditioning vector \eqn{X_t}: a constant, the
#' principal components extracted from financial asset returns, and (when
#' \code{y1}/\code{y1_lags} are supplied) the own-lags of \code{y1}. Setting
#' \code{impose_b_zero = TRUE} gives the exact-news case \eqn{B = 0}, in which the
#' residual is the centered SDF news itself and all coefficients are zero.
#'
#' @template param-yields-term-premia
#' @param maturities Vector of maturities to process. NULL (default)
#'   uses the step-spaced horizons from \code{step} to
#'   \code{MAX_MATURITY - step} satisfying the news contract (see
#'   \code{\link{default_w2_maturities}}).
#' @template param-step
#' @template param-n-pcs
#' @template param-pc-data
#' @param return_df Logical, if TRUE returns a tidy data frame (default FALSE
#'   returns the richer list). Both shapes carry dates.
#' @param dates Required vector of period-end \code{Date}s, one per yield row
#'   (length \code{nrow(yields)}); internally shifted to the t+1 realization
#'   dates \eqn{d_2, \ldots, d_T} of the W2 news (matching
#'   \code{compute_w1_residuals}).
#' @param y1 Optional outcome vector (length \code{nrow(pcs)}) supplying the
#'   own-lag block of the common conditioning vector \eqn{X_t}; required when
#'   \code{y1_lags > 0}.
#' @param y1_lags Integer number of own-lags \eqn{H \ge 0} of \code{y1} to
#'   append to the PC regressors (default 0 = PC-only, backward compatible).
#'   Lagging drops the first \eqn{H - 1} leading rows.
#' @param impose_b_zero Logical; if TRUE, impose \eqn{B = 0} structurally (no
#'   regression): \eqn{W_2} equals the SDF innovation \eqn{Y_2}, the
#'   \code{coefficients} row is the full-width
#'   \code{(Intercept, pc1..pcJ, l.y1..l<H>.y1)} vector of zeros, and
#'   \code{r_squared} is \code{NA} (default FALSE).
#'
#' @return If return_df = FALSE, returns a list containing:
#' \describe{
#'   \item{residuals}{List of residual vectors, one for each maturity}
#'   \item{fitted}{List of fitted value vectors}
#'   \item{dates}{Per-maturity list of t+1 realization \code{Date} vectors
#'     parallel to \code{residuals}, the lead date index subset by each
#'     maturity's \code{kept_idx}.}
#'   \item{coefficients}{Matrix of regression coefficients (maturities x predictors)}
#'   \item{r_squared}{Vector of R-squared values for each maturity}
#'   \item{n_obs}{Number of observations used in each regression}
#'   \item{kept_idx}{List of logical vectors indicating which
#'     rows survived complete.cases filtering, one per maturity}
#' }
#' If return_df = TRUE, returns a data frame with columns \code{date},
#' \code{maturity}, \code{residuals} (\eqn{W_{2,t+1}}), and \code{fitted}.
#'
#' Maturities whose required columns or observations are missing are
#' skipped with a warning rather than aborting the run: their entries
#' are absent from \code{residuals}, \code{fitted}, and \code{kept_idx},
#' and their \code{coefficients} rows, \code{r_squared}, and \code{n_obs}
#' are \code{NA}. With \code{return_df = TRUE}, skipped maturities
#' contribute no rows (zero rows if all are skipped).
#'
#' @details
#' For each maturity i, computes Y_\{2,t+1\}^\{(i)\} as the SDF
#' innovation (via \code{compute_sdf_innovations()}, the centered
#' second-order approximation to the SDF news
#' \eqn{E_{t+1}[SDF_{t+1+i}] - E_t[SDF_{t+1+i}]}), then regresses it on
#' the conditioning vector \eqn{X_t} (PC_t, optionally with
#' \code{y1_lags} own-lags of \code{y1}) for residuals W_\{2,t+1\}.
#'
#' \strong{PC alignment:} \code{pcs} must be supplied as a numeric matrix
#' with one row per yield row, already aligned to the yields by calendar
#' date. Join the principal components to the yields by \code{date} before
#' calling (both share the package period-end convention). See the example.
#'
#' @importFrom stats lm residuals fitted coef
#' @importFrom utils data
#' @export
#' @examples
#' # The bundled dataset ships as imported (quarter-start dates); normalize to
#' # the package period-end convention so it merges with ACM by calendar date,
#' # then pass the aligned PCs via `pcs`.
#' mats <- c(12, 24, 36, 48)
#' acm_data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = mats, frequency = "quarterly"
#' )
#' data("variables", package = "hetid")
#' variables$date <- to_period_end(variables$date, "quarterly")
#' pc_cols <- paste0("pc", 1:4)
#' merged <- merge(
#'   variables[, c("date", pc_cols)],
#'   acm_data[, c("date", paste0("y", mats), paste0("tp", mats))],
#'   by = "date"
#' )
#' res_w2 <- compute_w2_residuals(
#'   merged[, paste0("y", mats)], merged[, paste0("tp", mats)],
#'   maturities = c(24, 36), n_pcs = 4, pcs = as.matrix(merged[, pc_cols]),
#'   dates = merged$date
#' )
compute_w2_residuals <- function(yields, term_premia,
                                 maturities = NULL,
                                 n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
                                 pcs = NULL, return_df = FALSE, dates = NULL,
                                 step = HETID_CONSTANTS$DEFAULT_STEP,
                                 y1 = NULL, y1_lags = 0L,
                                 impose_b_zero = FALSE) {
  if (is.null(maturities)) {
    maturities <- default_w2_maturities(step)
  }
  if (is.null(pcs)) {
    validate_n_pcs(n_pcs)
  } else {
    assert_scalar_integer_in_range(
      n_pcs, "n_pcs", 1, NCOL(pcs)
    )
  }
  validated <- validate_w2_inputs( # nolint: object_usage_linter
    yields, term_premia, maturities,
    step = step
  )
  yields_df <- validated$yields
  term_premia_df <- validated$term_premia
  maturities <- validated$maturities

  w2_dates <- resolve_w2_dates(dates, nrow(yields_df))
  pc_result <- load_w2_pcs(pcs, n_pcs, nrow(yields_df))
  pcs <- pc_result$pcs

  y1_lags <- validate_y1_lags(y1_lags, nrow(pcs))
  pc_lag_names <- if (y1_lags > 0L) lag_grammar_names("y1", y1_lags) else NULL

  residuals_list <- list()
  fitted_list <- list()
  coef_list <- vector("list", length(maturities))
  r_squared <- rep(NA_real_, length(maturities))
  n_obs_used <- rep(NA_real_, length(maturities))
  kept_idx_list <- list()

  for (idx in seq_along(maturities)) {
    i <- maturities[idx]
    result <- process_w2_maturity( # nolint: object_usage_linter
      i, yields_df, term_premia_df, pcs, n_pcs,
      step = step, y1 = y1, y1_lags = y1_lags,
      impose_b_zero = impose_b_zero
    )

    if (is.null(result)) {
      next
    }
    residuals_list[[maturity_names(i)]] <- result$residuals
    fitted_list[[maturity_names(i)]] <- result$fitted
    coef_list[[idx]] <- result$coefficients
    r_squared[idx] <- result$r_squared
    n_obs_used[idx] <- result$n_obs
    kept_idx_list[[maturity_names(i)]] <- result$kept_idx
  }

  # Column names from lm() coefficients, not derived from n_pcs + 1
  coef_matrix <- assemble_w2_coef_matrix(
    coef_list,
    row_names = maturity_names(maturities),
    fallback_names = c("(Intercept)", pc_result$pc_names, pc_lag_names)
  )

  dates_list <- lapply(kept_idx_list, function(kept) w2_dates[which(kept)])

  if (return_df) {
    return(format_w2_dataframe(
      residuals_list = residuals_list,
      fitted_list = fitted_list,
      dates_list = dates_list,
      maturities = maturities
    ))
  }

  list(
    residuals = residuals_list,
    fitted = fitted_list,
    dates = dates_list,
    coefficients = coef_matrix,
    r_squared = r_squared,
    n_obs = n_obs_used,
    kept_idx = kept_idx_list
  )
}
