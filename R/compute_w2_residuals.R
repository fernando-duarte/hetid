#' Compute Reduced Form Residuals for Y2 Variables
#'
#' Regresses SDF innovations \eqn{Y_{2,t+1}^{(i)}} on a constant, principal
#' components of nominal financial asset returns, and optional own-lags of
#' \code{y1}, returning residuals \eqn{\omega_{2,t+1}^{(i)}}. With
#' \code{impose_b_zero = TRUE}, the residual is the SDF news itself.
#'
#' @template param-yields-term-premia
#' @param maturities Nonempty vector of distinct integer bond maturities in months,
#'   from \code{MIN_MATURITY} to \code{MAX_MATURITY - step}. Each must equal
#'   \code{step} or satisfy \code{maturity - step >= MIN_MATURITY}.
#'   \code{NULL} uses \code{\link{default_w2_maturities}}.
#' @template param-n-pcs
#' @template param-pc-data
#' @param return_df Logical; \code{FALSE} (default) returns a list,
#'   \code{TRUE} a long-format data frame. Both shapes carry dates.
#' @param dates Required non-missing period-end \code{Date} vector, one per yield
#'   row. News realization dates are \code{dates[-1]}, subset by \code{kept_idx}.
#' @template param-step
#' @param y1 Optional numeric outcome vector, aligned to the yield rows;
#'   required with length \code{nrow(pcs)} when \code{y1_lags > 0}.
#' @param y1_lags Integer number of own-lags \eqn{H \ge 0} to append to the
#'   PC regressors (default 0). Cannot exceed \code{nrow(pcs) - 1}.
#'   For \eqn{H > 0}, lagging drops the first \eqn{H - 1} news rows.
#' @param impose_b_zero Logical; \code{TRUE} imposes \eqn{B = 0} without regression:
#'   coefficients and fitted values are zero and \code{r_squared} is \code{NA}.
#'   Complete-case filtering still applies. Default is \code{FALSE}.
#'
#' @return With \code{return_df = FALSE}, a list containing:
#' \describe{
#'   \item{residuals, fitted}{Numeric-vector lists keyed by \code{maturity_N}
#'     for successfully processed bond maturities \code{N} in months.}
#'   \item{dates}{Parallel lists of realization \code{Date} vectors.}
#'   \item{coefficients}{Numeric matrix with one \code{maturity_N} row per requested
#'     maturity and \code{1 + n_pcs + y1_lags} columns: intercept, selected PC
#'     labels, and \code{l.y1}, \code{l2.y1}, etc. Regression labels are sanitized.}
#'   \item{r_squared, n_obs}{Numeric vectors in requested-maturity order:
#'     R-squared and complete observation counts, respectively.}
#'   \item{kept_idx}{List keyed by \code{maturity_N} of logical complete-case masks
#'     over the \code{nrow(yields) - 1} news rows.}
#'   \item{skipped}{Skip reasons named \code{maturity_N}; empty when none are skipped.}
#' }
#' With \code{return_df = TRUE}, a data frame with columns \code{date},
#' \code{maturity} (months), \code{residuals}, and \code{fitted}, and the
#' same skip reasons in its \code{skipped_maturities} attribute.
#' Skipped maturities are absent from the lists (including \code{dates});
#' their coefficient rows, R-squared values, and observation counts are \code{NA}.
#' No data-frame rows are added for skips; if all are skipped, the frame has zero rows.
#' Dropping constraints can only widen the set; check skips before comparing estimates.
#'
#' @details
#' SDF news uses the centered second-order approximation in
#' \code{\link{compute_sdf_innovations}}. Predictor row \eqn{t} is paired with
#' news realized at \eqn{t+1}. Yields and term premia must have the same dimensions,
#' with numeric columns named \code{yN} and \code{tpN}, in annualized percentage points.
#' Maturity \code{i} needs columns at \code{i - step}, \code{i}, and
#' \code{i + step}; at \code{i == step}, only \code{i} and \code{i + step} are needed.
#' Supply \code{pcs} already joined to yields by \code{date}, with one row per yield
#' row. Matrices and numeric data frames are accepted. Set \code{step} to the number
#' of months per observation period; the default is an annual news clock.
#' The rollover convention must also match; see \code{\link{compute_n_hat}}.
#'
#' Rows with missing SDF innovations, selected PCs, or own-lags are removed per
#' maturity. Missing required columns or fewer than \code{n_pcs + y1_lags + 2}
#' complete rows cause a \code{hetid_warning_skipped_maturity} warning and a skip;
#' the imposed path needs only two complete rows. Argument-validation failures,
#' no valid SDF news, and rank-deficient designs abort with a \code{hetid_error}.
#' @importFrom stats lm residuals fitted coef
#' @importFrom utils data
#' @export
#' @examples
#' local({
#'   old_seed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
#'   on.exit(if (is.null(old_seed)) {
#'     rm(".Random.seed", envir = globalenv())
#'   } else {
#'     assign(".Random.seed", old_seed, envir = globalenv())
#'   })
#'   set.seed(42)
#'   acm <- extract_acm_data(
#'     data_types = c("yields", "term_premia"), maturities = 1:3
#'   )
#'   # Simulated nominal asset returns supply monthly conditioning PCs
#'   returns <- matrix(rnorm(nrow(acm) * 2), ncol = 2)
#'   inputs <- data.frame(date = acm$date, stats::prcomp(returns)$x, outcome = rnorm(nrow(acm)))
#'   merged <- merge(inputs, acm, by = "date")
#'   yields <- merged[, paste0("y", 1:3)]
#'   term_premia <- merged[, paste0("tp", 1:3)]
#'   pcs <- as.matrix(merged[, c("PC1", "PC2")])
#'   fit <- compute_w2_residuals(
#'     yields, term_premia,
#'     maturities = 2, n_pcs = 2, pcs = pcs,
#'     dates = merged$date, step = 1, y1 = merged$outcome, y1_lags = 2
#'   )
#'   imposed <- compute_w2_residuals(
#'     yields, term_premia,
#'     maturities = 2, n_pcs = 2, pcs = pcs,
#'     dates = merged$date, step = 1, y1 = merged$outcome, y1_lags = 2,
#'     impose_b_zero = TRUE, return_df = TRUE
#'   )
#'   print(head(imposed))
#' })
compute_w2_residuals <- function(yields, term_premia,
                                 maturities = NULL,
                                 n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
                                 pcs = NULL, return_df = FALSE, dates = NULL,
                                 step = HETID_CONSTANTS$DEFAULT_STEP,
                                 y1 = NULL, y1_lags = 0L,
                                 impose_b_zero = FALSE) {
  assert_flag(return_df, "return_df")
  assert_flag(impose_b_zero, "impose_b_zero")
  if (is.null(maturities)) {
    maturities <- default_w2_maturities(step)
  }
  if (is.null(pcs)) {
    validate_n_pcs(n_pcs)
  } else {
    assert_scalar_integer_in_range(n_pcs, "n_pcs", 1, NCOL(pcs))
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
  skipped <- character(0)

  for (idx in seq_along(maturities)) {
    i <- maturities[idx]
    skip_reason <- NA_character_
    withCallingHandlers(
      result <- process_w2_maturity( # nolint: object_usage_linter
        i, yields_df, term_premia_df, pcs, n_pcs,
        step = step, y1 = y1, y1_lags = y1_lags,
        impose_b_zero = impose_b_zero
      ),
      hetid_warning_skipped_maturity = function(w) {
        skip_reason <<- conditionMessage(w)
      }
    )

    if (is.null(result)) {
      skipped[maturity_names(i)] <- skip_reason
      next
    }
    residuals_list[[maturity_names(i)]] <- result$residuals
    fitted_list[[maturity_names(i)]] <- result$fitted
    coef_list[[idx]] <- result$coefficients
    r_squared[idx] <- result$r_squared
    n_obs_used[idx] <- result$n_obs
    kept_idx_list[[maturity_names(i)]] <- result$kept_idx
  }

  coef_matrix <- assemble_w2_coef_matrix(
    coef_list,
    row_names = maturity_names(maturities),
    fallback_names = c("(Intercept)", pc_result$pc_names, pc_lag_names)
  )

  dates_list <- lapply(kept_idx_list, function(kept) w2_dates[which(kept)])

  if (return_df) {
    w2_df <- format_w2_dataframe(
      residuals_list = residuals_list,
      fitted_list = fitted_list,
      dates_list = dates_list,
      maturities = maturities
    )
    attr(w2_df, "skipped_maturities") <- skipped
    return(w2_df)
  }

  list(
    residuals = residuals_list,
    fitted = fitted_list,
    dates = dates_list,
    coefficients = coef_matrix,
    r_squared = r_squared,
    n_obs = n_obs_used,
    kept_idx = kept_idx_list,
    skipped = skipped
  )
}
