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
#'
#' @return List with regression results or NULL if skipped
#' @keywords internal
process_w2_maturity <- function(i, yields_df, term_premia_df, pcs, n_pcs,
                                step = HETID_CONSTANTS$DEFAULT_STEP) {
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
  sdf_innov <- compute_sdf_innovations( # nolint
    yields_df, term_premia_df,
    i = i, step = step
  )

  # Create lagged PCs
  # SDF innovations have length T-1
  # Handle case where PCs have different length
  n_sdf <- length(sdf_innov)
  n_pcs_available <- nrow(pcs) - 1

  # Guard against empty/insufficient PCs
  if (n_pcs_available < 1 || n_sdf < 1) {
    return(skip_maturity(paste0(
      "Insufficient data for maturity ", i, ". Skipping."
    )))
  }

  if (n_pcs_available < n_sdf) {
    pcs_lagged <- pcs[
      seq_len(n_pcs_available), ,
      drop = FALSE
    ]
    sdf_innov <- sdf_innov[
      seq_len(n_pcs_available)
    ]
  } else {
    pcs_lagged <- pcs[
      seq_len(n_sdf), ,
      drop = FALSE
    ]
  }

  # Subset to relevant PCs before checking completeness
  pcs_subset <- pcs_lagged[, seq_len(n_pcs), drop = FALSE]
  complete_idx <- complete.cases(sdf_innov, pcs_subset)
  n_complete <- sum(complete_idx)
  min_obs_for_regression <- min_obs_for_pc_regression(n_pcs)
  if (n_complete < min_obs_for_regression) {
    return(skip_maturity(paste0(
      "Insufficient data for maturity ", i, ". Skipping."
    )))
  }

  reg <- run_pc_regression(
    sdf_innov, pcs_lagged, n_pcs
  )

  list(
    residuals = reg$residuals,
    fitted = reg$fitted,
    coefficients = reg$coefficients,
    r_squared = reg$r_squared,
    n_obs = n_complete,
    kept_idx = reg$complete_idx
  )
}
