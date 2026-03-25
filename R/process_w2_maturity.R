#' Process Single Maturity for W2
#'
#' Internal function to process regression for a single maturity
#'
#' @param i Maturity index
#' @param yields_df Yields data frame
#' @param term_premia_df Term premia data frame
#' @param pcs Principal components matrix
#' @param n_pcs Number of PCs
#'
#' @return List with regression results or NULL if skipped
#' @keywords internal
process_w2_maturity <- function(i, yields_df, term_premia_df, pcs, n_pcs) {
  # Get yield column
  y_col <- paste0("y", i)
  if (!y_col %in% names(yields_df)) {
    warning(
      paste("Yield column", y_col, "not found. Skipping maturity", i),
      call. = FALSE
    )
    return(NULL)
  }

  # Check term premium column exists
  tp_col <- paste0("tp", i)
  if (!tp_col %in% names(term_premia_df)) {
    warning(
      paste("Term premium column", tp_col, "not found. Skipping maturity", i),
      call. = FALSE
    )
    return(NULL)
  }

  # Compute SDF innovations for this maturity
  # This gives us Y_{2,t+1}^{(i)} = E_{t+1}[SDF_{t+1+i}] - E_t[SDF_{t+1+i}]
  sdf_innov <- compute_sdf_innovations(yields_df, term_premia_df, i = i) # nolint

  # Create lagged PCs
  # SDF innovations have length T-1
  # Handle case where PCs have different length
  n_sdf <- length(sdf_innov)
  n_pcs_available <- nrow(pcs) - 1

  # Guard against empty/insufficient PCs
  if (n_pcs_available < 1 || n_sdf < 1) {
    warning(
      "Insufficient data for maturity ", i,
      ". Skipping.",
      call. = FALSE
    )
    return(NULL)
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
  min_obs_for_regression <- n_pcs + 2L
  if (n_complete < min_obs_for_regression) {
    warning(
      paste(
        "Insufficient data for maturity",
        i, ". Skipping."
      ),
      call. = FALSE
    )
    return(NULL)
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
