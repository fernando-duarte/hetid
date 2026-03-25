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
    warning(paste("Yield column", y_col, "not found. Skipping maturity", i))
    return(NULL)
  }

  # Check term premium column exists
  tp_col <- paste0("tp", i)
  if (!tp_col %in% names(term_premia_df)) {
    warning(paste("Term premium column", tp_col, "not found. Skipping maturity", i))
    return(NULL)
  }

  # Compute SDF innovations for this maturity
  # This gives us Y_{2,t+1}^{(i)} = E_{t+1}[SDF_{t+1+i}] - E_t[SDF_{t+1+i}]
  sdf_innov <- compute_sdf_innovations(yields_df, term_premia_df, i = i) # nolint

  # Create lagged PCs
  # SDF innovations have length T-1
  # Handle case where PCs have different length than yields
  n_sdf <- length(sdf_innov)
  n_pcs_available <- nrow(pcs) - 1

  if (n_pcs_available < n_sdf) {
    # PCs are shorter - use what we have
    pcs_lagged <- pcs[1:n_pcs_available, , drop = FALSE]
    sdf_innov <- sdf_innov[1:n_pcs_available]
  } else {
    # PCs are same length or longer - use first n_sdf rows
    pcs_lagged <- pcs[1:n_sdf, , drop = FALSE]
  }

  # Clean data
  complete_idx <- complete.cases(sdf_innov, pcs_lagged)
  y2_clean <- sdf_innov[complete_idx]
  pcs_clean <- pcs_lagged[complete_idx, , drop = FALSE]

  # Intercept + n_pcs slopes need at least 1 residual df
  min_obs_for_regression <- n_pcs + 2L
  if (length(y2_clean) < min_obs_for_regression) {
    warning(paste("Insufficient data for maturity", i, ". Skipping."))
    return(NULL)
  }

  # Run regression
  reg_data <- data.frame(y = y2_clean, pcs_clean)
  pc_names <- get_pc_column_names(n_pcs)
  names(reg_data)[-1] <- pc_names

  formula_str <- paste("y ~", paste(pc_names, collapse = " + "))
  model <- lm(as.formula(formula_str), data = reg_data)

  list(
    residuals = residuals(model),
    fitted = fitted(model),
    coefficients = coef(model),
    r_squared = summary(model)$r.squared,
    n_obs = length(y2_clean),
    kept_idx = complete_idx
  )
}
