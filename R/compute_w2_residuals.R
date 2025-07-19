#' Compute Reduced Form Residuals for Y2 Variables
#'
#' Computes residuals W_{2,t+1}^{(i)} from regressing Y_{2,t+1}^{(i)} variables
#' (yield or term premium changes) on principal components and a constant.
#'
#' @param yields Data frame with yield columns (y1, y2, ..., y10)
#' @param term_premia Data frame with term premium columns (tp1, tp2, ..., tp10)
#' @param maturities Vector of maturities to process (default: 1:9)
#' @param n_pcs Number of principal components to use (default: 4)
#' @param pcs Matrix of principal components (T x n_pcs). If NULL, loads from package data.
#' @param use_tp_adjustment Logical, whether to adjust yields by term premia (default: TRUE)
#'
#' @return A list containing:
#' \describe{
#'   \item{residuals}{List of residual vectors, one for each maturity}
#'   \item{fitted}{List of fitted value vectors}
#'   \item{coefficients}{Matrix of regression coefficients (maturities x predictors)}
#'   \item{r_squared}{Vector of R-squared values for each maturity}
#'   \item{n_obs}{Number of observations used in each regression}
#' }
#'
#' @details
#' For each maturity i, computes Y_{2,t+1}^{(i)} as:
#' - If use_tp_adjustment = TRUE: Y_{2,t+1}^{(i)} = y_{t+1}^{(i)} - tp_{t+1}^{(i)}
#' - If use_tp_adjustment = FALSE: Y_{2,t+1}^{(i)} = y_{t+1}^{(i)}
#'
#' Then regresses Y_{2,t+1}^{(i)} on PC_t to get residuals W_{2,t+1}^{(i)}.
#'
#' @importFrom stats lm residuals fitted coef
#' @importFrom utils data
#' @export
#'
#' @examples
#' \dontrun{
#' # Load ACM data
#' acm_data <- extract_acm_data()
#' yields <- acm_data[, grep("^y", names(acm_data))]
#' term_premia <- acm_data[, grep("^tp", names(acm_data))]
#'
#' # Compute residuals for all maturities
#' res_y2 <- compute_w2_residuals(yields, term_premia)
#'
#' # Compute for specific maturities only
#' res_y2_short <- compute_w2_residuals(yields, term_premia, maturities = 1:3)
#' }
compute_w2_residuals <- function(yields, term_premia, maturities = 1:9,
                                 n_pcs = 4, pcs = NULL,
                                 use_tp_adjustment = TRUE) {
  # Validate and prepare inputs
  validated <- validate_w2_inputs(yields, term_premia, maturities) # nolint: object_usage_linter
  yields_df <- validated$yields
  term_premia_df <- validated$term_premia
  maturities <- validated$maturities

  # Load or validate PCs
  pcs <- load_w2_pcs(pcs, n_pcs, nrow(yields_df)) # nolint: object_usage_linter

  # Initialize storage
  residuals_list <- list()
  fitted_list <- list()
  coef_matrix <- matrix(NA,
    nrow = length(maturities),
    ncol = n_pcs + 1
  ) # +1 for intercept
  r_squared <- numeric(length(maturities))
  n_obs_used <- numeric(length(maturities))

  # Process each maturity
  for (idx in seq_along(maturities)) {
    i <- maturities[idx]

    # Process single maturity
    result <- process_w2_maturity( # nolint: object_usage_linter
      i, yields_df, term_premia_df, pcs, n_pcs, use_tp_adjustment
    )

    # Skip if NULL result
    if (is.null(result)) {
      next
    }

    # Store results
    residuals_list[[paste0("maturity_", i)]] <- result$residuals
    fitted_list[[paste0("maturity_", i)]] <- result$fitted
    coef_matrix[idx, ] <- result$coefficients
    r_squared[idx] <- result$r_squared
    n_obs_used[idx] <- result$n_obs
  }

  # Set row/column names for coefficient matrix
  rownames(coef_matrix) <- paste0("maturity_", maturities)
  colnames(coef_matrix) <- c("(Intercept)", paste0("PC", 1:n_pcs))

  # Return results
  list(
    residuals = residuals_list,
    fitted = fitted_list,
    coefficients = coef_matrix,
    r_squared = r_squared,
    n_obs = n_obs_used
  )
}
