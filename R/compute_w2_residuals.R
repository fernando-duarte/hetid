#' Compute Reduced Form Residuals for Y2 Variables
#'
#' Computes residuals W_\{2,t+1\}^\{(i)\} from regressing Y_\{2,t+1\}^\{(i)\} variables
#' (SDF innovations) on principal components extracted from financial asset returns and a constant.
#'
#' @template param-yields-term-premia
#' @param maturities Vector of maturities to process (default: 1:9)
#' @template param-n-pcs
#' @template param-pc-data
#' @param return_df Logical, if TRUE returns a data frame with
#'   dates (default FALSE)
#' @param dates Optional vector of dates for the returned data
#'   frame. If NULL and bundled PCs are used, bundled dates are
#'   used. If NULL and custom \code{pcs} are supplied, row
#'   indices are used instead.
#'
#' @return If return_df = FALSE, returns a list containing:
#' \describe{
#'   \item{residuals}{List of residual vectors, one for each maturity}
#'   \item{fitted}{List of fitted value vectors}
#'   \item{coefficients}{Matrix of regression coefficients (maturities x predictors)}
#'   \item{r_squared}{Vector of R-squared values for each maturity}
#'   \item{n_obs}{Number of observations used in each regression}
#'   \item{kept_idx}{List of logical vectors indicating which
#'     rows survived complete.cases filtering, one per maturity}
#' }
#' If return_df = TRUE, returns a data frame with columns:
#' \describe{
#'   \item{date}{Date column}
#'   \item{maturity}{Maturity identifier}
#'   \item{residuals}{Residuals \eqn{W_{2,t+1}}}
#'   \item{fitted}{Fitted values from the regression}
#' }
#'
#' @details
#' For each maturity i, computes Y_\{2,t+1\}^\{(i)\} as the SDF
#' innovation:
#' \eqn{Y_{2,t+1}^{(i)} = E_{t+1}[SDF_{t+1+i}] - E_t[SDF_{t+1+i}]}
#'
#' This is computed using the \code{compute_sdf_innovations()}
#' function. Then regresses Y_\{2,t+1\}^\{(i)\} on PC_t to get
#' residuals W_\{2,t+1\}^\{(i)\}.
#'
#' \strong{Bundled PCs alignment:} When \code{pcs = NULL},
#' bundled principal components from the \code{variables}
#' dataset are paired with yields \strong{by row position},
#' not by calendar date. If yields cover a different time
#' period than the bundled data (e.g., quarterly ACM data
#' starts 1961-Q2 while bundled PCs start 1962-Q1), each
#' row pairs observations from different quarters, producing
#' invalid regression results. For correct results, merge
#' your yield data with the bundled PCs by year-quarter and
#' pass the aligned matrix via the \code{pcs} parameter. See
#' the examples below for this workflow.
#'
#' @importFrom stats lm residuals fitted coef
#' @importFrom utils data
#' @export
#'
#' @examples
#' \dontrun{
#' # Load quarterly ACM data and bundled PCs
#' acm_data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(1, 2, 3, 4),
#'   frequency = "quarterly"
#' )
#' data("variables", package = "hetid")
#'
#' # Year-quarter keys for date alignment
#' acm_data$yq <- paste0(
#'   format(acm_data$date, "%Y"), "-",
#'   quarters(acm_data$date)
#' )
#' variables$yq <- paste0(
#'   format(variables$date, "%Y"), "-",
#'   quarters(variables$date)
#' )
#'
#' # Merge by year-quarter (covers quarters in both datasets)
#' pc_cols <- paste0("pc", 1:4)
#' merged <- merge(
#'   variables[, c("yq", pc_cols)],
#'   acm_data[, c("yq", grep("^(y[0-9]|tp)",
#'     names(acm_data),
#'     value = TRUE
#'   ))],
#'   by = "yq"
#' )
#'
#' # Extract aligned components
#' pcs <- as.matrix(merged[, pc_cols])
#' yields <- merged[, grep("^y[0-9]", names(merged))]
#' tp <- merged[, grep("^tp", names(merged))]
#'
#' # Compute residuals with aligned PCs
#' res_w2 <- compute_w2_residuals(
#'   yields, tp,
#'   maturities = c(2, 3), n_pcs = 4, pcs = pcs
#' )
#' }
compute_w2_residuals <- function(yields, term_premia,
                                 maturities = HETID_CONSTANTS$MIN_MATURITY:(
                                   HETID_CONSTANTS$EFFECTIVE_MAX_MATURITY),
                                 n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
                                 pcs = NULL, return_df = FALSE, dates = NULL) {
  # Validate inputs
  validate_n_pcs(n_pcs)
  validated <- validate_w2_inputs(yields, term_premia, maturities) # nolint: object_usage_linter
  yields_df <- validated$yields
  term_premia_df <- validated$term_premia
  maturities <- validated$maturities

  # Load or validate PCs (also returns bundled dates if available)
  pc_result <- load_w2_pcs(pcs, n_pcs, nrow(yields_df))
  pcs <- pc_result$pcs
  bundled_dates <- pc_result$dates # nolint: object_usage_linter

  # Initialize storage
  residuals_list <- list()
  fitted_list <- list()
  coef_matrix <- matrix(NA,
    nrow = length(maturities),
    ncol = n_pcs + 1
  ) # +1 for intercept
  r_squared <- numeric(length(maturities))
  n_obs_used <- numeric(length(maturities))
  kept_idx_list <- list()

  # Process each maturity
  for (idx in seq_along(maturities)) {
    i <- maturities[idx]

    # Process single maturity
    result <- process_w2_maturity( # nolint: object_usage_linter
      i, yields_df, term_premia_df, pcs, n_pcs
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
    kept_idx_list[[paste0("maturity_", i)]] <- result$kept_idx
  }

  # Set row/column names for coefficient matrix
  rownames(coef_matrix) <- paste0("maturity_", maturities)
  colnames(coef_matrix) <- c(
    "(Intercept)", get_pc_column_names(n_pcs)
  )

  if (return_df) {
    return(format_w2_dataframe(
      residuals_list = residuals_list,
      fitted_list = fitted_list,
      kept_idx_list = kept_idx_list,
      maturities = maturities,
      dates = dates,
      bundled_dates = bundled_dates,
      n_yield_rows = nrow(yields_df)
    ))
  }

  # Return list format (original)
  list(
    residuals = residuals_list,
    fitted = fitted_list,
    coefficients = coef_matrix,
    r_squared = r_squared,
    n_obs = n_obs_used,
    kept_idx = kept_idx_list
  )
}
