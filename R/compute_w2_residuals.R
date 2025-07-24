#' Compute Reduced Form Residuals for Y2 Variables
#'
#' Computes residuals W_\{2,t+1\}^\{(i)\} from regressing Y_\{2,t+1\}^\{(i)\} variables
#' (SDF innovations) on principal components extracted from financial asset returns and a constant.
#'
#' @template param-yields-term-premia
#' @param maturities Vector of maturities to process (default: 1:9)
#' @template param-n-pcs
#' @template param-pc-data
#' @template param-return-df-dates
#'
#' @return If return_df = FALSE, returns a list containing:
#' \describe{
#'   \item{residuals}{List of residual vectors, one for each maturity}
#'   \item{fitted}{List of fitted value vectors}
#'   \item{coefficients}{Matrix of regression coefficients (maturities x predictors)}
#'   \item{r_squared}{Vector of R-squared values for each maturity}
#'   \item{n_obs}{Number of observations used in each regression}
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
#' For each maturity i, computes Y_\{2,t+1\}^\{(i)\} as the SDF innovation:
#' \eqn{Y_{2,t+1}^{(i)} = E_{t+1}[SDF_{t+1+i}] - E_t[SDF_{t+1+i}]}
#'
#' This is computed using the compute_sdf_innovations() function.
#' Then regresses Y_\{2,t+1\}^\{(i)\} on PC_t to get residuals W_\{2,t+1\}^\{(i)\}.
#'
#' @importFrom stats lm residuals fitted coef
#' @importFrom utils data
#' @export
#'
#' @examples
#' \dontrun{
#' # Load ACM data - need maturities 1, i-1, i, i+1 for each maturity i
#' # For maturities 2, 3, we need maturities 1, 2, 3, 4
#' acm_data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(1, 2, 3, 4)
#' )
#' yields <- acm_data[, grep("^y", names(acm_data))]
#' term_premia <- acm_data[, grep("^tp", names(acm_data))]
#'
#' # Compute residuals for specific maturities
#' res_y2 <- compute_w2_residuals(yields, term_premia, maturities = c(2, 3))
#'
#' # Get results as data frame
#' res_y2_df <- compute_w2_residuals(yields, term_premia,
#'   maturities = c(2, 3),
#'   return_df = TRUE
#' )
#' head(res_y2_df)
#' }
compute_w2_residuals <- function(yields, term_premia,
                                 maturities = HETID_CONSTANTS$MIN_MATURITY:(
                                   HETID_CONSTANTS$MAX_MATURITY - 1),
                                 n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
                                 pcs = NULL, return_df = FALSE, dates = NULL) {
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
  }

  # Set row/column names for coefficient matrix
  rownames(coef_matrix) <- paste0("maturity_", maturities)
  colnames(coef_matrix) <- c("(Intercept)", paste0("pc", 1:n_pcs))

  # Handle return_df option
  if (return_df) {
    # Create data frame format
    df_list <- list()

    # Get dates - try to extract from data or use provided dates
    if (is.null(dates)) {
      # Try to get dates from variables data
      data("variables", package = "hetid", envir = environment())
      variables <- get("variables", envir = environment())
      if ("date" %in% names(variables)) {
        # SDF innovations have T-1 observations, aligned with dates 2:T
        dates <- variables$date[2:nrow(variables)]
      } else {
        # Use generic indices
        dates <- 1:(nrow(yields_df) - 1)
      }
    }

    # Build data frame for each maturity
    for (idx in seq_along(maturities)) {
      i <- maturities[idx]
      mat_key <- paste0("maturity_", i)

      if (mat_key %in% names(residuals_list)) {
        n_obs <- length(residuals_list[[mat_key]])
        df_list[[idx]] <- data.frame(
          date = dates[1:n_obs],
          maturity = i,
          residuals = residuals_list[[mat_key]],
          fitted = fitted_list[[mat_key]],
          stringsAsFactors = FALSE
        )
      }
    }

    # Combine all maturities
    return(do.call(rbind, df_list))
  }

  # Return list format (original)
  list(
    residuals = residuals_list,
    fitted = fitted_list,
    coefficients = coef_matrix,
    r_squared = r_squared,
    n_obs = n_obs_used
  )
}
