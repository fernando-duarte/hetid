#' Compute Reduced Form Residual for Primary Endogenous Variable (Y1)
#'
#' Computes the residual W_\{1,t+1\} from regressing consumption growth (Y_\{1,t+1\})
#' on principal components extracted from financial asset returns (PC_t) and a constant.
#'
#' @param n_pcs Integer, number of principal components to use (1-6). Default is 4.
#' @param data Optional data frame containing the variables. If NULL,
#'   loads from package data. A \code{date} column is required when
#'   \code{return_df = TRUE} but optional otherwise.
#' @param return_df Logical, if TRUE returns a data frame with dates
#'   (default FALSE). Requires a \code{date} column in \code{data}.
#'
#' @return If return_df = FALSE, returns a list containing:
#' \describe{
#'   \item{residuals}{Numeric vector of residuals W_\{1,t+1\}}
#'   \item{fitted}{Numeric vector of fitted values}
#'   \item{coefficients}{Regression coefficients}
#'   \item{r_squared}{R-squared of the regression}
#'   \item{dates}{Date vector corresponding to residuals, or NULL
#'     if no \code{date} column was provided}
#'   \item{model}{The lm object from the regression}
#' }
#' If return_df = TRUE, returns a data frame with columns:
#' \describe{
#'   \item{date}{Date column}
#'   \item{residuals}{Residuals W_\{1,t+1\}}
#'   \item{fitted}{Fitted values from the regression}
#' }
#'
#' @details
#' The function performs the regression:
#' Y_\{1,t+1\} = alpha + beta' * PC_t + W_\{1,t+1\}
#'
#' where Y_\{1,t+1\} is consumption growth and PC_t are the first n_pcs
#' principal components extracted from financial asset returns (pc1, ..., pc6).
#'
#' @importFrom stats lm residuals fitted coef as.formula complete.cases
#' @export
#'
#' @examples
#' \dontrun{
#' # Compute residuals using 4 PCs (default)
#' res_y1 <- compute_w1_residuals()
#'
#' # Use only first 2 PCs
#' res_y1_2pc <- compute_w1_residuals(n_pcs = 2)
#'
#' # Get results as data frame
#' res_y1_df <- compute_w1_residuals(n_pcs = 4, return_df = TRUE)
#' head(res_y1_df)
#'
#' # Plot residuals
#' plot(res_y1$dates, res_y1$residuals,
#'   type = "l",
#'   xlab = "Date", ylab = "Residual",
#'   main = "Reduced Form Residuals for Consumption Growth"
#' )
#' }
#'
compute_w1_residuals <- function(n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
                                 data = NULL, return_df = FALSE) {
  # Validate inputs
  validate_n_pcs(n_pcs)

  # Load data if not provided
  if (is.null(data)) {
    message(
      "Using bundled 'variables' dataset. ",
      "Pass data= explicitly to use your own data."
    )
    data <- get_bundled_variables()
  }

  # Check required columns (date is optional unless return_df)
  has_dates <- "date" %in% names(data)
  required_cols <- c(
    HETID_CONSTANTS$CONSUMPTION_GROWTH_COL,
    get_pc_column_names(n_pcs)
  )
  if (return_df && !has_dates) {
    required_cols <- c("date", required_cols)
  }
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Extract relevant variables
  y1 <- data[[HETID_CONSTANTS$CONSUMPTION_GROWTH_COL]]
  dates <- if (has_dates) data$date else NULL

  # Create PC matrix
  pc_cols <- get_pc_column_names(n_pcs)
  pc_matrix <- as.matrix(data[, pc_cols])

  # Regress Y_{t+1} on PC_t: lag PCs, lead Y1
  n <- length(y1)
  pc_lagged <- pc_matrix[1:(n - 1), , drop = FALSE]
  y1_future <- y1[2:n]
  dates_future <- if (!is.null(dates)) dates[2:n] else NULL

  reg <- run_pc_regression(y1_future, pc_lagged, n_pcs)
  dates_clean <- if (!is.null(dates_future)) {
    dates_future[reg$complete_idx]
  } else {
    NULL
  }

  if (return_df) {
    return(data.frame(
      date = dates_clean,
      residuals = reg$residuals,
      fitted = reg$fitted,
      stringsAsFactors = FALSE
    ))
  }

  list(
    residuals = reg$residuals,
    fitted = reg$fitted,
    coefficients = reg$coefficients,
    r_squared = reg$r_squared,
    dates = dates_clean,
    model = reg$model
  )
}
