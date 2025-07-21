#' Compute Reduced Form Residual for Primary Endogenous Variable (Y1)
#'
#' Computes the residual W_{1,t+1} from regressing consumption growth (Y_{1,t+1})
#' on principal components (PC_t) and a constant.
#'
#' @param n_pcs Integer, number of principal components to use (1-6). Default is 4.
#' @param data Optional data frame containing the variables. If NULL, loads from package data.
#' @param return_df Logical, if TRUE returns a data frame with dates (default FALSE).
#'
#' @return If return_df = FALSE, returns a list containing:
#' \describe{
#'   \item{residuals}{Numeric vector of residuals W_{1,t+1}}
#'   \item{fitted}{Numeric vector of fitted values}
#'   \item{coefficients}{Regression coefficients}
#'   \item{r_squared}{R-squared of the regression}
#'   \item{dates}{Date vector corresponding to residuals}
#'   \item{model}{The lm object from the regression}
#' }
#' If return_df = TRUE, returns a data frame with columns:
#' \describe{
#'   \item{date}{Date column}
#'   \item{residuals}{Residuals W_{1,t+1}}
#'   \item{fitted}{Fitted values from the regression}
#' }
#'
#' @details
#' The function performs the regression:
#' Y_{1,t+1} = alpha + beta' * PC_t + W_{1,t+1}
#'
#' where Y_{1,t+1} is consumption growth (gr1.pcecc96) and PC_t are the first n_pcs
#' principal components (pc1, ..., pc6).
#'
#' @importFrom stats lm residuals fitted coef as.formula complete.cases
#' @importFrom utils data
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
compute_w1_residuals <- function(n_pcs = 4, data = NULL, return_df = FALSE) {
  # Validate inputs
  if (!n_pcs %in% 1:6) {
    stop("n_pcs must be between 1 and 6")
  }

  # Load data if not provided
  if (is.null(data)) {
    # Get the variables data from the package
    data("variables", package = "hetid", envir = environment())
    data <- get("variables", envir = environment())
  }

  # Check required columns
  required_cols <- c("date", "gr1.pcecc96", paste0("pc", 1:n_pcs))
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Extract relevant variables
  y1 <- data$gr1.pcecc96 # Consumption growth (not forwarded)
  dates <- data$date

  # Create PC matrix
  pc_cols <- paste0("pc", 1:n_pcs)
  pc_matrix <- as.matrix(data[, pc_cols])

  # Create lagged PCs to align with Y_{t+1}
  # Since y1 is gr1 (not fgr1), we need to lag PCs
  n <- length(y1)
  pc_lagged <- pc_matrix[1:(n - 1), , drop = FALSE]
  y1_future <- y1[2:n]
  dates_future <- dates[2:n]

  # Remove any rows with missing values
  complete_idx <- complete.cases(y1_future, pc_lagged)
  y1_clean <- y1_future[complete_idx]
  pc_clean <- pc_lagged[complete_idx, , drop = FALSE]
  dates_clean <- dates_future[complete_idx]

  # Create formula for regression
  pc_names <- paste0("PC", 1:n_pcs)
  colnames(pc_clean) <- pc_names
  formula_str <- paste("y ~ ", paste(pc_names, collapse = " + "))
  reg_formula <- as.formula(formula_str)

  # Combine into data frame for regression
  reg_data <- data.frame(y = y1_clean, pc_clean)

  # Run regression
  model <- lm(reg_formula, data = reg_data)

  # Extract results
  residuals <- residuals(model)
  fitted_values <- fitted(model)
  coefficients <- coef(model)
  r_squared <- summary(model)$r.squared

  # Return results
  if (return_df) {
    # Return data frame with dates
    return(data.frame(
      date = dates_clean,
      residuals = residuals,
      fitted = fitted_values,
      stringsAsFactors = FALSE
    ))
  }

  # Return list (original format)
  list(
    residuals = residuals,
    fitted = fitted_values,
    coefficients = coefficients,
    r_squared = r_squared,
    dates = dates_clean,
    model = model
  )
}
