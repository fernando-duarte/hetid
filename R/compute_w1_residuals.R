#' Compute Reduced Form Residual for Primary Endogenous Variable (Y1)
#'
#' Computes the residual W_\{1,t+1\} from regressing consumption growth (Y_\{1,t+1\})
#' on principal components extracted from financial asset returns (PC_t) and a constant.
#'
#' @param n_pcs Integer, number of principal components to use (1 to
#'   \code{HETID_CONSTANTS$MAX_N_PCS}). Default is
#'   \code{HETID_CONSTANTS$DEFAULT_N_PCS}.
#' @param data Optional data frame containing the variables. If NULL,
#'   loads from package data. A \code{date} column (period-end \code{Date}) is
#'   always required: the residual series carries its realization dates in both
#'   return shapes.
#' @param return_df Logical, if TRUE returns a tidy data frame (default FALSE
#'   returns the richer list). Both shapes carry dates.
#' @param exog Optional T x K numeric matrix of exogenous regressors
#'   that replaces the bundled PC columns in the first-stage
#'   regression. Cannot be combined with an explicit \code{n_pcs}.
#' @param y1_lags Integer number of own-lags \eqn{H} of the outcome to include
#'   as predetermined regressors \eqn{Y_{1,t+1-h}}, \eqn{h = 1, \ldots, H}
#'   (default 0 = none). Lagging drops the first \eqn{H - 1} leading rows; the
#'   lag columns are appended to the PC (or \code{exog}) regressors.
#'
#' @return If return_df = FALSE, returns a list containing:
#' \describe{
#'   \item{residuals}{Numeric vector of residuals W_\{1,t+1\}}
#'   \item{fitted}{Numeric vector of fitted values}
#'   \item{coefficients}{Regression coefficients}
#'   \item{r_squared}{R-squared of the regression}
#'   \item{dates}{Date vector of the t+1 realization dates of the residuals
#'     W_\{1,t+1\} (the lead dates subset by the complete-case filter)}
#'   \item{kept_idx}{Integer indices (into the lagged/leading rows) of the
#'     observations retained by the regression's complete-case filter; used
#'     downstream to assert that \eqn{Y_1} and \eqn{Y_2} are fit on the same
#'     sample}
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
#' \deqn{Y_{1,t+1} = \alpha + \beta^{\top} PC_t + W_{1,t+1}}
#'
#' where Y_\{1,t+1\} is consumption growth and PC_t are the first n_pcs
#' principal components extracted from financial asset returns (pc1, ..., pc6).
#'
#' When \code{exog} is supplied, its columns replace the PCs as
#' regressors under the same one-period lag convention: row t of
#' \code{exog} is paired with consumption growth at t+1.
#'
#' @importFrom stats lm residuals fitted coef as.formula complete.cases
#' @export
#'
#' @examples
#' # Compute residuals using 4 PCs (default)
#' res_y1 <- compute_w1_residuals()
#'
#' # Use only first 2 PCs
#' res_y1_2pc <- compute_w1_residuals(n_pcs = 2)
#'
#' # Include one own-lag of the outcome as a predetermined regressor
#' res_y1_lag <- compute_w1_residuals(y1_lags = 1L)
#'
#' # Replace the PCs with custom exogenous regressors (not combined with n_pcs)
#' data("variables", package = "hetid")
#' exog <- as.matrix(variables[, c("pc1", "pc2", "pc3")])
#' res_y1_exog <- compute_w1_residuals(exog = exog)
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
#'
compute_w1_residuals <- function(n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
                                 data = NULL, return_df = FALSE,
                                 exog = NULL, y1_lags = 0L) {
  assert_flag(return_df, "return_df")
  if (is.null(exog)) {
    validate_n_pcs(n_pcs)
  } else {
    assert_bad_argument_ok(
      missing(n_pcs),
      "supply either n_pcs (bundled PCs) or exog, not both",
      arg = "n_pcs"
    )
    assert_tabular(exog, "exog")
    exog <- as.matrix(exog)
    assert_numeric_finite_values(exog, "exog")
    if (is.null(colnames(exog))) {
      colnames(exog) <- paste0("z", seq_len(ncol(exog)))
    }
  }

  if (is.null(data)) {
    message(
      "Using bundled 'variables' dataset. ",
      "Pass data= explicitly to use your own data."
    )
    data <- get_bundled_variables()
  }

  # assert_tabular alone admits matrices (NULL names); require data.frame for clear errors
  assert_tabular(data, "data")
  assert_bad_argument_ok(
    is.data.frame(data),
    "data must be a data frame, not a matrix; convert with as.data.frame()",
    arg = "data"
  )

  required_cols <- c("date", HETID_CONSTANTS$CONSUMPTION_GROWTH_COL)
  if (is.null(exog)) {
    required_cols <- c(required_cols, get_pc_column_names(n_pcs))
  }
  assert_columns_exist(data, required_cols, arg = "data")

  y1 <- data[[HETID_CONSTANTS$CONSUMPTION_GROWTH_COL]]
  dates <- data$date
  validate_dates_vector(dates, length(y1))
  y1_lags <- validate_y1_lags(y1_lags, length(y1))

  if (is.null(exog)) {
    reg_matrix <- as.matrix(data[, get_pc_column_names(n_pcs), drop = FALSE])
    n_reg <- n_pcs
  } else {
    assert_dimension_ok(
      nrow(exog) == length(y1),
      "exog must have one row per row of data"
    )
    reg_matrix <- exog
    n_reg <- ncol(exog)
  }

  if (y1_lags > 0L) {
    reg_matrix <- append_y1_lags(reg_matrix, y1, y1_lags)
    n_reg <- n_reg + y1_lags
  }

  n <- length(y1)
  assert_insufficient_data_ok(
    n >= 2,
    "Need at least 2 observations for lagging"
  )
  reg_lagged <- reg_matrix[
    seq_len(n - 1), ,
    drop = FALSE
  ]
  y1_future <- y1[seq.int(2L, n)]
  dates_future <- dates[seq.int(2L, n)]

  reg <- run_pc_regression(y1_future, reg_lagged, n_reg)
  dates_clean <- dates_future[reg$complete_idx]

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
    kept_idx = reg$complete_idx,
    model = reg$model
  )
}
