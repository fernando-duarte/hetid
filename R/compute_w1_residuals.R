#' Compute Reduced Form Residual for Primary Endogenous Variable (Y1)
#'
#' Computes the residual \eqn{\omega_{1,t+1}} from regressing consumption growth
#' (\eqn{Y_{1,t+1}}) on principal components of nominal financial asset returns
#' (\eqn{PC_t}) and a constant, optionally including own-lags of the outcome.
#'
#' @param n_pcs Integer, number of principal components to use (1 to
#'   \code{HETID_CONSTANTS$MAX_N_PCS}). Default is
#'   \code{HETID_CONSTANTS$DEFAULT_N_PCS}.
#' @param data Optional data frame in chronological order, with a non-missing
#'   period-end \code{Date} column \code{date}, numeric consumption growth in
#'   \code{HETID_CONSTANTS$CONSUMPTION_GROWTH_COL}, and numeric \code{pc1} through
#'   \code{pc<n_pcs>} columns unless \code{exog} is supplied. The default
#'   \code{NULL} loads bundled data and normalizes its dates to quarter ends.
#' @param return_df Logical scalar; \code{TRUE} returns a data frame, while
#'   the default \code{FALSE} returns a list. Both shapes carry dates.
#' @param exog Optional finite numeric \eqn{T \times K} matrix or data frame with
#'   \eqn{K \ge 1}, replacing the PCs. Rows must match \code{data} in order.
#'   The default \code{NULL} uses PCs. Cannot be combined with explicit \code{n_pcs}.
#' @param y1_lags Non-negative integer number of own-lags \eqn{H} to include
#'   as predetermined regressors \eqn{Y_{1,t+1-h}}, \eqn{h = 1, \ldots, H}
#'   (default 0 = none). Lagging drops the first \eqn{H - 1} leading rows; the
#'   lag columns are appended to the PC (or \code{exog}) regressors.
#'
#' @return If \code{return_df = FALSE}, a list containing:
#' \describe{
#'   \item{residuals}{Numeric vector of residuals \eqn{\omega_{1,t+1}}.}
#'   \item{fitted}{Numeric vector of fitted values in the outcome's units.}
#'   \item{coefficients}{Named regression coefficients, including the intercept.}
#'   \item{r_squared}{Numeric scalar R-squared of the regression.}
#'   \item{dates}{Date vector of the t+1 realization dates of the residuals
#'     \eqn{\omega_{1,t+1}} (lead dates subset by the complete-case filter).}
#'   \item{kept_idx}{Logical vector of length \code{nrow(data) - 1}; \code{TRUE}
#'     marks retained predictor/next-outcome pairs. Used downstream to check
#'     that \eqn{Y_1} and \eqn{Y_2} are fit on the same sample.}
#'   \item{model}{The \code{lm} object from the regression. Formula-reserved
#'     regressor labels use internal names; its \code{hetid_regressor_names}
#'     attribute maps those names to public labels. Prediction data use the
#'     internal names. Ordinary labels require no mapping.}
#' }
#' If \code{return_df = TRUE}, a data frame with one row per retained pair and columns:
#' \describe{
#'   \item{date}{Date column of outcome realization dates.}
#'   \item{residuals}{Numeric residuals \eqn{\omega_{1,t+1}} in the outcome's units.}
#'   \item{fitted}{Numeric fitted values in the outcome's units.}
#' }
#'
#' @details
#' With no own-lags and the default PC regressors, the regression is:
#' \deqn{Y_{1,t+1} = \alpha + \beta^{\top} PC_t + \omega_{1,t+1}}
#'
#' where \eqn{Y_{1,t+1}} is consumption growth and \eqn{PC_t} contains the first
#' \code{n_pcs} principal components of nominal financial asset returns.
#'
#' When \code{exog} is supplied, its columns replace the PCs as
#' regressors under the same one-period lag convention: row t of
#' \code{exog} is paired with consumption growth at t+1.
#'
#' Rows are paired by position; supplied dates are neither sorted nor normalized.
#' Missing outcomes, PCs, or own-lags remove the affected pairs by complete cases;
#' \code{exog} must contain no missing or infinite values. Other inputs must be
#' numeric and finite on the complete pairs fitted. The regression needs at least
#' \code{n_reg + 2} complete pairs, where \code{n_reg} counts PCs or exogenous
#' columns plus own-lags. Too few pairs or collinear regressors raise a
#' \code{hetid_error}; invalid arguments and dimensions have specific subclasses.
#' Loading bundled data emits an informational message. No growth rescaling is applied.
#'
#' @importFrom stats lm residuals fitted coef as.formula complete.cases
#' @export
#'
#' @examples
#' # Bundled data use quarter-end realization dates
#' res_y1 <- compute_w1_residuals()
#'
#' # Use only first 2 PCs
#' res_y1_2pc <- compute_w1_residuals(n_pcs = 2)
#'
#' # Include one own-lag of the outcome as a predetermined regressor
#' res_y1_lag <- compute_w1_residuals(y1_lags = 1L)
#'
#' # Custom data and regressors must share the same dated rows
#' data("variables", package = "hetid")
#' variables$date <- to_period_end(variables$date, "quarterly")
#' exog <- as.matrix(variables[, c("pc1", "pc2", "pc3")])
#' res_y1_exog <- compute_w1_residuals(data = variables, exog = exog)
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
    exog <- prepare_w1_exog(exog, missing(n_pcs))
  }

  if (is.null(data)) {
    message(
      "Using bundled 'variables' dataset. ",
      "Pass data= explicitly to use your own data."
    )
    data <- get_bundled_variables()
  }

  # data$date requires a data frame even though assert_tabular() accepts matrices
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
    assert_bad_argument_ok(
      is.numeric(reg_matrix),
      "the pc columns of data must contain only numeric values",
      arg = "data"
    )
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
