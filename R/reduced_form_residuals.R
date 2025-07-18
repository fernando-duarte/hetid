#' Compute Reduced Form Residual for Primary Endogenous Variable (Y1)
#'
#' Computes the residual W_\{1,t+1\} from regressing consumption growth (Y_\{1,t+1\})
#' on principal components (PC_t) and a constant.
#'
#' @param n_pcs Integer, number of principal components to use (1-6). Default is 4.
#' @param data Optional data frame containing the variables. If NULL, loads from package data.
#'
#' @return A list containing:
#' \describe{
#'   \item{residuals}{Numeric vector of residuals W_\{1,t+1\}}
#'   \item{fitted}{Numeric vector of fitted values}
#'   \item{coefficients}{Regression coefficients}
#'   \item{r_squared}{R-squared of the regression}
#'   \item{dates}{Date vector corresponding to residuals}
#'   \item{model}{The lm object from the regression}
#' }
#'
#' @details
#' The function performs the regression:
#' Y_\{1,t+1\} = alpha + beta' * PC_t + W_\{1,t+1\}
#'
#' where Y_\{1,t+1\} is consumption growth (fgr1.gdpc1) and PC_t are the first n_pcs
#' principal components (pc1, ..., pc6).
#'
#' @importFrom stats lm residuals fitted coef as.formula complete.cases
#' @importFrom utils data
#' @export
#'
#' @examples
#' \dontrun{
#' # Compute residuals using 4 PCs (default)
#' res_y1 <- compute_reduced_form_residual_y1()
#'
#' # Use only first 2 PCs
#' res_y1_2pc <- compute_reduced_form_residual_y1(n_pcs = 2)
#'
#' # Plot residuals
#' plot(res_y1$dates, res_y1$residuals,
#'   type = "l",
#'   xlab = "Date", ylab = "Residual",
#'   main = "Reduced Form Residuals for Consumption Growth"
#' )
#' }
#'
compute_reduced_form_residual_y1 <- function(n_pcs = 4, data = NULL) {
  # Validate inputs
  if (!n_pcs %in% 1:6) {
    stop("n_pcs must be between 1 and 6")
  }

  # Load data if not provided
  if (is.null(data)) {
    if (!exists("variables")) {
      data("variables", package = "hetid", envir = environment())
    }
    data <- get("variables", envir = environment())
  }

  # Check required columns
  required_cols <- c("fgr1.gdpc1", paste0("pc", 1:n_pcs), "date")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Extract Y1 (consumption growth)
  y1 <- data$fgr1.gdpc1

  # Extract PCs
  pc_cols <- paste0("pc", 1:n_pcs)
  pc_data <- as.matrix(data[, pc_cols, drop = FALSE])

  # Remove rows with any NA values
  complete_rows <- complete.cases(y1, pc_data)
  y1_clean <- y1[complete_rows]
  pc_clean <- pc_data[complete_rows, , drop = FALSE]
  dates_clean <- data$date[complete_rows]

  # Check if we have enough data
  if (length(y1_clean) < n_pcs + 2) {
    stop("Not enough complete observations for regression")
  }

  # Run regression
  formula_str <- paste("y1_clean ~ ", paste(pc_cols, collapse = " + "))
  reg_data <- as.data.frame(cbind(y1_clean = y1_clean, pc_clean))

  model <- lm(as.formula(formula_str), data = reg_data)

  # Extract results
  residuals <- residuals(model)
  fitted <- fitted(model)
  coefficients <- coef(model)
  r_squared <- summary(model)$r.squared

  # Return results
  list(
    residuals = residuals,
    fitted = fitted,
    coefficients = coefficients,
    r_squared = r_squared,
    dates = dates_clean,
    model = model,
    n_pcs = n_pcs,
    n_obs = length(residuals)
  )
}


#' Compute Reduced Form Residuals for Secondary Endogenous Variables (Y2)
#'
#' Computes the residuals W_\{2,i,t+1\} from regressing SDF news innovations (Y_\{2,i,t+1\})
#' on principal components (PC_t) and a constant for each maturity i.
#'
#' @param yields Data frame with columns y1, y2, ..., containing yields
#' @param term_premia Data frame with columns tp1, tp2, ..., containing term premia
#' @param maturities Vector of maturities (i values) to compute residuals for
#' @param n_pcs Integer, number of principal components to use (1-6). Default is 4.
#' @param variables_data Optional data frame containing PC variables. If NULL, loads from package.
#'
#' @return A list containing results for each maturity i:
#' \describe{
#'   \item{residuals}{List of residual vectors W_\{2,i,t+1\} for each i}
#'   \item{fitted}{List of fitted value vectors for each i}
#'   \item{coefficients}{Matrix of regression coefficients (rows = maturities)}
#'   \item{r_squared}{Vector of R-squared values for each maturity}
#'   \item{dates}{Date vector corresponding to residuals}
#'   \item{models}{List of lm objects for each maturity}
#'   \item{sdf_innovations}{List of raw SDF innovation series for each maturity}
#' }
#'
#' @details
#' For each maturity i, the function:
#' 1. Computes SDF innovations Y_\{2,i,t+1\} using compute_sdf_innovations()
#' 2. Performs the regression: Y_\{2,i,t+1\} = alpha_i + beta_i' * PC_t + W_\{2,i,t+1\}
#'
#' The residuals W_\{2,i,t+1\} are returned for each maturity.
#'
#' @importFrom stats lm residuals fitted coef as.formula complete.cases
#' @importFrom utils data
#' @export
#'
#' @examples
#' \dontrun{
#' # Load ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#' yields_data <- data[, grep("^y", names(data))]
#' tp_data <- data[, grep("^tp", names(data))]
#'
#' # Compute residuals for maturities 1, 2, 5
#' res_y2 <- compute_reduced_form_residual_y2(
#'   yields = yields_data,
#'   term_premia = tp_data,
#'   maturities = c(1, 2, 5),
#'   n_pcs = 4
#' )
#'
#' # Access residuals for maturity i=5
#' w2_5 <- res_y2$residuals[[which(res_y2$maturities == 5)]]
#' }
#'
compute_reduced_form_residual_y2 <- function(yields, term_premia, maturities,
                                             n_pcs = 4, variables_data = NULL) {
  # Validate inputs
  if (!n_pcs %in% 1:6) {
    stop("n_pcs must be between 1 and 6")
  }

  # Load variables data if not provided
  if (is.null(variables_data)) {
    if (!exists("variables")) {
      data("variables", package = "hetid", envir = environment())
    }
    variables_data <- get("variables", envir = environment())
  }

  # Check required columns in variables data
  pc_cols <- paste0("pc", 1:n_pcs)
  required_cols <- c(pc_cols, "date")
  missing_cols <- setdiff(required_cols, names(variables_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in variables data: ", paste(missing_cols, collapse = ", "))
  }

  # Initialize storage
  residuals_list <- list()
  fitted_list <- list()
  coefficients_matrix <- matrix(NA,
    nrow = length(maturities),
    ncol = n_pcs + 1, # +1 for intercept
    dimnames = list(
      paste0("i=", maturities),
      c("(Intercept)", pc_cols)
    )
  )
  r_squared_vec <- numeric(length(maturities))
  models_list <- list()
  sdf_innovations_list <- list()

  # Process each maturity
  for (idx in seq_along(maturities)) {
    i <- maturities[idx]

    # Compute SDF innovations for this maturity
    sdf_innov <- compute_sdf_innovations(yields, term_premia, i)
    sdf_innovations_list[[idx]] <- sdf_innov

    # The SDF innovations have T-1 observations (where T is the number of yield observations)
    # We need to align with the variables data

    # Get the number of yield observations
    n_yield_obs <- nrow(yields)

    # Create alignment: SDF innovations correspond to t=1:(T-1) in original data
    # This means we use PC data from t=1:(T-1) to explain innovations at t+1
    if (nrow(variables_data) < n_yield_obs) {
      warning("Variables data has fewer observations than yield data. Results may be misaligned.")
    }

    # Extract PC data for regression (first T-1 observations)
    max_obs <- min(length(sdf_innov), nrow(variables_data))
    pc_data <- as.matrix(variables_data[1:max_obs, pc_cols, drop = FALSE])
    y2_data <- sdf_innov[1:max_obs]
    dates_subset <- variables_data$date[1:max_obs]

    # Remove rows with any NA values
    complete_rows <- complete.cases(y2_data, pc_data)
    y2_clean <- y2_data[complete_rows]
    pc_clean <- pc_data[complete_rows, , drop = FALSE]
    dates_clean <- dates_subset[complete_rows]

    # Check if we have enough data
    if (length(y2_clean) < n_pcs + 2) {
      warning(paste(
        "Not enough complete observations for maturity i =", i,
        ". Skipping."
      ))
      next
    }

    # Run regression
    formula_str <- paste("y2_clean ~ ", paste(pc_cols, collapse = " + "))
    reg_data <- as.data.frame(cbind(y2_clean = y2_clean, pc_clean))

    model <- lm(as.formula(formula_str), data = reg_data)

    # Store results
    residuals_list[[idx]] <- residuals(model)
    fitted_list[[idx]] <- fitted(model)
    coefficients_matrix[idx, ] <- coef(model)
    r_squared_vec[idx] <- summary(model)$r.squared
    models_list[[idx]] <- model
  }

  # Return results
  list(
    residuals = residuals_list,
    fitted = fitted_list,
    coefficients = coefficients_matrix,
    r_squared = r_squared_vec,
    dates = dates_clean, # Uses dates from last maturity processed
    models = models_list,
    sdf_innovations = sdf_innovations_list,
    maturities = maturities,
    n_pcs = n_pcs
  )
}
