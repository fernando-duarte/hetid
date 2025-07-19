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
  # Validate inputs
  if (!is.data.frame(yields) && !is.matrix(yields)) {
    stop("yields must be a data frame or matrix")
  }
  if (!is.data.frame(term_premia) && !is.matrix(term_premia)) {
    stop("term_premia must be a data frame or matrix")
  }

  # Convert to data frames if matrices
  if (is.matrix(yields)) yields <- as.data.frame(yields)
  if (is.matrix(term_premia)) term_premia <- as.data.frame(term_premia)

  # Check maturity range
  max_maturity <- min(10, ncol(yields), ncol(term_premia))
  if (any(maturities > max_maturity)) {
    warning(paste("Some maturities exceed available data. Using 1:", max_maturity))
    maturities <- maturities[maturities <= max_maturity]
  }

  # Load PCs if not provided
  if (is.null(pcs)) {
    # Get the variables data from the package
    data("variables", package = "hetid", envir = environment())
    variables <- get("variables", envir = environment())

    # Extract PCs
    pc_cols <- paste0("pc", 1:n_pcs)
    if (!all(pc_cols %in% names(variables))) {
      stop(paste(
        "Missing PC columns in variables data:",
        paste(setdiff(pc_cols, names(variables)), collapse = ", ")
      ))
    }
    pcs <- as.matrix(variables[, pc_cols])
  }

  # Ensure PCs is a matrix
  if (!is.matrix(pcs)) {
    pcs <- as.matrix(pcs)
  }

  # Check dimensions
  n_obs <- nrow(yields)
  if (nrow(pcs) != n_obs) {
    stop("Number of rows in pcs must match number of rows in yields")
  }

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

    # Get yield column
    y_col <- paste0("y", i)
    if (!y_col %in% names(yields)) {
      warning(paste("Yield column", y_col, "not found. Skipping maturity", i))
      next
    }
    y_i <- yields[[y_col]]

    # Get term premium if using adjustment
    if (use_tp_adjustment) {
      tp_col <- paste0("tp", i)
      if (!tp_col %in% names(term_premia)) {
        warning(paste("Term premium column", tp_col, "not found. Skipping maturity", i))
        next
      }
      tp_i <- term_premia[[tp_col]]
      # Convert to decimal if needed (assuming inputs are in percentage)
      y_i_decimal <- y_i / 100
      tp_i_decimal <- tp_i / 100
      y2_base <- y_i_decimal - tp_i_decimal
    } else {
      y_i_decimal <- y_i / 100
      y2_base <- y_i_decimal
    }

    # Create Y2_{t+1} (shift forward by 1)
    y2_future <- y2_base[-1]

    # Use lagged PCs (PC_t to predict Y2_{t+1})
    pcs_lagged <- pcs[-nrow(pcs), , drop = FALSE]

    # Remove missing values
    complete_idx <- complete.cases(y2_future, pcs_lagged)
    y2_clean <- y2_future[complete_idx]
    pcs_clean <- pcs_lagged[complete_idx, , drop = FALSE]

    # Skip if insufficient data
    if (length(y2_clean) < n_pcs + 2) {
      warning(paste("Insufficient data for maturity", i, ". Skipping."))
      next
    }

    # Prepare regression data
    reg_data <- data.frame(
      y = y2_clean,
      pcs_clean
    )
    names(reg_data)[-1] <- paste0("PC", 1:n_pcs)

    # Create formula
    formula_str <- paste("y ~", paste(paste0("PC", 1:n_pcs), collapse = " + "))

    # Run regression
    model <- lm(as.formula(formula_str), data = reg_data)

    # Store results
    residuals_list[[paste0("maturity_", i)]] <- residuals(model)
    fitted_list[[paste0("maturity_", i)]] <- fitted(model)
    coef_matrix[idx, ] <- coef(model)
    r_squared[idx] <- summary(model)$r.squared
    n_obs_used[idx] <- length(y2_clean)
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

#' @rdname compute_w2_residuals
#' @export
compute_reduced_form_residual_y2 <- function(yields, term_premia, maturities = 1:9,
                                             n_pcs = 4, pcs = NULL,
                                             use_tp_adjustment = TRUE) {
  .Deprecated("compute_w2_residuals")
  compute_w2_residuals(yields, term_premia, maturities, n_pcs, pcs, use_tp_adjustment)
}
