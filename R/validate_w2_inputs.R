#' Validate and Convert W2 Input Data
#'
#' Internal function to validate and convert yields and term_premia inputs
#'
#' @param yields Yields data (data frame or matrix)
#' @param term_premia Term premia data (data frame or matrix)
#' @param maturities Vector of maturities
#'
#' @return List with converted data frames and validated maturities
#' @keywords internal
validate_w2_inputs <- function(yields, term_premia, maturities) {
  # Check input types
  yields_valid <- is.data.frame(yields) || is.matrix(yields)
  tp_valid <- is.data.frame(term_premia) || is.matrix(term_premia)

  if (!yields_valid) {
    stop("yields must be a data frame or matrix")
  }
  if (!tp_valid) {
    stop("term_premia must be a data frame or matrix")
  }

  # Convert to data frames
  yields_df <- as.data.frame(yields)
  term_premia_df <- as.data.frame(term_premia)

  # Use standardized data dimension validation
  validate_data_dimensions(yields_df, term_premia_df)

  # Validate maturity range using academic constants
  max_maturity <- min(HETID_CONSTANTS$MAX_MATURITY, ncol(yields_df), ncol(term_premia_df))
  valid_maturities <- maturities[maturities <= max_maturity]

  if (length(valid_maturities) < length(maturities)) {
    warning(paste("Some maturities exceed available data. Using 1:", max_maturity))
  }

  list(
    yields = yields_df,
    term_premia = term_premia_df,
    maturities = valid_maturities
  )
}

#' Load Principal Components for W2
#'
#' Internal function to load or validate principal components
#'
#' @param pcs Provided PCs or NULL
#' @param n_pcs Number of PCs to use
#' @param n_obs Number of observations (for validation)
#'
#' @return Matrix of principal components
#' @keywords internal
load_w2_pcs <- function(pcs, n_pcs, n_obs) {
  if (is.null(pcs)) {
    # Get the variables data from the package
    data("variables", package = "hetid", envir = environment())
    variables <- get("variables", envir = environment())

    # Extract PCs
    pc_cols <- paste0("pc", 1:n_pcs)
    missing_cols <- setdiff(pc_cols, names(variables))

    if (length(missing_cols) > 0) {
      stop(paste(
        "Missing PC columns in variables data:",
        paste(missing_cols, collapse = ", ")
      ))
    }
    pcs <- as.matrix(variables[, pc_cols])

    # Return the PCs as-is - alignment will be handled by process_w2_maturity
    # This is expected when using quarterly ACM data with variables data
    return(pcs)
  } else {
    # User provided PCs
    pcs <- as.matrix(pcs)

    # Validate dimensions for user-provided PCs
    if (nrow(pcs) != n_obs) {
      stop("Number of rows in user-provided pcs must match number of rows in yields")
    }

    return(pcs)
  }
}

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

  # Check sufficient data
  if (length(y2_clean) < n_pcs + 2) {
    warning(paste("Insufficient data for maturity", i, ". Skipping."))
    return(NULL)
  }

  # Run regression
  reg_data <- data.frame(y = y2_clean, pcs_clean)
  names(reg_data)[-1] <- paste0("pc", 1:n_pcs)

  formula_str <- paste("y ~", paste(paste0("pc", 1:n_pcs), collapse = " + "))
  model <- lm(as.formula(formula_str), data = reg_data)

  list(
    residuals = residuals(model),
    fitted = fitted(model),
    coefficients = coef(model),
    r_squared = summary(model)$r.squared,
    n_obs = length(y2_clean)
  )
}
