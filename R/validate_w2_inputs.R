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

  # Validate maturity values
  if (any(maturities %% 1 != 0) || any(maturities < 1)) {
    stop("maturities must be positive integers")
  }

  max_maturity <- min(
    HETID_CONSTANTS$EFFECTIVE_MAX_MATURITY,
    ncol(yields_df), ncol(term_premia_df)
  )
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

#' Get Bundled Variables Dataset
#'
#' Loads the bundled variables dataset from package data.
#' Extracted as a separate function for testability.
#'
#' @return Data frame containing the variables dataset
#' @keywords internal
get_bundled_variables <- function() {
  data("variables", package = "hetid", envir = environment())
  get("variables", envir = environment())
}

#' Load Principal Components (and optionally dates) for W2
#'
#' Internal function to load or validate principal components.
#' When falling back to package data, also extracts dates
#' to avoid a redundant second load.
#'
#' @param pcs Provided PCs or NULL
#' @param n_pcs Number of PCs to use
#' @param n_obs Number of observations (for validation)
#'
#' @return List with components:
#'   \describe{
#'     \item{pcs}{Matrix of principal components}
#'     \item{dates}{Date vector from bundled data, or NULL
#'       if user provided PCs}
#'   }
#' @keywords internal
load_w2_pcs <- function(pcs, n_pcs, n_obs) {
  if (is.null(pcs)) {
    warning(
      "Using bundled 'variables' dataset for PCs. ",
      "Bundled PCs are aligned with yields by row ",
      "position, not by calendar date. For correct ",
      "date alignment, merge datasets by year-quarter",
      " and pass pcs= explicitly.",
      call. = FALSE
    )
    variables <- get_bundled_variables()

    # Extract PCs
    pc_cols <- get_pc_column_names(n_pcs)
    missing_cols <- setdiff(pc_cols, names(variables))

    if (length(missing_cols) > 0) {
      stop(paste(
        "Missing PC columns in variables data:",
        paste(missing_cols, collapse = ", ")
      ))
    }

    # One-period offset: dates[2:T] aligns with T-1 innovations
    bundled_dates <- if ("date" %in% names(variables)) {
      variables$date[2:nrow(variables)]
    } else {
      NULL
    }

    list(
      pcs = as.matrix(variables[, pc_cols]),
      dates = bundled_dates
    )
  } else {
    # User provided PCs
    pcs <- as.matrix(pcs)

    # Validate dimensions for user-provided PCs
    if (nrow(pcs) != n_obs) {
      stop(
        "Number of rows in user-provided pcs ",
        "must match number of rows in yields"
      )
    }
    if (ncol(pcs) < n_pcs) {
      stop(
        "pcs has ", ncol(pcs), " columns but n_pcs = ",
        n_pcs, "; supply at least ", n_pcs, " columns"
      )
    }

    list(pcs = pcs, dates = NULL)
  }
}
