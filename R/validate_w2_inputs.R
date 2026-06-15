#' Default W2 Maturity Horizons
#'
#' Step-spaced news horizons from \code{step} to
#' \code{MAX_MATURITY - step} that satisfy the news contract: each
#' horizon equals \code{step} (the boundary case, needing the
#' step-maturity yield) or keeps \code{horizon - step} at or above
#' \code{MIN_MATURITY}.
#'
#' @template param-step
#' @return Integer vector of valid default maturities
#' @keywords internal
default_w2_maturities <- function(step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_step(step)
  candidates <- seq(step, HETID_CONSTANTS$MAX_MATURITY - step, by = step)
  keep <- news_contract_ok(candidates, step) &
    (candidates != step | step >= HETID_CONSTANTS$MIN_MATURITY)
  candidates[keep]
}

#' Validate and Convert W2 Input Data
#'
#' Internal function to validate and convert yields and term_premia inputs
#'
#' @param yields Yields data (data frame or matrix)
#' @param term_premia Term premia data (data frame or matrix)
#' @param maturities Vector of maturities
#' @template param-step
#'
#' @return List with converted data frames and validated maturities
#' @keywords internal
validate_w2_inputs <- function(yields, term_premia, maturities,
                               step = HETID_CONSTANTS$DEFAULT_STEP) {
  # Check input types
  assert_tabular(yields, "yields")
  assert_tabular(term_premia, "term_premia")

  # Convert to data frames
  yields_df <- as.data.frame(yields)
  term_premia_df <- as.data.frame(term_premia)

  # Use standardized data dimension validation
  validate_data_dimensions(yields_df, term_premia_df)

  # Validate maturity values via the shared vector validator.
  # No ncol-based cap: inputs may hold non-contiguous column
  # subsets (e.g. y12/y48/y60/y72 for maturity 60), so column
  # availability is checked per maturity in process_w2_maturity
  validate_step(step)
  validate_maturities(
    maturities,
    max_value = effective_max_maturity(step),
    max_label = "MAX_MATURITY - step",
    min_value = HETID_CONSTANTS$MIN_MATURITY
  )

  # Each maturity must satisfy the news contract: the previous-period
  # index is the boundary case or stays within the data range
  assert_news_contract_ok(
    maturities, step,
    arg = "maturities", subject = "maturities", offset_label = "maturity",
    include_invalid = TRUE
  )

  list(
    yields = yields_df,
    term_premia = term_premia_df,
    maturities = maturities
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
#' Internal function to validate the supplied principal components.
#'
#' @param pcs Supplied PC matrix (required; aligned to yields by date)
#' @param n_pcs Number of PCs to use
#' @param n_obs Number of observations (for validation)
#'
#' @return List with components:
#'   \describe{
#'     \item{pcs}{Matrix of principal components}
#'     \item{pc_names}{Character labels for the first n_pcs
#'       regressor columns}
#'   }
#' @keywords internal
load_w2_pcs <- function(pcs, n_pcs, n_obs) {
  # PCs must be supplied already aligned to the yields (one row per yield row,
  # paired by calendar date upstream). There is no bundled-by-row-position
  # fallback: principal components and yields are joined by date before they
  # reach this function (see compute_identification_residuals / create_data.R).
  assert_bad_argument_ok(
    !is.null(pcs),
    paste0(
      "pcs must be supplied as a numeric matrix aligned to yields by ",
      "calendar date (one row per yield row)."
    ),
    arg = "pcs"
  )

  assert_tabular(pcs, "pcs")
  pcs <- as.matrix(pcs)
  # Only a type guard here (NOT assert_numeric_finite_values): unlike
  # the moments path, the W2 regression tolerates interior NA in PCs
  # and drops those rows via complete.cases in run_pc_regression (see
  # the interior-NA alignment test), so NA must be allowed through.
  assert_bad_argument_ok(
    is.numeric(pcs),
    "pcs must contain only numeric values",
    arg = "pcs"
  )

  # Validate dimensions; the n_pcs <= ncol(pcs) cap is enforced upstream
  # in compute_w2_residuals
  assert_dimension_ok(
    nrow(pcs) == n_obs,
    paste0(
      "Number of rows in pcs must match number of rows in yields"
    )
  )

  pc_names <- colnames(pcs)[seq_len(n_pcs)]
  if (is.null(pc_names) || anyNA(pc_names) || !all(nzchar(pc_names))) {
    pc_names <- get_pc_column_names(n_pcs)
  }

  list(pcs = pcs, pc_names = pc_names)
}
