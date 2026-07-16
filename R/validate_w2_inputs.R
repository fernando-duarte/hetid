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
  assert_tabular(yields, "yields")
  assert_tabular(term_premia, "term_premia")

  yields_df <- as.data.frame(yields)
  term_premia_df <- as.data.frame(term_premia)

  validate_data_dimensions(yields_df, term_premia_df)

  # No ncol-based cap: inputs may hold non-contiguous column subsets,
  # so column availability is checked per maturity in process_w2_maturity
  validate_step(step)
  validate_maturities(
    maturities,
    max_value = effective_max_maturity(step),
    max_label = "MAX_MATURITY - step",
    min_value = HETID_CONSTANTS$MIN_MATURITY
  )

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
#' Loads the bundled variables dataset from package data and normalizes its
#' dates to the package-wide period-end convention. The shipped file is
#' imported verbatim from its source repository with quarter-start labels,
#' so normalization happens here, at ingestion.
#'
#' @return Data frame containing the variables dataset, period-end dated
#' @keywords internal
get_bundled_variables <- function() {
  data("variables", package = "hetid", envir = environment())
  variables <- get("variables", envir = environment())
  variables[["date"]] <- to_period_end(variables[["date"]], "quarterly")
  variables
}

#' Validate Principal Components for W2
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
  # No bundled-by-position fallback
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
  # Type guard only: W2 regression tolerates interior NA in PCs
  # (dropped via complete.cases in run_pc_regression), so no finite check
  assert_bad_argument_ok(
    is.numeric(pcs),
    "pcs must contain only numeric values",
    arg = "pcs"
  )

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
