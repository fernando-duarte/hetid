#' Default \eqn{\omega_2} Maturity Horizons
#'
#' Step-spaced news horizons from \code{step} to
#' \code{MAX_MATURITY - step} that satisfy the news contract: each
#' horizon equals \code{step} (the boundary case, needing the
#' step-maturity yield) or keeps \code{horizon - step} at or above
#' \code{MIN_MATURITY}.
#'
#' @template param-step
#' @details
#' Maturity indices and \code{step} are in months. The step must be a
#' finite positive integer no greater than half of \code{HETID_CONSTANTS$MAX_MATURITY};
#' invalid values raise a \code{hetid_error_bad_argument} condition.
#' @return Numeric vector of valid default maturities in increasing order.
#' @keywords internal
default_w2_maturities <- function(step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_step(step)
  candidates <- seq(step, effective_max_maturity(step), by = step)
  keep <- news_contract_ok(candidates, step) &
    (candidates != step | step >= HETID_CONSTANTS$MIN_MATURITY)
  candidates[keep]
}

#' Validate and Convert \eqn{\omega_2} Input Data
#'
#' Checks input dimensions and news horizons, converting yields and term
#' premia to data frames without dropping observations.
#'
#' @param yields Data frame or matrix of yields.
#' @param term_premia Data frame or matrix of term premia with the same
#'   numbers of rows and columns as \code{yields}.
#' @param maturities Nonempty numeric vector of distinct, finite integer
#'   bond maturities in months, between \code{MIN_MATURITY} and
#'   \code{MAX_MATURITY - step}. Each must equal \code{step} or satisfy
#'   \code{maturity - step >= MIN_MATURITY}.
#' @template param-step
#'
#' @details
#' Inputs must already be aligned by calendar date. This helper checks
#' dimensions, not dates, column availability, numeric contents, or missing
#' values. Column availability is checked later for each maturity.
#' The step must be a finite positive integer no greater than half of
#' \code{HETID_CONSTANTS$MAX_MATURITY}. Invalid types or horizons raise
#' \code{hetid_error_bad_argument}; unequal dimensions raise
#' \code{hetid_error_dimension_mismatch}.
#'
#' @return A list with \code{yields} and \code{term_premia} as data frames,
#'   and \code{maturities} unchanged, including its order and names.
#' @keywords internal
validate_w2_inputs <- function(yields, term_premia, maturities,
                               step = HETID_CONSTANTS$DEFAULT_STEP) {
  assert_tabular(yields, "yields")
  assert_tabular(term_premia, "term_premia")

  yields_df <- as.data.frame(yields)
  term_premia_df <- as.data.frame(term_premia)

  validate_data_dimensions(yields_df, term_premia_df)

  # Inputs can omit maturity columns, so ncol cannot bound maturity values
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
#' @return A data frame containing the bundled \code{\link{variables}}
#'   dataset with quarterly period-end \code{date} values. Other columns
#'   and row order are unchanged; the packaged data file is not modified.
#' @keywords internal
get_bundled_variables <- function() {
  data("variables", package = "hetid", envir = environment())
  variables <- get("variables", envir = environment())
  variables[["date"]] <- to_period_end(variables[["date"]], "quarterly")
  variables
}

#' Validate Principal Components for \eqn{\omega_2}
#'
#' Converts supplied principal components of nominal financial asset
#' returns to a numeric matrix and checks its row count.
#'
#' @param pcs Required numeric matrix or data frame of principal components,
#'   already aligned to yields by calendar date, with \code{n_obs} rows.
#' @param n_pcs Positive integer number of leading PC columns to label,
#'   validated by the caller not to exceed \code{ncol(pcs)}.
#' @param n_obs Nonnegative integer number of yield observations expected.
#'
#' @details
#' Missing and nonfinite entries are retained. Subsequent regression drops
#' incomplete observations; this helper does not check finiteness or dates.
#' Missing inputs or nonnumeric contents raise
#' \code{hetid_error_bad_argument}; a row-count mismatch raises
#' \code{hetid_error_dimension_mismatch}.
#'
#' @return A list with components:
#'   \describe{
#'     \item{pcs}{Numeric matrix retaining all supplied columns and rows.}
#'     \item{pc_names}{Character labels for the first \code{n_pcs}
#'       columns. If any selected label is absent, missing, or empty, all
#'       labels use the \code{HETID_CONSTANTS$PC_PREFIX} prefix followed
#'       by their column indices. Matrix column names are unchanged.}
#'   }
#' @keywords internal
load_w2_pcs <- function(pcs, n_pcs, n_obs) {
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
