#' Align Per-Component Instrument Sets onto a Union Matrix
#'
#' Unites per-component instrument matrices by column name into one
#' \code{T x J_union} matrix and lists the column positions used by
#' each component.
#'
#' @details Columns sharing a name across components must be identical
#'   after coercion to double and removal of names. Comparison uses
#'   \code{\link[base:identical]{identical}()} with its defaults, so
#'   positive and negative zero compare equal. Differing columns with
#'   the same name raise a \code{hetid_error_bad_argument} error.
#'   All sets must have the same row count and already refer to the
#'   same observations in the same order; this function does not align
#'   dates or check observation identities. Missing and non-finite
#'   values are rejected rather than omitted.
#'
#'   Compute the moments from the returned \code{instruments} matrix.
#'   The returned \code{support} indexes its columns and can be passed
#'   to \code{\link{lambda_from_support}} and to the scripts-layer
#'   optimizer's support mask.
#'
#' @param z_sets List of length \code{n_components}: a numeric
#'   matrix or data frame of finite numeric values (\code{T x J_i}) at
#'   every constrained system column, and \code{NULL} elsewhere.
#'   Each set must have at least one column, with unique, non-empty,
#'   non-missing column names.
#' @param n_components A single positive integer giving the number of
#'   system columns (theta axis).
#' @param maturities A non-empty numeric vector of unique integer system
#'   column indices in \code{1..n_components}, or \code{NULL} for all
#'   system columns. These are positional indices, not bond maturities
#'   in months or years. Their supplied order determines the traversal
#'   of the instrument sets.
#'
#' @return A list with the following elements.
#' \describe{
#'   \item{instruments}{A \code{T x J_union} double matrix with unique
#'     column names in first-appearance order across sets visited in
#'     \code{maturities} order. Row names are not retained.}
#'   \item{support}{List of length \code{n_components}: integer
#'     positions of component i's columns within
#'     \code{colnames(instruments)}, in the component's original column
#'     order, and \code{NULL} at unconstrained columns.}
#' }
#'
#' @template section-general-instruments
#'
#' @export
#'
#' @examples
#' t_obs <- 20
#' z <- matrix(seq_len(t_obs * 3), t_obs,
#'   dimnames = list(NULL, c("pc1", "pc2", "pc3"))
#' )
#' aligned <- align_instrument_sets(
#'   list(z[, c("pc1", "pc2")], z[, c("pc2", "pc3")]),
#'   n_components = 2
#' )
#' colnames(aligned$instruments)
#' aligned$support
#'
#' subset <- align_instrument_sets(
#'   list(z[, c("pc1", "pc2")], NULL, z[, c("pc2", "pc3")]),
#'   n_components = 3, maturities = c(3, 1)
#' )
#' colnames(subset$instruments)
#' subset$support
align_instrument_sets <- function(z_sets, n_components,
                                  maturities = NULL) {
  assert_bad_argument_ok(
    positive_count_ok(n_components),
    "n_components must be a single positive integer",
    arg = "n_components"
  )
  n_components <- as.integer(n_components)
  if (is.null(maturities)) {
    maturities <- seq_len(n_components)
  }
  validate_maturities(
    maturities, n_components,
    max_label = "n_components", arg = "maturities"
  )
  maturities <- as.integer(maturities)
  assert_bad_argument_ok(
    is.list(z_sets) && length(z_sets) == n_components,
    paste0(
      "z_sets must be a list of length n_components (",
      n_components, ") with an instrument matrix at every ",
      "constrained system column and NULL elsewhere"
    ),
    arg = "z_sets"
  )
  unconstrained <- setdiff(seq_len(n_components), maturities)
  bad_extra <- unconstrained[
    !vapply(z_sets[unconstrained], is.null, logical(1))
  ]
  assert_bad_argument_ok(
    length(bad_extra) == 0,
    paste0(
      "z_sets must be NULL at unconstrained system column(s) ",
      paste(bad_extra, collapse = ", ")
    ),
    arg = "z_sets"
  )
  mats <- lapply(maturities, function(i) {
    as_instrument_set(z_sets[[i]], paste0("z_sets[[", i, "]]"))
  })
  t_rows <- vapply(mats, nrow, integer(1))
  assert_dimension_ok(
    length(unique(t_rows)) == 1,
    paste0(
      "all instrument sets must share one row count; got: ",
      paste(t_rows, collapse = ", ")
    )
  )
  instruments <- unite_named_columns(mats)
  inst_names <- colnames(instruments)
  support <- vector("list", n_components)
  for (k in seq_along(maturities)) {
    support[[maturities[k]]] <- match(
      colnames(mats[[k]]), inst_names
    )
  }
  list(instruments = instruments, support = support)
}

#' Validate and Coerce One Component's Instrument Set
#'
#' @param z_i Matrix or data frame of instruments for one component.
#' @param label Label for error messages.
#' @return Numeric matrix (storage mode double) with valid names.
#' @noRd
as_instrument_set <- function(z_i, label) {
  assert_tabular(z_i, label)
  z_i <- as.matrix(z_i)
  assert_bad_argument_ok(
    ncol(z_i) >= 1,
    paste0(label, " must have at least one column"),
    arg = label
  )
  assert_numeric_finite_values(z_i, label)
  assert_instrument_names(colnames(z_i), label)
  storage.mode(z_i) <- "double"
  z_i
}

#' Unite Named Instrument Columns Across Sets
#'
#' First-appearance order; same-name columns must compare equal under
#' \code{identical()} across sets.
#'
#' @param mats List of validated double matrices with unique names.
#' @return \code{T x J_union} matrix with unique column names.
#' @noRd
unite_named_columns <- function(mats) {
  vals <- list()
  for (m in mats) {
    cn <- colnames(m)
    for (j in seq_len(ncol(m))) {
      nm <- cn[j]
      column <- unname(m[, j])
      if (is.null(vals[[nm]])) {
        vals[[nm]] <- column
      } else {
        assert_bad_argument_ok(
          identical(vals[[nm]], column),
          paste0(
            "instrument column '", nm, "' differs across z_sets; ",
            "same-name columns must be content-identical"
          ),
          arg = "z_sets"
        )
      }
    }
  }
  out <- do.call(cbind, vals)
  colnames(out) <- names(vals)
  out
}
