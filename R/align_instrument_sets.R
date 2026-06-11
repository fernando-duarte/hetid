#' Align Per-Component Instrument Sets onto a Union Matrix
#'
#' Unites per-component instrument matrices by column NAME into one
#' T x J_union matrix plus the per-component support index lists
#' that locate each component's columns on the union axis. Columns
#' sharing a name across components must be content-identical
#' (bitwise, after coercion to double): the union is keyed on names,
#' and silently uniting two different series under one name is the
#' exact misalignment this validator exists to catch. Compute the
#' moments from \code{instruments} -- the returned \code{support}
#' indexes ITS columns -- then feed \code{support} to
#' \code{\link{lambda_from_support}} (and to the scripts-layer
#' optimizer's support mask).
#'
#' @param z_sets List of length \code{n_components}: a numeric
#'   matrix or data frame (T x J_i, named columns) at every
#'   constrained system column, NULL at unconstrained columns
#' @param n_components Number of system columns (theta axis)
#' @param maturities Constrained system columns (default NULL means
#'   all of \code{1..n_components})
#'
#' @return List with elements
#' \describe{
#'   \item{instruments}{T x J_union numeric matrix, columns in
#'     first-appearance order across constrained components}
#'   \item{support}{List of length \code{n_components}: integer
#'     positions of component i's columns within
#'     \code{colnames(instruments)}, NULL at unconstrained columns}
#' }
#'
#' @template section-general-instruments
#'
#' @export
#'
#' @examples
#' t_obs <- 20
#' z <- matrix(rnorm(t_obs * 3), t_obs,
#'   dimnames = list(NULL, c("pc1", "pc2", "pc3"))
#' )
#' aligned <- align_instrument_sets(
#'   list(z[, c("pc1", "pc2")], z[, c("pc2", "pc3")]),
#'   n_components = 2
#' )
#' colnames(aligned$instruments)
#' aligned$support
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
  support <- vector("list", n_components)
  for (k in seq_along(maturities)) {
    support[[maturities[k]]] <- match(
      colnames(mats[[k]]), colnames(instruments)
    )
  }
  list(instruments = instruments, support = support)
}

#' Validate and Coerce One Component's Instrument Set
#'
#' @param z_i Matrix or data frame of instruments for one component
#' @param label Label for error messages
#' @return Numeric matrix (storage mode double) with valid names
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
#' First-appearance order; same-name columns must be bitwise
#' identical across sets.
#'
#' @param mats List of validated double matrices with unique names
#' @return T x J_union matrix with unique column names
#' @noRd
unite_named_columns <- function(mats) {
  vals <- list()
  for (m in mats) {
    for (j in seq_len(ncol(m))) {
      nm <- colnames(m)[j]
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
