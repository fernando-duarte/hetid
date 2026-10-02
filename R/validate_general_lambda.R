#' Coerce Weights to the Per-Component List Form
#'
#' Accepts a J x I matrix (one combination per component) or a
#' list of length \code{n_components} indexed by system column, with a
#' numeric J x K_i matrix at every constrained column. List inputs require
#' \code{NULL} at unconstrained columns; matrix inputs discard those columns.
#' Here I is \code{n_components}, J is the number of instruments, and K_i
#' is the number of combinations at constrained system column i.
#' Every constrained matrix must have at least one column.
#' System columns are w2 column indices, not bond maturity values.
#' Constrained weights must be finite, with no \code{NA}, \code{NaN}, or
#' infinite values.
#' All-zero weight columns are rejected: a zero column gives a constant instrument,
#' which the spec excludes (each instrument must be nonconstant), and it would
#' silently add a vacuous constraint.
#'
#' @param lambda A J x I matrix or a length-I list of weight matrices.
#' @param moments A validated \code{hetid_moments} object supplying the
#'   constrained system columns, system width, and instrument count.
#' @return A list of length \code{n_components}, indexed by system column,
#'   with numeric J x K_i matrices at constrained columns and \code{NULL}
#'   elsewhere. Matrix inputs give one-column matrices; list inputs retain
#'   their names and matrix dimensions.
#' @noRd
as_lambda_list <- function(lambda, moments) {
  maturities <- attr(moments, "maturities")
  n_components <- attr(moments, "n_components")
  j_rows <- nrow(moments$r_i_0)

  if (is.matrix(lambda)) {
    assert_dimension_ok(
      ncol(lambda) == n_components,
      paste0(
        "lambda must have n_components (", n_components,
        ") columns to match the moments' system"
      )
    )
    assert_dimension_ok(
      nrow(lambda) == j_rows,
      paste0(
        "lambda must have the same number of rows (J = ", j_rows,
        ") as the moments' instruments"
      )
    )
    lambda <- lapply(seq_len(n_components), function(i) {
      if (i %in% maturities) lambda[, i, drop = FALSE] else NULL
    })
  }

  assert_bad_argument_ok(
    is.list(lambda) && length(lambda) == n_components,
    paste0(
      "lambda must be a J x I matrix or a list of length ",
      "n_components (", n_components,
      ") with one J x K_i weight matrix per system column"
    ),
    arg = "lambda"
  )
  assert_null_at_unconstrained(
    lambda, n_components, maturities, "lambda",
    suffix = "; weights there would be silently ignored"
  )
  for (i in maturities) {
    mat_i <- lambda[[i]]
    assert_bad_argument_ok(
      is.matrix(mat_i) && is.numeric(mat_i) && ncol(mat_i) >= 1,
      paste0(
        "lambda[[", i, "]] must be a numeric J x K matrix with at ",
        "least one column (system column ", i, " is constrained)"
      ),
      arg = "lambda"
    )
    assert_numeric_finite_values(mat_i, paste0("lambda[[", i, "]]"))
    assert_dimension_ok(
      nrow(mat_i) == j_rows,
      paste0("lambda[[", i, "]] must have J = ", j_rows, " rows")
    )
    zero_cols <- which(colSums(mat_i != 0) == 0)
    assert_bad_argument_ok(
      length(zero_cols) == 0,
      paste0(
        "lambda[[", i, "]] has all-zero weight column(s) ",
        paste(zero_cols, collapse = ", "),
        "; every combination needs a nonzero weight direction"
      ),
      arg = "lambda"
    )
  }
  lambda
}

#' Constraint Labels for the General System
#'
#' One row per (component, combination) pair, in the supplied
#' \code{maturities} order and then in weight-column order.
#' Single-combination components use \code{maturity_N}; multiple
#' combinations use \code{maturity_N_combo_k}.
#' Here N is a w2 column index, not a bond maturity value.
#'
#' @param lambda_list A per-system-column list from \code{as_lambda_list()}.
#' @param maturities An integer vector of constrained w2 column indices.
#' @return A data frame with one row per constraint and columns
#'   \code{constraint} (sequential row index), \code{maturity} (w2 column
#'   index), \code{combo} (weight-column index), and \code{name} (constraint
#'   label).
#' @noRd
general_constraint_labels <- function(lambda_list, maturities) {
  k_per <- vapply(
    maturities, function(i) ncol(lambda_list[[i]]), integer(1)
  )
  maturity <- rep(maturities, times = k_per)
  combo <- sequence(k_per)
  base <- maturity_names(maturity)
  name <- ifelse(
    rep(k_per, times = k_per) == 1L,
    base,
    paste0(base, "_combo_", combo)
  )
  data.frame(
    constraint = seq_along(maturity),
    maturity = maturity,
    combo = combo,
    name = name,
    stringsAsFactors = FALSE
  )
}
