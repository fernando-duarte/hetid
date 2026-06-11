#' Zero-Pad Per-Component Weights onto the Union Instrument Axis
#'
#' Builds the padded per-component weight list consumed by
#' \code{\link{build_general_quadratic_system}} from compact weight
#' matrices defined only on each component's instrument subset:
#' element i of the result is a \code{j_total} x K_i matrix that is
#' zero everywhere except rows \code{support[[i]]}, which carry
#' \code{weights[[i]]}. Off-support rows are exact structural zeros,
#' so the result is also a valid masked start for the scripts-layer
#' optimizer (pass the same \code{support} as its mask). Constrained
#' columns are the non-NULL positions of \code{support};
#' \code{weights} must be NULL exactly where \code{support} is NULL.
#'
#' All-zero weight COLUMNS are not rejected here; they are rejected
#' by \code{\link{build_general_quadratic_system}} when the padded
#' weights are used, so the nonzero-direction rule lives in one
#' place.
#'
#' @param support List with one integer vector of free instrument
#'   rows (unique, in \code{1..j_total}, at least one) per
#'   constrained system column and NULL at unconstrained columns
#' @param weights List of numeric K_i-column matrices, one per
#'   constrained column with \code{length(support[[i]])} rows; NULL
#'   exactly where \code{support} is NULL
#' @param j_total Total number of instrument columns on the union
#'   axis (e.g. \code{ncol} of the moments' instrument matrix)
#'
#' @return List of length \code{length(support)}: a numeric
#'   \code{j_total} x K_i matrix at constrained columns, NULL at
#'   unconstrained columns
#'
#' @template section-general-instruments
#'
#' @export
#'
#' @examples
#' support <- list(c(1L, 3L), 2L)
#' weights <- list(matrix(c(0.6, 0.8), 2, 1), matrix(1, 1, 1))
#' lambda_from_support(support, weights, j_total = 4)
lambda_from_support <- function(support, weights, j_total) {
  assert_bad_argument_ok(
    positive_count_ok(j_total),
    paste0(
      "j_total must be a single positive integer (the union ",
      "instrument count)"
    ),
    arg = "j_total"
  )
  j_total <- as.integer(j_total)
  assert_bad_argument_ok(
    is.list(support) && length(support) >= 1,
    "support must be a non-empty list with one entry per system column",
    arg = "support"
  )
  constrained <- which(!vapply(support, is.null, logical(1)))
  assert_bad_argument_ok(
    length(constrained) >= 1,
    "support must carry indices for at least one system column",
    arg = "support"
  )
  support <- assert_support_list(
    support, j_total, length(support), constrained
  )
  assert_bad_argument_ok(
    is.list(weights) && length(weights) == length(support),
    paste0(
      "weights must be a list of length ", length(support),
      " aligned with support"
    ),
    arg = "weights"
  )
  null_mismatch <- which(
    vapply(weights, is.null, logical(1)) !=
      vapply(support, is.null, logical(1))
  )
  assert_bad_argument_ok(
    length(null_mismatch) == 0,
    paste0(
      "weights must be NULL exactly where support is NULL; ",
      "mismatched column(s): ",
      paste(null_mismatch, collapse = ", ")
    ),
    arg = "weights"
  )
  out <- vector("list", length(support))
  for (i in constrained) {
    out[[i]] <- pad_weights_entry(
      weights[[i]], support[[i]], j_total, i
    )
  }
  out
}

#' Pad One Compact Weight Matrix onto the Union Axis
#'
#' @param w_i Compact weight matrix for component i
#' @param s_i Integer support rows for component i
#' @param j_total Union axis length
#' @param i Component index (for error messages)
#' @return Numeric \code{j_total} x K_i matrix
#' @noRd
pad_weights_entry <- function(w_i, s_i, j_total, i) {
  assert_bad_argument_ok(
    is.matrix(w_i) && is.numeric(w_i) && ncol(w_i) >= 1,
    paste0(
      "weights[[", i, "]] must be a numeric matrix with at least ",
      "one column"
    ),
    arg = "weights"
  )
  assert_numeric_finite_values(w_i, paste0("weights[[", i, "]]"))
  assert_dimension_ok(
    nrow(w_i) == length(s_i),
    paste0(
      "weights[[", i, "]] must have one row per support index (",
      length(s_i), "); got ", nrow(w_i)
    )
  )
  padded <- matrix(0, nrow = j_total, ncol = ncol(w_i))
  padded[s_i, ] <- w_i
  padded
}
