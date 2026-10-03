#' Check Harvey Starts Without Fitting
#'
#' Check the initial criterion and score, fitted-variance positivity,
#' information finiteness, and one full Fisher-scoring proposal for each
#' response/start pair. No fitting iterations or backtracking are performed.
#'
#' @param pairs List of lists containing \code{y} and \code{start}. Each
#'   response is a nonnegative numeric vector with \code{nrow(x_mat)}
#'   entries; each start is a numeric vector with \code{ncol(x_mat)} entries.
#'   Response and start must already use the same response scale. Order is
#'   positional; names do not reorder observations or coefficients.
#' @param x_mat Finite numeric matrix with at least one row and one column.
#'   Supply the complete design, including any intercept; none is added.
#'   Its cross-product must be finite and have a usable Cholesky factor.
#'
#' @return A character vector in pair order, retaining the list's names.
#'   Passing pairs return \code{NA_character_}; failures return
#'   \code{invalid_response}, \code{invalid_start},
#'   \code{nonfinite_start_eval}, \code{nonpositive_mu},
#'   \code{nonfinite_info}, or \code{proposal_nonfinite}.
#'   An empty pair list returns \code{character(0)} after design validation.
#' @details
#' A pass establishes only the numerical stability of these evaluations.
#' It does not establish convergence, a finite minimizer, or endpoint
#' reliability. An all-zero response can pass even though fitting it fails.
#' Pair response/start defects are returned as reasons. Malformed list
#' containers or elements, and unusable shared designs, raise structured
#' \code{hetid_error_bad_argument} conditions. No rows are dropped or rescaled.
#' @seealso \code{\link{compute_harvey_ratio}},
#'   \code{\link{log_variance_fit_ok}}, \code{\link{fit_log_variance}}
#' @export
#' @examples
#' x_mat <- cbind(1, c(-1, 0, 1))
#' precheck_harvey_starts(list(reference = list(y = c(1, 1, 1), start = c(0, 0))), x_mat)
precheck_harvey_starts <- function(pairs, x_mat) {
  assert_bad_argument_ok(is.list(pairs), "pairs must be a list", arg = "pairs")
  for (i in seq_along(pairs)) {
    assert_bad_argument_ok(
      is.list(pairs[[i]]), "each pair must be a list",
      arg = paste0("pairs[[", i, "]]")
    )
  }
  validate_harvey_design(x_mat)
  xx <- crossprod(x_mat)
  assert_bad_argument_ok(
    all(is.finite(xx)), "x_mat must have a finite cross-product",
    arg = "x_mat"
  )
  chol_xx <- tryCatch(chol(xx), error = function(cond) NULL)
  assert_bad_argument_ok(
    !is.null(chol_xx) && all(is.finite(chol_xx)),
    "x_mat must have a usable cross-product Cholesky factor",
    arg = "x_mat"
  )
  col_abs <- colSums(abs(x_mat))
  assert_bad_argument_ok(
    all(is.finite(col_abs)) && all(col_abs > 0),
    "x_mat must have finite positive column absolute sums",
    arg = "x_mat"
  )
  vapply(pairs, harvey_precheck_pair, character(1),
    x_mat = x_mat,
    col_abs = col_abs, chol_xx = chol_xx
  )
}

#' @noRd
harvey_precheck_pair <- function(pair, x_mat, col_abs, chol_xx) {
  y <- pair$y
  coef_start <- pair$start
  if (!harvey_precheck_vector_ok(y, nrow(x_mat)) || any(y < 0)) {
    return("invalid_response")
  }
  if (!harvey_precheck_vector_ok(coef_start, ncol(x_mat))) {
    return("invalid_start")
  }
  current <- harvey_eval(coef_start, y, x_mat, y > 0, col_abs)
  if (is.null(current)) {
    return("nonfinite_start_eval")
  }
  if (!all(exp(current$eta) > 0)) {
    return("nonpositive_mu")
  }
  if (!all(is.finite(crossprod(x_mat, current$r * x_mat)))) {
    return("nonfinite_info")
  }
  direction <- harvey_chol_solve(chol_xx, current$moment)
  if (is.null(harvey_eval(coef_start + direction, y, x_mat, y > 0, col_abs))) {
    "proposal_nonfinite"
  } else {
    NA_character_
  }
}

#' @noRd
harvey_precheck_vector_ok <- function(value, n) {
  is.numeric(value) && is.null(dim(value)) && length(value) == n && all(is.finite(value))
}
