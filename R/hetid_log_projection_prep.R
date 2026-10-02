#' Prepared Log-Projection Inputs
#'
#' Container returned by \code{\link{prepare_log_projection}}: the fixed
#' samples, row map, centered volatility regressors, OLS operator, and scales
#' that every candidate evaluation reuses.
#'
#' @name hetid_log_projection_prep
#' @keywords internal
NULL

LOG_PROJECTION_PREP_FIELDS <- c(
  "projection", "x_centered", "x_center", "w1", "w2", "w1_mean",
  "w2_mean", "mean_ids", "volatility_ids", "volatility_rows",
  "log_scale_common", "log_scale_lower", "scale_lower_certified",
  "rank_tolerance", "projection_rcond"
)

new_hetid_log_projection_prep <- function(fields, n_mean, n_vol) {
  assert_scalar_integer_in_range(n_mean, "n_mean", 1, .Machine$integer.max)
  assert_scalar_integer_in_range(n_vol, "n_vol", 1, .Machine$integer.max)
  structure(
    fields,
    n_mean = as.integer(n_mean),
    n_vol = as.integer(n_vol),
    class = "hetid_log_projection_prep"
  )
}

assert_hetid_log_projection_prep <- function(x, arg = "prep") {
  assert_bad_argument_ok(
    inherits(x, "hetid_log_projection_prep"),
    paste0(
      arg, " must be a hetid_log_projection_prep object created by ",
      "prepare_log_projection()"
    ),
    arg = arg
  )
  invisible(TRUE)
}

validate_hetid_log_projection_prep <- function(x) {
  assert_hetid_log_projection_prep(x)
  assert_bad_argument_ok(
    all(LOG_PROJECTION_PREP_FIELDS %in% names(x)),
    "prep is missing log-projection fields",
    arg = "prep"
  )
  n_mean <- attr(x, "n_mean")
  n_vol <- attr(x, "n_vol")
  actual <- c(
    length(x$w1_mean), nrow(x$w2_mean), length(x$volatility_rows),
    length(x$w1), nrow(x$w2), ncol(x$projection), nrow(x$x_centered),
    nrow(x$projection)
  )
  expected <- c(n_mean, n_mean, rep(n_vol, 5L), ncol(x$x_centered) + 1L)
  assert_dimension_ok(
    identical(as.integer(actual), as.integer(expected)),
    "prep dimensions are inconsistent"
  )
  assert_bad_argument_ok(
    identical(x$w1, x$w1_mean[x$volatility_rows]) &&
      identical(x$w2, x$w2_mean[x$volatility_rows, , drop = FALSE]),
    "prep volatility rows do not match the mean sample",
    arg = "prep"
  )
  invisible(x)
}

#' Print a Prepared Log Projection
#'
#' @param x A \code{hetid_log_projection_prep} object.
#' @param ... Unused.
#' @return \code{x}, invisibly.
#' @keywords internal
#' @export
print.hetid_log_projection_prep <- function(x, ...) {
  cat("<hetid_log_projection_prep>\n")
  cat("  mean sample: ", attr(x, "n_mean"), "\n", sep = "")
  cat("  volatility sample: ", attr(x, "n_vol"), "\n", sep = "")
  cat("  coefficients: ", paste(rownames(x$projection), collapse = ", "),
    "\n",
    sep = ""
  )
  cat("  Fuller scale certified: ", x$scale_lower_certified, "\n", sep = "")
  invisible(x)
}
