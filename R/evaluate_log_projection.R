#' Evaluate a Log Projection of Squared Residuals at Candidate b
#'
#' Regresses a transformation of the squared candidate residuals
#' \eqn{e(b) = w_1 - W_2 b} on an intercept and the centered volatility
#' regressors through the fixed OLS operator of
#' \code{\link{prepare_log_projection}}, and optionally returns the
#' coefficients' Jacobian with respect to \eqn{b}.
#'
#' @param prep A \code{hetid_log_projection_prep} object.
#' @param b Numeric vector with one entry per news column, or a numeric
#'   matrix with one candidate per row (batch evaluation, no Jacobians).
#' @param method One of \code{LOG_PROJECTION_CONTROL$METHODS}. \code{"log"}
#'   regresses \eqn{\log e_t^2}.
#' @param multiplier Positive tuning multiplier \eqn{m}; ignored by
#'   \code{"log"}.
#' @param jacobian Logical; return the \eqn{p \times d_N} Jacobian (single
#'   candidate only).
#'
#' @return A list with \code{coef} (named coefficients, intercept first, in
#'   the centered-regressor convention; a \eqn{p \times k} matrix for a
#'   batch), \code{jacobian} (\code{NULL} unless requested and the status is
#'   \code{"ok"}), \code{status} (\code{"ok"}, \code{"domain_failure"}, or
#'   \code{"numerical_failure"}, one per candidate), and \code{diagnostics}.
#' @details
#' \code{"log"} reports \code{"domain_failure"} when a volatility-sample
#' residual is exactly zero and keeps its (non-finite) coefficients. A
#' non-finite residual or result is a \code{"numerical_failure"} with
#' \code{NA} coefficients; failures in one batch column never affect
#' another. A raw-coordinate intercept is
#' \code{coef[1] - sum(prep$x_center * coef[-1])}. These are projection
#' coefficients of transformed squared residuals; exponentiating a fitted
#' index does not estimate a conditional variance without further
#' assumptions. When \code{b} (or the candidate matrix's columns) and the
#' news residuals are both named, the names must agree exactly, as in
#' \code{\link{fit_log_variance_at_b}}.
#'
#' @section Using the map over a candidate set:
#' Evaluation is pointwise. A set search (coefficient bounds, fitted-index
#' envelopes) wraps this function in its own optimizer, maps the statuses
#' onto its own failure vocabulary, and includes the method, multiplier, and
#' the full preparation identity in any cache key.
#' @seealso \code{\link{prepare_log_projection}},
#'   \code{\link{LOG_PROJECTION_CONTROL}},
#'   \code{\link{fit_log_variance_at_b}} for the exponential-mean estimators
#' @examples
#' local({
#'   old_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
#'   on.exit(if (is.null(old_seed)) {
#'     rm(".Random.seed", envir = .GlobalEnv)
#'   } else {
#'     assign(".Random.seed", old_seed, envir = .GlobalEnv)
#'   })
#'   set.seed(1)
#'   n_mean <- 120
#'   z <- cbind(1, rnorm(n_mean))
#'   news <- cbind(n1 = rnorm(n_mean), n2 = rnorm(n_mean))
#'   y <- drop(news %*% c(0.5, -0.3)) + rnorm(n_mean)
#'   w1 <- drop(lm.fit(z, y)$residuals)
#'   w2 <- lm.fit(z, news)$residuals
#'   ids <- seq_len(n_mean)
#'   vol_ids <- ids[-(1:20)]
#'   x_var <- cbind(pc1 = rnorm(100), pc2 = rnorm(100))
#'   prep <- prepare_log_projection(w1, w2, x_var, ids, vol_ids)
#'   b <- c(n1 = 0.4, n2 = -0.2)
#'   fit <- evaluate_log_projection(prep, b, "log")
#'   print(fit$coef)
#'   print(dim(fit$jacobian))
#'   batch <- evaluate_log_projection(prep, rbind(b, b / 2), "log",
#'     jacobian = FALSE
#'   )
#'   print(batch$status)
#' })
#' @export
evaluate_log_projection <- function(prep, b, method,
                                    multiplier = LOG_PROJECTION_CONTROL$MULTIPLIER,
                                    jacobian = TRUE) {
  assert_hetid_log_projection_prep(prep)
  method_values <- LOG_PROJECTION_CONTROL$METHODS
  assert_bad_argument_ok(
    is.character(method) && length(method) == 1L && !is.na(method) &&
      method %in% method_values,
    paste0("method must be one of: ", paste(method_values, collapse = ", ")),
    arg = "method"
  )
  assert_scalar_finite(multiplier, "multiplier")
  assert_bad_argument_ok(multiplier > 0, "multiplier must be positive",
    arg = "multiplier"
  )
  assert_flag(jacobian, "jacobian")
  is_single <- is.null(dim(b))
  b_mat <- if (is_single) matrix(b, nrow = 1L) else b
  assert_bad_argument_ok(
    is.numeric(b_mat) && is.matrix(b_mat) && nrow(b_mat) >= 1L,
    "b must be a numeric vector or a matrix with one candidate per row",
    arg = "b"
  )
  assert_numeric_finite_values(b_mat, "b")
  assert_dimension_ok(
    ncol(b_mat) == ncol(prep$w2),
    sprintf("each candidate needs %d entries, not %d", ncol(prep$w2), ncol(b_mat))
  )
  # a permuted named candidate would silently change the residuals
  b_names <- if (is_single) names(b) else colnames(b)
  assert_bad_argument_ok(
    is.null(b_names) || is.null(colnames(prep$w2)) ||
      identical(b_names, colnames(prep$w2)),
    "names of b must equal colnames(w2) in order",
    arg = "b"
  )
  assert_bad_argument_ok(is_single || !jacobian,
    "jacobian = TRUE needs a single candidate vector b",
    arg = "jacobian"
  )
  e <- prep$w1 - prep$w2 %*% t(b_mat)
  screening <- log_projection_screen(prep, b_mat, e, method)
  run <- !screening$bad & (method == "log" | !screening$domain)
  pass <- NULL
  if (any(run)) {
    e_run <- e[, run, drop = FALSE]
    log_x <- 2 * log(abs(e_run))
    pass <- switch(method,
      log = log_projection_log(prep, e_run, log_x)
    )
    pass$e <- e_run
    pass$log_x <- log_x
  }
  log_projection_result(
    prep, method, multiplier, is_single, jacobian, screening, run, pass
  )
}

log_projection_log <- function(prep, e, log_x) {
  list(
    coef = prep$projection %*% log_x,
    diagnostics = list(min_abs_resid = apply(abs(e), 2L, min)),
    work = list()
  )
}
