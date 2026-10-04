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
#'   regresses \eqn{\log e_t^2}; \code{"log_plus"} regresses
#'   \eqn{\log(e_t^2 + h_T^2)} with \eqn{h_T = m \hat s / \sqrt{T}} and
#'   \eqn{\hat s} the common mean-sample scale; \code{"log_fuller"} applies
#'   the two-pass Fuller transformation
#'   \eqn{F(x, \delta) = \log(x + \delta) - \delta / (x + \delta)} with
#'   \eqn{c_T = m^2 / T} and the candidate's own mean-sample scale.
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
#' residual is exactly zero and keeps its (non-finite) coefficients.
#' \code{"log_plus"} fails when the common scale is zero and
#' \code{"log_fuller"} when the candidate's mean-sample scale is zero; their
#' coefficients are then \code{NA}. Individual zero residuals are valid for
#' both regularized methods. A
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
#' the full preparation identity in any cache key. A successful
#' \code{"log_fuller"} evaluation does not establish a positive scale over a
#' whole set: when \code{prep$scale_lower_certified} is \code{FALSE}, the
#' search must treat every endpoint as unresolved unless it certifies
#' positivity itself.
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
  validated_args <- log_projection_args(prep, b, method, multiplier)
  assert_flag(jacobian, "jacobian")
  assert_bad_argument_ok(validated_args$is_single || !jacobian,
    "jacobian = TRUE needs a single candidate vector b",
    arg = "jacobian"
  )
  passes <- log_projection_passes(
    prep, validated_args$b_mat, method, multiplier
  )
  log_projection_result(
    prep, method, multiplier, validated_args$is_single, jacobian,
    passes$screening, passes$run, passes$pass
  )
}

log_projection_plus <- function(prep, e, log_x, multiplier) {
  log_h2 <- 2 * log(multiplier) - log(nrow(e)) + prep$log_scale_common
  a <- log_add_exp(log_x, log_h2)
  list(
    coef = prep$projection %*% a,
    diagnostics = list(
      log_threshold = rep(log_h2, ncol(e)),
      share_small = colMeans(log_x < log_h2)
    ),
    work = list(a = a, response = a)
  )
}

log_projection_log <- function(prep, e, log_x) {
  list(
    coef = prep$projection %*% log_x,
    diagnostics = list(min_abs_resid = log_projection_col_stat(abs(e), min)),
    work = list(response = log_x)
  )
}
