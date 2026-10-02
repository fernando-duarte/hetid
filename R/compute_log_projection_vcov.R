#' Covariance Matrices for a Log Projection at One Candidate
#'
#' Least-squares covariance matrices for the coefficients that
#' \code{\link{evaluate_log_projection}} returns at one candidate \eqn{b}: the
#' regression of the method's transformed squared residual \eqn{z_t} on
#' \eqn{V_t = (1, x_t')'} through the fixed operator \eqn{P = (V'V)^{-1}V'} of
#' \code{\link{prepare_log_projection}}.
#'
#' @inheritParams evaluate_log_projection
#' @param b Numeric vector with one entry per news column (one candidate).
#' @param hac_lags Single nonnegative integer Newey-West lag truncation,
#'   counted in volatility-sample rows. Default
#'   \code{LOG_VARIANCE_CONTROL$HAC_LAGS} (the paper's quarterly heuristic).
#'   \code{0} makes \code{hac} equal \code{hc0}.
#'
#' @return A named list of square covariance matrices keyed by
#'   \code{LOG_VARIANCE_CONTROL$SE_TYPES}, each labelled on both axes with
#'   the coefficient names of \code{evaluate_log_projection}. With residuals
#'   \eqn{r = z - V\hat\theta} and scores \eqn{q_t = P_{\cdot t} r_t}:
#'   \describe{
#'     \item{naive}{\eqn{\hat\sigma^2 (V'V)^{-1}},
#'       \eqn{\hat\sigma^2 = r'r / (n - p)}}
#'     \item{hc0}{the Eicker-White sandwich \eqn{\sum_t q_t q_t'}}
#'     \item{hc1}{\code{hc0} times \eqn{n / (n - p)}}
#'     \item{hac}{the Newey-West Bartlett extension of \code{hc0} over
#'       \code{hac_lags} lags}
#'   }
#'   Every matrix is all-NA when the candidate's evaluation status is not
#'   \code{"ok"}; a single matrix is all-NA when its products overflow.
#'
#' @details
#' These are plug-in second-stage covariances at a fixed \eqn{b}, with the
#' method's nuisance quantities held at their estimates: the threshold
#' \eqn{h_T} for \code{"log_plus"}; \eqn{c_T}, the candidate scale and the
#' first-pass adjustment profile for \code{"log_fuller"}. The Fuller profile
#' is fitted on the same rows, and its estimation error is not propagated;
#' neither is the sampling error of an estimated \eqn{b} or of the scale. A
#' bootstrap over the whole chain accounts for those. \code{multiplier}
#' enters only through the transformed response. HAC assumes the volatility
#' rows are in chronological order, which cannot be checked here.
#' @seealso \code{\link{evaluate_log_projection}},
#'   \code{\link{compute_log_variance_vcov_at_coef}}
#' @export
#' @examples
#' local({
#'   old_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
#'   on.exit(if (is.null(old_seed)) {
#'     rm(".Random.seed", envir = .GlobalEnv)
#'   } else {
#'     assign(".Random.seed", old_seed, envir = .GlobalEnv)
#'   })
#'   set.seed(1)
#'   n <- 120
#'   z <- cbind(1, rnorm(n))
#'   news <- cbind(n1 = rnorm(n))
#'   w1 <- drop(lm.fit(z, drop(news %*% 0.5) + rnorm(n))$residuals)
#'   w2 <- as.matrix(lm.fit(z, news)$residuals)
#'   colnames(w2) <- "n1"
#'   ids <- seq_len(n)
#'   prep <- prepare_log_projection(w1, w2, cbind(pc1 = rnorm(n)), ids, ids)
#'   v <- compute_log_projection_vcov(prep, c(n1 = 0.4), "log_plus")
#'   sqrt(diag(v$hac))
#' })
compute_log_projection_vcov <- function(prep, b, method,
                                        multiplier = LOG_PROJECTION_CONTROL$MULTIPLIER,
                                        hac_lags = LOG_VARIANCE_CONTROL$HAC_LAGS) {
  validated_args <- log_projection_args(prep, b, method, multiplier)
  assert_bad_argument_ok(
    validated_args$is_single, "b must be a single candidate vector",
    arg = "b"
  )
  assert_scalar_integer_in_range(hac_lags, "hac_lags", 0, .Machine$integer.max)
  passes <- log_projection_passes(
    prep, validated_args$b_mat, method, multiplier
  )
  status <- log_projection_result(
    prep, method, multiplier, TRUE, FALSE,
    passes$screening, passes$run, passes$pass
  )$status
  coef_names <- rownames(prep$projection)
  na_mat <- matrix(NA_real_, length(coef_names), length(coef_names),
    dimnames = list(coef_names, coef_names)
  )
  se_types <- LOG_VARIANCE_CONTROL$SE_TYPES
  if (!identical(status, LOG_PROJECTION_STATUS[["ok"]])) {
    return(stats::setNames(rep(list(na_mat), length(se_types)), se_types))
  }
  out <- log_projection_ols_vcov(
    prep, drop(passes$pass$work$response), as.integer(hac_lags)
  )
  # fail closed per matrix: overflow in a product is unavailable, never Inf
  lapply(out, function(m) if (all(is.finite(m))) m else na_mat)
}

# naive, HC0, HC1 and Bartlett HAC through the fixed operator: the bread
# (V'V)^{-1} is P P', and row t of the score matrix is P[, t] * r_t
log_projection_ols_vcov <- function(prep, z, hac_lags) {
  p_mat <- prep$projection
  n <- length(z)
  p <- nrow(p_mat)
  coef_value <- drop(p_mat %*% z)
  residual_values <- z - coef_value[[1L]] -
    drop(prep$x_centered %*% coef_value[-1L])
  scores <- t(p_mat) * residual_values
  hc0 <- crossprod(scores)
  out <- list(
    naive = (sum(residual_values^2) / (n - p)) * tcrossprod(p_mat),
    hc0 = hc0,
    hc1 = (n / (n - p)) * hc0,
    hac = se_bartlett_meat(scores, hac_lags)
  )
  coef_names <- rownames(p_mat)
  lapply(out, function(m) {
    dimnames(m) <- list(coef_names, coef_names)
    m
  })
}
