#' PPML Acceptance Machinery
#'
#' The post-fit half of the PPML log-variance response solve: the
#' positive-response rank diagnostic, the single \code{glm.fit} call site, and
#' the fail-closed acceptance check applied to each start-ladder rung. Ported
#' from the paper pipeline
#' (\code{scripts-paper/log_variance/estimators/ppml/acceptance.R}).
#'
#' @name ppml_acceptance
#' @keywords internal
NULL

#' Rank of the Positive-Response Design Rows
#'
#' Scales each nonzero column of the positive-response rows by its Euclidean
#' norm and counts singular values above
#' \code{RANK_TOLERANCE * d[1]}. A zero column keeps its zero singular value
#' and so lowers the count.
#'
#' @param y_scaled Numeric response on the scaled (fitted) scale
#' @param x_mat Numeric design matrix, intercept column included
#'
#' @return Integer rank of the column-normalized positive-response rows
#' @keywords internal
ppml_pos_rank <- function(y_scaled, x_mat) {
  x_pos <- x_mat[y_scaled > 0, , drop = FALSE]
  col_norms <- sqrt(colSums(x_pos^2))
  divisor <- ifelse(col_norms > 0, col_norms, 1)
  d <- svd(sweep(x_pos, 2, divisor, "/"))$d
  sum(d > LOG_VARIANCE_CONTROL$RANK_TOLERANCE * d[1])
}

#' Run One glm.fit Rung
#'
#' The one \code{glm.fit} call site of the package's log-variance estimator.
#' Conditions are recorded, never silenced, and an IRLS error comes back as a
#' \code{NULL} fit rather than propagating: the ladder decides what a failed
#' rung means.
#'
#' @param start Numeric start vector, or \code{NULL} for the
#'   \code{glm.fit} default
#' @param y_scaled Numeric response on the scaled (fitted) scale
#' @param x_mat Numeric design matrix, intercept column included
#'
#' @return List with \code{fit} (\code{NULL} on error), \code{warnings},
#'   \code{messages}, \code{error_class}, and \code{error_message}
#' @keywords internal
#' @importFrom stats glm.fit quasipoisson glm.control
ppml_run_glm <- function(start, y_scaled, x_mat) {
  captured <- capture_glm_conditions(stats::glm.fit(
    x = x_mat, y = y_scaled,
    family = stats::quasipoisson(link = "log"), start = start,
    control = stats::glm.control(
      epsilon = LOG_VARIANCE_CONTROL$GLM_EPSILON,
      maxit = LOG_VARIANCE_CONTROL$GLM_MAXIT
    )
  ))
  error_warning <- if (is.na(captured$error_message)) {
    character(0)
  } else {
    captured$error_message
  }
  list(
    fit = captured$value,
    warnings = c(captured$warnings, error_warning),
    messages = captured$messages,
    error_class = captured$error_class,
    error_message = captured$error_message
  )
}

#' Accept or Reject One Fitted Rung
#'
#' Fail-closed post-fit check on the scaled response: every gate is
#' unconditional and short-circuits with its reason, so an ill-posed fit never
#' reaches the score and conditioning computations. Rejection is the default,
#' since a silently accepted non-solution would be reported with standard
#' errors as if it were one.
#'
#' @param fit A \code{glm.fit} result (or any list with
#'   \code{coefficients}, \code{converged}, and \code{boundary})
#' @param y_scaled Numeric response on the scaled (fitted) scale
#' @param x_mat Numeric design matrix, intercept column included
#'
#' @return List with \code{accepted}, \code{reason}, and \code{coef_scaled};
#'   accepted (and score-or-conditioning rejected) verdicts also carry
#'   \code{mu}, \code{pos}, \code{score_norm}, \code{score_norm_raw},
#'   \code{info_col_scale}, \code{condition_weighted_scaled}, and
#'   \code{rcond_info_raw}
#' @keywords internal
#' @importFrom stats median
ppml_accept <- function(fit, y_scaled, x_mat) {
  coef_hat <- fit$coefficients
  bad <- function(reason) {
    list(accepted = FALSE, reason = reason, coef_scaled = coef_hat)
  }
  if (any(!is.finite(coef_hat))) {
    return(bad("nonfinite_coef"))
  }
  mu <- exp(drop(x_mat %*% coef_hat))
  if (any(!is.finite(mu)) || any(mu <= 0)) {
    return(bad("nonpositive_mu"))
  }
  if (!isTRUE(fit$converged)) {
    return(bad("irls_not_converged"))
  }
  if (isTRUE(fit$boundary)) {
    return(bad("boundary"))
  }
  pos <- y_scaled > 0
  sc <- drop(crossprod(x_mat, y_scaled - mu))
  # the score gate is scaled per coordinate: one absolute tolerance on
  # X'(y - mu) would pass or fail on each regressor's units alone
  bound_unit <- max(1, stats::median(y_scaled[pos])) * colSums(abs(x_mat))
  score_norm <- max(abs(sc) / bound_unit)
  info_col_scale <- sqrt(colSums(mu * x_mat^2))
  if (any(!is.finite(info_col_scale)) || any(info_col_scale <= 0)) {
    return(bad("info_scale"))
  }
  rcond_scaled <- rcond(crossprod(
    sweep(sqrt(mu) * x_mat, 2, info_col_scale, "/")
  ))
  reason <- NA_character_
  if (!(score_norm <= LOG_VARIANCE_CONTROL$SCORE_TOLERANCE)) {
    reason <- "score_tolerance"
  } else if (!(rcond_scaled >= LOG_VARIANCE_CONTROL$RCOND_TOLERANCE)) {
    reason <- "ill_conditioned"
  }
  list(
    accepted = is.na(reason), reason = reason, coef_scaled = coef_hat, mu = mu,
    pos = pos, score_norm = score_norm, score_norm_raw = max(abs(sc)),
    info_col_scale = info_col_scale,
    condition_weighted_scaled = 1 / rcond_scaled,
    rcond_info_raw = rcond(crossprod(x_mat, mu * x_mat))
  )
}
