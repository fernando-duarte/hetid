#' PPML Acceptance Machinery
#'
#' The positive-response rank diagnostic, \code{glm.fit} call, and
#' acceptance checks used for each PPML log-variance fitting attempt.
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
#' Inputs are already validated by the fitting boundary; missing values are
#' not removed here. The relative cutoff uses \code{control$RANK_TOLERANCE}.
#'
#' @param y_scaled Finite nonnegative numeric response vector on the scaled
#'   (fitted) scale, with at least one positive value.
#' @param x_mat Finite numeric design matrix with one row per response and
#'   an intercept column.
#' @param control Validated fitting controls; defaults to the PPML controls
#'   from \code{log_variance_fit_control("ppml")}.
#'
#' @return A scalar integer rank of the column-normalized rows for which
#'   \code{y_scaled > 0}.
#' @keywords internal
ppml_pos_rank <- function(y_scaled, x_mat, control = log_variance_fit_control("ppml")) {
  x_pos <- x_mat[y_scaled > 0, , drop = FALSE]
  col_norms <- sqrt(colSums(x_pos^2))
  divisor <- ifelse(col_norms > 0, col_norms, 1)
  d <- svd(sweep(x_pos, 2, divisor, "/"))$d
  sum(d > control$RANK_TOLERANCE * d[1])
}

#' Run One glm.fit Rung
#'
#' The one \code{glm.fit} call site of the package's log-variance estimator.
#' Warnings and messages are captured in the returned list instead of printed.
#' An IRLS error comes back as a \code{NULL} fit rather than propagating:
#' the ladder decides what a failed rung means.
#'
#' Inputs are already validated by the fitting boundary; missing values are
#' not removed here. The fit uses a quasi-Poisson family with a log link and
#' \code{control$GLM_EPSILON} and \code{control$GLM_MAXIT} as IRLS controls.
#'
#' @param start Numeric coefficient start vector on the scaled response,
#'   with one element per design column, or \code{NULL} for the
#'   \code{glm.fit} default.
#' @param y_scaled Finite nonnegative numeric response vector on the scaled
#'   (fitted) scale, with at least one positive value.
#' @param x_mat Finite numeric design matrix with one row per response and
#'   an intercept column.
#' @param control Validated fitting controls; defaults to the PPML controls
#'   from \code{log_variance_fit_control("ppml")}.
#'
#' @return A list with \code{fit} (the \code{glm.fit} result, or \code{NULL}
#'   on error), character vectors \code{warnings} and \code{messages}, and
#'   scalar strings \code{error_class} and \code{error_message} (both
#'   \code{NA_character_} on success). On error, the prefixed error message
#'   is also appended to \code{warnings}.
#' @keywords internal
#' @importFrom stats glm.fit quasipoisson glm.control
ppml_run_glm <- function(start, y_scaled, x_mat, control = log_variance_fit_control("ppml")) {
  captured <- capture_glm_conditions(stats::glm.fit(
    x = x_mat, y = y_scaled,
    family = stats::quasipoisson(link = "log"), start = start,
    control = stats::glm.control(
      epsilon = control$GLM_EPSILON,
      maxit = control$GLM_MAXIT
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
#' Checks finite coefficients, positive finite fitted means, convergence,
#' and the boundary flag before computing score and conditioning diagnostics.
#' Rejection is the default, since a silently accepted non-solution would be
#' reported with standard errors as if it were one.
#'
#' Response and design inputs are already validated by the fitting boundary;
#' missing values are not removed here. Acceptance requires the normalized
#' score to be at most \code{control$SCORE_TOLERANCE} and the reciprocal
#' condition estimate of the column-normalized information matrix to be at
#' least \code{control$RCOND_TOLERANCE}.
#'
#' @param fit A \code{glm.fit} result, or a list with a numeric
#'   \code{coefficients} vector matching the design columns and logical
#'   \code{converged} and \code{boundary} flags.
#' @param y_scaled Finite nonnegative numeric response vector on the scaled
#'   (fitted) scale, with at least one positive value.
#' @param x_mat Finite numeric design matrix with one row per response and
#'   an intercept column.
#' @param control Validated fitting controls; defaults to the PPML controls
#'   from \code{log_variance_fit_control("ppml")}.
#'
#' @return A list with logical scalar \code{accepted}, scalar string
#'   \code{reason} (\code{NA_character_} on acceptance), and coefficient
#'   vector \code{coef_scaled}. Early rejection reasons are
#'   \code{"nonfinite_coef"}, \code{"nonpositive_mu"},
#'   \code{"irls_not_converged"}, \code{"boundary"}, and \code{"info_scale"}.
#'   Accepted verdicts and rejections for \code{"score_tolerance"} or
#'   \code{"ill_conditioned"} also carry fitted scaled means \code{mu},
#'   a logical positive-response mask \code{pos}, the maximum normalized
#'   score \code{score_norm}, the maximum absolute unnormalized score
#'   \code{score_norm_raw}, information column norms \code{info_col_scale},
#'   the condition estimate \code{condition_weighted_scaled} (\code{1 / rcond}
#'   for the column-normalized information matrix), and the raw information
#'   matrix's reciprocal condition estimate \code{rcond_info_raw}.
#' @keywords internal
#' @importFrom stats median
ppml_accept <- function(fit, y_scaled, x_mat, control = log_variance_fit_control("ppml")) {
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
  # the score check is scaled per coordinate: one absolute tolerance on
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
  if (!(score_norm <= control$SCORE_TOLERANCE)) {
    reason <- "score_tolerance"
  } else if (!(rcond_scaled >= control$RCOND_TOLERANCE)) {
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
