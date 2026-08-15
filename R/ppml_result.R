#' PPML Result Assembly
#'
#' The two ways a PPML response solve ends -- an accepted fit and a
#' fail-closed result -- plus the diagnostics container they share and the
#' condition recorder wrapped around \code{glm.fit}. Success and every failure
#' branch go through \code{\link{new_hetid_log_variance_fit}}, so all of them
#' come back in one shape.
#'
#' @name ppml_result
#' @keywords internal
NULL

#' Record Conditions Around a glm.fit Call
#'
#' Collects warnings and messages instead of letting them print (nothing is
#' suppressed: every condition ends up in the diagnostics), and turns an error
#' into a \code{NULL} value plus its class, which the ladder maps to a
#' \code{fit_status} of \code{"nonconvergence"}.
#'
#' @param expression Expression to evaluate, captured lazily
#'
#' @return List with \code{value} (\code{NULL} on error), \code{warnings},
#'   \code{messages}, \code{error_class}, and \code{error_message} (the last
#'   two \code{NA} when the expression completed)
#' @keywords internal
capture_glm_conditions <- function(expression) {
  warning_msgs <- character(0)
  message_msgs <- character(0)
  captured_error <- NULL
  value <- tryCatch(
    withCallingHandlers(
      expression,
      warning = function(condition) {
        warning_msgs <<- c(warning_msgs, conditionMessage(condition))
        invokeRestart("muffleWarning")
      },
      message = function(condition) {
        message_msgs <<- c(message_msgs, conditionMessage(condition))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(condition) {
      captured_error <<- condition
      NULL
    }
  )
  failed <- !is.null(captured_error)
  list(
    value = value, warnings = warning_msgs, messages = message_msgs,
    error_class = if (failed) class(captured_error)[[1L]] else NA_character_,
    error_message = if (failed) {
      paste0("error: ", conditionMessage(captured_error))
    } else {
      NA_character_
    }
  )
}

#' Build the PPML Diagnostics List
#'
#' NA and empty defaults for every diagnostic field; callers override only
#' what they can populate, so an early fail-closed return stays
#' field-compatible with an accepted fit.
#'
#' @param error_class Single string naming the failure, or \code{NA}
#' @param start_attempts List of per-rung attempt records
#' @param ... Fields to override in the defaults
#'
#' @return A diagnostics list
#' @keywords internal
#' @importFrom utils modifyList
ppml_diagnostics <- function(error_class, start_attempts, ...) {
  base <- list(
    warnings = character(0), messages = character(0),
    error_class = error_class, start_attempts = start_attempts,
    min_pos_response = NA_real_, rank_x_pos = NA_integer_,
    condition_weighted_scaled = NA_real_, rcond_info_raw = NA_real_,
    info_col_scale = NA_real_, score_norm_raw = NA_real_,
    score_norm_scaled = NA_real_
  )
  modifyList(base, list(...))
}

#' Assemble an Accepted PPML Fit
#'
#' Recovers the original-scale coefficients by adding
#' \code{log(response_scale)} to the intercept only, keeps the raw scaled-fit
#' vector as \code{warm_start}, and reports the scaled objective.
#'
#' @param acc Accepted verdict from \code{\link{ppml_accept}}
#' @param run Runner result from \code{\link{ppml_run_glm}}
#' @param y Numeric response on the original scale
#' @param y_scaled Numeric response on the scaled (fitted) scale
#' @param x_mat Numeric design matrix, intercept column included
#' @param response_scale Positive finite scalar the response was divided by
#' @param attempts List of per-rung attempt records
#' @param rank_x_pos Integer rank of the positive-response design rows
#'
#' @return A validated \code{hetid_log_variance_fit} object
#' @keywords internal
ppml_success <- function(acc, run, y, y_scaled, x_mat, response_scale,
                         attempts, rank_x_pos) {
  coef_original <- acc$coef_scaled
  coef_original[1] <- coef_original[1] + log(response_scale)
  objective <- sum(acc$mu) - sum(y_scaled[acc$pos] * log(acc$mu[acc$pos]))
  out <- validate_hetid_log_variance_fit(new_hetid_log_variance_fit(
    coef = coef_original, fit_status = LOG_VARIANCE_FIT_STATUS[["ok"]],
    converged = TRUE, objective = objective, score_norm = acc$score_norm,
    convergence_code = as.integer(run$fit$iter),
    warm_start = acc$coef_scaled,
    diagnostics = ppml_diagnostics(
      NA_character_, attempts,
      warnings = run$warnings, messages = run$messages,
      min_pos_response = min(y_scaled[acc$pos]), rank_x_pos = rank_x_pos,
      condition_weighted_scaled = acc$condition_weighted_scaled,
      rcond_info_raw = acc$rcond_info_raw,
      info_col_scale = acc$info_col_scale,
      score_norm_raw = acc$score_norm_raw, score_norm_scaled = acc$score_norm
    ),
    y = y, x_design = x_mat, estimator = "ppml",
    response_scale = response_scale, n_obs = length(y),
    coef_labels = colnames(x_mat)
  ))
  out
}

#' Assemble a Fail-Closed PPML Result
#'
#' A statistically impossible fit is a result, not an error: the caller gets
#' the same container with \code{fit_status = "nonconvergence"} and the reason
#' in \code{diagnostics$error_class}.
#'
#' @param error_class Single string naming the failure
#' @param y Numeric response on the original scale
#' @param x_mat Numeric design matrix, intercept column included
#' @param response_scale Positive finite scalar the response was divided by
#' @param attempts List of per-rung attempt records (empty before the ladder)
#' @param ... Extra diagnostics fields to record
#'
#' @return A validated \code{hetid_log_variance_fit} object
#' @keywords internal
ppml_failure <- function(error_class, y, x_mat, response_scale,
                         attempts = list(), ...) {
  out <- validate_hetid_log_variance_fit(new_hetid_log_variance_fit(
    coef = NULL, fit_status = LOG_VARIANCE_FIT_STATUS[["nonconvergence"]],
    converged = FALSE, objective = NA_real_, score_norm = NA_real_,
    convergence_code = -1L, warm_start = NULL,
    diagnostics = ppml_diagnostics(error_class, attempts, ...),
    y = y, x_design = x_mat, estimator = "ppml",
    response_scale = response_scale, n_obs = length(y),
    coef_labels = colnames(x_mat)
  ))
  out
}
