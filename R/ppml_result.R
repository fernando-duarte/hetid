#' PPML Result Assembly
#'
#' The two ways a PPML response solve ends -- an accepted fit and a
#' fail-closed result -- plus the diagnostics container they share and the
#' condition recorder wrapped around \code{glm.fit}. Success and every failure
#' branch build the same container, so all of them come back in one shape.
#'
#' @name ppml_result
#' @keywords internal
NULL

#' Record Conditions Around a glm.fit Call
#'
#' Records warning and message text and muffles their console output. An error
#' returns a \code{NULL} value with its first class and message; the PPML
#' ladder can then try another start before returning nonconvergence.
#'
#' @param expression Expression to evaluate lazily in the caller's environment.
#'
#' @return List with \code{value} (\code{NULL} on error), \code{warnings},
#'   \code{messages} (character vectors), \code{error_class}, and
#'   \code{error_message} (character scalars). The last two are
#'   \code{NA_character_} on completion; errors have an \code{"error: "}
#'   message prefix. A successful expression can also return \code{NULL}.
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
#' Provides missing-value and empty defaults for diagnostics, with the supplied
#' failure reason and start attempts. Callers replace fields they can populate.
#'
#' @param error_class Character scalar naming the failure, or
#'   \code{NA_character_} for an accepted fit.
#' @param start_attempts List of per-rung attempt records.
#' @param ... Named fields merged with the defaults by
#'   \code{\link[utils:modifyList]{modifyList}}. New names are added, and
#'   \code{NULL} removes a field.
#'
#' @return Named list with \code{warnings} and \code{messages} (empty character
#'   vectors), \code{error_class}, \code{start_attempts}, and missing defaults
#'   for \code{min_pos_response}, \code{rank_x_pos},
#'   \code{condition_weighted_scaled}, \code{rcond_info_raw},
#'   \code{info_col_scale}, \code{score_norm_raw}, and \code{score_norm_scaled},
#'   modified by \code{...}.
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
#' @param acc Accepted verdict list from \code{\link{ppml_accept}}.
#' @param run Runner result list from \code{\link{ppml_run_glm}}.
#' @param y Finite nonnegative numeric response vector on the original scale.
#' @param y_scaled Numeric vector equal to \code{y / response_scale}, with
#'   at least one positive entry.
#' @param x_mat Finite numeric design matrix with \code{length(y)} rows,
#'   column labels matching the coefficients, and an intercept in column one.
#' @param response_scale Positive finite numeric scalar used to divide the response.
#' @param attempts List of per-rung attempt records.
#' @param rank_x_pos Integer rank of the positive-response design rows.
#'
#' @return A \code{\link{hetid_log_variance_fit}} list with
#'   \code{fit_status = "ok"}, original-scale coefficients, scaled
#'   \code{warm_start}, solver iterations in \code{convergence_code}, and
#'   the response, design, and diagnostics. It is assembled from validated
#'   inputs without re-validating the container.
#' @keywords internal
ppml_success <- function(acc, run, y, y_scaled, x_mat, response_scale,
                         attempts, rank_x_pos) {
  coef_original <- acc$coef_scaled
  coef_original[1] <- coef_original[1] + log(response_scale)
  objective <- sum(acc$mu) - sum(y_scaled[acc$pos] * log(acc$mu[acc$pos]))
  diagnostics <- list(
    warnings = run$warnings, messages = run$messages,
    error_class = NA_character_, start_attempts = attempts,
    min_pos_response = min(y_scaled[acc$pos]), rank_x_pos = rank_x_pos,
    condition_weighted_scaled = acc$condition_weighted_scaled,
    rcond_info_raw = acc$rcond_info_raw, info_col_scale = acc$info_col_scale,
    score_norm_raw = acc$score_norm_raw, score_norm_scaled = acc$score_norm
  )
  log_variance_fit_object(
    coef_original, LOG_VARIANCE_FIT_STATUS[["ok"]], TRUE, objective, acc$score_norm,
    as.integer(run$fit$iter), acc$coef_scaled, diagnostics, y, x_mat, "ppml",
    response_scale, length(y), colnames(x_mat)
  )
}

#' Assemble a Fail-Closed PPML Result
#'
#' An unsuccessful response solve is a result: the caller gets
#' the same container with \code{fit_status = "nonconvergence"} and the reason
#' in \code{diagnostics$error_class}.
#'
#' @param error_class Nonmissing character scalar naming the failure.
#' @param y Finite nonnegative numeric response vector on the original scale.
#' @param x_mat Finite numeric design matrix with \code{length(y)} rows,
#'   column labels, and an intercept in column one.
#' @param response_scale Positive finite numeric scalar used to divide the response.
#' @param attempts List of per-rung attempt records; defaults to an empty list
#'   before the start ladder is attempted.
#' @param ... Named diagnostics fields merged by \code{\link{ppml_diagnostics}}.
#'
#' @return A \code{\link{hetid_log_variance_fit}} list with
#'   \code{coef} and \code{warm_start} set to \code{NULL}, \code{objective}
#'   and \code{score_norm} set to \code{NA_real_}, \code{converged = FALSE},
#'   and \code{convergence_code = -1L}. The response, design, and diagnostics
#'   are retained.
#' @keywords internal
ppml_failure <- function(error_class, y, x_mat, response_scale,
                         attempts = list(), ...) {
  log_variance_failure_fit(
    y, x_mat, response_scale, "ppml",
    ppml_diagnostics(error_class, attempts, ...)
  )
}
