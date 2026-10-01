#' Harvey Result Assembly
#'
#' The two ways a Harvey response solve ends -- an accepted fit and a
#' fail-closed result -- plus the diagnostics container they share. Both go
#' through \code{\link{new_hetid_log_variance_fit}}, so every branch comes back
#' in one shape, exactly as \code{\link{ppml_result}} does for PPML.
#'
#' @name harvey_result
#' @keywords internal
NULL

#' Build the Harvey Diagnostics List
#'
#' NA and empty defaults for every diagnostic field; callers override only what
#' they can populate, so an early fail-closed return stays field-compatible
#' with an accepted fit. Per-start criteria and the accepted information
#' matrix refer to the scaled response. Recession certificates remain caller owned.
#'
#' @param error_class A single string naming the failure, or
#'   \code{NA_character_} for an accepted fit.
#' @param start_attempts A list of per-rung attempt records.
#' @param ... Named diagnostic fields to override or add. A \code{NULL}
#'   override is retained, rather than removing the field.
#'
#' @return A named list containing \code{warnings}, \code{messages},
#'   \code{error_class}, \code{start_attempts}, \code{n_zero_response},
#'   \code{rank_x_pos}, \code{rcond_info}, \code{n_halvings},
#'   \code{per_start_criteria}, and \code{info_matrix}, plus any added fields.
#'   Unpopulated fields are empty character vectors, typed missing scalars,
#'   or \code{NULL}; \code{error_class} and \code{start_attempts} are supplied
#'   by the caller. This helper does not validate diagnostic values.
#' @keywords internal
#' @importFrom utils modifyList
harvey_diagnostics <- function(error_class, start_attempts, ...) {
  base <- list(
    warnings = character(0), messages = character(0),
    error_class = error_class, start_attempts = start_attempts,
    n_zero_response = NA_integer_, rank_x_pos = NA_integer_,
    rcond_info = NA_real_, n_halvings = NA_integer_,
    per_start_criteria = NULL, info_matrix = NULL
  )
  modifyList(base, list(...), keep.null = TRUE)
}

#' Assemble an Accepted Harvey Fit
#'
#' Recovers the original-scale coefficients by adding
#' \code{log(response_scale)} to the intercept only, keeps the raw scaled-fit
#' vector as \code{warm_start}, and reports the criterion and score norm the
#' post-stop gate recomputed on the scaled response.
#'
#' @param accepted A non-\code{NULL} post-stop verdict list from
#'   \code{\link{harvey_post_stop}}, evaluated on the scaled response.
#' @param scored A scoring result list from \code{\link{harvey_scoring}}.
#' @param y A nonempty, finite, nonnegative numeric response vector on the original scale.
#' @param x_mat A finite numeric design matrix with \code{length(y)} rows,
#'   column names, and the intercept in its first column.
#' @param response_scale A positive finite numeric scalar the response was divided by.
#' @param attempts A list of per-rung attempt records.
#' @param n_zero_response The number of zero response rows.
#' @param rank_x_pos The integer rank of the positive-response design rows.
#' @param criteria A list of per-start numerical evidence, or \code{NULL}
#'   (the default) when that evidence is not supplied.
#'
#' @return A validated \code{hetid_log_variance_fit} list, returned visibly,
#'   with \code{fit_status = "ok"}, \code{converged = TRUE}, and coefficient
#'   vectors named by \code{colnames(x_mat)}. The original response and design
#'   are retained as \code{y} and \code{x_design}; \code{convergence_code}
#'   records the number of scoring iterations and \code{diagnostics} contains
#'   the acceptance evidence.
#' @seealso \code{\link{hetid_log_variance_fit}}.
#' @keywords internal
harvey_success <- function(accepted, scored, y, x_mat, response_scale,
                           attempts, n_zero_response, rank_x_pos, criteria = NULL) {
  coef_scaled <- accepted$eval$theta
  names(coef_scaled) <- colnames(x_mat)
  coef_original <- coef_scaled
  coef_original[1] <- coef_original[1] + log(response_scale)
  out <- validate_hetid_log_variance_fit(new_hetid_log_variance_fit(
    coef = coef_original, fit_status = LOG_VARIANCE_FIT_STATUS[["ok"]],
    converged = TRUE, objective = accepted$eval$q,
    score_norm = accepted$eval$score_norm,
    convergence_code = as.integer(scored$iters), warm_start = coef_scaled,
    diagnostics = harvey_diagnostics(
      NA_character_, attempts,
      n_zero_response = n_zero_response, rank_x_pos = rank_x_pos,
      rcond_info = accepted$rcond, n_halvings = scored$halves,
      per_start_criteria = criteria, info_matrix = accepted$info
    ),
    y = y, x_design = x_mat, estimator = "harvey",
    response_scale = response_scale, n_obs = length(y),
    coef_labels = colnames(x_mat)
  ))
  out
}

#' Assemble a Fail-Closed Harvey Result
#'
#' A response the solver cannot fit is a result, not an error: the caller gets
#' the same container with \code{fit_status = "nonconvergence"} and the reason
#' in \code{diagnostics$error_class}.
#'
#' @details
#' Solver failure is recorded without an error. Construction and validation can
#' still raise structured \code{hetid_error} conditions for malformed container
#' inputs. Missing or non-finite response and design values are rejected;
#' rows are not removed.
#'
#' @param error_class A single non-missing string naming the failure.
#' @param y A nonempty, finite, nonnegative numeric response vector on the original scale.
#' @param x_mat A finite numeric design matrix with \code{length(y)} rows,
#'   column names, and the intercept in its first column.
#' @param response_scale A positive finite numeric scalar the response was divided by.
#' @param attempts A list of per-rung attempt records. The default is an empty
#'   list for a failure before the start ladder.
#' @param ... Named diagnostic fields passed to \code{\link{harvey_diagnostics}}.
#'
#' @return A validated \code{hetid_log_variance_fit} list, returned visibly,
#'   with \code{converged = FALSE}, \code{coef = NULL}, \code{warm_start = NULL},
#'   \code{objective = NA_real_}, \code{score_norm = NA_real_}, and
#'   \code{convergence_code = -1L}. The original response and design are retained
#'   as \code{y} and \code{x_design}, together with the failure diagnostics.
#' @seealso \code{\link{hetid_log_variance_fit}}.
#' @keywords internal
harvey_failure <- function(error_class, y, x_mat, response_scale,
                           attempts = list(), ...) {
  log_variance_failure_fit(
    y, x_mat, response_scale, "harvey",
    harvey_diagnostics(error_class, attempts, ...)
  )
}
