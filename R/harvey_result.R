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
#' with an accepted fit. The paper's per-start criteria, recession certificate,
#' and stored information matrix are not carried here.
#'
#' @param error_class Single string naming the failure, or \code{NA}
#' @param start_attempts List of per-rung attempt records
#' @param ... Fields to override in the defaults
#'
#' @return A diagnostics list
#' @keywords internal
#' @importFrom utils modifyList
harvey_diagnostics <- function(error_class, start_attempts, ...) {
  base <- list(
    warnings = character(0), messages = character(0),
    error_class = error_class, start_attempts = start_attempts,
    n_zero_response = NA_integer_, rank_x_pos = NA_integer_,
    rcond_info = NA_real_, n_halvings = NA_integer_
  )
  modifyList(base, list(...))
}

#' Assemble an Accepted Harvey Fit
#'
#' Recovers the original-scale coefficients by adding
#' \code{log(response_scale)} to the intercept only, keeps the raw scaled-fit
#' vector as \code{warm_start}, and reports the criterion and score norm the
#' post-stop gate recomputed on the scaled response.
#'
#' @param accepted Post-stop verdict from \code{\link{harvey_post_stop}}
#' @param scored Scoring result from \code{\link{harvey_scoring}}
#' @param y Numeric response on the original scale
#' @param x_mat Numeric design matrix, intercept column included
#' @param response_scale Positive finite scalar the response was divided by
#' @param attempts List of per-rung attempt records
#' @param n_zero_response Count of zero response rows
#' @param rank_x_pos Integer rank of the positive-response design rows
#'
#' @return A validated \code{hetid_log_variance_fit} object
#' @keywords internal
harvey_success <- function(accepted, scored, y, x_mat, response_scale,
                           attempts, n_zero_response, rank_x_pos) {
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
      rcond_info = accepted$rcond, n_halvings = scored$halves
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
#' @param error_class Single string naming the failure
#' @param y Numeric response on the original scale
#' @param x_mat Numeric design matrix, intercept column included
#' @param response_scale Positive finite scalar the response was divided by
#' @param attempts List of per-rung attempt records (empty before the ladder)
#' @param ... Extra diagnostics fields to record
#'
#' @return A validated \code{hetid_log_variance_fit} object
#' @keywords internal
harvey_failure <- function(error_class, y, x_mat, response_scale,
                           attempts = list(), ...) {
  out <- validate_hetid_log_variance_fit(new_hetid_log_variance_fit(
    coef = NULL, fit_status = LOG_VARIANCE_FIT_STATUS[["nonconvergence"]],
    converged = FALSE, objective = NA_real_, score_norm = NA_real_,
    convergence_code = -1L, warm_start = NULL,
    diagnostics = harvey_diagnostics(error_class, attempts, ...),
    y = y, x_design = x_mat, estimator = "harvey",
    response_scale = response_scale, n_obs = length(y),
    coef_labels = colnames(x_mat)
  ))
  out
}
