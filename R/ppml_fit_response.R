# The core IRLS solve behind the log-variance equation: glm.fit with
# quasipoisson(link = "log") on y / response_scale, walked over a
# deterministic start ladder, with the fail-closed acceptance check applied to
# each rung. Ported from the paper pipeline
# (scripts-paper/log_variance/estimators/ppml/fit.R). No clamping, no epsilon
# added to y, no suppressed conditions. The scaled-response guard is the
# estimator-neutral log_variance_scaled_response_class(). A file-level roxygen
# block would collide with ppml_fit_response's own Rd page, so this header
# stays a comment.

#' Build the Start Ladder
#'
#' Hard-coded rung order: the supplied start, each fallback start, the
#' intercept-only start, then the \code{glm.fit} default (\code{NULL}).
#'
#' @param start Numeric start vector, or \code{NULL}
#' @param fallback_starts List of numeric start vectors
#' @param y_scaled Numeric response on the scaled (fitted) scale
#' @param p Number of design columns
#'
#' @return List with \code{candidates} (a list whose last element is
#'   \code{NULL}) and the matching \code{labels}
#' @noRd
ppml_start_ladder <- function(start, fallback_starts, y_scaled, p) {
  intercept_start <- if (mean(y_scaled) > 0) {
    list(c(log(mean(y_scaled)), rep(0, p - 1L)))
  } else {
    list()
  }
  groups <- list(
    supplied = if (is.null(start)) list() else list(start),
    fallback = fallback_starts,
    intercept_only = intercept_start,
    glm_default = list(NULL)
  )
  list(
    candidates = unlist(groups, recursive = FALSE),
    labels = rep(names(groups), lengths(groups))
  )
}

#' Screen a Candidate Start
#'
#' @param cand Candidate start vector, or \code{NULL} for the glm.fit default
#' @param x_mat Numeric design matrix, intercept column included
#'
#' @return \code{TRUE} when the start overflows the log link and must be
#'   skipped
#' @noRd
ppml_start_invalid <- function(cand, x_mat) {
  !is.null(cand) &&
    !(all(is.finite(cand)) && all(is.finite(exp(drop(x_mat %*% cand)))))
}

#' Fit the PPML Log-Variance Response
#'
#' Walks the start ladder and returns the first accepted fit, recovering the
#' original-scale coefficients from the scaled solve. A response the estimator
#' cannot fit -- an all-zero response, a rank-unresolved positive-response
#' design, a scaled response that under- or overflowed, or a ladder with no
#' accepted rung -- comes back as a fail-closed result, never an error;
#' malformed arguments are the exported boundary's business.
#'
#' @param y Numeric nonnegative response on the original scale
#' @param x_mat Numeric design matrix from \code{\link{log_variance_design}},
#'   intercept column included and column labels validated
#' @param start Numeric start vector on the scaled response, or \code{NULL}
#' @param fallback_starts List of numeric start vectors on the scaled response
#' @param response_scale Positive finite scalar to divide \code{y} by
#'
#' @return A validated \code{hetid_log_variance_fit} object
#' @keywords internal
ppml_fit_response <- function(y, x_mat, start = NULL, fallback_starts = list(),
                              response_scale = 1) {
  y_scaled <- y / response_scale
  scale_failure <- log_variance_scaled_response_class(y, y_scaled)
  if (!is.na(scale_failure)) {
    return(ppml_failure(scale_failure, y, x_mat, response_scale))
  }
  rank_x_pos <- ppml_pos_rank(y_scaled, x_mat)
  if (rank_x_pos != ncol(x_mat)) {
    return(ppml_failure(
      "rank_unresolved", y, x_mat, response_scale,
      rank_x_pos = rank_x_pos,
      min_pos_response = min(y_scaled[y_scaled > 0])
    ))
  }
  ladder <- ppml_start_ladder(start, fallback_starts, y_scaled, ncol(x_mat))
  attempts <- list()
  last <- list(
    warnings = character(0), messages = character(0),
    error_class = "no_accepted_start"
  )
  for (i in seq_along(ladder$candidates)) {
    cand <- ladder$candidates[[i]]
    if (ppml_start_invalid(cand, x_mat)) {
      attempts <- c(attempts, list(list(
        source = ladder$labels[i], error_class = "invalid_start"
      )))
      last$error_class <- "invalid_start"
      next
    }
    run <- ppml_run_glm(cand, y_scaled, x_mat)
    last$warnings <- run$warnings
    last$messages <- run$messages
    if (is.null(run$fit)) {
      attempts <- c(attempts, list(list(
        source = ladder$labels[i], error_class = "fit_error"
      )))
      last$error_class <- "fit_error"
      next
    }
    acc <- ppml_accept(run$fit, y_scaled, x_mat)
    attempts <- c(attempts, list(list(
      source = ladder$labels[i],
      error_class = if (acc$accepted) NA_character_ else acc$reason
    )))
    if (acc$accepted) {
      return(ppml_success(
        acc, run, y, y_scaled, x_mat, response_scale, attempts, rank_x_pos
      ))
    }
    last$error_class <- acc$reason
  }
  ppml_failure(
    last$error_class, y, x_mat, response_scale, attempts,
    warnings = last$warnings, messages = last$messages,
    rank_x_pos = rank_x_pos, min_pos_response = min(y_scaled[y_scaled > 0])
  )
}
