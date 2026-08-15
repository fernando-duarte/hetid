# The Fisher-scoring solve behind the log-variance equation's Harvey
# (Gaussian multiplicative-heteroskedasticity) estimator: the criterion
# 0.5 * (sum(eta) + sum(y / exp(eta))) with eta = X theta, minimized on
# y / response_scale over a deterministic start ladder, with the fresh
# post-stop acceptance gate applied to each rung. Ported from the paper
# pipeline (scripts-paper/log_variance/estimators/harvey/solver.R). No
# clamping, no epsilon added to y, no eta capping. The scaled-response guard is
# the estimator-neutral log_variance_scaled_response_class(); the result
# assembly lives in R/harvey_result.R. A file-level roxygen block would collide
# with harvey_fit_response's own Rd page, so this header stays a comment.

#' Build the Harvey Start Ladder
#'
#' Hard-coded rung order: the supplied start, each fallback start, then the
#' intercept-only start. There is no \code{glm.fit}-default rung to close the
#' ladder with, since this solver has no data-driven start of its own; the
#' intercept-only rung is that role here, and it always exists because the
#' all-zero response was ruled out before the ladder is built.
#'
#' @param start Numeric start vector, or \code{NULL}
#' @param fallback_starts List of numeric start vectors
#' @param y_scaled Numeric response on the scaled (fitted) scale
#' @param p Number of design columns
#'
#' @return List with \code{candidates} and the matching \code{labels}
#' @noRd
harvey_start_ladder <- function(start, fallback_starts, y_scaled, p) {
  groups <- list(
    supplied = if (is.null(start)) list() else list(start),
    fallback = fallback_starts,
    intercept_only = list(c(log(mean(y_scaled)), rep(0, p - 1L)))
  )
  list(
    candidates = unlist(groups, recursive = FALSE),
    labels = rep(names(groups), lengths(groups))
  )
}

#' Fit the Harvey Log-Variance Response
#'
#' Walks the start ladder and returns the first accepted fit, recovering the
#' original-scale coefficients from the scaled solve. A response the estimator
#' cannot fit -- an all-zero response, a scaled response that under- or
#' overflowed, a design whose cross-product is singular, or a ladder with no
#' accepted rung -- comes back as a fail-closed result, never an error;
#' malformed arguments are the exported boundary's business.
#'
#' @details
#' Zero response rows are first-class and go straight to the solve: the ratio
#' helper keeps them exact, and \code{rank_x_pos} is recorded as a diagnostic
#' rather than gating anything, since a positive-response design that cannot
#' resolve the coefficients is caught by the post-stop conditioning gate. The
#' paper's recession certificate, which decides in advance whether interior
#' zeros push the criterion to its infimum along a ray, is deliberately not
#' ported: it needs a linear program the package has no dependency for. A
#' recessing likelihood therefore fails closed the slow way, through
#' \code{"line_search_stall"}, \code{"iteration_cap"}, or a rung whose start
#' does not evaluate -- the same verdict, reached by iterating rather than by
#' certificate.
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
harvey_fit_response <- function(y, x_mat, start = NULL,
                                fallback_starts = list(), response_scale = 1) {
  y_scaled <- y / response_scale
  scale_failure <- log_variance_scaled_response_class(y, y_scaled)
  if (!is.na(scale_failure)) {
    return(harvey_failure(scale_failure, y, x_mat, response_scale))
  }
  pos <- y_scaled > 0
  n_zero <- sum(!pos)
  rank_x_pos <- qr(
    x_mat[pos, , drop = FALSE],
    tol = LOG_VARIANCE_HARVEY_CONTROL$RANK_TOLERANCE
  )$rank
  # the Fisher direction needs this factor, so a design the Cholesky rejects
  # leaves the solver with no globally safe step at all
  chol_xx <- tryCatch(chol(crossprod(x_mat)), error = function(cond) NULL)
  if (is.null(chol_xx)) {
    return(harvey_failure(
      "singular_design", y, x_mat, response_scale,
      n_zero_response = n_zero, rank_x_pos = rank_x_pos
    ))
  }
  col_abs <- colSums(abs(x_mat))
  ladder <- harvey_start_ladder(start, fallback_starts, y_scaled, ncol(x_mat))
  attempts <- list()
  last_error <- "no_accepted_start"
  for (i in seq_along(ladder$candidates)) {
    src <- ladder$labels[i]
    cur <- harvey_eval(ladder$candidates[[i]], y_scaled, x_mat, pos, col_abs)
    if (is.null(cur)) {
      attempts <- c(attempts, list(list(
        source = src, error_class = "invalid_start"
      )))
      last_error <- "invalid_start"
      next
    }
    scored <- harvey_scoring(cur, y_scaled, x_mat, pos, col_abs, chol_xx)
    if (scored$status != "converged") {
      attempts <- c(attempts, list(list(
        source = src, error_class = scored$status
      )))
      last_error <- scored$status
      next
    }
    accepted <- harvey_post_stop(
      scored$eval$theta, y_scaled, x_mat, pos, col_abs
    )
    if (is.null(accepted)) {
      attempts <- c(attempts, list(list(
        source = src, error_class = "post_stop_reject"
      )))
      last_error <- "post_stop_reject"
      next
    }
    attempts <- c(attempts, list(list(
      source = src, error_class = NA_character_
    )))
    return(harvey_success(
      accepted, scored, y, x_mat, response_scale, attempts, n_zero, rank_x_pos
    ))
  }
  harvey_failure(
    last_error, y, x_mat, response_scale, attempts,
    n_zero_response = n_zero, rank_x_pos = rank_x_pos
  )
}
