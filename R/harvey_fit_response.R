#' Build the Harvey Start Ladder
#'
#' Hard-coded rung order: the supplied start, each fallback start, then the
#' intercept-only start. There is no \code{glm.fit}-default rung to close the
#' ladder with, since this solver has no data-driven start of its own; the
#' intercept-only rung fills that role when \code{AUTO_INTERCEPT} is TRUE.
#' Disabling it permits warm-only fitting or an empty ladder.
#'
#' @param start Numeric coefficient vector of length \code{p}, or \code{NULL}.
#' @param fallback_starts List of numeric coefficient vectors of length \code{p}.
#' @param y_scaled Finite nonnegative response vector on the scaled (fitted)
#'   scale, with at least one positive value.
#' @param p Positive integer giving the number of design columns.
#' @param control Resolved Harvey fitting-control list; the default enables
#'   the intercept-only start through \code{AUTO_INTERCEPT = TRUE}.
#'
#' @return List with \code{candidates} and the matching \code{labels}.
#' @noRd
harvey_start_ladder <- function(
  start, fallback_starts, y_scaled, p, control = log_variance_fit_control("harvey")
) {
  groups <- list(
    supplied = if (is.null(start)) list() else list(start),
    fallback = fallback_starts,
    intercept_only = if (control$AUTO_INTERCEPT) {
      list(c(log(mean(y_scaled)), rep(0, p - 1L)))
    } else {
      list()
    }
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
#' malformed arguments are checked by \code{\link{fit_log_variance}} before
#' dispatch to this internal worker.
#'
#' @details
#' Zero response rows are first-class and go straight to the solve: the ratio
#' helper keeps them exact, and \code{rank_x_pos} is recorded as a diagnostic
#' rather than used as an acceptance condition. The post-stop check measures
#' the information matrix's conditioning. The solver does not establish
#' whether the criterion has a finite minimizer: numerical acceptance depends
#' on the configured tolerances. A failed line search or an exhausted
#' iteration limit returns a failed fit.
#'
#' @param y Finite nonnegative numeric vector of length \code{nrow(x_mat)},
#'   giving the response on the original scale. Missing values are not allowed.
#' @param x_mat Finite numeric design matrix from
#'   \code{\link{log_variance_design}}, with a leading intercept column and
#'   validated column labels. Rows correspond to \code{y} in the same order.
#' @param start Numeric coefficient vector of length \code{ncol(x_mat)} on
#'   the scaled response \code{y / response_scale}, or \code{NULL} (default).
#'   Named vectors must match the design column labels in order. Values must
#'   be finite unless \code{control$SKIP_NONFINITE_STARTS} is \code{TRUE}.
#' @param fallback_starts List of coefficient vectors following the same
#'   scale, length, naming, and finiteness rules as \code{start}, tried in
#'   list order after the supplied start. The default is an empty list.
#' @param response_scale Positive finite numeric scalar dividing \code{y}
#'   before fitting. The default \code{1} leaves the response unchanged.
#' @param control Resolved, validated Harvey fitting-control list, not a list
#'   of overrides. Defaults come from \code{\link{LOG_VARIANCE_HARVEY_CONTROL}},
#'   with \code{AUTO_INTERCEPT = TRUE} and \code{SKIP_NONFINITE_STARTS = FALSE}.
#' @param design List of fixed-design quantities for this \code{x_mat} and
#'   \code{control}, computed by \code{log_variance_fixed_design()} by default.
#'
#' @return A validated \code{\link{hetid_log_variance_fit}} list retaining
#'   the original response and design. On success, \code{fit_status = "ok"},
#'   \code{coef} contains named original-scale coefficients, and
#'   \code{warm_start} contains named scaled-response coefficients, each of
#'   length \code{ncol(x_mat)}. Only the intercept differs by
#'   \code{log(response_scale)}. The criterion and score use the scaled
#'   response. On failure, \code{fit_status = "nonconvergence"}, \code{coef}
#'   and \code{warm_start} are \code{NULL}, and \code{diagnostics$error_class}
#'   records the reason. Attempted starts are recorded in
#'   \code{diagnostics$start_attempts}.
#' @keywords internal
harvey_fit_response <- function(y, x_mat, start = NULL,
                                fallback_starts = list(), response_scale = 1,
                                control = log_variance_fit_control("harvey"),
                                design = log_variance_fixed_design(x_mat, "harvey", control)) {
  y_scaled <- y / response_scale
  scale_failure <- log_variance_scaled_response_class(y, y_scaled)
  if (!is.na(scale_failure)) {
    return(harvey_failure(scale_failure, y, x_mat, response_scale))
  }
  pos <- y_scaled > 0
  n_zero <- sum(!pos)
  rank_x_pos <- harvey_positive_rank(pos, x_mat, control, design)
  # Rounding can let Cholesky succeed with dependent columns, so check rank separately
  rank_x <- design$rank
  chol_xx <- design$chol_xx
  if (rank_x < ncol(x_mat) || is.null(chol_xx)) {
    return(harvey_failure(
      "singular_design", y, x_mat, response_scale,
      n_zero_response = n_zero, rank_x_pos = rank_x_pos
    ))
  }
  col_abs <- design$col_abs
  ladder <- harvey_start_ladder(start, fallback_starts, y_scaled, ncol(x_mat), control)
  attempts <- list()
  criteria <- list()
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
    scored <- harvey_scoring(cur, y_scaled, x_mat, pos, col_abs, chol_xx, control)
    criteria <- c(criteria, list(list(
      source = src, status = scored$status,
      score_norm = scored$eval$score_norm, objective = scored$eval$q
    )))
    if (scored$status != "converged") {
      attempts <- c(attempts, list(list(
        source = src, error_class = scored$status
      )))
      last_error <- scored$status
      next
    }
    accepted <- harvey_post_stop(scored$eval, x_mat, control)
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
      accepted, scored, y, x_mat, response_scale, attempts, n_zero, rank_x_pos,
      criteria = if (length(ladder$candidates) > 1L) criteria else NULL
    ))
  }
  harvey_failure(
    last_error, y, x_mat, response_scale, attempts,
    n_zero_response = n_zero, rank_x_pos = rank_x_pos,
    per_start_criteria = if (length(criteria)) criteria else NULL
  )
}

harvey_positive_rank <- function(pos, x_mat, control, design) {
  if (all(pos)) {
    design$rank
  } else {
    qr(x_mat[pos, , drop = FALSE], tol = control$RANK_TOLERANCE)$rank
  }
}
