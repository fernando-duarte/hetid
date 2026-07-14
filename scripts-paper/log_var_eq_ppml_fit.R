# The PPML log-variance response fit: the core IRLS solve on a frozen design
# plus the small score / information primitives it (and Plan 4's moment layer)
# lifts. logvar_ppml_fit_response runs glm.fit with quasipoisson(link = "log")
# on y / response_scale over a deterministic start ladder, applies the
# fail-closed post-fit acceptance checks (finite coefficients and strictly
# positive fitted means, IRLS convergence, boundary rejection, per-coordinate
# scaled score, column-normalized information gate), and returns the recovered
# original-scale coefficients with the raw scaled-fit vector as warm_start.
# Definitions only; sourced by log_var_eq_ppml.R before the b wrapper and the
# implicit Jacobian. No clamping, no epsilons on y, no suppressed warnings --
# every condition is recorded, not silenced. The post-fit machinery (result
# skeleton, diagnostics, rank check, acceptance, glm.fit runner, assembler)
# lives in log_var_eq_ppml_accept.R to keep both files under the 200-line cap.

source("scripts-paper/log_var_eq_ppml_accept.R")

# Quasi-Poisson score X'(y - mu) and information X' diag(mu) X at theta on the
# scaled response, factored so the acceptance check, the Jacobian, and Plan 4
# share one derivation instead of re-deriving the moment.
logvar_ppml_score <- function(theta_scaled, y_scaled, x_mat) {
  drop(crossprod(x_mat, y_scaled - exp(drop(x_mat %*% theta_scaled))))
}

logvar_ppml_info <- function(theta_scaled, x_mat) {
  crossprod(x_mat, exp(drop(x_mat %*% theta_scaled)) * x_mat)
}

# The core IRLS solve on arbitrary original-scale nonnegative responses:
# validate the frozen positive scale, fail closed on an all-zero or
# rank-unresolved positive-response design, then walk the start ladder and
# return the first accepted fit (else a nonconvergence result carrying the last
# error class and every start attempt).
logvar_ppml_fit_response <- function(y, x_mat, start = NULL,
                                     fallback_starts = list(),
                                     response_scale = 1) {
  if (!(is.numeric(response_scale) && length(response_scale) == 1L &&
    is.finite(response_scale) && response_scale > 0)) {
    stop("response_scale must be one finite positive scalar")
  }
  if (!is.numeric(y) || length(y) != nrow(x_mat) || any(!is.finite(y)) ||
    any(y < 0)) {
    stop("y must be a finite nonnegative vector of length nrow(x_mat)")
  }
  p <- ncol(x_mat)
  fail <- function(error_class, ...) {
    logvar_ppml_result(
      coef = NULL, fit_status = "nonconvergence", converged = FALSE,
      objective = NA_real_, score_norm = NA_real_, convergence_code = -1L,
      warm_start = NULL, diagnostics = logvar_ppml_diag(error_class, list(), ...)
    )
  }
  if (!any(y > 0)) {
    return(fail("all_zero_response"))
  }
  y_scaled <- y / response_scale
  rank_x_pos <- logvar_ppml_pos_rank(y_scaled, x_mat)
  if (rank_x_pos != p) {
    return(fail("rank_unresolved",
      rank_x_pos = rank_x_pos,
      min_pos_response = min(y_scaled[y_scaled > 0])
    ))
  }
  candidates <- list()
  labels <- character(0)
  if (!is.null(start)) {
    candidates <- c(candidates, list(start))
    labels <- c(labels, "supplied")
  }
  if (length(fallback_starts) > 0L) {
    candidates <- c(candidates, fallback_starts)
    labels <- c(labels, rep("fallback", length(fallback_starts)))
  }
  if (mean(y_scaled) > 0) {
    candidates <- c(candidates, list(c(log(mean(y_scaled)), rep(0, p - 1L))))
    labels <- c(labels, "intercept_only")
  }
  candidates <- c(candidates, list(NULL))
  labels <- c(labels, "glm_default")
  attempts <- list()
  last <- list(
    warnings = character(0), messages = character(0),
    error_class = "nonconvergence"
  )
  for (i in seq_along(candidates)) {
    cand <- candidates[[i]]
    if (!is.null(cand) && !(all(is.finite(cand)) &&
      all(is.finite(exp(drop(x_mat %*% cand)))))) {
      attempts <- c(attempts, list(list(
        source = labels[i],
        error_class = "invalid_start"
      )))
      last$error_class <- "invalid_start"
      next
    }
    run <- logvar_ppml_run(cand, y_scaled, x_mat)
    last$warnings <- run$warnings
    last$messages <- run$messages
    if (is.null(run$fit)) {
      attempts <- c(attempts, list(list(
        source = labels[i],
        error_class = "fit_error"
      )))
      last$error_class <- "fit_error"
      next
    }
    acc <- logvar_ppml_accept(run$fit, y_scaled, x_mat)
    attempts <- c(attempts, list(list(
      source = labels[i],
      error_class = if (acc$accepted) NA_character_ else acc$reason
    )))
    if (acc$accepted) {
      return(logvar_ppml_success(
        acc, run, y_scaled, response_scale, attempts,
        rank_x_pos
      ))
    }
    last$error_class <- acc$reason
  }
  logvar_ppml_result(
    coef = NULL, fit_status = "nonconvergence", converged = FALSE,
    objective = NA_real_, score_norm = NA_real_, convergence_code = -1L,
    warm_start = NULL,
    diagnostics = logvar_ppml_diag(
      last$error_class, attempts,
      warnings = last$warnings,
      messages = last$messages, rank_x_pos = rank_x_pos,
      min_pos_response = min(y_scaled[y_scaled > 0])
    )
  )
}

# The only sanctioned cross-estimator start object: NULL unless fit is an
# accepted point fit, else a typed bundle whose coef_original is the recovered
# vector and coef_scaled the raw scaled-fit warm start.
logvar_ppml_start_bundle <- function(fit, response_scale, source, b) {
  if (!(identical(fit$fit_status, "ok") && isTRUE(fit$converged))) {
    return(NULL)
  }
  list(
    coef_original = fit$coef, coef_scaled = fit$warm_start,
    response_scale = response_scale,
    valid_for = c("variance_start", "irls_warm_start"),
    source = source, b = as.numeric(b)
  )
}
