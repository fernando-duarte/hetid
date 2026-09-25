# Paper-to-package fitting controls and result conversion.
logvar_package_control <- function(control, estimator, auto_intercept = TRUE) {
  map <- if (estimator == "ppml") {
    required <- list(
      fit_function = "glm.fit", family = "quasipoisson", link = "log",
      rank_switch = TRUE, finite_mean_switch = TRUE, boundary_switch = TRUE
    )
    for (key in names(required)) {
      if (!identical(control[[key]], required[[key]])) {
        stop("PPML control ", key, " must equal ", required[[key]], call. = FALSE)
      }
    }
    c(
      GLM_EPSILON = "glm_epsilon", GLM_MAXIT = "glm_maxit",
      SCORE_TOLERANCE = "score_tol", RANK_TOLERANCE = "rank_tol",
      RCOND_TOLERANCE = "rcond_tol"
    )
  } else {
    c(
      SCORE_TOLERANCE = "score_tol", RANK_TOLERANCE = "response_rank_tol",
      RCOND_TOLERANCE = "rcond_tol", NEWTON_RCOND_TOLERANCE = "newton_rcond_tol",
      LINE_SEARCH_HALVINGS = "line_search_halvings", Q_NOISE_MULTIPLIER = "q_noise_multiplier",
      SCORE_PROGRESS_MULTIPLIER = "score_progress_multiplier", MAXIT = "maxit",
      REL_CHANGE_TOLERANCE = "rel_change_tol"
    )
  }
  out <- stats::setNames(lapply(unname(map), function(key) control[[key]]), names(map))
  out$SKIP_NONFINITE_STARTS <- TRUE
  if (estimator == "ppml") {
    groups <- c(
      supplied_start = "supplied", fallback_starts = "fallback",
      intercept_only = "intercept_only", glm_default = "glm_default"
    )
    order <- strsplit(control$fallback_order, ",", fixed = TRUE)[[1L]]
    out$START_ORDER <- unname(groups[order])
  } else {
    out$AUTO_INTERCEPT <- auto_intercept
  }
  out
}

# Preserve the paper design labels while the package uses a unique internal axis.
logvar_package_fitter <- function(x_mat, estimator, control) {
  stopifnot(
    is.matrix(x_mat), is.numeric(x_mat), ncol(x_mat) >= 1L,
    all(is.finite(x_mat[, 1L])), all(x_mat[, 1L] == 1)
  )
  labels <- colnames(x_mat)
  p <- ncol(x_mat)
  package_labels <- c("(Intercept)", if (p > 1L) paste0("paper_x", seq_len(p - 1L)))
  x <- x_mat[, -1L, drop = FALSE]
  colnames(x) <- package_labels[-1L]
  fitter <- hetid::make_log_variance_fitter(x, estimator, control)
  translate_start <- function(value) {
    if (!is.null(names(value))) {
      expected <- if (is.null(labels) && estimator == "harvey") paste0("V", seq_len(p)) else labels
      if (!identical(names(value), expected)) {
        stop("Start names must equal the paper design labels in order", call. = FALSE)
      }
      names(value) <- package_labels
    }
    value
  }
  function(y, start = NULL, fallback_starts = list(), response_scale = 1) {
    stopifnot(is.list(fallback_starts))
    fit <- fitter(
      y, translate_start(start),
      lapply(fallback_starts, translate_start), response_scale
    )
    if (isTRUE(fit$converged)) {
      names(fit$coef) <- if (estimator == "harvey" && is.null(labels)) {
        paste0("V", seq_len(p))
      } else {
        labels
      }
      names(fit$warm_start) <- labels
      if (estimator == "ppml") {
        names(fit$diagnostics$info_col_scale) <- labels
      } else {
        dimnames(fit$diagnostics$info_matrix) <- if (is.null(labels)) {
          NULL
        } else {
          list(labels, labels)
        }
      }
    }
    # The paper records controls in estimator metadata and keeps its established fit schema.
    fit$diagnostics$fit_control <- NULL
    new_logvar_fit_result(
      coef = fit$coef, fit_status = fit$fit_status, converged = fit$converged,
      objective = fit$objective, score_norm = fit$score_norm,
      convergence_code = fit$convergence_code, warm_start = fit$warm_start,
      diagnostics = fit$diagnostics
    )
  }
}

logvar_package_fit <- function(y, x_mat, estimator, start, fallback_starts,
                               response_scale, control) {
  fitter <- logvar_package_fitter(x_mat, estimator, control)
  fitter(y, start, fallback_starts, response_scale)
}
