# The zero-safe Fisher-scoring solver for the Harvey multiplicative-variance
# likelihood: the response-level fit, the residual-forming b wrapper, and the
# pure no-fit stability precheck. The update is an
# observed-Newton step (Fisher-scoring fallback when the observed information
# is ill conditioned), driven by a monotone
# backtracking line search that accepts a strict criterion decrease or, on an
# exact criterion tie, only a strict scaled-score improvement. Convergence is a
# passed scaled score plus a relative criterion or parameter change, with the
# initial-start shortcut the one score-only acceptance; a fresh post-stop
# recomputation of the safe ratio, criterion, strictly positive variance, and
# information conditioning then gates acceptance. Zero responses are first-class:
# the ratio helper keeps them exact, all(y == 0) is the explicit negative
# intercept recession, and any interior zero routes through the cone certificate
# before fitting. No full-vector y * exp(-eta), y / mu, or e / mu ever forms, no
# eta capping, no clamping. The scoring and acceptance helpers live in
# solver_acceptance.R (line-count split); this file owns the public
# fit, wrapper, and precheck. Sourced by estimator.R after math/recession.

paper_source_once(paper_path("log_variance", "estimators", "harvey", "solver_acceptance.R"))
paper_source_once(paper_path("log_variance", "estimators", "harvey", "solver_result.R"))

# The core solver on an arbitrary nonnegative response. Programming errors (bad
# dimensions, negative y) stop(); every modelled failure returns a typed result.
# auto_intercept = FALSE suppresses the closing intercept-only rung so the
# constructor's warm-only first attempt can genuinely fall through to its
# PPML and standalone escalation stages.
logvar_harvey_fit_response <- function(y, x_mat, start = NULL,
                                       fallback_starts = list(), chol_xx = NULL,
                                       auto_intercept = TRUE,
                                       control = LOGVAR_HARVEY_CONTROL) {
  if (!is.matrix(x_mat) || !is.numeric(x_mat)) {
    stop("x_mat must be a numeric matrix")
  }
  n <- nrow(x_mat)
  p <- ncol(x_mat)
  if (!is.numeric(y) || length(y) != n || anyNA(y) || any(!is.finite(y))) {
    stop("y must be a finite numeric vector of length nrow(x_mat)")
  }
  if (any(y < 0)) {
    stop("y must be nonnegative")
  }
  cn <- colnames(x_mat)
  if (is.null(cn)) {
    cn <- paste0("V", seq_len(p))
  }
  pos <- y > 0
  n_zero <- sum(!pos)
  col_abs <- colSums(abs(x_mat))
  rank_x_pos <- if (any(pos)) {
    qr(
      x_mat[pos, , drop = FALSE],
      tol = control$response_rank_tol
    )$rank
  } else {
    0L
  }
  if (!any(pos)) {
    return(hv_result(
      fit_status = "nonexistence",
      error_class = "negative_recession_all_zero", n_zero = n_zero,
      rank_x_pos = rank_x_pos
    ))
  }
  recession <- NULL
  if (n_zero > 0L) {
    recession <- logvar_harvey_recession_certificate(
      y, x_mat, control
    )
    cls <- recession$classification
    if (!identical(cls, "pass")) {
      mapped <- hv_recession_map[[cls]]
      if (is.null(mapped)) {
        mapped <- c("nonconvergence", "recession_certificate_failed")
      }
      return(hv_result(
        fit_status = mapped[[1]], error_class = mapped[[2]],
        n_zero = n_zero, rank_x_pos = rank_x_pos, recession = recession
      ))
    }
  }
  chol_xx <- hv_chol_xx(x_mat, chol_xx)
  rungs <- list()
  sources <- character(0)
  if (!is.null(start)) {
    rungs <- c(rungs, list(start))
    sources <- c(sources, "supplied")
  }
  for (fb in fallback_starts) {
    rungs <- c(rungs, list(fb))
    sources <- c(sources, "fallback")
  }
  if (auto_intercept && mean(y) > 0) {
    rungs <- c(rungs, list(c(log(mean(y)), rep(0, p - 1L))))
    sources <- c(sources, "intercept_only")
  }
  attempts <- list()
  crit <- list()
  last_ec <- "nonconvergence"
  for (i in seq_along(rungs)) {
    src <- sources[i]
    cur <- hv_eval(rungs[[i]], y, x_mat, pos, col_abs)
    if (is.null(cur)) {
      attempts <- c(attempts, list(list(source = src, error_class = "invalid_start")))
      last_ec <- "invalid_start"
      next
    }
    sc <- hv_scoring(
      cur, y, x_mat, pos, col_abs, chol_xx, control
    )
    crit <- c(crit, list(list(
      source = src, status = sc$status,
      score_norm = sc$eval$score_norm, objective = sc$eval$q
    )))
    if (sc$status != "converged") {
      attempts <- c(attempts, list(list(source = src, error_class = sc$status)))
      last_ec <- sc$status
      next
    }
    ps <- hv_post_stop(
      sc$eval$theta, y, x_mat, pos, col_abs, control
    )
    if (is.null(ps)) {
      attempts <- c(attempts, list(list(source = src, error_class = "post_stop_reject")))
      last_ec <- "post_stop_reject"
      next
    }
    attempts <- c(attempts, list(list(source = src, error_class = NA_character_)))
    coef <- ps$eval$theta
    names(coef) <- cn
    return(hv_result(
      coef = coef, fit_status = "ok", converged = TRUE,
      objective = ps$eval$q, score_norm = ps$eval$score_norm,
      convergence_code = as.integer(sc$iters), warm_start = ps$eval$theta,
      n_zero = n_zero, rank_x_pos = rank_x_pos, rcond_info = ps$rcond,
      n_halvings = sc$halves, start_attempts = attempts,
      per_start_criteria = if (length(rungs) > 1L) crit else NULL,
      recession = recession, info_matrix = ps$info
    ))
  }
  hv_result(
    fit_status = "nonconvergence", error_class = last_ec, n_zero = n_zero,
    rank_x_pos = rank_x_pos, start_attempts = attempts,
    per_start_criteria = if (length(crit) > 0L) crit else NULL,
    recession = recession
  )
}

# The b wrapper: form the squared residual response, forward every argument
# including the shared Cholesky, and record the minimum absolute residual.
logvar_harvey_fit <- function(b, w1, w2, x_mat, start = NULL,
                              fallback_starts = list(), chol_xx = NULL,
                              auto_intercept = TRUE,
                              control = LOGVAR_HARVEY_CONTROL) {
  eps <- drop(w1 - w2 %*% b)
  fit <- logvar_harvey_fit_response(eps^2, x_mat,
    start = start,
    fallback_starts = fallback_starts, chol_xx = chol_xx,
    auto_intercept = auto_intercept,
    control = control
  )
  fit$diagnostics$min_abs_eps <- min(abs(eps))
  fit
}

# The pure driver-level precheck over the named response/start pairs. Returns one
# typed record per pair (with a `passed` attribute); it never fits or constructs.
logvar_harvey_stability_precheck <- function(response_start_pairs, x_mat,
                                             chol_xx = NULL,
                                             control = LOGVAR_HARVEY_CONTROL) {
  if (!is.matrix(x_mat) || !is.numeric(x_mat)) {
    stop("x_mat must be a numeric matrix")
  }
  n <- nrow(x_mat)
  p <- ncol(x_mat)
  chol_xx <- hv_chol_xx(x_mat, chol_xx)
  col_abs <- colSums(abs(x_mat))
  nms <- names(response_start_pairs)
  if (is.null(nms)) {
    nms <- as.character(seq_along(response_start_pairs))
  }
  out <- vector("list", length(response_start_pairs))
  names(out) <- nms
  for (i in seq_along(response_start_pairs)) {
    pair <- response_start_pairs[[i]]
    label <- if (!is.null(pair$label)) pair$label else nms[i]
    out[[i]] <- hv_precheck_pair(pair, label, x_mat, n, p, col_abs, chol_xx)
  }
  attr(out, "passed") <- all(vapply(out, function(r) isTRUE(r$ok), logical(1)))
  out
}
