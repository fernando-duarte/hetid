# Harvey paper existence prechecks, fit adapter and driver stability checks.

paper_source_once(paper_path("log_variance", "estimators", "harvey", "solver_primitives.R"))
paper_source_once(paper_path("log_variance", "estimators", "harvey", "solver_result.R"))
paper_source_once(paper_path("log_variance", "estimators", "package_fit.R"))

# The core solver on an arbitrary nonnegative response. Programming errors (bad
# dimensions, negative y) stop(); every modelled failure returns a typed result.
# auto_intercept = FALSE suppresses the closing intercept-only rung so the
# constructor's warm-only first attempt can genuinely fall through to its
# PPML and standalone escalation stages.
logvar_harvey_fitter <- function(x_mat, control = LOGVAR_HARVEY_CONTROL) {
  if (!is.matrix(x_mat) || !is.numeric(x_mat)) {
    stop("x_mat must be a numeric matrix")
  }
  n <- nrow(x_mat)
  p <- ncol(x_mat)
  complete_fit <- logvar_package_fitter(
    x_mat, "harvey",
    logvar_package_control(control, "harvey", TRUE)
  )
  warm_fit <- logvar_package_fitter(
    x_mat, "harvey",
    logvar_package_control(control, "harvey", FALSE)
  )
  diagnostic_template <- hv_result(fit_status = "nonconvergence")$diagnostics
  function(y, start = NULL, fallback_starts = list(), auto_intercept = TRUE) {
    if (!is.numeric(y) || length(y) != n || anyNA(y) || any(!is.finite(y))) {
      stop("y must be a finite numeric vector of length nrow(x_mat)")
    }
    if (any(y < 0)) {
      stop("y must be nonnegative")
    }
    pos <- y > 0
    n_zero <- sum(!pos)
    rank_x_pos <- if (n_zero > 0L && any(pos)) {
      qr(x_mat[pos, , drop = FALSE], tol = control$response_rank_tol)$rank
    } else if (n_zero > 0L) 0L else NA_integer_
    if (!any(pos)) {
      return(hv_result(
        fit_status = LOGVAR_FIT_STATUS[["nonexistence"]],
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
          mapped <- c(LOGVAR_FIT_STATUS[["nonconvergence"]], "recession_certificate_failed")
        }
        return(hv_result(
          fit_status = mapped[[1]], error_class = mapped[[2]],
          n_zero = n_zero, rank_x_pos = rank_x_pos, recession = recession
        ))
      }
    }
    fitter <- if (auto_intercept) complete_fit else warm_fit
    fit <- fitter(y, start, fallback_starts)
    diagnostics <- diagnostic_template
    shared <- intersect(names(diagnostics), names(fit$diagnostics))
    diagnostics[shared] <- fit$diagnostics[shared]
    diagnostics["recession_certificate"] <- list(recession)
    if (identical(diagnostics$error_class, "no_accepted_start")) {
      diagnostics$error_class <- "nonconvergence"
    }
    fit$diagnostics <- diagnostics
    if (logvar_fit_ok(fit)) {
      rungs <- c(if (!is.null(start)) list(start) else list(), fallback_starts)
      if (auto_intercept) rungs <- c(rungs, list(c(log(mean(y)), rep(0, p - 1L))))
      winner <- length(fit$diagnostics$start_attempts)
      names(fit$warm_start) <- names(rungs[[winner]])
    }
    fit
  }
}

logvar_harvey_fit_response <- function(y, x_mat, start = NULL,
                                       fallback_starts = list(), auto_intercept = TRUE,
                                       control = LOGVAR_HARVEY_CONTROL) {
  fitter <- logvar_harvey_fitter(x_mat, control)
  fitter(y, start, fallback_starts, auto_intercept)
}

# The b wrapper: form the squared residual response, forward every argument
# and record the minimum absolute residual.
logvar_harvey_fit <- function(b, w1, w2, x_mat, start = NULL,
                              fallback_starts = list(),
                              auto_intercept = TRUE,
                              control = LOGVAR_HARVEY_CONTROL) {
  eps <- drop(w1 - w2 %*% b)
  fit <- logvar_harvey_fit_response(eps^2, x_mat,
    start = start,
    fallback_starts = fallback_starts,
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
