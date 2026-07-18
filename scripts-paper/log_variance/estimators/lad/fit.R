# The median (LAD) log-variance inner fit: the b-indexed response map minimizing
# sum |z_t - x_t' coef|, z = 2 log|w1 - W2 b|, on the punctured domain, under the
# estimator-engine fit contract. Holds the br response fit, the scale reference, the b
# wrapper with its exact/guarded domain split, the fn nonuniqueness probe, and the
# fn schedule. Inner solver quantreg::rq.fit(method = "br") -- the br vertex is the
# selection rule, fn the probe, never the map. No residual floor, no log(pmax()), no
# epsilon in the log; quantreg is called lazily. Naming: b the outer length-3 news
# coef, coef the inner length-5 LAD one.

# Complete fit-result skeleton: every estimator-engine field is present so the ok,
# nonconvergence, and domain-failure branches share one shape (nonsmooth map).
logvar_lad_result <- new_logvar_fit_result

# Slim diagnostics container with NA / empty defaults; callers override only the
# fields they can populate, keeping every fail-closed return field-compatible.
logvar_lad_diag <- function(...) {
  base <- list(
    warnings = character(0), messages = character(0), error_class = NA_character_,
    solver_flag = NA_integer_, multiple_solution_sensitive = FALSE,
    n_resid_pos = NA_integer_, n_resid_neg = NA_integer_, n_resid_zero = NA_integer_,
    min_abs_eps = NA_real_, domain_state = NA_character_, guard_ratio = NA_real_,
    guard_rows = integer(0)
  )
  utils::modifyList(base, list(...))
}

# The br-selected response fit wrapped in the estimator-engine contract. Warnings and messages
# are captured (not silenced): the br "Solution may be nonunique" flag becomes the
# free first-pass multiple_solution_sensitive marker, "Premature end" downgrades to
# nonconvergence. x_mat must already carry the intercept column (rq.fit adds none).
logvar_lad_fit_response <- function(
  z,
  x_mat,
  control = LOGVAR_LAD_CONTROL
) {
  captured <- paper_capture_conditions(
    quantreg::rq.fit(
      x = x_mat,
      y = z,
      tau = control$quantile,
      method = control$primary_method
    )
  )
  fit <- captured$value
  warns <- captured$warnings
  if (!is.na(captured$error_message)) {
    warns <- c(warns, captured$error_message)
  }
  msgs <- captured$messages
  p <- ncol(x_mat)
  if (is.null(fit) || any(!is.finite(fit$coefficients))) {
    co <- stats::setNames(rep(NA_real_, p), colnames(x_mat))
    return(logvar_lad_result(
      coef = co, fit_status = LOGVAR_FIT_STATUS[["nonconvergence"]], converged = FALSE,
      objective = NA_real_, score_norm = NA_real_, convergence_code = NA_integer_,
      warm_start = NULL,
      diagnostics = logvar_lad_diag(
        error_class = "fit_error", warnings = warns, messages = msgs
      )
    ))
  }
  coef <- stats::setNames(as.numeric(fit$coefficients), colnames(x_mat))
  resid <- drop(z - drop(x_mat %*% coef))
  solver_flag <- 0L
  if (any(grepl("nonunique", warns, fixed = TRUE))) solver_flag <- 1L
  if (any(grepl("Premature end", warns, fixed = TRUE))) solver_flag <- 2L
  converged <- solver_flag != 2L
  logvar_lad_result(
    coef = coef,
    fit_status = if (converged) {
      LOGVAR_FIT_STATUS[["ok"]]
    } else {
      LOGVAR_FIT_STATUS[["nonconvergence"]]
    },
    converged = converged, objective = sum(abs(resid)), score_norm = NA_real_,
    convergence_code = solver_flag, warm_start = NULL,
    diagnostics = logvar_lad_diag(
      warnings = warns, messages = msgs, solver_flag = solver_flag,
      multiple_solution_sensitive = solver_flag == 1L,
      n_resid_pos = sum(resid > 0), n_resid_neg = sum(resid < 0),
      n_resid_zero = sum(resid == 0)
    )
  )
}

# A finite, strictly positive, scale-equivariant residual reference. Nonfinite
# e_ref fails construction. Otherwise median(|e_ref|); if zero, the median of the
# positive |e_ref|; else max(|e_ref|); if no positive finite residual, fail closed.
logvar_lad_scale_reference <- function(e_ref) {
  if (!is.numeric(e_ref) || length(e_ref) == 0L || any(!is.finite(e_ref))) {
    stop("logvar_lad_scale_reference: e_ref must be finite and non-empty")
  }
  a <- abs(e_ref)
  s <- stats::median(a)
  if (!(is.finite(s) && s > 0)) {
    pos <- a[a > 0]
    if (length(pos) > 0L) s <- stats::median(pos)
  }
  if (!(is.finite(s) && s > 0)) s <- max(a)
  if (!(is.finite(s) && s > 0)) {
    stop("logvar_lad_scale_reference: no positive finite residual for the scale")
  }
  s
}

# The b wrapper: form e = w1 - W2 b and z = 2 log|e|, then delegate. The domain is
# punctured, so an exact zero residual is mathematically unavailable
# (exact_domain_failure) while a guarded near-zero (0 < |e_i| <= guard * scale) is
# in-domain but refused at this resolution (numerically_unresolved_near_crossing).
# Both return named NA coefficients and the implicated rows; nothing is clamped.
logvar_lad_fit <- function(
  b,
  w1,
  w2,
  x_mat,
  e_scale_ref,
  control = LOGVAR_LAD_CONTROL
) {
  e <- drop(w1 - w2 %*% b)
  abs_e <- abs(e)
  min_abs <- min(abs_e)
  guard <- control$guard_ratio * e_scale_ref
  fail_domain <- function(state, rows) {
    co <- stats::setNames(rep(NA_real_, ncol(x_mat)), colnames(x_mat))
    logvar_lad_result(
      coef = co, fit_status = LOGVAR_FIT_STATUS[["domain_failure"]], converged = FALSE,
      objective = NA_real_, score_norm = NA_real_, convergence_code = NA_integer_,
      warm_start = NULL,
      diagnostics = logvar_lad_diag(
        error_class = state, domain_state = state, min_abs_eps = min_abs,
        guard_ratio = min_abs / e_scale_ref, guard_rows = rows
      )
    )
  }
  zero_rows <- which(e == 0)
  if (length(zero_rows) > 0L) {
    return(fail_domain("exact_domain_failure", zero_rows))
  }
  near_rows <- which(abs_e <= guard)
  if (length(near_rows) > 0L) {
    return(fail_domain("numerically_unresolved_near_crossing", near_rows))
  }
  fit <- logvar_lad_fit_response(
    2 * log(abs_e),
    x_mat,
    control
  )
  fit$diagnostics$min_abs_eps <- min_abs
  fit$diagnostics$guard_ratio <- min_abs / e_scale_ref
  fit$diagnostics$domain_state <- "in_domain"
  fit
}

# The fn nonuniqueness probe: an independent Frisch-Newton refit at pinned
# tolerances. Objectives tie iff |obj_br - obj_fn| <= obj_rtol * max(1, |obj_br|);
# selections differ materially iff the max coefficient gap exceeds coef_rtol *
# max(1, max|coef_br|). Both flag sensitivity. Coefficients are never averaged: br
# stays the map, fn is only evidence. fn warnings are muffled but CAPTURED into
# fn_warnings (never discarded) so a warning with close coefficients still surfaces.
logvar_lad_nonunique_probe <- function(
  fit,
  z,
  x_mat,
  control = LOGVAR_LAD_CONTROL
) {
  coef_br <- fit$coef
  obj_br <- sum(abs(z - drop(x_mat %*% coef_br)))
  captured <- paper_capture_conditions(
    quantreg::rq.fit(
      x = x_mat,
      y = z,
      tau = control$quantile,
      method = control$nonunique_method,
      eps = control$fn_epsilon
    )
  )
  if (!is.null(captured$error)) stop(captured$error)
  fn <- captured$value
  fn_warnings <- captured$warnings
  coef_fn <- as.numeric(fn$coefficients)
  obj_fn <- sum(abs(z - drop(x_mat %*% coef_fn)))
  obj_tie <- abs(obj_br - obj_fn) <=
    control$objective_rtol * max(1, abs(obj_br))
  coef_max_diff <- max(abs(as.numeric(coef_br) - coef_fn))
  coef_apart <- coef_max_diff >
    control$coefficient_rtol * max(1, max(abs(coef_br)))
  list(
    objective_br = obj_br, objective_fn = obj_fn, coef_max_diff = coef_max_diff,
    fn_warnings = fn_warnings, multiple_solution_sensitive = obj_tie && coef_apart
  )
}
