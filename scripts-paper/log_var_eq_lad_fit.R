# The median (LAD) log-variance inner fit: the b-indexed response map minimizing
# sum |z_t - x_t' coef|, z = 2 log|w1 - W2 b|, on the punctured domain, packaged
# in the Plan 7 fit contract. Holds the br response fit, the positive scale
# reference, the b wrapper with its exact/guarded domain split, the fn
# nonuniqueness probe, and the deterministic fn schedule. Inner solver is
# quantreg::rq.fit(method = "br") -- the br vertex is the selection rule, fn the
# probe, never the map. No residual floor, no log(pmax()), no epsilon in the log.
# quantreg is called lazily so the file sources without the package. Naming: b is
# the outer length-3 news coefficient, coef the inner length-5 LAD coefficient.

# Pinned probe tolerances, the fn interior-point step, and the numerical crossing
# guard, single-sourced here so the estimator spec_id can cite them verbatim.
logvar_lad_obj_rtol <- 1e-8
logvar_lad_coef_rtol <- 1e-4
logvar_lad_fn_eps <- 1e-6
logvar_lad_guard_ratio <- 1e-12

# Complete fit-result skeleton: every Plan 7 field is present so the ok,
# nonconvergence, and domain-failure branches share one shape (nonsmooth map).
logvar_lad_result <- function(coef, fit_status, converged, objective,
                              score_norm, convergence_code, warm_start,
                              diagnostics) {
  list(
    coef = coef, fit_status = fit_status, converged = converged,
    objective = objective, score_norm = score_norm,
    convergence_code = convergence_code, diagnostics = diagnostics,
    warm_start = warm_start
  )
}

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

# The br-selected response fit wrapped in the Plan 7 contract. Warnings and
# messages are captured (not silenced): the br "Solution may be nonunique" flag
# becomes the free first-pass multiple_solution_sensitive marker, and "Premature
# end" downgrades to nonconvergence. x_mat must already carry the intercept
# column -- rq.fit does not add one, unlike the rq() formula interface.
logvar_lad_fit_response <- function(z, x_mat) {
  warns <- character(0)
  msgs <- character(0)
  fit <- tryCatch(
    withCallingHandlers(
      quantreg::rq.fit(x = x_mat, y = z, tau = 0.5, method = "br"),
      warning = function(w) {
        warns[[length(warns) + 1L]] <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        msgs[[length(msgs) + 1L]] <<- conditionMessage(m)
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) {
      warns[[length(warns) + 1L]] <<- paste0("error: ", conditionMessage(e))
      NULL
    }
  )
  p <- ncol(x_mat)
  if (is.null(fit) || any(!is.finite(fit$coefficients))) {
    co <- stats::setNames(rep(NA_real_, p), colnames(x_mat))
    return(logvar_lad_result(
      coef = co, fit_status = "nonconvergence", converged = FALSE,
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
    coef = coef, fit_status = if (converged) "ok" else "nonconvergence",
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

# A finite, strictly positive, scale-equivariant residual reference. Any nonfinite
# e_ref fails construction. Otherwise median(|e_ref|); if zero, the median of the
# strictly positive |e_ref|; if still unavailable, max(|e_ref|); if no positive
# finite residual exists, fail closed rather than invent a scale.
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
logvar_lad_fit <- function(b, w1, w2, x_mat, e_scale_ref) {
  e <- drop(w1 - w2 %*% b)
  abs_e <- abs(e)
  min_abs <- min(abs_e)
  guard <- logvar_lad_guard_ratio * e_scale_ref
  fail_domain <- function(state, rows) {
    co <- stats::setNames(rep(NA_real_, ncol(x_mat)), colnames(x_mat))
    logvar_lad_result(
      coef = co, fit_status = "domain_failure", converged = FALSE,
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
  fit <- logvar_lad_fit_response(2 * log(abs_e), x_mat)
  fit$diagnostics$min_abs_eps <- min_abs
  fit$diagnostics$guard_ratio <- min_abs / e_scale_ref
  fit$diagnostics$domain_state <- "in_domain"
  fit
}

# The fn nonuniqueness probe: an independent Frisch-Newton refit at pinned
# tolerances. Objectives tie iff |obj_br - obj_fn| <= obj_rtol * max(1, |obj_br|);
# selections differ materially iff the max coefficient gap exceeds coef_rtol *
# max(1, max|coef_br|). Both flag sensitivity. Coefficients are never averaged:
# the br result stays the map, the fn fit is only evidence.
logvar_lad_nonunique_probe <- function(fit, z, x_mat) {
  coef_br <- fit$coef
  obj_br <- sum(abs(z - drop(x_mat %*% coef_br)))
  fn <- withCallingHandlers(
    quantreg::rq.fit(x = x_mat, y = z, tau = 0.5, method = "fn", eps = logvar_lad_fn_eps),
    warning = function(w) invokeRestart("muffleWarning")
  )
  coef_fn <- as.numeric(fn$coefficients)
  obj_fn <- sum(abs(z - drop(x_mat %*% coef_fn)))
  obj_tie <- abs(obj_br - obj_fn) <= logvar_lad_obj_rtol * max(1, abs(obj_br))
  coef_max_diff <- max(abs(as.numeric(coef_br) - coef_fn))
  coef_apart <- coef_max_diff > logvar_lad_coef_rtol * max(1, max(abs(coef_br)))
  list(
    objective_br = obj_br, objective_fn = obj_fn, coef_max_diff = coef_max_diff,
    multiple_solution_sensitive = obj_tie && coef_apart
  )
}

# The deterministic fn schedule: the always-probed point classes (benchmark,
# attained endpoints, promoted endpoint candidates) and, for a crossing path of n
# valid points, the entry / midpoint / tail indices. A br warning or coefficient
# jump escalates that path to full fn coverage. Consumed by the driver under the
# nonunique phase cap; no endpoint-relevant point is silently skipped.
logvar_lad_nonunique_schedule <- function() {
  list(
    always = c("benchmark", "endpoint", "promoted"),
    path_positions = function(n) {
      n <- as.integer(n)
      if (n < 1L) {
        return(integer(0))
      }
      unique(c(1L, as.integer(floor((n + 1L) / 2L)), n))
    },
    escalate_after = c("br_warning", "coef_jump"),
    phase = "nonunique"
  )
}
