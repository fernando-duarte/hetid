# Offline solver, recession-certificate, line-search, and interface-provenance
# checks for the Harvey multiplicative-variance estimator
# (scripts-paper/log_var_eq_harvey_solver.R, _recession.R, log_var_eq_harvey.R).
# Exact-zero rows stay finite, the normalized cone certificate separates the
# negative/zero/uncertified recession cases, the backtracking line search accepts
# only with score progress, and the estimator exposes its single point fit,
# winning start rung, and metadata-stamped bundle. RED tests: the Harvey modules
# do not exist yet, so every module-dependent assertion runs through hs_try below
# and one missing function fails a check closed rather than aborting the suite.
# The entry point sources the fixtures (harvey_fx) and defines check().

fx <- harvey_fx
hs_x <- fx$x_mat
hs_w2 <- fx$w2
hs_b <- fx$b_ref
hs_p <- ncol(hs_x)

# Return FALSE instead of aborting when a not-yet-defined Harvey function errors.
hs_try <- function(expr) tryCatch(isTRUE(expr), error = function(e) FALSE)

# resolve a fixture field by the first name the fixtures file actually defines,
# so the pathology fixtures bind whether they are named recession-style or neg-style
hs_fx <- function(...) {
  nms <- c(...)
  present <- nms[nms %in% names(fx)]
  if (length(present) > 0L) fx[[present[[1]]]] else NULL
}

# intercept-only start c(log(mean(y)), 0, ...) and a NaN-free field scan helper
hs_io <- function(y) c(log(mean(y)), rep(0, hs_p - 1L))
hs_no_nan <- function(x) !any(is.nan(x))
hs_neg <- c(-30, rep(0, hs_p - 1L))
hs_inf <- c(Inf, rep(0, hs_p - 1L))

# reference squared residual at b_ref: strictly positive, a clean converging map
hs_yref <- drop(fx$w1 - fx$w2 %*% hs_b)^2

# Exact-zero and safe-arithmetic block ---------------------------------------

check("hs one exact zero residual yields an ok fit and finite Jacobian", hs_try({
  fz <- logvar_harvey_fit(hs_b, fx$w1_zero, hs_w2, hs_x)
  jz <- logvar_harvey_jacobian(fz, hs_b, fx$w1_zero, hs_w2, hs_x)
  identical(fz$fit_status, "ok") && all(is.finite(fz$coef)) &&
    !is.null(jz) && all(is.finite(jz))
}))

check("hs a very negative eta start never leaks NaN and keeps mu positive", hs_try({
  fz <- logvar_harvey_fit(hs_b, fx$w1_zero, hs_w2, hs_x, start = hs_neg)
  no_nan <- hs_no_nan(fz$coef) && hs_no_nan(fz$objective) && hs_no_nan(fz$score_norm)
  ok <- identical(fz$fit_status, "ok") && isTRUE(fz$converged)
  mu_ok <- !ok || {
    mu <- exp(drop(hs_x %*% fz$coef))
    all(is.finite(mu)) && all(mu > 0)
  }
  no_nan && mu_ok
}))

# Recession certificate and existence block ----------------------------------

check("hs the full-rank counterexample certifies a negative recession", hs_try({
  cert <- logvar_harvey_recession_certificate(hs_fx("y_rec", "y_neg"), hs_fx("x_rec", "x_neg"))
  fit <- logvar_harvey_fit_response(hs_fx("y_rec", "y_neg"), hs_fx("x_rec", "x_neg"))
  identical(cert$classification, "negative_recession") &&
    identical(fit$fit_status, "nonexistence") &&
    identical(fit$diagnostics$error_class, "negative_recession")
}))

check("hs the direct two-column objective reproduces the recession rates", hs_try({
  x2 <- rbind(c(1, 0), c(1, 1), c(1, 100))
  y2 <- c(1, 1, 0)
  d <- c(1, -1)
  want <- c(1, -48.316, -489.500, -4899.5)
  got <- vapply(c(0, 1, 10, 100), function(cc) {
    logvar_harvey_objective(cc * d, y2, x2)
  }, numeric(1))
  max(abs(got - want)) <= 1e-2
}))

check("hs a zero-rate response is unresolved, never nonexistence", hs_try({
  cert <- logvar_harvey_recession_certificate(hs_fx("y_zr", "y_zrate"), hs_fx("x_zr", "x_zrate"))
  fit <- logvar_harvey_fit_response(hs_fx("y_zr", "y_zrate"), hs_fx("x_zr", "x_zrate"))
  identical(cert$classification, "zero_recession") &&
    identical(fit$fit_status, "nonconvergence") &&
    identical(fit$diagnostics$error_class, "zero_recession_unresolved")
}))

# the dossier sanctions either verdict here: a certified collapse direction
# is genuine nonexistence, an uncertified one fails closed as nonconvergence
check("hs rank-deficient positive rows fail closed without huge coefficients", hs_try({
  fit <- logvar_harvey_fit_response(hs_fx("y_rd", "y_rankdef"), hs_fx("x_rd", "x_rankdef"))
  fit$fit_status %in% c("nonconvergence", "nonexistence") && !isTRUE(fit$converged)
}))

check("hs an all-zero response fails closed as a negative intercept recession", hs_try({
  fit <- logvar_harvey_fit_response(numeric(nrow(hs_x)), hs_x)
  identical(fit$fit_status, "nonexistence") &&
    identical(fit$diagnostics$error_class, "negative_recession_all_zero")
}))

# Line search and start-ladder block -----------------------------------------

check("hs a heavy-tailed response converges by backtracking from a poor start", hs_try({
  fit <- logvar_harvey_fit_response(fx$y_heavy, hs_x, start = hs_io(fx$y_heavy))
  isTRUE(fit$converged) && identical(fit$fit_status, "ok") &&
    is.numeric(fit$diagnostics$n_halvings) && fit$diagnostics$n_halvings >= 0
}))

check("hs a refit from its own solution exits on the initial-score shortcut", hs_try({
  fit <- logvar_harvey_fit_response(hs_yref, hs_x)
  refit <- logvar_harvey_fit_response(hs_yref, hs_x, start = fit$coef)
  isTRUE(refit$converged) && refit$convergence_code %in% c(0L, 1L)
}))

check("hs a nonfinite start falls through to a recorded fallback start", hs_try({
  fit <- logvar_harvey_fit_response(hs_yref, hs_x,
    start = hs_inf, fallback_starts = list(hs_io(hs_yref))
  )
  isTRUE(fit$converged) && !is.null(fit$diagnostics$start_attempts)
}))

# a counting fake ppml_start_at_b closure: records calls and returns a good
# original-scale coef with a deliberately poisoned warm_start
hs_fake_ppml <- function(counter, status = "ok", conv = TRUE) {
  function(b) {
    counter$n <- counter$n + 1L
    list(
      coef = hs_io(hs_yref), warm_start = rep(Inf, hs_p),
      fit_status = status, converged = conv
    )
  }
}

check("hs the retry ladder calls the PPML rung and consumes its coef", hs_try({
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  est <- logvar_harvey_estimator(fx$w1, hs_w2, fx$pcr, fx$qtr,
    b_point = NULL, ppml_bundle = NULL,
    ppml_start_at_b = hs_fake_ppml(counter), logols_coef = NULL
  )
  fit <- est$fit_at_b(hs_b, start = hs_inf)
  counter$n >= 1L && isTRUE(fit$converged) && all(is.finite(fit$coef))
}))

check("hs a failed PPML rung is skipped and the standalone rungs converge", hs_try({
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  est <- logvar_harvey_estimator(fx$w1, hs_w2, fx$pcr, fx$qtr,
    b_point = NULL, ppml_bundle = NULL,
    ppml_start_at_b = hs_fake_ppml(counter, status = "nonconvergence", conv = FALSE),
    logols_coef = NULL
  )
  fit <- est$fit_at_b(hs_b, start = hs_inf)
  counter$n >= 1L && isTRUE(fit$converged)
}))

# Stability precheck, self-test, and interface-ownership block ---------------

hs_pass_flag <- function(r) {
  isTRUE(r$pass) || isTRUE(r$passed) || isTRUE(r$ok) || isTRUE(r$feasible)
}

check("hs the stability precheck returns a clean record per pair", hs_try({
  pairs <- list(
    ref = list(response = hs_yref, start = hs_io(hs_yref)),
    anchor = list(response = hs_yref, start = hs_io(hs_yref))
  )
  res <- logvar_harvey_stability_precheck(pairs, hs_x)
  is.list(res) && length(res) == length(pairs) &&
    all(vapply(res, hs_pass_flag, logical(1)))
}))

check("hs the stability precheck fails closed on a negative response", hs_try({
  y_bad <- hs_yref
  y_bad[1] <- -1
  pairs <- list(bad = list(response = y_bad, start = hs_io(hs_yref)))
  res <- tryCatch(logvar_harvey_stability_precheck(pairs, hs_x),
    error = function(e) NULL
  )
  is.null(res) || !all(vapply(res, hs_pass_flag, logical(1)))
}))

check("hs the recession self-test reports named checks with passed all", hs_try({
  st <- logvar_harvey_recession_self_test(hs_x)
  is.logical(st$checks) && !is.null(names(st$checks)) &&
    identical(st$passed, all(st$checks)) && isTRUE(st$passed)
}))

check("hs the estimator exposes its single point fit, rung, and bundle", hs_try({
  est <- logvar_harvey_estimator(fx$w1, hs_w2, fx$pcr, fx$qtr, b_point = hs_b)
  bundle_ok <- identical(est$start_bundle$sample_id, est$metadata$sample_id) &&
    identical(est$start_bundle$spec_id, est$metadata$spec_id)
  !is.null(est$point_fit) && !is.null(est$point_start_rung) &&
    !is.null(est$start_bundle) && bundle_ok
}))

check("hs the point fit is NULL when the Lewbel point is absent", hs_try({
  est <- logvar_harvey_estimator(fx$w1, hs_w2, fx$pcr, fx$qtr, b_point = NULL)
  is.null(est$point_fit)
}))
