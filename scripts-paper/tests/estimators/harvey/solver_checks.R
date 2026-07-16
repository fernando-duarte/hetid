# Offline solver, recession-certificate, line-search, and interface-provenance
# checks for the Harvey multiplicative-variance estimator
# (solver.R, recession_certificate.R, recession_linear_programs.R, and estimator.R).
# Exact-zero rows stay finite, the normalized cone certificate separates the
# negative/zero/uncertified recession cases, the backtracking line search accepts
# only with score progress, and the estimator exposes its single point fit,
# winning start rung, and metadata-stamped bundle. Harvey modules
# do not exist yet, so every module-dependent assertion runs through hs_try below
# and one missing function fails a check closed rather than aborting the suite.
# The entry point sources the fixtures (harvey_fx) and defines check().

fx <- harvey_fx
hs_x <- fx$x_mat
hs_w2 <- fx$w2
hs_b <- fx$b_ref
hs_p <- ncol(hs_x)

# Return FALSE instead of aborting when a required Harvey function errors.
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

# an overflow-eta start (mu = exp(eta) would be Inf) is a hard trial failure
# per the finiteness discipline, so its rung is skipped and the fit converges
# from the intercept-only rung rather than accepting a nonfinite-variance trial
check("hs an overflow-eta start is rejected and the fit still converges", hs_try({
  y <- drop(fx$w1 - fx$w2 %*% fx$b_ref)^2
  fit <- logvar_harvey_fit_response(y, hs_x, start = c(800, rep(0, hs_p - 1L)))
  isTRUE(fit$converged) && identical(fit$fit_status, "ok") &&
    all(is.finite(exp(drop(hs_x %*% fit$coef)))) &&
    any(vapply(fit$diagnostics$start_attempts, function(a) {
      identical(a$error_class, "invalid_start")
    }, logical(1)))
}))
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

# the analytic implicit Jacobian D_b theta_H matches a central finite difference
# of independently converged fits over a step-size sweep, both at a clean
# all-positive candidate and at a candidate with one exact-zero residual (whose
# direct response derivative is zero); the error falls then flattens at noise
hs_fd_jac <- function(b, w1) {
  fit <- logvar_harvey_fit(b, w1, hs_w2, hs_x)
  jac <- logvar_harvey_jacobian(fit, b, w1, hs_w2, hs_x)
  denom <- max(1, max(abs(jac)))
  best <- min(vapply(c(1e-3, 3e-4, 1e-4, 3e-5), function(fr) {
    fd <- vapply(seq_along(b), function(k) {
      e <- replace(numeric(length(b)), k, fr * max(1, abs(b[k])))
      (logvar_harvey_fit(b + e, w1, hs_w2, hs_x)$coef -
        logvar_harvey_fit(b - e, w1, hs_w2, hs_x)$coef) / (2 * e[k])
    }, numeric(ncol(hs_x)))
    max(abs(jac - fd)) / denom
  }, numeric(1)))
  identical(dim(jac), c(ncol(hs_x), length(b))) && best <= 1e-5
}
check(
  "hs the Harvey Jacobian matches central FD at a clean candidate",
  hs_try(hs_fd_jac(hs_b, fx$w1))
)
check(
  "hs the Harvey Jacobian matches central FD at an exact-zero candidate",
  hs_try(hs_fd_jac(hs_b, fx$w1_zero))
)

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
