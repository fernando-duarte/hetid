# Offline algebraic, zero/existence, and normalization checks for the PPML
# log-variance response fit (scripts-paper/log_variance/estimators/ppml/fit.R) and its b
# estimator wrapper. Poisson and quasi-Poisson agree on
# positive responses; a fixed response scale only shifts the intercept by log s
# while warm_start stays on the scaled-fit scale; the per-coordinate scaled
# score holds and survives a regressor-column rescale; the analytic implicit
# Jacobian matches a central-difference sweep (including an exact-zero
# candidate); exact-zero and separated designs are handled fail-closed; and the
# Gaussian-scale intercept identity holds. PPML modules
# exist yet, so every fit-dependent assertion is guarded by safe() below so one
# missing function cannot abort the suite. The entry point sources the fixtures
# and defines check(); the identity/spec_id block lives in specification_checks.R.

fx <- ppml_fx
w1 <- fx$w1
w2 <- fx$w2
x_mat <- fx$x_mat
b_ref <- fx$b_ref
y <- fx$y
s <- fx$scale_s

# Return FALSE instead of aborting when a required PPML function errors.
safe <- .test$safe

# Central-difference Jacobian from independently converged cold-start fits at
# b +/- h e_k, with the per-coordinate relative step frac * max(1, |b[k]|).
fd_jac <- function(b, w1, w2, x, scale, frac) {
  jj <- matrix(0, ncol(x), length(b))
  for (col in seq_along(b)) {
    h <- frac * max(1, abs(b[col]))
    bp <- b
    bm <- b
    bp[col] <- bp[col] + h
    bm[col] <- bm[col] - h
    fp <- logvar_ppml_fit(bp, w1, w2, x, response_scale = scale)
    fm <- logvar_ppml_fit(bm, w1, w2, x, response_scale = scale)
    jj[, col] <- (fp$coef - fm$coef) / (2 * h)
  }
  jj
}

# Minimum over the step-size sweep of the scale-relative max error: the analytic
# Jacobian error should fall, then flatten at floating-point noise.
jac_min_rel_err <- function(b, w1, w2, x, scale) {
  fit <- logvar_ppml_fit(b, w1, w2, x, response_scale = scale)
  an <- logvar_ppml_jacobian(fit, b, w1, w2, x, response_scale = scale)
  denom <- max(1, max(abs(an)))
  fracs <- c(1e-3, 3e-4, 1e-4, 3e-5, 1e-5, 3e-6)
  min(vapply(fracs, function(fr) {
    max(abs(an - fd_jac(b, w1, w2, x, scale, fr))) / denom
  }, numeric(1)))
}

# Algebraic block ------------------------------------------------------------

check("poisson and quasipoisson coefficients agree", safe({
  fit <- logvar_ppml_fit_response(y, x_mat)
  oracle <- stats::glm.fit(
    x_mat, y,
    family = stats::poisson(link = "log"),
    control = stats::glm.control(maxit = 100L, epsilon = 1e-10)
  )
  max(abs(unname(fit$coef) - unname(oracle$coefficients))) < 1e-8
}))

check("a fixed response scale recovers identical coefficients", safe({
  f1 <- logvar_ppml_fit_response(y, x_mat)
  f2 <- logvar_ppml_fit_response(y, x_mat, response_scale = s)
  max(abs(f1$coef - f2$coef)) < 1e-8
}))

check("scaling the response shifts only the intercept by log(s)", safe({
  f1 <- logvar_ppml_fit_response(y, x_mat)
  f3 <- logvar_ppml_fit_response(y * s, x_mat)
  abs((f3$coef[1] - f1$coef[1]) - log(s)) < 1e-6 &&
    max(abs(f3$coef[-1] - f1$coef[-1])) < 1e-6
}))

check("warm_start stays on the scaled-fit scale", safe({
  f2 <- logvar_ppml_fit_response(y, x_mat, response_scale = s)
  abs((f2$coef[1] - f2$warm_start[1]) - log(s)) < 1e-8 &&
    max(abs(f2$coef[-1] - f2$warm_start[-1])) < 1e-10
}))

check("a warm restart from warm_start converges in <= 2 iterations", safe({
  f2 <- logvar_ppml_fit_response(y, x_mat, response_scale = s)
  f2b <- logvar_ppml_fit_response(y, x_mat, start = f2$warm_start, response_scale = s)
  isTRUE(f2b$converged) && f2b$convergence_code <= 2L
}))

check("the b wrapper matches the direct response path at a non-unit scale", safe({
  fw <- logvar_ppml_fit(b_ref, w1, w2, x_mat, response_scale = s)
  yv <- drop(w1 - w2 %*% b_ref)^2
  fr <- logvar_ppml_fit_response(yv, x_mat, response_scale = s)
  max(abs(fw$coef - fr$coef)) < 1e-10
}))

check("the score at the solution passes the per-coordinate tolerance", safe({
  fit <- logvar_ppml_fit_response(y, x_mat)
  sc <- logvar_ppml_score(fit$warm_start, y, x_mat)
  bound <- max(1, stats::median(y[y > 0])) * colSums(abs(x_mat))
  max(abs(sc) / bound) <= 1e-8
}))

check("rescaling a regressor column preserves acceptance and coefficients", safe({
  fit <- logvar_ppml_fit_response(y, x_mat)
  x_big <- x_mat
  x_big[, 2] <- x_big[, 2] * 1e8
  fb <- logvar_ppml_fit_response(y, x_big)
  isTRUE(fb$converged) &&
    abs(fb$coef[2] * 1e8 - fit$coef[2]) < 1e-5 * max(1, abs(fit$coef[2])) &&
    max(abs(fb$coef[-2] - fit$coef[-2])) < 1e-5
}))

check("the Jacobian recovers original coordinates after undoing units", safe({
  fw <- logvar_ppml_fit(b_ref, w1, w2, x_mat)
  jac <- logvar_ppml_jacobian(fw, b_ref, w1, w2, x_mat)
  x_big <- x_mat
  x_big[, 2] <- x_big[, 2] * 1e8
  fw_b <- logvar_ppml_fit(b_ref, w1, w2, x_big)
  jac_b <- logvar_ppml_jacobian(fw_b, b_ref, w1, w2, x_big)
  jac_b[2, ] <- jac_b[2, ] * 1e8
  max(abs(jac_b - jac)) <= 1e-5 * max(1, max(abs(jac)))
}))

check(
  "analytic Jacobian matches central FD at response_scale = 1",
  safe(jac_min_rel_err(b_ref, w1, w2, x_mat, 1) <= 1e-5)
)

check(
  "analytic Jacobian matches central FD at a non-unit response scale",
  safe(jac_min_rel_err(b_ref, w1, w2, x_mat, s) <= 1e-5)
)

check(
  "analytic Jacobian matches central FD at an exact-zero candidate",
  safe(jac_min_rel_err(fx$b_zero, fx$w1_zero, w2, x_mat, 1) <= 1e-5)
)

check("the dimension-generic design returns a 3 x K Jacobian", safe({
  fit3 <- logvar_ppml_fit(b_ref, w1, w2, fx$x3)
  jac3 <- logvar_ppml_jacobian(fit3, b_ref, w1, w2, fx$x3)
  isTRUE(fit3$converged) && identical(dim(jac3), c(3L, ncol(w2))) &&
    all(is.finite(jac3))
}))

# Zero and existence block ---------------------------------------------------

check("one exact zero residual yields an ok fit and finite Jacobian", safe({
  fz <- logvar_ppml_fit(fx$b_zero, fx$w1_zero, w2, x_mat)
  jz <- logvar_ppml_jacobian(fz, fx$b_zero, fx$w1_zero, w2, x_mat)
  identical(fz$fit_status, "ok") && all(is.finite(fz$coef)) &&
    !is.null(jz) && all(is.finite(jz))
}))

check("several exact zeros with full-rank positive rows stay finite", safe({
  fzm <- logvar_ppml_fit(fx$b_zero, fx$w1_zeros_multi, w2, x_mat)
  identical(fzm$fit_status, "ok") && all(is.finite(fzm$coef))
}))

check("rank-deficient positive rows report rank_unresolved nonconvergence", safe({
  frd <- logvar_ppml_fit_response(fx$y_rankdef, fx$x_rankdef)
  identical(frd$fit_status, "nonconvergence") &&
    identical(frd$diagnostics$error_class, "rank_unresolved") &&
    !isTRUE(frd$converged)
}))

check("all-zero responses fail closed", safe({
  faz <- logvar_ppml_fit_response(rep(0, fx$n), x_mat)
  !identical(faz$fit_status, "ok") && !isTRUE(faz$converged)
}))

check("every accepted fit reports a converged, score-tolerant solution", safe({
  fit <- logvar_ppml_fit_response(y, x_mat)
  fw <- logvar_ppml_fit(b_ref, w1, w2, x_mat)
  isTRUE(fit$converged) && fit$score_norm <= 1e-8 &&
    isTRUE(fw$converged) && fw$score_norm <= 1e-8
}))

# Normalization block --------------------------------------------------------

check("PPML minus log-OLS intercept matches the Gaussian identity", safe({
  set.seed(20260713L)
  n_big <- 100000L
  rr <- scale(matrix(rnorm(n_big * 4L), n_big, 4L), center = TRUE, scale = FALSE)
  log_h <- -0.4 + drop(rr %*% c(0.25, -0.15, 0.10, 0.20))
  e <- sqrt(exp(log_h)) * rnorm(n_big)
  xg <- cbind(1, rr)
  ppml <- logvar_ppml_fit_response(e^2, xg)
  ols <- stats::lm.fit(xg, log(e^2))$coefficients
  gap <- unname(ppml$coef[1]) - unname(ols[1])
  slope <- unname(ppml$coef[-1]) - unname(ols[-1])
  abs(gap - 1.270362845) <= 0.04 && max(abs(slope)) <= 0.03
}))
