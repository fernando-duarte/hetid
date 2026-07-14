# Offline checks for the analytic Harvey QMLE standard errors
# (scripts-paper/log_var_eq_harvey_se.R): the five covariance variants against
# their closed forms, the HAC-at-lag-0 collapse to the robust sandwich, a
# Bartlett HAC pinned to an independent brute-force double sum, the fail-closed
# guards, and the SE-frame shape. Mirrors test_logvar_ppml_se.R; reuses
# harvey_fx. The observed information itself is already checked against a
# finite-difference Hessian in logvar_harvey_math_checks.R.

hse_fx <- local({
  y <- harvey_fx$y
  x_mat <- harvey_fx$x_mat
  fit <- logvar_harvey_fit_response(y, x_mat)
  stopifnot(logvar_harvey_accepted(fit))
  coef <- fit$coef
  mu <- exp(drop(x_mat %*% coef))
  r <- y / mu
  g <- 0.5 * (1 - r) * x_mat
  h <- 0.5 * crossprod(x_mat, r * x_mat)
  list(y = y, x_mat = x_mat, coef = coef, g = g, h = h)
})
hse_d <- function(a, b) max(abs(unname(a) - unname(b)))

check("harvey se types are the five canonical keys", {
  identical(LOGVAR_HARVEY_SE_TYPES, c("expected", "observed", "opg", "robust", "hac")) &&
    identical(match.arg("hac", LOGVAR_HARVEY_SE_TYPES), "hac") &&
    tryCatch(
      {
        match.arg("bogus", LOGVAR_HARVEY_SE_TYPES)
        FALSE
      },
      error = function(e) TRUE
    )
})

check("harvey vcov variants match their closed forms", {
  vc <- logvar_harvey_vcov(hse_fx$coef, hse_fx$y, hse_fx$x_mat, 4L)
  h_inv <- solve(hse_fx$h)
  ok_exp <- hse_d(vc$expected, solve(0.5 * crossprod(hse_fx$x_mat))) < 1e-8
  ok_obs <- hse_d(vc$observed, h_inv) < 1e-8
  ok_opg <- hse_d(vc$opg, solve(crossprod(hse_fx$g))) < 1e-8
  ok_rob <- hse_d(vc$robust, h_inv %*% crossprod(hse_fx$g) %*% h_inv) < 1e-8
  ok_exp && ok_obs && ok_opg && ok_rob
})

check("harvey hac at lag 0 collapses to the robust sandwich", {
  v0 <- logvar_harvey_vcov(hse_fx$coef, hse_fx$y, hse_fx$x_mat, 0L)
  hse_d(v0$hac, v0$robust) < 1e-12
})

check("harvey hac meat matches an independent brute-force reference at lag 4", {
  h_inv <- solve(hse_fx$h)
  g <- hse_fx$g
  n <- nrow(g)
  lag <- 4L
  meat <- matrix(0, ncol(g), ncol(g))
  for (a in seq_len(n)) {
    for (b in seq_len(n)) {
      w <- max(0, 1 - abs(a - b) / (lag + 1))
      meat <- meat + w * tcrossprod(g[a, ], g[b, ])
    }
  }
  vc <- logvar_harvey_vcov(hse_fx$coef, hse_fx$y, hse_fx$x_mat, lag)
  hse_d(vc$hac, h_inv %*% meat %*% h_inv) < 1e-8
})

check("harvey vcov fails closed on non-finite coef", {
  bad <- logvar_harvey_vcov(
    rep(NA_real_, ncol(hse_fx$x_mat)), hse_fx$y, hse_fx$x_mat, 4L
  )
  all(vapply(bad, function(v) all(is.na(v)), logical(1)))
})

check("harvey vcov fails closed on a rank-deficient design", {
  xd <- cbind(hse_fx$x_mat, hse_fx$x_mat[, 2]) # duplicate column -> singular H
  v <- logvar_harvey_vcov(rep(0, ncol(xd)), hse_fx$y, xd, 4L)
  all(vapply(v, function(m) all(is.na(m)), logical(1)))
})

check("harvey vcov fails closed when n <= p", {
  xw <- hse_fx$x_mat[1:3, , drop = FALSE]
  v <- logvar_harvey_vcov(rep(0, ncol(xw)), hse_fx$y[1:3], xw, 4L)
  all(is.na(v$hac))
})

check("harvey vcov rejects a malformed hac_lags argument", {
  tryCatch(
    {
      logvar_harvey_vcov(hse_fx$coef, hse_fx$y, hse_fx$x_mat, -1L)
      FALSE
    },
    error = function(e) TRUE
  )
})

check("harvey se frame is one row per coef, one column per variant", {
  fr <- logvar_harvey_se_frame(hse_fx$coef, hse_fx$y, hse_fx$x_mat, 4L)
  identical(fr$coef, colnames(hse_fx$x_mat)) &&
    all(LOGVAR_HARVEY_SE_TYPES %in% names(fr)) &&
    all(fr$observed > 0) && nrow(fr) == ncol(hse_fx$x_mat)
})

check("harvey se na frame is all-NA with the coef axis", {
  fr <- logvar_harvey_se_na_frame(colnames(hse_fx$x_mat))
  identical(fr$coef, colnames(hse_fx$x_mat)) &&
    all(vapply(fr[LOGVAR_HARVEY_SE_TYPES], function(c) all(is.na(c)), logical(1)))
})

rm(hse_fx, hse_d)
