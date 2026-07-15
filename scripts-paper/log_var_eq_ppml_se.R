# Analytic (non-bootstrap) standard errors for the PPML (quasi-Poisson
# log-link) log-variance QMLE, reported under the two point columns of the
# log-variance panels: the OLS reference fit and the tau = 0 Lewbel-point fit.
# theta_hat solves X'(eps^2 - exp(X theta)) = 0, so every variance is a pure
# function of the accepted coefficient, the squared-residual response y, and the
# design X -- no fit object or response_scale is needed (the map is
# scale-invariant and the original-scale coef reproduces mu). Four variants are
# computed and stored; run_all.R's logvar_ppml_se_type picks which prints (the
# HAC default matches the log-OLS panel's Newey-West lag-4 inference):
#   naive  Pearson-dispersion-scaled model information  phi_hat * A^-1
#   hc0    Eicker-White sandwich                         A^-1 (X'diag(r^2)X) A^-1
#   hc1    hc0 with the n/(n-p) factor
#   hac    Newey-West Bartlett HAC of the score          A^-1 M_hac A^-1
# with A = X'diag(mu)X, r = eps^2 - mu. The moving-block bootstrap is deferred.
# The estimator-agnostic scaffolding (normalized inverse, HAC meat, SE frames,
# response reconstruction, attach diagnostic) lives in log_var_eq_se_utils.R;
# this module keeps only the PPML bread and variant assembly. Consumers validate
# a requested type with base match.arg(se_type, LOGVAR_PPML_SE_TYPES); the SEs
# are hand-rolled in base R rather than delegated to a reconstructed
# glm()/sandwich object -- a glm refit is a second optimization that can diverge
# from the exact coefficient the table prints and bypasses the estimator's
# fail-closed acceptance gate, and the four formulas are pinned to sandwich in
# the tests.

# canonical variant keys (the values run_all.R's logvar_ppml_se_type may take)
LOGVAR_PPML_SE_TYPES <- c("naive", "hc0", "hc1", "hac")

# QMLE covariance variants at an accepted fit. hac_lags is a programmer contract
# (loud on a malformed value). The bread A = X'diag(mu)X is inverted through the
# shared normalized conditioning gate (logvar_se_norm_inv); a NULL inverse (a
# non-finite, singular, or ill-conditioned bread) fails every variant closed to
# an all-NA matrix, exactly as the prologue does for a bad coefficient, response,
# or nonpositive mu. Returns a named list of p x p matrices keyed by
# LOGVAR_PPML_SE_TYPES.
logvar_ppml_vcov <- function(coef, y, x_mat, hac_lags) {
  stopifnot(
    is.matrix(x_mat),
    length(hac_lags) == 1L, is.finite(hac_lags), hac_lags >= 0
  )
  hac_lags <- as.integer(hac_lags)
  p <- ncol(x_mat)
  n <- nrow(x_mat)
  na_mat <- matrix(
    NA_real_, p, p,
    dimnames = list(colnames(x_mat), colnames(x_mat))
  )
  na_out <- stats::setNames(
    rep(list(na_mat), length(LOGVAR_PPML_SE_TYPES)), LOGVAR_PPML_SE_TYPES
  )
  if (n <= p || length(coef) != p || any(!is.finite(coef)) || !is.numeric(y) ||
    length(y) != n || any(!is.finite(y)) || any(y < 0)) {
    return(na_out)
  }
  mu <- exp(drop(x_mat %*% coef))
  if (any(!is.finite(mu)) || any(mu <= 0)) {
    return(na_out)
  }
  a_inv <- logvar_se_norm_inv(crossprod(x_mat, mu * x_mat)) # NULL -> variants NA
  r <- y - mu
  u <- x_mat * r # per-observation score rows
  meat_hc0 <- crossprod(u)
  phi <- sum(r^2 / mu) / (n - p) # Pearson dispersion
  sandwich_v <- function(meat) {
    if (is.null(a_inv)) na_mat else a_inv %*% meat %*% a_inv
  }
  v_hc0 <- sandwich_v(meat_hc0)
  list(
    naive = if (is.null(a_inv)) na_mat else phi * a_inv,
    hc0 = v_hc0,
    hc1 = (n / (n - p)) * v_hc0,
    hac = sandwich_v(logvar_se_bartlett_meat(u, hac_lags))
  )
}

# SE frames: thin wrappers over the shared builders (log_var_eq_se_utils.R) so
# the coefficient axis and variant columns match the stored vcov list.
logvar_ppml_se_frame <- function(coef, y, x_mat, hac_lags) {
  logvar_se_frame(logvar_ppml_vcov(coef, y, x_mat, hac_lags), colnames(x_mat))
}
logvar_ppml_se_na_frame <- function(coef_names) {
  logvar_se_na_frame(coef_names, LOGVAR_PPML_SE_TYPES)
}

# Assemble the SE frames for the two PPML point columns from the frozen pipeline
# objects (reference = OLS-residual fit, point = tau = 0 Lewbel fit); the shared
# reconstruction rebuilds each column's squared-residual response.
logvar_ppml_se_columns <- function(ppml, inputs, mean_eq, hac_lags) {
  logvar_se_columns(
    logvar_ppml_vcov, ppml$table, inputs, mean_eq, hac_lags, LOGVAR_PPML_SE_TYPES
  )
}

# attach the SE frames to the frozen PPML object (all variants; the table picks
# logvar_ppml_se_type at render time). Guarded so the offline test can source
# this module for definitions only. The diagnostic ratio contrasts the HAC and
# naive reference SEs (see logvar_se_report).
if (exists("log_var_eq_ppml")) {
  log_var_eq_ppml$se <- logvar_ppml_se_columns(
    log_var_eq_ppml, log_var_eq$inputs, set_id_mean_eq, logvar_ppml_se_hac_lags
  )
  logvar_se_report(
    log_var_eq_ppml$se, "PPML", LOGVAR_PPML_SE_TYPES,
    logvar_ppml_se_type, logvar_ppml_se_hac_lags, c("hac", "naive")
  )
}
