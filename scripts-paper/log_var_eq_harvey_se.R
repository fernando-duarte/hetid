# Analytic (non-bootstrap) standard errors for the Harvey Gaussian
# multiplicative-variance log-variance QMLE, reported under the two point columns
# of the Harvey table: the OLS-residual reference fit and the tau = 0 Lewbel
# point fit. theta_hat maximizes -0.5 sum(eta + y exp(-eta)), so every variance
# is a pure function of the accepted coefficient, the squared-residual response
# y, and the design X (no fit object needed; the map reproduces mu = exp(X
# theta)). Five variants are computed and stored; run_all.R's
# logvar_harvey_se_type picks which prints (the HAC default matches the PPML and
# log-OLS panels' Newey-West lag-4 inference):
#   expected  0.5 X'X inverse           Gaussian working-model Fisher information
#   observed  0.5 X'diag(r)X inverse    Gaussian working-model observed info
#   opg       (G'G) inverse             outer-product-of-gradients (BHHH)
#   robust    H^-1 (G'G) H^-1           Eicker-White QMLE sandwich
#   hac       H^-1 M_hac H^-1           Newey-West Bartlett HAC of the score
# with r = y/mu, G_i = 0.5(1 - r_i) x_i, H = 0.5 X'diag(r)X. The moving-block
# bootstrap is deferred. The estimator-agnostic scaffolding (normalized inverse,
# HAC meat, SE frames, response reconstruction, attach diagnostic) lives in
# log_var_eq_se_utils.R; this module keeps only the Harvey breads and variant
# assembly. Consumers validate a requested type with base match.arg(se_type,
# LOGVAR_HARVEY_SE_TYPES); hand-rolled in base R because the Harvey QMLE is not a
# glm() object sandwich can dispatch on.

# canonical variant keys (the values run_all.R's logvar_harvey_se_type may take)
LOGVAR_HARVEY_SE_TYPES <- c("expected", "observed", "opg", "robust", "hac")

# QMLE covariance variants at an accepted fit. hac_lags is a programmer contract
# (loud on a malformed value). Each variant fails closed to an all-NA matrix on a
# conditioning problem in its bread: a non-finite coefficient, invalid response,
# nonpositive mu, n <= p, or a bread the shared normalized gate
# (logvar_se_norm_inv) rejects. Returns a named list of p x p matrices keyed by
# LOGVAR_HARVEY_SE_TYPES.
logvar_harvey_vcov <- function(coef, y, x_mat, hac_lags) {
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
    rep(list(na_mat), length(LOGVAR_HARVEY_SE_TYPES)), LOGVAR_HARVEY_SE_TYPES
  )
  if (n <= p || length(coef) != p || any(!is.finite(coef)) || !is.numeric(y) ||
    length(y) != n || any(!is.finite(y)) || any(y < 0)) {
    return(na_out)
  }
  mu <- exp(drop(x_mat %*% coef))
  if (any(!is.finite(mu)) || any(mu <= 0)) {
    return(na_out)
  }
  r <- y / mu # zero-safe: y >= 0, mu > 0 (a zero response gives r = 0)
  g <- 0.5 * (1 - r) * x_mat # per-observation score rows
  h_inv <- logvar_se_norm_inv(0.5 * crossprod(x_mat, r * x_mat)) # observed info
  ex_inv <- logvar_se_norm_inv(0.5 * crossprod(x_mat)) # expected information
  meat_opg <- crossprod(g)
  opg_inv <- logvar_se_norm_inv(meat_opg)
  sandwich_v <- function(bread, meat) {
    if (is.null(bread)) na_mat else bread %*% meat %*% bread
  }
  list(
    expected = if (is.null(ex_inv)) na_mat else ex_inv,
    observed = if (is.null(h_inv)) na_mat else h_inv,
    opg = if (is.null(opg_inv)) na_mat else opg_inv,
    robust = sandwich_v(h_inv, meat_opg),
    hac = sandwich_v(h_inv, logvar_se_bartlett_meat(g, hac_lags))
  )
}

# SE frames: thin wrappers over the shared builders (log_var_eq_se_utils.R) so
# the coefficient axis and variant columns match the stored vcov list.
logvar_harvey_se_frame <- function(coef, y, x_mat, hac_lags) {
  logvar_se_frame(logvar_harvey_vcov(coef, y, x_mat, hac_lags), colnames(x_mat))
}
logvar_harvey_se_na_frame <- function(coef_names) {
  logvar_se_na_frame(coef_names, LOGVAR_HARVEY_SE_TYPES)
}

# Assemble the SE frames for the two Harvey point columns from the frozen
# pipeline objects (reference = OLS-residual fit, point = tau = 0 Lewbel fit);
# the shared reconstruction rebuilds each column's squared-residual response.
logvar_harvey_se_columns <- function(harvey, inputs, mean_eq, hac_lags) {
  logvar_se_columns(
    logvar_harvey_vcov, harvey$table, inputs, mean_eq, hac_lags,
    LOGVAR_HARVEY_SE_TYPES
  )
}

# attach the SE frames to the frozen Harvey object (all variants; the table picks
# logvar_harvey_se_type at render time). Guarded so the offline test can source
# this module for definitions only. The diagnostic ratio contrasts the HAC and
# observed reference SEs (see logvar_se_report).
if (exists("log_var_eq_harvey")) {
  log_var_eq_harvey$se <- logvar_harvey_se_columns(
    log_var_eq_harvey, log_var_eq$inputs, set_id_mean_eq, logvar_harvey_se_hac_lags
  )
  logvar_se_report(
    log_var_eq_harvey$se, "Harvey", LOGVAR_HARVEY_SE_TYPES,
    logvar_harvey_se_type, logvar_harvey_se_hac_lags, c("hac", "observed")
  )
}
