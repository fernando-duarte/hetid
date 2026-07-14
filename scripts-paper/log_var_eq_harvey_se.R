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
# bootstrap is deferred. Mirrors log_var_eq_ppml_se.R: definitions plus a guarded
# attach driver; sourced by log_var_eq_harvey_run.R after the sets driver and
# before the table builder. Consumers validate a requested type with base
# match.arg(se_type, LOGVAR_HARVEY_SE_TYPES); hand-rolled in base R because the
# Harvey QMLE is not a glm() object sandwich can dispatch on.

# canonical variant keys (the values run_all.R's logvar_harvey_se_type may take)
LOGVAR_HARVEY_SE_TYPES <- c("expected", "observed", "opg", "robust", "hac")

# QMLE covariance variants at an accepted fit. hac_lags is a programmer contract
# (loud on a malformed value). Each variant fails closed to an all-NA matrix on a
# conditioning problem in its bread: a non-finite coefficient, invalid response,
# nonpositive mu, n <= p, or a column-normalized bread failing the rcond gate.
# The inversion mirrors logvar_ppml_se.R (and logvar_ppml_jacobian): normalize a
# symmetric bread by its diagonal, gate rcond, chol-invert, transform back.
# Returns a named list of p x p matrices keyed by LOGVAR_HARVEY_SE_TYPES.
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
  # normalize a symmetric bread by its diagonal, gate rcond, chol-invert
  norm_inv <- function(m) {
    if (!all(is.finite(m))) {
      return(NULL)
    }
    d <- sqrt(diag(m))
    if (any(!is.finite(d)) || any(d <= 0)) {
      return(NULL)
    }
    ms <- m / tcrossprod(d)
    if (!all(is.finite(ms)) || rcond(ms) < 1e-10) {
      return(NULL)
    }
    ch <- tryCatch(chol(ms), error = function(cond) NULL)
    if (is.null(ch)) {
      return(NULL)
    }
    inv <- chol2inv(ch) / tcrossprod(d)
    dimnames(inv) <- dimnames(m)
    inv
  }
  h_inv <- norm_inv(0.5 * crossprod(x_mat, r * x_mat)) # observed information
  ex_inv <- norm_inv(0.5 * crossprod(x_mat)) # expected information
  meat_opg <- crossprod(g)
  opg_inv <- norm_inv(meat_opg)
  meat_hac <- meat_opg
  for (l in seq_len(hac_lags)) {
    if (l >= n) break
    gamma_l <- crossprod(
      g[(l + 1L):n, , drop = FALSE], g[1:(n - l), , drop = FALSE]
    )
    meat_hac <- meat_hac + (1 - l / (hac_lags + 1L)) * (gamma_l + t(gamma_l))
  }
  sandwich_v <- function(bread, meat) {
    if (is.null(bread)) na_mat else bread %*% meat %*% bread
  }
  list(
    expected = if (is.null(ex_inv)) na_mat else ex_inv,
    observed = if (is.null(h_inv)) na_mat else h_inv,
    opg = if (is.null(opg_inv)) na_mat else opg_inv,
    robust = sandwich_v(h_inv, meat_opg),
    hac = sandwich_v(h_inv, meat_hac)
  )
}

# SE data.frame (one row per coefficient, one numeric column per variant). A
# nonpositive or NA diagonal renders NA; pmax(d, 0) keeps sqrt off negatives.
logvar_harvey_se_frame <- function(coef, y, x_mat, hac_lags) {
  vc <- logvar_harvey_vcov(coef, y, x_mat, hac_lags)
  se <- lapply(vc, function(v) {
    d <- diag(v)
    ifelse(is.finite(d) & d >= 0, sqrt(pmax(d, 0)), NA_real_)
  })
  data.frame(coef = colnames(x_mat), se, row.names = NULL, check.names = FALSE)
}

# an all-NA SE frame for an unavailable column (point not certified feasible)
logvar_harvey_se_na_frame <- function(coef_names) {
  data.frame(
    coef = coef_names,
    stats::setNames(
      rep(list(NA_real_), length(LOGVAR_HARVEY_SE_TYPES)), LOGVAR_HARVEY_SE_TYPES
    ),
    row.names = NULL, check.names = FALSE
  )
}

# Assemble the SE frames for the two Harvey point columns from the frozen
# pipeline objects, reconstructing each column's squared-residual response the
# same way log_var_eq_harvey_sets.R built the coefficients:
#   reference  y = residuals(mean-eq OLS)^2 on the log-variance rows
#   point      y = (w1 - W2 b_point)^2 at the Lewbel point (NA when unavailable)
# Rows are chronological (inputs$qtr is arrange(qtr)-ordered), so the HAC lag
# structure is well defined. Returns list(reference, point, hac_lags).
logvar_harvey_se_columns <- function(harvey, inputs, mean_eq, hac_lags) {
  x_mat <- cbind(1, inputs$pcr)
  colnames(x_mat) <- c("(Intercept)", colnames(inputs$pcr))
  stopifnot(identical(colnames(x_mat), harvey$table$coef))
  ref_rows <- match(inputs$qtr, mean_eq$qtr)
  stopifnot(!anyNA(ref_rows))
  y_ref <- as.numeric(stats::residuals(mean_eq$ols_fit)[ref_rows])^2
  se_ref <- logvar_harvey_se_frame(harvey$table$reference, y_ref, x_mat, hac_lags)
  b_point <- mean_eq$theta_table$point
  se_point <- if (!anyNA(b_point) && !anyNA(harvey$table$point)) {
    y_point <- drop(inputs$w1 - inputs$w2 %*% b_point)^2
    logvar_harvey_se_frame(harvey$table$point, y_point, x_mat, hac_lags)
  } else {
    logvar_harvey_se_na_frame(colnames(x_mat))
  }
  list(reference = se_ref, point = se_point, hac_lags = hac_lags)
}

# attach the SE frames to the frozen Harvey object (all variants; the table picks
# logvar_harvey_se_type at render time). Guarded so the offline test can source
# this module for definitions only. The diagnostic reports the reference-column
# HAC/observed SE ratio (max over coefficients): near 1 means the Gaussian
# model-based variance holds and observed would suffice; well above 1 means the
# robust/HAC correction bites (why hac is the default).
if (exists("log_var_eq_harvey")) {
  log_var_eq_harvey$se <- logvar_harvey_se_columns(
    log_var_eq_harvey, log_var_eq$inputs, set_id_mean_eq, logvar_harvey_se_hac_lags
  )
  ref_ratio <- log_var_eq_harvey$se$reference$hac /
    log_var_eq_harvey$se$reference$observed
  cat(sprintf(
    paste0(
      "  Harvey SEs: %s for reference%s (printed: %s; %d HAC lags; ",
      "HAC/observed ref ratio max = %.2f)\n"
    ),
    paste(LOGVAR_HARVEY_SE_TYPES, collapse = "/"),
    if (all(is.na(log_var_eq_harvey$se$point$observed))) "" else " and tau=0",
    match.arg(logvar_harvey_se_type, LOGVAR_HARVEY_SE_TYPES),
    logvar_harvey_se_hac_lags,
    if (all(is.na(ref_ratio))) NA_real_ else max(ref_ratio, na.rm = TRUE)
  ))
}
