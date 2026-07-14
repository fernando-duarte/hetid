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
# Definitions plus a guarded attach driver; sourced by run_all.R after
# log_var_eq_ppml_sets.R and before the table builders. Consumers validate a
# requested type with base match.arg(se_type, LOGVAR_PPML_SE_TYPES); the SEs are
# hand-rolled in base R rather than delegated to a reconstructed glm()/sandwich
# object -- a glm refit is a second optimization that can diverge from the exact
# coefficient the table prints and bypasses the estimator's fail-closed
# acceptance gate, and the four formulas are pinned to sandwich in the tests.

# canonical variant keys (the values run_all.R's logvar_ppml_se_type may take)
LOGVAR_PPML_SE_TYPES <- c("naive", "hc0", "hc1", "hac")

# QMLE covariance variants at an accepted fit. hac_lags is a programmer contract
# (loud on a malformed value). Fails closed (all-NA variance) on any data or
# conditioning problem: a non-finite coefficient, an invalid response, mu not
# strictly positive, n <= p, or a column-normalized information matrix that fails
# the estimator's conditioning gate. The inversion mirrors logvar_ppml_jacobian:
# gate and invert D^-1 A D^-1 (D = column scales sqrt(colSums(mu X^2))), then
# transform back, so this accepts exactly the fits the estimator accepted.
# Returns a named list of p x p matrices keyed by LOGVAR_PPML_SE_TYPES.
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
  a_mat <- crossprod(x_mat, mu * x_mat)
  d_scale <- sqrt(colSums(mu * x_mat^2)) # info column scales
  if (!all(is.finite(d_scale)) || any(d_scale <= 0)) {
    return(na_out)
  }
  dd <- tcrossprod(d_scale)
  a_s <- a_mat / dd # D^-1 A D^-1
  if (!all(is.finite(a_s)) || rcond(a_s) < 1e-10) {
    return(na_out)
  }
  r_chol <- tryCatch(chol(a_s), error = function(cond) NULL)
  if (is.null(r_chol)) {
    return(na_out)
  }
  a_inv <- chol2inv(r_chol) / dd # A^-1 = D^-1 A_s^-1 D^-1
  dimnames(a_inv) <- dimnames(a_mat)
  r <- y - mu
  u <- x_mat * r
  meat_hc0 <- crossprod(u)
  phi <- sum(r^2 / mu) / (n - p)
  sandwich_v <- function(meat) a_inv %*% meat %*% a_inv
  v_hc0 <- sandwich_v(meat_hc0)
  meat_hac <- meat_hc0
  for (l in seq_len(hac_lags)) {
    if (l >= n) break
    gamma_l <- crossprod(
      u[(l + 1L):n, , drop = FALSE], u[1:(n - l), , drop = FALSE]
    )
    meat_hac <- meat_hac + (1 - l / (hac_lags + 1L)) * (gamma_l + t(gamma_l))
  }
  list(
    naive = phi * a_inv, hc0 = v_hc0,
    hc1 = (n / (n - p)) * v_hc0, hac = sandwich_v(meat_hac)
  )
}

# SE data.frame (one row per coefficient, one numeric column per variant). A
# nonpositive or NA diagonal renders NA; pmax(d, 0) keeps sqrt off negatives so
# no spurious "NaNs produced" warning is raised (all four variants are PSD).
logvar_ppml_se_frame <- function(coef, y, x_mat, hac_lags) {
  vc <- logvar_ppml_vcov(coef, y, x_mat, hac_lags)
  se <- lapply(vc, function(v) {
    d <- diag(v)
    ifelse(is.finite(d) & d >= 0, sqrt(pmax(d, 0)), NA_real_)
  })
  data.frame(
    coef = colnames(x_mat), se, row.names = NULL, check.names = FALSE
  )
}

# an all-NA SE frame for an unavailable column (point not certified feasible)
logvar_ppml_se_na_frame <- function(coef_names) {
  data.frame(
    coef = coef_names,
    stats::setNames(
      rep(list(NA_real_), length(LOGVAR_PPML_SE_TYPES)), LOGVAR_PPML_SE_TYPES
    ),
    row.names = NULL, check.names = FALSE
  )
}

# Assemble the SE frames for the two PPML point columns from the frozen pipeline
# objects, reconstructing each column's squared-residual response the same way
# log_var_eq_ppml_sets.R built the coefficients:
#   reference  y = residuals(mean-eq OLS)^2 on the log-variance rows
#   point      y = (w1 - W2 b_point)^2 at the Lewbel point (NA when unavailable)
# Rows are chronological (inputs$qtr is arrange(qtr)-ordered), so the HAC lag
# structure is well defined. Returns list(reference, point, hac_lags).
logvar_ppml_se_columns <- function(ppml, inputs, mean_eq, hac_lags) {
  x_mat <- cbind(1, inputs$pcr)
  colnames(x_mat) <- c("(Intercept)", colnames(inputs$pcr))
  stopifnot(identical(colnames(x_mat), ppml$table$coef))
  ref_rows <- match(inputs$qtr, mean_eq$qtr)
  stopifnot(!anyNA(ref_rows))
  y_ref <- as.numeric(stats::residuals(mean_eq$ols_fit)[ref_rows])^2
  se_ref <- logvar_ppml_se_frame(ppml$table$reference, y_ref, x_mat, hac_lags)
  b_point <- mean_eq$theta_table$point
  se_point <- if (!anyNA(b_point) && !anyNA(ppml$table$point)) {
    y_point <- drop(inputs$w1 - inputs$w2 %*% b_point)^2
    logvar_ppml_se_frame(ppml$table$point, y_point, x_mat, hac_lags)
  } else {
    logvar_ppml_se_na_frame(colnames(x_mat))
  }
  list(reference = se_ref, point = se_point, hac_lags = hac_lags)
}

# attach the SE frames to the frozen PPML object (all variants; the table picks
# logvar_ppml_se_type at render time). Guarded so the offline test can source
# this module for definitions only. The diagnostic reports the reference-column
# HAC/naive SE ratio (max over coefficients): near 1 means the model-based
# variance holds and naive would suffice; well above 1 means the robust/HAC
# correction bites and naive would understate uncertainty (why hac is default).
if (exists("log_var_eq_ppml")) {
  log_var_eq_ppml$se <- logvar_ppml_se_columns(
    log_var_eq_ppml, log_var_eq$inputs, set_id_mean_eq, logvar_ppml_se_hac_lags
  )
  ref_ratio <- log_var_eq_ppml$se$reference$hac / log_var_eq_ppml$se$reference$naive
  cat(sprintf(
    "  PPML SEs: %s for OLS%s (printed: %s; %d HAC lags; HAC/naive ref ratio max = %.2f)\n",
    paste(LOGVAR_PPML_SE_TYPES, collapse = "/"),
    if (all(is.na(log_var_eq_ppml$se$point$naive))) "" else " and tau=0",
    match.arg(logvar_ppml_se_type, LOGVAR_PPML_SE_TYPES), logvar_ppml_se_hac_lags,
    if (all(is.na(ref_ratio))) NA_real_ else max(ref_ratio, na.rm = TRUE)
  ))
}
