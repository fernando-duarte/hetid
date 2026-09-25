# Analytic HARVEY covariance calculation delegates to hetid at the stored
# original-scale coefficient, response and complete design. The paper owns
# its conditioning control, SE frames, dated response reconstruction and
# reporting. No refit or synthetic successful-fit object is introduced.

# Canonical paper variant keys, checked against the package result in tests.
LOGVAR_HARVEY_SE_TYPES <- c("expected", "observed", "opg", "robust", "hac")

logvar_harvey_vcov <- function(
  coef,
  y,
  x_mat,
  hac_lags,
  rcond_tol = LOGVAR_HARVEY_CONTROL$rcond_tol
) {
  if (is.null(rcond_tol)) {
    stop("rcond_tol is missing from the paper fit control", call. = FALSE)
  }
  hetid::compute_log_variance_vcov_at_coef(
    coef, y,
    x_design = x_mat, estimator = "harvey",
    hac_lags = hac_lags, rcond_tol = rcond_tol
  )
}

# SE frames: thin wrappers over the shared builders (standard_error_estimators.R) so
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
  rcond_tol <- harvey$estimator$metadata$fit_control$rcond_tol
  vcov_fn <- function(coef, y, x_mat, hac_lags) {
    logvar_harvey_vcov(coef, y, x_mat, hac_lags, rcond_tol)
  }
  logvar_se_columns(
    vcov_fn, harvey$table, inputs, mean_eq, hac_lags,
    LOGVAR_HARVEY_SE_TYPES
  )
}

# attach the SE frames to the frozen Harvey object (all variants; the table picks
# logvar_harvey_se_type at render time). Guarded so the offline test can source
# this module for definitions only. The diagnostic ratio contrasts the HAC and
# observed reference SEs (see logvar_se_report).
harvey_result <- paper_logvar_result("harvey", required = FALSE)
if (!is.null(harvey_result)) {
  harvey_result$se <- logvar_harvey_se_columns(
    harvey_result, log_var_eq$inputs, set_id_mean_eq, logvar_harvey_se_hac_lags
  )
  logvar_se_report(
    harvey_result$se, "Harvey", LOGVAR_HARVEY_SE_TYPES,
    logvar_harvey_se_type, logvar_harvey_se_hac_lags, c("hac", "observed")
  )
  paper_logvar_assign_result("harvey", harvey_result)
}
