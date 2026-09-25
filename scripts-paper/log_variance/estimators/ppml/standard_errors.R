# Analytic PPML covariance calculation delegates to hetid at the stored
# original-scale coefficient, response and complete design. The paper owns
# its conditioning control, SE frames, dated response reconstruction and
# reporting. No refit or synthetic successful-fit object is introduced.

# Canonical paper variant keys, checked against the package result in tests.
LOGVAR_PPML_SE_TYPES <- c("naive", "hc0", "hc1", "hac")

logvar_ppml_vcov <- function(
  coef,
  y,
  x_mat,
  hac_lags,
  rcond_tol = LOGVAR_PPML_CONTROL$rcond_tol
) {
  if (is.null(rcond_tol)) {
    stop("rcond_tol is missing from the paper fit control", call. = FALSE)
  }
  hetid::compute_log_variance_vcov_at_coef(
    coef, y,
    x_design = x_mat, estimator = "ppml",
    hac_lags = hac_lags, rcond_tol = rcond_tol
  )
}

# SE frames: thin wrappers over the shared builders (standard_error_estimators.R) so
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
  rcond_tol <- ppml$estimator$metadata$fit_control$rcond_tol
  vcov_fn <- function(coef, y, x_mat, hac_lags) {
    logvar_ppml_vcov(coef, y, x_mat, hac_lags, rcond_tol)
  }
  logvar_se_columns(
    vcov_fn, ppml$table, inputs, mean_eq, hac_lags, LOGVAR_PPML_SE_TYPES
  )
}

# attach the SE frames to the frozen PPML object (all variants; the table picks
# logvar_ppml_se_type at render time). Guarded so the offline test can source
# this module for definitions only. The diagnostic ratio contrasts the HAC and
# naive reference SEs (see logvar_se_report).
ppml_result <- paper_logvar_result("ppml", required = FALSE)
if (!is.null(ppml_result)) {
  ppml_result$se <- logvar_ppml_se_columns(
    ppml_result, log_var_eq$inputs, set_id_mean_eq, logvar_ppml_se_hac_lags
  )
  logvar_se_report(
    ppml_result$se, "PPML", LOGVAR_PPML_SE_TYPES,
    logvar_ppml_se_type, logvar_ppml_se_hac_lags, c("hac", "naive")
  )
  paper_logvar_assign_result("ppml", ppml_result)
}
