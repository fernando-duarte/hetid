# PPML paper adapter, score/information primitives and cross-estimator start bundle.

paper_source_once(paper_path("log_variance", "estimators", "package_fit.R"))

# Quasi-Poisson score X'(y - mu) and information X' diag(mu) X at theta on the
# scaled response, factored so the acceptance check, the Jacobian, and joint-GMM
# share one derivation instead of re-deriving the moment.
logvar_ppml_score <- function(theta_scaled, y_scaled, x_mat) {
  drop(crossprod(x_mat, y_scaled - exp(drop(x_mat %*% theta_scaled))))
}

logvar_ppml_info <- function(theta_scaled, x_mat) {
  crossprod(x_mat, exp(drop(x_mat %*% theta_scaled)) * x_mat)
}

# Delegate fitting; original-scale coefficients and scaled warm starts retain their meaning.
logvar_ppml_fit_response <- function(y, x_mat, start = NULL,
                                     fallback_starts = list(), response_scale = 1,
                                     control = LOGVAR_PPML_CONTROL) {
  logvar_package_fit(
    y, x_mat, "ppml", start, fallback_starts, response_scale,
    logvar_package_control(control, "ppml")
  )
}

# The only sanctioned cross-estimator start object: NULL unless fit is an
# accepted point fit, else a typed bundle whose coef_original is the recovered
# vector and coef_scaled the raw scaled-fit warm start.
logvar_ppml_start_bundle <- function(fit, response_scale, source, b) {
  if (!logvar_fit_ok(fit)) {
    return(NULL)
  }
  list(
    coef_original = fit$coef, coef_scaled = fit$warm_start,
    response_scale = response_scale,
    valid_for = c("variance_start", "irls_warm_start"),
    source = source, b = as.numeric(b)
  )
}
