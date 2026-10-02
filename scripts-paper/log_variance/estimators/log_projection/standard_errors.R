# Analytic standard errors for the regularized log projections' two point
# columns: hetid's least-squares covariance of the transformed squared residual
# at the reference news vector (the exogenous-news OLS coefficients) and at the
# Lewbel point, through the shared SE frame builders. Plug-in second-stage
# variances with the method's tuning held at its estimates (see
# hetid::compute_log_projection_vcov). Definitions only; sourced by run_sets.R
# and by the estimator page specs.

paper_source_once(paper_path(
  "log_variance", "inference", "standard_error_estimators.R"
))

LOGVAR_LOG_PROJECTION_SE_TYPES <- hetid::LOG_VARIANCE_CONTROL$SE_TYPES

logvar_log_projection_se_columns <- function(mapped, prep, ctx, hac_lags) {
  est <- mapped$estimator
  frame <- function(b) {
    if (is.null(b)) {
      return(logvar_se_na_frame(est$coef_labels, LOGVAR_LOG_PROJECTION_SE_TYPES))
    }
    logvar_se_frame(
      hetid::compute_log_projection_vcov(
        prep, unname(b), est$metadata$fit_control$method, mapped$multiplier,
        hac_lags
      ),
      est$coef_labels
    )
  }
  point_ok <- isTRUE(ctx$point_feasible) && all(is.finite(mapped$point))
  list(
    reference = frame(ctx$b_ref),
    point = frame(if (point_ok) ctx$b_point),
    hac_lags = hac_lags
  )
}
