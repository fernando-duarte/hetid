# Paper-owned SE frames, dated response reconstruction and reporting.
# Numerical covariance formulas and their conditioning/HAC helpers live in hetid.

paper_source_once(paper_path("support", "reporting", "cells.R"))

# SE data.frame from a named vcov list (one row per coefficient, one numeric
# column per variant). A nonpositive or NA diagonal renders NA; pmax(d, 0) keeps
# sqrt off negatives so no spurious "NaNs produced" warning is raised.
logvar_se_frame <- function(vcov_list, coef_names) {
  se <- lapply(vcov_list, function(v) {
    d <- diag(v)
    ifelse(is.finite(d) & d >= 0, sqrt(pmax(d, 0)), NA_real_)
  })
  data.frame(coef = coef_names, se, row.names = NULL, check.names = FALSE)
}

# an all-NA SE frame for an unavailable column (point not certified feasible)
logvar_se_na_frame <- function(coef_names, se_types) {
  data.frame(
    coef = coef_names,
    stats::setNames(rep(list(NA_real_), length(se_types)), se_types),
    row.names = NULL, check.names = FALSE
  )
}

# Assemble the SE frames for the two point columns from the frozen pipeline
# objects, reconstructing each column's squared-residual response the same way
# the sets driver built the coefficients:
#   reference  y = residuals(mean-eq OLS)^2 on the log-variance rows
#   point      y = (w1 - W2 b_point)^2 at the Lewbel point (NA when unavailable)
# vcov_fn is the estimator's (coef, y, x_mat, hac_lags) -> named vcov list; tab
# is the estimator's $table (coef/reference/point). Rows are chronological
# (inputs$qtr is arrange(qtr)-ordered), so the HAC lag structure is well defined.
# Returns list(reference, point, hac_lags).
logvar_se_columns <- function(vcov_fn, tab, inputs, mean_eq, hac_lags, se_types) {
  x_mat <- logvar_design_matrix(inputs$pcr)
  stopifnot(identical(colnames(x_mat), tab$coef))
  ref_rows <- match(inputs$qtr, mean_eq$qtr)
  stopifnot(!anyNA(ref_rows))
  y_ref <- as.numeric(stats::residuals(mean_eq$ols_fit)[ref_rows])^2
  se_ref <- logvar_se_frame(vcov_fn(tab$reference, y_ref, x_mat, hac_lags), tab$coef)
  b_point <- mean_eq$theta_table$point
  se_point <- if (!anyNA(b_point) && !anyNA(tab$point)) {
    y_point <- drop(inputs$w1 - inputs$w2 %*% b_point)^2
    logvar_se_frame(vcov_fn(tab$point, y_point, x_mat, hac_lags), tab$coef)
  } else {
    logvar_se_na_frame(tab$coef, se_types)
  }
  list(reference = se_ref, point = se_point, hac_lags = hac_lags)
}

# attach-time console diagnostic shared by both modules' guarded drivers. Reports
# the printed variant and the reference-column ratio of the robust/HAC variant to
# the model-based variant (max over coefficients): near 1 means the model-based
# variance suffices; well above 1 means the correction bites (why hac is default).
# ratio_keys = c(numerator, denominator) variant names; label is the estimator.
logvar_se_report <- function(se, label, se_types, se_type, hac_lags, ratio_keys) {
  ratio <- se$reference[[ratio_keys[1]]] / se$reference[[ratio_keys[2]]]
  cat(sprintf(
    paste0(
      "  %s SEs: %s for reference%s (printed: %s; %d HAC lags; ",
      "%s/%s ref ratio max = %s)\n"
    ),
    label,
    paste(se_types, collapse = "/"),
    if (all(is.na(se$point[[ratio_keys[2]]]))) "" else " and tau=0",
    match.arg(se_type, se_types),
    hac_lags,
    ratio_keys[1], ratio_keys[2],
    paper_format_number(
      if (all(is.na(ratio))) NA_real_ else max(ratio, na.rm = TRUE),
      PAPER_REPORTING_CONTROL$cells$statistic_digits,
      "na"
    )
  ))
}
