# The log-OLS estimator object: the benchmark two-step map b_N -> theta_hat
# packaged as the first estimator for the shared set engine. Its coefficients
# and Jacobian come from the package log projection (method "log"); the census +
# sign-tracker union that certifies the per-side divergences is the only
# log-OLS-specific piece; it rides in analyze_domain rather than the engine.
# Definitions only; sourced by run.R after the map, engine, and
# log_projection/estimator.R modules. Also defines logvar_sample_id, the
# base-tools md5 sample guard every result carries.

paper_source_once(paper_path(
  "log_variance", "estimators", "log_projection", "estimator.R"
))

# md5 guard over the frozen (qtr, w1, w2, pcr) tuple: a same-environment
# sample-drift check (pinned RDS version 3, tempfile unlinked, base tools
# only). Prefixed with the sample size and qtr span so a mismatch is legible.
logvar_sample_id <- function(qtr, w1, w2, pcr) {
  md5 <- paper_md5_rds(list(
    qtr = qtr,
    w1 = w1,
    w2 = w2,
    pcr = pcr
  ))
  sprintf(
    "n%d_%s_%s_%s", length(w1),
    format(min(qtr)), format(max(qtr)), unname(md5)
  )
}

# construct the log-OLS estimator object for logvar_engine_set_at_tau: the
# package log projection (method "log") through the generic wrapper in
# log_projection/estimator.R, with the log-OLS-specific pieces kept here.
# coef_objective hands the polish the package value regardless of status, so a
# side that crosses a residual zero keeps its +/-Inf divergence semantics;
# scan_grid adds the both-signs crossing tracker; and analyze_domain packages
# the census + sign-tracker union that the engine defers to the estimator for
# divergence certification.
logvar_logols_estimator <- function(
  prep,
  qtr,
  w1,
  w2,
  pcr,
  control = LOGVAR_LOGOLS_CONTROL
) {
  stopifnot(identical(unname(prep$w1), unname(w1)))
  proj <- prep$projection
  est <- logvar_log_projection_estimator(
    prep, "logols", "log", 1,
    logvar_sample_id(qtr, w1, w2, pcr), control
  )
  est$metadata$intercept_normalization <-
    "mean-log (theta_0 absorbs 2 log|m_0| + E[log v^2])"
  evaluate <- function(b, jacobian) {
    hetid::evaluate_log_projection(prep, b, "log", jacobian = jacobian)
  }
  est$coef_objective <- function(j) {
    force(j)
    list(
      fn = function(b) evaluate(b, FALSE)$coef[[j]],
      gr = function(b) est$jacobian_at_b(b)[j, ]
    )
  }
  est$scan_grid <- function(b_feas) {
    s <- logvar_projection_scan(
      b_feas,
      function(b_rows) {
        ev <- evaluate(b_rows, FALSE)
        list(coef = ev$coef, eligible = rep(TRUE, nrow(b_rows)))
      },
      nrow(proj),
      residuals_at = function(b_rows) w1 - w2 %*% t(b_rows)
    )
    list(
      min = s$min, max = s$max, arg_min = s$arg_min, arg_max = s$arg_max,
      domain_info = list(cross_grid = s$cross_grid),
      n_fit_failures = 0L, fit_statuses = NULL
    )
  }
  est$analyze_domain <- list(
    precheck = function(qs, b_tab, ctx) {
      box <- paper_containing_box(b_tab)
      census <- logvar_crossing_census(qs, box$lower, box$upper, w1, w2)
      list(
        unresolved = census$unresolved,
        n_flagged = length(census$cross),
        info = list(cross = census$cross)
      )
    },
    sides = function(qs, b_tab, scan, ctx) {
      cross_all <- sort(union(
        ctx$precheck$info$cross, scan$domain_info$cross_grid
      ))
      lower_unb <- apply(proj[, cross_all, drop = FALSE] > 0, 1, any)
      upper_unb <- apply(proj[, cross_all, drop = FALSE] < 0, 1, any)
      list(
        lower_unbounded = lower_unb, upper_unbounded = upper_unb,
        unresolved_endpoints = character(0),
        closure_diagnostics = NULL,
        info = list(cross_all = cross_all)
      )
    }
  )
  est
}
