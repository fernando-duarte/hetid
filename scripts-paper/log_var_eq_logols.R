# The log-OLS estimator object: the benchmark two-step map b_N -> theta_hat
# packaged as the first estimator for the shared set engine. Its closures are
# the exact arithmetic the driver ran inline -- logvar_theta_hat / _grad /
# _jacobian / _grid_scan / _crossing_census from log_var_eq_map.R -- so the
# engine's benchmark path stays numerically identical. The census + sign-tracker
# union that certifies the per-side divergences is the only log-OLS-specific
# piece; it rides in analyze_domain rather than the engine. Definitions only;
# sourced by log_var_eq.R after the map and engine modules. Also defines
# logvar_sample_id, the base-tools md5 sample guard every result carries.

# md5 guard over the frozen (qtr, w1, w2, pcr) tuple: a same-environment
# sample-drift check (pinned RDS version 3, tempfile unlinked, base tools
# only). Prefixed with the sample size and qtr span so a mismatch is legible.
logvar_sample_id <- function(qtr, w1, w2, pcr) {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(list(qtr = qtr, w1 = w1, w2 = w2, pcr = pcr), tmp, version = 3)
  md5 <- tools::md5sum(tmp)
  sprintf(
    "n%d_%s_%s_%s", length(w1),
    format(min(qtr)), format(max(qtr)), unname(md5)
  )
}

# construct the log-OLS estimator object for logvar_engine_set_at_tau: a plain
# list of closures over the frozen (w1, w2, proj) plus metadata. fit_at_b is
# the closed-form map (domain_failure at an exact residual zero); coef_objective
# hands the polish the same sum(proj_row * log(...)) / logvar_theta_grad
# closures the driver used; scan_grid wraps the vectorized grid scan; and
# analyze_domain packages the census + sign-tracker union that the engine defers
# to the estimator for divergence certification.
logvar_logols_estimator <- function(w1, w2, proj, qtr, pcr) {
  list(
    metadata = list(
      estimator = "logols",
      target_functional = "theta_log",
      intercept_normalization = "mean-log (theta_0 absorbs 2 log|m_0| + E[log v^2])",
      sample_id = logvar_sample_id(qtr, w1, w2, pcr),
      smoothness = "smooth",
      inner_solver = "closed-form projection",
      response_scale = "log",
      spec_id = "logols-v1",
      cold_start_rtol = 1e-8
    ),
    coef_labels = rownames(proj),
    fit_at_b = function(b, start = NULL) {
      eps <- drop(w1 - w2 %*% b)
      th <- logvar_theta_hat(b, w1, w2, proj)
      list(
        coef = th,
        fit_status = if (any(eps == 0)) "domain_failure" else "ok",
        converged = TRUE, objective = 0, score_norm = 0,
        convergence_code = 0L,
        diagnostics = list(min_abs_eps = min(abs(eps))),
        warm_start = NULL
      )
    },
    coef_objective = function(j) {
      force(j)
      list(
        fn = function(b) sum(proj[j, ] * log(drop(w1 - w2 %*% b)^2)),
        gr = function(b) logvar_theta_grad(b, w1, w2, proj[j, ])
      )
    },
    jacobian_at_b = function(b, fit = NULL) {
      logvar_theta_jacobian(b, w1, w2, proj)
    },
    scan_grid = function(b_feas) {
      s <- logvar_grid_scan(b_feas, w1, w2, proj)
      list(
        min = s$min, max = s$max, arg_min = s$arg_min, arg_max = s$arg_max,
        domain_info = list(cross_grid = s$cross_grid),
        n_fit_failures = 0L, fit_statuses = NULL
      )
    },
    analyze_domain = list(
      precheck = function(qs, b_tab, ctx) {
        census <- logvar_crossing_census(
          qs, b_tab$set_lower, b_tab$set_upper, w1, w2
        )
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
  )
}
