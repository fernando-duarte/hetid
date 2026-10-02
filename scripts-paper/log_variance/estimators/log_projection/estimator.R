# Engine estimator object over the package's log projections: closures over
# one hetid_log_projection_prep, a method, and a multiplier. fit_at_b and
# jacobian_at_b are the package evaluation; scan_grid is the batch fast path,
# with separated start pools when pool_k > 1. spec_id folds in the method,
# multiplier, package version, and mean-sample identity, so caches never mix
# tunings or samples. A withheld Jacobian is a NaN matrix of the documented
# shape, so the polish rejects the start instead of indexing NULL.
# Definitions only; sourced by log_ols/estimator.R.

paper_source_once(paper_path("log_variance", "engine", "contracts.R"))
paper_source_once(paper_path("log_variance", "core", "projection_scan.R"))

LOGVAR_LOG_PROJECTION_FIT_STATUS <- c(
  ok = LOGVAR_FIT_STATUS[["ok"]],
  domain_failure = LOGVAR_FIT_STATUS[["domain_failure"]],
  numerical_failure = LOGVAR_FIT_STATUS[["nonfinite_fitted_log_variance"]]
)

logvar_log_projection_estimator <- function(prep, id, method, multiplier,
                                            sample_id, control, pool_k = 1L) {
  spec <- c(control, list(
    method = method, multiplier = paper_numeric_key(multiplier),
    package_version = as.character(utils::packageVersion("hetid")),
    mean_sample_md5 = paper_md5_rds(list(
      prep$mean_ids, prep$w1_mean, prep$w2_mean
    ))
  ))
  evaluate <- function(b, jacobian) {
    hetid::evaluate_log_projection(prep, b, method, multiplier, jacobian)
  }
  labels <- rownames(prep$projection)
  nan_jacobian <- matrix(NaN, length(labels), ncol(prep$w2))
  est <- list(
    metadata = list(
      estimator = id, target_functional = "theta_log",
      intercept_normalization = sprintf("%s projection intercept", method),
      sample_id = sample_id, smoothness = "smooth",
      inner_solver = "closed-form projection", response_scale = "log",
      spec_id = logvar_spec_id(logvar_flatten_spec(spec, "control")),
      fit_control = spec, cold_start_rtol = control$cold_start_rtol
    ),
    coef_labels = labels,
    fit_at_b = function(b, start = NULL, phase = NULL) {
      stopifnot(is.null(dim(b)))
      ev <- evaluate(b, FALSE)
      new_logvar_fit_result(
        coef = ev$coef,
        fit_status = unname(LOGVAR_LOG_PROJECTION_FIT_STATUS[[ev$status]]),
        converged = TRUE, objective = 0, score_norm = 0,
        convergence_code = 0L, diagnostics = list(status = ev$status),
        warm_start = NULL
      )
    },
    jacobian_at_b = function(b, fit = NULL) {
      jac <- evaluate(b, TRUE)$jacobian
      if (is.null(jac)) nan_jacobian else jac
    },
    scan_grid = function(b_feas) {
      s <- logvar_projection_scan(b_feas, function(b_rows) {
        ev <- evaluate(b_rows, FALSE)
        engine_status <- unname(LOGVAR_LOG_PROJECTION_FIT_STATUS[ev$status])
        list(
          coef = ev$coef,
          eligible = engine_status == LOGVAR_FIT_STATUS[["ok"]]
        )
      }, length(labels), pool_k = pool_k)
      list(
        min = s$min, max = s$max, arg_min = s$arg_min, arg_max = s$arg_max,
        arg_min_pool = s$arg_min_pool, arg_max_pool = s$arg_max_pool,
        domain_info = list(), n_fit_failures = s$n_fail, fit_statuses = NULL
      )
    }
  )
  if (identical(method, "log_fuller") && !isTRUE(prep$scale_lower_certified)) {
    est$analyze_domain <- logvar_log_projection_uncertified_domain(labels)
  }
  est
}

# Every side of an uncertified Fuller map is unresolved: positivity of the
# candidate scale over the set is not established (spec, Fuller scale), so no
# consumer of the estimator (display map, audit, bounds-by-tau figure,
# fitted-volatility envelopes) can certify an endpoint
logvar_log_projection_uncertified_domain <- function(labels) {
  force(labels)
  list(sides = function(qs, b_tab, scan, ctx) {
    list(
      lower_unbounded = rep(FALSE, length(labels)),
      upper_unbounded = rep(FALSE, length(labels)),
      unresolved_endpoints = c(
        paste(labels, "min", sep = ":"), paste(labels, "max", sep = ":")
      ),
      closure_diagnostics = NULL,
      info = list(reason = "fuller_scale_uncertified")
    )
  })
}
