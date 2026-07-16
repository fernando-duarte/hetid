# PPML (quasi-Poisson log-link) identified sets for the log-variance equation:
# theta_hat_P(b_N) is the exponential-mean QMLE of eps_hat(b_N)^2 = (w1 - W2 b_N)^2
# on (1, PC_R), mapped over the mean equation's warm-refined display-tau news sets
# (mean_eq_bounds_tau) through the shared set engine, with a scaling pilot and an
# independent Morton-grid coverage gate before any result ships. Console block in
# console_report.R. Run via run_pipeline.R after compute_bounds_by_tau.R.

source(paper_path("log_variance", "estimators", "ppml", "estimator.R"))
source(paper_path("log_variance", "estimators", "ppml", "pilot_and_grid.R"))

# one preparation path: validate the frozen inputs and the mean-zero PC_R
# convention, then derive the scale anchor/seed base at the PPML grid cap; the
# w1/w2/pcr/qtr/x_mat and anchor pieces are injected as locals for the driver
list2env(
  logvar_prepare_map_context(
    log_var_eq$inputs, log_var_eq$sample_contract, set_id_mean_eq,
    mean_eq_bounds_tau, logvar_ppml_grid_cap
  ),
  environment()
)
# scale anchor and search seed: the Lewbel point when finite and unit-omega
# feasible in the baseline set, else the first coarsened baseline-grid point
scale_anchor_b <- if (point_feasible) b_point else grid_base[1, ]
scale_anchor_source <- if (point_feasible) "lewbel_point" else "baseline_grid_first"

# pre-mapping scaling pilot at response_scale = 1: anchor plus the first ten
# other coarsened grid points; response_scale freezes here, before any fit/cache
anchor_key <- logvar_b_key(scale_anchor_b)
grid_keys <- apply(grid_base, 1L, logvar_b_key)
pilot_pts <- utils::head(grid_base[grid_keys != anchor_key, , drop = FALSE], 10L)
pilot <- logvar_ppml_pilot(w1, w2, x_mat, scale_anchor_b, pilot_pts)
response_scale <- pilot$response_scale
cat(sprintf(
  "  PPML pilot: %d fits, %d triggered; response_scale = %g\n",
  pilot$n_fits, pilot$n_triggered, response_scale
))

# estimator with the frozen response_scale; the Lewbel point fit (and its
# start bundle) exists only when b_point is finite and certified feasible
b_point_arg <- if (point_feasible) b_point else NULL
est_ppml <- logvar_ppml_estimator(
  w1, w2, pcr, qtr,
  b_point = b_point_arg,
  scale_anchor_b = scale_anchor_b, scale_anchor_source = scale_anchor_source,
  response_scale = response_scale
)
stopifnot(identical(est_ppml$metadata$sample_id, log_var_eq$sample_id))
search_seed <- if (point_feasible) b_point else scale_anchor_b
# Lewbel-point PPML column (NA when unavailable) and kappa(X' diag(mu) X) at the
# point (or anchor) fit
point_fit <- if (point_feasible) est_ppml$fit_at_b(search_seed) else NULL
na_coef <- stats::setNames(rep(NA_real_, ncol(x_mat)), colnames(x_mat))
theta_point_ppml <- if (!is.null(point_fit)) point_fit$coef else na_coef
cond_fit <- if (!is.null(point_fit)) point_fit else est_ppml$fit_at_b(scale_anchor_b)
cond_weighted_xx <- cond_fit$diagnostics$condition_weighted_scaled
# naive reference column: PPML on the exogenous-news OLS fit's squared
# residuals, same rows (matched by qtr) and the same frozen response_scale
ref_rows <- match(qtr, set_id_mean_eq$qtr)
stopifnot(!anyNA(ref_rows))
ref_resid <- as.numeric(stats::residuals(set_id_mean_eq$ols_fit)[ref_rows])
theta_reference <- logvar_ppml_fit_response(
  ref_resid^2, x_mat,
  response_scale = response_scale
)$coef
# per display tau (keyed sprintf("%.17g", tau) so the coverage helpers and cache
# align): scan the warm-refined display box through the shared engine with three
# separated starts per side, one shared cache, a fresh per-tau budget, warm extras
taus <- set_id_mean_eq$tau_display
ppml_cache <- new.env(parent = emptyenv())
primary_results <- list()
b_tab_list <- list()
primary_diag <- list()
warm_extra <- NULL
for (idx in seq_along(taus)) {
  tau_i <- taus[idx]
  key_i <- sprintf("%.17g", tau_i)
  b_tab_i <- mean_eq_bounds_tau[[key_i]]
  stopifnot(!is.null(b_tab_i))
  qs_i <- tau_quadratic_system(set_id_mean_eq$gamma, tau_i, set_id_mean_eq$moments)
  ppml_bs <- logvar_budget_state(logvar_ppml_fit_budget)
  res_i <- logvar_engine_set_at_tau(
    est_ppml, qs_i, b_tab_i,
    b_seed = search_seed,
    max_grid_points = logvar_ppml_grid_cap, starts_per_side = 3L,
    cache = ppml_cache, budget_state = ppml_bs, extra_starts = warm_extra,
    cold_start_check = TRUE, tau = tau_i
  )
  primary_results[[key_i]] <- res_i
  b_tab_list[[key_i]] <- b_tab_i
  primary_diag[[key_i]] <- res_i$diagnostics
  warm_extra <- logvar_bounded_args(res_i$schema)
}

# independent coverage gate: a second estimator over an up-to-8000 Morton grid,
# five starts, fresh cache/budget; apply_coverage unions endpoints, demotes sides
est_cov <- logvar_ppml_estimator(
  w1, w2, pcr, qtr,
  b_point = b_point_arg,
  scale_anchor_b = scale_anchor_b, scale_anchor_source = scale_anchor_source,
  response_scale = response_scale
)
qs_fn <- function(tau) tau_quadratic_system(set_id_mean_eq$gamma, tau, set_id_mean_eq$moments)
coverage <- logvar_ppml_coverage_run(
  est_cov, taus, b_tab_list, search_seed,
  logvar_ppml_coverage_grid_cap, logvar_ppml_coverage_fit_budget, qs_fn
)
adjusted <- logvar_ppml_apply_coverage(
  primary_results, coverage,
  grid_cap = logvar_ppml_coverage_grid_cap,
  fit_budget = logvar_ppml_coverage_fit_budget,
  cache_stamp = est_cov$metadata$spec_id
)
final_res <- adjusted$results

# fragility: the benchmark's divergence directions from log_var_eq$schema (the
# pointwise crossing-nearness measure is min_feasible_abs_eps in the list below)
benchmark_divergence <- lapply(log_var_eq$schema, function(s) {
  data.frame(
    coef = s$coef, lower_unbounded = s$lower_status == "unbounded",
    upper_unbounded = s$upper_status == "unbounded", row.names = NULL
  )
})

base_tab <- final_res[[sprintf("%.17g", taus[1])]]$table
stopifnot(identical(base_tab$coef, colnames(x_mat)))
log_var_eq_ppml <- list(
  sample = list(n = length(qtr), span = range(qtr)),
  sample_id = log_var_eq$sample_id,
  table = data.frame(
    coef = colnames(x_mat), reference = unname(theta_reference),
    point = unname(theta_point_ppml), set_lower = base_tab$set_lower,
    set_upper = base_tab$set_upper, status = base_tab$status, row.names = NULL
  ),
  sets = lapply(final_res, function(r) r$table),
  schema = lapply(final_res, function(r) r$schema),
  n_feasible = vapply(primary_results, function(r) r$n_feasible, integer(1)),
  counts = primary_diag,
  fit_failures = vapply(primary_results, function(r) r$diagnostics$n_failed, integer(1)),
  min_feasible_abs_eps = vapply(
    final_res, function(r) logvar_min_feasible_eps(r$schema, w1, w2, search_seed),
    numeric(1)
  ),
  cond_weighted_xx = cond_weighted_xx,
  benchmark_divergence = benchmark_divergence,
  estimator = est_ppml, start_bundle = est_ppml$start_bundle,
  scale_anchor_bundle = est_ppml$scale_anchor_bundle,
  coverage_audit = list(audit = adjusted$audit, meta = adjusted$metadata),
  pilot = pilot
)

# register the PPML figure entry (engine opts and the shared cache included);
# this entry and ppml_cache must survive cleanup
logvar_bounds_tau_registry[[length(logvar_bounds_tau_registry) + 1L]] <- list(
  estimator = est_ppml,
  schema = lapply(final_res, function(r) r$schema),
  sets = lapply(final_res, function(r) r$table), b_seed = search_seed,
  engine_opts = list(
    max_grid_points = logvar_ppml_grid_cap, max_fit_evals = logvar_ppml_fit_budget,
    starts_per_side = 3L, cache = ppml_cache
  ),
  output_path = logvar_bounds_tau_path(out_dir, est_ppml$metadata)
)

source(paper_path("log_variance", "estimators", "ppml", "console_report.R"))

# remove only scratch locals: keep log_var_eq_ppml, logvar_bounds_tau_registry,
# ppml_cache (registry-referenced), and every sourced function
rm(
  w1, w2, pcr, qtr, x_mat, b_point, tau_base, qs_base, b_tab_base,
  point_feasible, grid_base, scale_anchor_b, scale_anchor_source, anchor_key,
  grid_keys, pilot_pts, pilot, response_scale, b_point_arg, est_ppml, point_fit,
  search_seed, na_coef, theta_point_ppml, cond_fit, cond_weighted_xx, ref_rows,
  ref_resid, theta_reference, taus, primary_results,
  b_tab_list, primary_diag, warm_extra, idx, tau_i, key_i, b_tab_i, qs_i, ppml_bs,
  res_i, est_cov, qs_fn, coverage, adjusted, final_res,
  benchmark_divergence, base_tab
)
