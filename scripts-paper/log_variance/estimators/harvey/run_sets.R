# Harvey Gaussian multiplicative-heteroskedasticity identified sets for the
# log-variance equation: theta_hat_H(b_N) minimizes Q_H(theta; b) =
# 0.5 sum(eta + y exp(-eta)) on eps_hat(b_N)^2 = (w1 - W2 b_N)^2 over (1, PC_R),
# mapped over the mean equation's warm-refined display-tau news sets
# (mean_eq_bounds_tau) through the shared set engine. No response scaling (Harvey
# absorbs scale in its intercept); instead a deterministic no-fit stability
# precheck guards the constructor, and a reduced five-start sensitivity gate
# re-polishes before any result ships. Console block in the driver helpers. Run
# via run_pipeline.R after run_sets.R, before render_panels.R.

paper_source_once(paper_path("log_variance", "estimators", "harvey", "estimator.R"))
paper_source_once(paper_path(
  "log_variance",
  "estimators",
  "harvey",
  "sensitivity_and_reporting.R"
))

# one preparation path: validate the frozen inputs and the mean-zero PC_R
# convention, then derive the anchor/seed base at the Harvey grid cap; the
# w1/w2/pcr/qtr/x_mat and anchor pieces are injected as locals for the driver
list2env(
  logvar_prepare_map_context(
    log_var_eq$inputs, log_var_eq$sample_contract, set_id_mean_eq,
    mean_eq_bounds_tau, logvar_harvey_grid_cap
  ),
  environment()
)
chol_xx <- chol(crossprod(x_mat))
# search anchor: the Lewbel point when feasible, else the first coarsened grid pt
anchor_b <- logvar_map_anchor(point_feasible, b_point, grid_base)

# naive reference squared residuals (matched by qtr), shared by the reference
# column and the precheck's reference pair
ref_resid <- logvar_reference_residuals(qtr, set_id_mean_eq)
y_ref <- ref_resid^2

# stability precheck first: the pure no-fit evaluation over the exact three
# response/start pairs must pass before any Harvey constructor or fit runs, so
# constructor-time point fitting cannot precede the guard
pairs <- logvar_harvey_precheck_pairs(
  y_ref, anchor_b, w1, w2, x_mat, log_var_eq_ppml$start_bundle
)
res_pre <- logvar_harvey_stability_precheck(
  pairs, x_mat, chol_xx, LOGVAR_HARVEY_CONTROL
)
stopifnot(isTRUE(attr(res_pre, "passed")))

# estimator, guarded by the precheck: PPML seeds rung one, PPML's own cached
# evaluator is the arbitrary-b retry rung (never the Harvey engine cache), and
# the benchmark log-OLS coefficients are the shifted diagnostic rung
logols_coef <- stats::setNames(log_var_eq$table$ols, log_var_eq$table$coef)
est_harvey <- logvar_harvey_estimator(
  w1, w2, pcr, qtr,
  b_point = if (point_feasible) b_point else NULL,
  ppml_bundle = log_var_eq_ppml$start_bundle,
  ppml_bundle_source_id = log_var_eq_ppml$estimator$metadata$spec_id,
  ppml_start_at_b = log_var_eq_ppml$estimator$fit_at_b,
  ppml_start_at_b_source_id = log_var_eq_ppml$estimator$metadata$spec_id,
  logols_coef = logols_coef,
  control = LOGVAR_HARVEY_CONTROL
)
stopifnot(identical(est_harvey$metadata$sample_id, log_var_eq$sample_id))
search_seed <- if (point_feasible) b_point else anchor_b

# reference column (coefficients only; SEs deferred) and the Lewbel-point column
# read straight off the estimator so no duplicate point solve happens; a missing
# point renders NA (the panel prints "--")
ref_fit <- logvar_harvey_fit_response(
  y_ref, x_mat,
  control = LOGVAR_HARVEY_CONTROL
)
if (!logvar_harvey_accepted(ref_fit)) {
  stop(sprintf(
    "log_var_eq_harvey_sets: reference Harvey fit failed (%s/%s)",
    ref_fit$fit_status, ref_fit$diagnostics$error_class
  ))
}
theta_reference <- ref_fit$coef
na_coef <- stats::setNames(rep(NA_real_, ncol(x_mat)), colnames(x_mat))
theta_point_harvey <- if (!is.null(est_harvey$point_fit)) {
  est_harvey$point_fit$coef
} else {
  na_coef
}

# per display tau (keyed by paper_tau_key so the gate and cache align): scan
# the warm-refined display box through the shared engine with three separated
# starts per side, one cache, a fresh per-tau budget, and prior-tau warm extras
taus <- set_id_mean_eq$tau_display
qs_fn <- mean_quadratic_system_factory(set_id_mean_eq)
harvey_cache <- new.env(parent = emptyenv())
mapped <- logvar_map_display_taus(
  taus = taus,
  bounds_tau = mean_eq_bounds_tau,
  quadratic_at_tau = qs_fn,
  map_one = logvar_engine_tau_mapper(
    estimator = est_harvey,
    b_seed = search_seed,
    max_grid_points = logvar_harvey_grid_cap,
    max_fit_evals = logvar_harvey_fit_budget,
    cache = harvey_cache
  )
)
primary_results <- mapped$results
b_tab_list <- mapped$boxes

# reduced sensitivity gate before anything renders: a five-start re-polish of the
# same warm-refined boxes over a fresh cache and budget, applied through Harvey's
# union/demotion machinery so moved or mismatched bounded sides go unreliable
gate <- logvar_harvey_sensitivity_gate(
  est_harvey, taus, b_tab_list, search_seed,
  logvar_harvey_grid_cap, logvar_harvey_sensitivity_fit_budget,
  qs_fn, primary_results
)
final_res <- gate$results

core <- logvar_set_result_core(
  qtr = qtr,
  sample_id = log_var_eq$sample_id,
  coef_labels = colnames(x_mat),
  reference = theta_reference,
  point = theta_point_harvey,
  baseline_tau = set_id_mean_eq$tau_baseline,
  primary_results = primary_results,
  final_results = final_res,
  w1 = w1,
  w2 = w2,
  search_seed = search_seed
)
log_var_eq_harvey <- c(core, list(
  fit_failures = vapply(primary_results, function(r) r$diagnostics$n_failed, integer(1)),
  min_feasible_abs_eps = vapply(
    final_res, function(r) logvar_min_feasible_eps(r$schema, w1, w2, search_seed),
    numeric(1)
  ),
  estimator = est_harvey, start_bundle_used = est_harvey$start_bundle,
  point_start_rung = est_harvey$point_start_rung,
  sensitivity_audit = list(audit = gate$audit, meta = gate$metadata),
  census_comparability = log_var_eq$n_cross
))

# register the Harvey figure entry (engine opts and the shared cache included);
# this entry and harvey_cache must survive cleanup
registry_entry <- logvar_bounds_registry_entry(
  estimator = est_harvey,
  results = final_res,
  b_seed = search_seed,
  engine_opts = list(
    max_grid_points = logvar_harvey_grid_cap,
    max_fit_evals = logvar_harvey_fit_budget,
    starts_per_side = LOGVAR_SEARCH_CONTROL$primary_starts_per_side,
    cache = harvey_cache
  )
)
logvar_bounds_tau_registry[[length(logvar_bounds_tau_registry) + 1L]] <-
  registry_entry

logvar_harvey_report(log_var_eq_harvey, taus)

# remove only scratch locals: keep log_var_eq_harvey, logvar_bounds_tau_registry,
# harvey_cache (registry-referenced), and every sourced function
rm(
  w1, w2, pcr, qtr, x_mat, chol_xx, b_point,
  point_feasible, grid_base, anchor_b, ref_resid, y_ref,
  pairs, res_pre, logols_coef, est_harvey, search_seed, theta_reference,
  na_coef, theta_point_harvey, taus, qs_fn, mapped, primary_results,
  b_tab_list, gate, final_res, core, registry_entry
)
