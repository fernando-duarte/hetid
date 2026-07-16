# Harvey Gaussian multiplicative-heteroskedasticity identified sets for the
# log-variance equation: theta_hat_H(b_N) minimizes Q_H(theta; b) =
# 0.5 sum(eta + y exp(-eta)) on eps_hat(b_N)^2 = (w1 - W2 b_N)^2 over (1, PC_R),
# mapped over the mean equation's warm-refined display-tau news sets
# (mean_eq_bounds_tau) through the shared set engine. No response scaling (Harvey
# absorbs scale in its intercept); instead a deterministic no-fit stability
# precheck guards the constructor, and a reduced five-start sensitivity gate
# re-polishes before any result ships. Console block in the driver helpers. Run
# via run_pipeline.R after run_sets.R, before render_panels.R.

source(paper_path("log_variance", "estimators", "harvey", "estimator.R"))
source(paper_path("log_variance", "estimators", "harvey", "sensitivity_and_reporting.R"))

# one preparation path: validate the frozen inputs and the mean-zero PC_R
# convention before any precheck or fit
harvey_inputs <- logvar_ppml_validate_inputs(
  log_var_eq$inputs, log_var_eq$sample_contract
)
stopifnot(max(abs(colMeans(harvey_inputs$pcr))) < 1e-8)
w1 <- harvey_inputs$w1
w2 <- harvey_inputs$w2
pcr <- harvey_inputs$pcr
qtr <- harvey_inputs$qtr
x_mat <- cbind(1, pcr)
colnames(x_mat) <- c("(Intercept)", colnames(pcr))
chol_xx <- chol(crossprod(x_mat))

# search anchor and seed: the Lewbel point when finite and unit-omega feasible in
# the baseline box, else the first coarsened baseline-grid point
b_point <- set_id_mean_eq$theta_table$point
tau_base <- set_id_mean_eq$tau_display[1]
qs_base <- tau_quadratic_system(set_id_mean_eq$gamma, tau_base, set_id_mean_eq$moments)
b_tab_base <- mean_eq_bounds_tau[[sprintf("%.17g", tau_base)]]
stopifnot(!is.null(b_tab_base))
point_feasible <- !anyNA(b_point) &&
  .feasibility_residual(qs_base, b_point, rep(1, length(qs_base$A_i))) <= 0
grid_base <- logvar_coarsen_grid(
  logvar_feasible_grid(qs_base, b_tab_base$set_lower, b_tab_base$set_upper, 41L),
  logvar_harvey_grid_cap
)
stopifnot(nrow(grid_base) > 0L)
anchor_b <- if (point_feasible) b_point else grid_base[1, ]

# naive reference squared residuals (matched by qtr), shared by the reference
# column and the precheck's reference pair
ref_rows <- match(qtr, set_id_mean_eq$qtr)
stopifnot(!anyNA(ref_rows))
ref_resid <- as.numeric(stats::residuals(set_id_mean_eq$ols_fit)[ref_rows])
y_ref <- ref_resid^2

# stability precheck first: the pure no-fit evaluation over the exact three
# response/start pairs must pass before any Harvey constructor or fit runs, so
# constructor-time point fitting cannot precede the guard
pairs <- logvar_harvey_precheck_pairs(
  y_ref, anchor_b, w1, w2, x_mat, log_var_eq_ppml$start_bundle
)
res_pre <- logvar_harvey_stability_precheck(pairs, x_mat, chol_xx)
stopifnot(isTRUE(attr(res_pre, "passed")))

# estimator, guarded by the precheck: PPML seeds rung one, PPML's own cached
# evaluator is the arbitrary-b retry rung (never the Harvey engine cache), and
# the benchmark log-OLS coefficients are the shifted diagnostic rung
logols_coef <- stats::setNames(log_var_eq$table$ols, log_var_eq$table$coef)
est_harvey <- logvar_harvey_estimator(
  w1, w2, pcr, qtr,
  b_point = if (point_feasible) b_point else NULL,
  ppml_bundle = log_var_eq_ppml$start_bundle,
  ppml_start_at_b = log_var_eq_ppml$estimator$fit_at_b,
  logols_coef = logols_coef
)
stopifnot(identical(est_harvey$metadata$sample_id, log_var_eq$sample_id))
search_seed <- if (point_feasible) b_point else anchor_b

# reference column (coefficients only; SEs deferred) and the Lewbel-point column
# read straight off the estimator so no duplicate point solve happens; a missing
# point renders NA (the panel prints "--")
ref_fit <- logvar_harvey_fit_response(y_ref, x_mat)
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

# per display tau (keyed sprintf("%.17g", tau) so the gate and cache align): scan
# the warm-refined display box through the shared engine with three separated
# starts per side, one cache, a fresh per-tau budget, and prior-tau warm extras
taus <- set_id_mean_eq$tau_display
qs_fn <- function(tau) tau_quadratic_system(set_id_mean_eq$gamma, tau, set_id_mean_eq$moments)
harvey_cache <- new.env(parent = emptyenv())
primary_results <- list()
b_tab_list <- list()
primary_diag <- list()
warm_extra <- NULL
for (idx in seq_along(taus)) {
  tau_i <- taus[idx]
  key_i <- sprintf("%.17g", tau_i)
  b_tab_i <- mean_eq_bounds_tau[[key_i]]
  stopifnot(!is.null(b_tab_i))
  qs_i <- qs_fn(tau_i)
  harvey_bs <- logvar_budget_state(logvar_harvey_fit_budget)
  res_i <- logvar_engine_set_at_tau(
    est_harvey, qs_i, b_tab_i,
    b_seed = search_seed,
    max_grid_points = logvar_harvey_grid_cap,
    max_fit_evals = logvar_harvey_fit_budget, starts_per_side = 3L,
    cache = harvey_cache, budget_state = harvey_bs, extra_starts = warm_extra,
    cold_start_check = TRUE, tau = tau_i
  )
  primary_results[[key_i]] <- res_i
  b_tab_list[[key_i]] <- b_tab_i
  primary_diag[[key_i]] <- res_i$diagnostics
  warm_extra <- logvar_ppml_bounded_args(res_i$schema)
}

# reduced sensitivity gate before anything renders: a five-start re-polish of the
# same warm-refined boxes over a fresh cache and budget, applied through Harvey's
# union/demotion machinery so moved or mismatched bounded sides go unreliable
gate <- logvar_harvey_sensitivity_gate(
  est_harvey, taus, b_tab_list, search_seed,
  logvar_harvey_grid_cap, logvar_harvey_sensitivity_fit_budget,
  qs_fn, primary_results
)
final_res <- gate$results

# fragility: min over each tau's bounded endpoint args and the seed of the
# pointwise min_t |eps_hat_t(b)| (the PPML-driver idiom)
min_feasible_eps <- function(schema) {
  args <- c(
    schema$arg_lower[schema$lower_status == "bounded"],
    schema$arg_upper[schema$upper_status == "bounded"]
  )
  args <- c(args[!vapply(args, anyNA, logical(1))], list(search_seed))
  min(vapply(args, function(a) min(abs(drop(w1 - w2 %*% a))), numeric(1)))
}

base_tab <- final_res[[sprintf("%.17g", taus[1])]]$table
stopifnot(identical(base_tab$coef, colnames(x_mat)))
log_var_eq_harvey <- list(
  sample = list(n = length(qtr), span = range(qtr)),
  sample_id = log_var_eq$sample_id,
  table = data.frame(
    coef = colnames(x_mat), reference = unname(theta_reference),
    point = unname(theta_point_harvey), set_lower = base_tab$set_lower,
    set_upper = base_tab$set_upper, status = base_tab$status, row.names = NULL
  ),
  sets = lapply(final_res, function(r) r$table),
  schema = lapply(final_res, function(r) r$schema),
  n_feasible = vapply(primary_results, function(r) r$n_feasible, integer(1)),
  counts = primary_diag,
  fit_failures = vapply(primary_results, function(r) r$diagnostics$n_failed, integer(1)),
  min_feasible_abs_eps = vapply(final_res, function(r) min_feasible_eps(r$schema), numeric(1)),
  estimator = est_harvey, start_bundle_used = est_harvey$start_bundle,
  point_start_rung = est_harvey$point_start_rung,
  sensitivity_audit = list(audit = gate$audit, meta = gate$metadata),
  census_comparability = log_var_eq$n_cross
)

# register the Harvey figure entry (engine opts and the shared cache included);
# this entry and harvey_cache must survive cleanup
logvar_bounds_tau_registry[[length(logvar_bounds_tau_registry) + 1L]] <- list(
  estimator = est_harvey,
  schema = lapply(final_res, function(r) r$schema),
  sets = lapply(final_res, function(r) r$table), b_seed = search_seed,
  engine_opts = list(
    max_grid_points = logvar_harvey_grid_cap,
    max_fit_evals = logvar_harvey_fit_budget,
    starts_per_side = 3L, cache = harvey_cache
  ),
  output_path = logvar_bounds_tau_path(out_dir, est_harvey$metadata)
)

logvar_harvey_report(log_var_eq_harvey, taus)

# remove only scratch locals: keep log_var_eq_harvey, logvar_bounds_tau_registry,
# harvey_cache (registry-referenced), and every sourced function
rm(
  harvey_inputs, w1, w2, pcr, qtr, x_mat, chol_xx, b_point, tau_base, qs_base,
  b_tab_base, point_feasible, grid_base, anchor_b, ref_rows, ref_resid, y_ref,
  pairs, res_pre, logols_coef, est_harvey, search_seed, theta_reference,
  na_coef, theta_point_harvey, taus, qs_fn, primary_results, b_tab_list,
  primary_diag, warm_extra, idx, tau_i, key_i, b_tab_i, qs_i, harvey_bs, res_i,
  gate, final_res, min_feasible_eps, base_tab
)
