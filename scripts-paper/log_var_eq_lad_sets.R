# Median (LAD) log-variance identified sets: theta_hat^0.5(b_N) is the
# conditional-median regression of z(b_N) = 2 log|w1 - W2 b_N| on (1, PC_R) via
# quantreg::rq.fit(method = "br"), mapped over the mean equation's warm-refined
# display-tau news sets through the shared set engine. Each tau runs one engine
# pass, a verify-or-rerun five-start sensitivity gate that never patches a row, and
# a last-word fn nonuniqueness demotion (see log_var_eq_lad_sets_map.R). The map is
# punctured at the residual crossings: no floor, no dropped rows, honest statuses,
# attained hull primary and the one-sided closure limits kept separate. The
# estimator/fit/domain and the diagnostics/console/closure helpers are sourced here
# so run_all gains one line; run after log_var_eq_joint_null.R, before the panels.

source("scripts-paper/log_var_eq_lad.R")
source("scripts-paper/log_var_eq_lad_fit.R")
source("scripts-paper/log_var_eq_lad_domain.R")
source("scripts-paper/log_var_eq_lad_sets_map.R")
source("scripts-paper/log_var_eq_lad_sets_report.R")
source("scripts-paper/log_var_eq_lad_console.R")
source("scripts-paper/log_var_eq_lad_closure.R")

# Guarded orchestration: runs only with the upstream benchmark objects present, so
# sourcing this offline (the estimator test suites) defines the helpers only.
if (exists("log_var_eq") && exists("set_id_mean_eq") && exists("mean_eq_bounds_tau")) {
  lad_inputs <- logvar_ppml_validate_inputs(
    log_var_eq$inputs, log_var_eq$sample_contract
  )
  stopifnot(max(abs(colMeans(lad_inputs$pcr))) < 1e-8)
  w1 <- lad_inputs$w1
  w2 <- lad_inputs$w2
  pcr <- lad_inputs$pcr
  qtr <- lad_inputs$qtr
  x_mat <- cbind(1, pcr)
  colnames(x_mat) <- c("(Intercept)", colnames(pcr))

  # naive reference residuals (by qtr): the reference column, the frozen scale, est
  ref_rows <- match(qtr, set_id_mean_eq$qtr)
  stopifnot(!anyNA(ref_rows))
  ref_resid <- as.numeric(stats::residuals(set_id_mean_eq$ols_fit)[ref_rows])
  e_scale_ref <- logvar_lad_scale_reference(ref_resid)

  est_lad <- logvar_lad_estimator(w1, w2, pcr, qtr, e_ref = ref_resid)
  stopifnot(identical(est_lad$metadata$sample_id, log_var_eq$sample_id))

  # reference column: the br median fit on the log-squared naive residuals
  ref_fit <- logvar_lad_fit_response(2 * log(abs(ref_resid)), x_mat)
  if (!identical(ref_fit$fit_status, "ok")) {
    stop(sprintf(
      "log_var_eq_lad_sets: reference LAD fit failed (%s)",
      ref_fit$diagnostics$error_class
    ))
  }
  theta_reference <- ref_fit$coef
  na_coef <- stats::setNames(rep(NA_real_, ncol(x_mat)), colnames(x_mat))

  # Lewbel-point column: br fit at b_point; NA (panel "--") when undefined, and NA
  # with a printed reason when the point sits on a residual crossing
  b_point <- set_id_mean_eq$theta_table$point
  pfit <- NULL
  theta_point <- na_coef
  if (!anyNA(b_point)) {
    pfit <- est_lad$fit_at_b(b_point)
    if (identical(pfit$fit_status, "ok")) {
      theta_point <- pfit$coef
    } else {
      cat(sprintf(
        "  LAD Lewbel point unavailable: %s (min|eps| at point = %.3g); rendering --\n",
        pfit$diagnostics$domain_state, log_var_eq$min_abs_eps_point
      ))
    }
  }

  tau_base <- set_id_mean_eq$tau_display[1]
  qs_base <- tau_quadratic_system(set_id_mean_eq$gamma, tau_base, set_id_mean_eq$moments)
  b_tab_base <- mean_eq_bounds_tau[[sprintf("%.17g", tau_base)]]
  stopifnot(!is.null(b_tab_base))
  point_feasible <- !anyNA(b_point) &&
    .feasibility_residual(qs_base, b_point, rep(1, length(qs_base$A_i))) <= 0
  grid_base <- logvar_coarsen_grid(
    logvar_feasible_grid(qs_base, b_tab_base$set_lower, b_tab_base$set_upper, 41L),
    logvar_lad_grid_cap
  )
  stopifnot(nrow(grid_base) > 0L)
  search_seed <- if (point_feasible) b_point else grid_base[1, ]

  # per display tau: one cache across taus, a fresh budget per tau, no warm chain
  taus <- set_id_mean_eq$tau_display
  qs_fn <- function(tau) tau_quadratic_system(set_id_mean_eq$gamma, tau, set_id_mean_eq$moments)
  cfg <- list(
    grid_cap = logvar_lad_grid_cap, fit_budget = logvar_lad_fit_budget,
    phase_caps = logvar_lad_phase_caps, b_seed = search_seed,
    w1 = w1, w2 = w2, x_mat = x_mat, e_scale_ref = e_scale_ref
  )
  lad_cache <- new.env(parent = emptyenv())
  final_res <- final_diag <- wit_cov <- sens_audit <- tail_cls <- closure_by_tau <- list()
  min_eps <- numeric(length(taus))
  for (idx in seq_along(taus)) {
    tau_i <- taus[idx]
    key_i <- sprintf("%.17g", tau_i)
    b_tab_i <- mean_eq_bounds_tau[[key_i]]
    stopifnot(!is.null(b_tab_i))
    qs_i <- qs_fn(tau_i)
    mt <- logvar_lad_map_tau(est_lad, qs_i, b_tab_i, tau_i, cfg, lad_cache)
    res_i <- mt$res
    final_res[[key_i]] <- res_i
    final_diag[[key_i]] <- res_i$diagnostics
    sens_audit[[key_i]] <- mt$audit
    delta_i <- .derive_theta_scale(qs_i)
    omega_i <- .derive_constraint_scales(qs_i, delta_i)
    geom_i <- list(
      w1 = w1, w2 = w2, x_mat = x_mat, e_scale_ref = e_scale_ref,
      delta = delta_i, omega = omega_i,
      check_feasible = local({
        qq <- qs_i
        om <- omega_i
        function(b) {
          v <- .feasibility_residual(qq, b, om)
          list(feasible = v <= 1e-10, max_violation = v)
        }
      })
    )
    paths_i <- logvar_lad_probe_report(res_i, tau_i, est_lad, geom_i, colnames(x_mat), qtr)
    tail_cls[[key_i]] <- paths_i
    closure_by_tau[[key_i]] <- logvar_lad_closure_rows(
      paths_i, "lad", log_var_eq$sample_id, est_lad$metadata$spec_id
    )
    pc <- res_i$domain_info$precheck
    wl <- if (is.null(pc)) list() else .lad_or(pc$info$witnesses, list())
    wit_cov[[key_i]] <- list(
      n_witnesses = length(wl),
      n_flagged = if (is.na(res_i$n_cross)) 0L else res_i$n_cross,
      n_unresolved = if (is.null(pc)) 0L else length(pc$unresolved),
      n_paths = sum(vapply(wl, function(wv) length(wv$anchors), integer(1)))
    )
    min_eps[idx] <- logvar_lad_min_feasible_eps(res_i$schema, w1, w2, search_seed)
  }

  base_tab <- final_res[[sprintf("%.17g", taus[1])]]$table
  stopifnot(identical(base_tab$coef, colnames(x_mat)))
  log_var_eq_lad <- list(
    sample = list(n = length(qtr), span = range(qtr)),
    sample_id = log_var_eq$sample_id,
    table = data.frame(
      coef = colnames(x_mat), reference = unname(theta_reference),
      point = unname(theta_point), set_lower = base_tab$set_lower,
      set_upper = base_tab$set_upper, status = base_tab$status, row.names = NULL
    ),
    sets = lapply(final_res, function(r) r$table),
    schema = lapply(final_res, function(r) r$schema),
    attained_hull = lapply(final_res, function(r) r$table),
    closure_diagnostics = closure_by_tau,
    counts = final_diag,
    n_feasible = vapply(final_res, function(r) r$n_feasible, integer(1)),
    min_feasible_abs_eps = stats::setNames(min_eps, names(final_res)),
    witness_coverage = wit_cov,
    tail_classifications = tail_cls,
    sensitivity_audit = sens_audit,
    guard = list(ratio = logvar_lad_guard_ratio, e_scale_ref = e_scale_ref),
    estimator = est_lad, start_bundle = NULL,
    quantreg_version = as.character(utils::packageVersion("quantreg")),
    census_comparability = log_var_eq$n_cross
  )

  # closure artifact: delete any stale copy, then require a round-tripping CSV
  lad_out <- if (exists("out_dir")) out_dir else "scripts-paper/output"
  lad_csv <- file.path(lad_out, "log_var_eq_lad_closure.csv")
  unlink(lad_csv)
  write_logvar_lad_closure_csv(
    do.call(rbind, c(list(logvar_lad_closure_schema()), unname(closure_by_tau))),
    lad_csv
  )

  # register the median bounds-by-tau figure entry (attained-only by construction);
  # this entry and lad_cache must survive cleanup. warm_chain = FALSE holds the
  # grid-tau walk to the same single-pass, no-extra-start protocol the display taus
  # use, so the two row sets on one figure are searched alike (D3.1)
  logvar_bounds_tau_registry[[length(logvar_bounds_tau_registry) + 1L]] <- list(
    estimator = est_lad,
    schema = lapply(final_res, function(r) r$schema),
    sets = lapply(final_res, function(r) r$table), b_seed = search_seed,
    warm_chain = FALSE,
    engine_opts = list(
      max_grid_points = logvar_lad_grid_cap, max_fit_evals = logvar_lad_fit_budget,
      starts_per_side = 3L, cache = lad_cache
    ),
    output_path = logvar_bounds_tau_path(out_dir, est_lad$metadata)
  )

  logvar_lad_console_block(log_var_eq_lad, taus)

  # remove only scratch locals: keep log_var_eq_lad, logvar_bounds_tau_registry,
  # lad_cache (registry-referenced), and every sourced function
  rm(
    lad_inputs, w1, w2, pcr, qtr, x_mat, ref_rows, ref_resid, e_scale_ref, est_lad,
    ref_fit, theta_reference, na_coef, b_point, pfit, theta_point, tau_base,
    qs_base, b_tab_base, point_feasible, grid_base, search_seed, taus, qs_fn, cfg,
    final_res, final_diag, wit_cov, sens_audit, tail_cls, closure_by_tau, min_eps,
    idx, tau_i, key_i, b_tab_i, qs_i, mt, res_i, delta_i, omega_i,
    geom_i, paths_i, pc, wl, base_tab, lad_out, lad_csv
  )
}
