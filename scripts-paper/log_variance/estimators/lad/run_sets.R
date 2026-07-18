# Median (LAD) log-variance identified sets: theta_hat^0.5(b_N) is the
# conditional-median regression of z(b_N) = 2 log|w1 - W2 b_N| on (1, PC_R) via
# quantreg::rq.fit(method = "br"), mapped over the mean equation's warm-refined
# display-tau news sets through the shared set engine. Each tau runs one engine
# pass, a verify-or-rerun five-start sensitivity gate that never patches a row, and
# a last-word fn nonuniqueness demotion (see set_mapping.R). The map is
# punctured at the residual crossings: no floor, no dropped rows, honest statuses,
# attained hull primary and the one-sided closure limits kept separate. The
# estimator/fit/domain and the diagnostics/console/closure helpers are sourced here
# so run_pipeline.R gains one line; run after run.R, before the panels.

paper_source_once(paper_path("log_variance", "estimators", "lad", "estimator.R"))
paper_source_once(paper_path("log_variance", "estimators", "lad", "fit.R"))
paper_source_once(paper_path("log_variance", "estimators", "lad", "crossing_domain.R"))
paper_source_once(paper_path("log_variance", "estimators", "lad", "set_mapping.R"))
paper_source_once(paper_path("log_variance", "estimators", "lad", "display_mapping.R"))
paper_source_once(paper_path("log_variance", "estimators", "lad", "set_report.R"))
paper_source_once(paper_path("log_variance", "estimators", "lad", "console_report.R"))
paper_source_once(paper_path("log_variance", "estimators", "lad", "closure_diagnostics.R"))

# Guarded orchestration: runs only with the upstream benchmark objects present, so
# sourcing this offline (the estimator test suites) defines the helpers only.
if (exists("log_var_eq") && exists("set_id_mean_eq") && exists("mean_eq_bounds_tau")) {
  # one preparation path (validate + scale anchor/seed base at the LAD grid cap);
  # the w1/w2/pcr/qtr/x_mat and anchor pieces are injected as locals here
  list2env(
    logvar_prepare_map_context(
      log_var_eq$inputs, log_var_eq$sample_contract, set_id_mean_eq,
      mean_eq_bounds_tau, LOGVAR_LAD_CONTROL$grid_cap
    ),
    environment()
  )

  # naive reference residuals (by qtr): the reference column, the frozen scale, est
  ref_resid <- logvar_reference_residuals(qtr, set_id_mean_eq)
  e_scale_ref <- logvar_lad_scale_reference(ref_resid)

  est_lad <- logvar_lad_estimator(
    w1,
    w2,
    pcr,
    qtr,
    e_ref = ref_resid,
    control = LOGVAR_LAD_CONTROL
  )
  stopifnot(identical(est_lad$metadata$sample_id, log_var_eq$sample_id))

  # reference column: the br median fit on the log-squared naive residuals
  ref_fit <- logvar_lad_fit_response(
    2 * log(abs(ref_resid)),
    x_mat,
    LOGVAR_LAD_CONTROL
  )
  if (!logvar_fit_ok(ref_fit)) {
    stop(sprintf(
      "log_var_eq_lad_sets: reference LAD fit failed (%s)",
      ref_fit$diagnostics$error_class
    ))
  }
  theta_reference <- ref_fit$coef
  na_coef <- stats::setNames(rep(NA_real_, ncol(x_mat)), colnames(x_mat))

  # Lewbel-point column: br fit at b_point (from the context above); NA (panel
  # "--") when undefined, and NA with a reason when the point sits on a crossing
  pfit <- NULL
  theta_point <- na_coef
  if (!anyNA(b_point)) {
    pfit <- est_lad$fit_at_b(b_point)
    if (logvar_fit_ok(pfit)) {
      theta_point <- pfit$coef
    } else {
      cat(sprintf(
        "  LAD Lewbel point unavailable: %s (min|eps| at point = %s); rendering --\n",
        pfit$diagnostics$domain_state,
        paper_format_general(
          log_var_eq$min_abs_eps_point,
          PAPER_REPORTING_CONTROL$precision$console_significant
        )
      ))
    }
  }

  # search seed: the Lewbel point when feasible, else the first coarsened grid pt
  search_seed <- if (point_feasible) b_point else grid_base[1, ]

  # per display tau: one cache across taus, a fresh budget per tau, no warm chain
  taus <- set_id_mean_eq$tau_display
  qs_fn <- mean_quadratic_system_factory(set_id_mean_eq)
  cfg <- list(
    grid_cap = LOGVAR_LAD_CONTROL$grid_cap,
    fit_budget = LOGVAR_LAD_CONTROL$fit_budget,
    phase_caps = LOGVAR_LAD_CONTROL$phase_caps,
    b_seed = search_seed,
    w1 = w1, w2 = w2, x_mat = x_mat, e_scale_ref = e_scale_ref
  )
  lad_cache <- new.env(parent = emptyenv())
  map_one <- logvar_lad_display_mapper(
    estimator = est_lad,
    config = cfg,
    cache = lad_cache,
    w1 = w1,
    w2 = w2,
    x_mat = x_mat,
    e_scale_ref = e_scale_ref,
    coef_labels = colnames(x_mat),
    qtr = qtr,
    sample_id = log_var_eq$sample_id,
    spec_id = est_lad$metadata$spec_id,
    search_seed = search_seed
  )
  mapped <- logvar_map_display_taus(
    taus = taus,
    bounds_tau = mean_eq_bounds_tau,
    quadratic_at_tau = qs_fn,
    map_one = map_one
  )
  final_res <- mapped$results
  details <- lapply(mapped$records, `[[`, "detail")
  sens_audit <- lapply(details, `[[`, "audit")
  tail_cls <- lapply(details, `[[`, "paths")
  closure_by_tau <- lapply(details, `[[`, "closure")
  wit_cov <- lapply(details, `[[`, "witness_coverage")
  min_eps <- vapply(details, `[[`, numeric(1), "min_eps")

  core <- logvar_set_result_core(
    qtr = qtr,
    sample_id = log_var_eq$sample_id,
    coef_labels = colnames(x_mat),
    reference = theta_reference,
    point = theta_point,
    baseline_tau = set_id_mean_eq$tau_baseline,
    primary_results = final_res,
    final_results = final_res,
    w1 = w1,
    w2 = w2,
    search_seed = search_seed
  )
  log_var_eq_lad <- c(core, list(
    attained_hull = lapply(final_res, function(r) r$table),
    closure_diagnostics = closure_by_tau,
    min_feasible_abs_eps = stats::setNames(min_eps, names(final_res)),
    witness_coverage = wit_cov,
    tail_classifications = tail_cls,
    sensitivity_audit = sens_audit,
    guard = list(
      ratio = LOGVAR_LAD_CONTROL$guard_ratio,
      e_scale_ref = e_scale_ref
    ),
    estimator = est_lad, start_bundle = NULL,
    quantreg_version = as.character(utils::packageVersion("quantreg")),
    census_comparability = log_var_eq$n_cross
  ))

  # closure artifact: startup lifecycle cleanup guarantees an absent target
  lad_out <- out_dir
  lad_csv <- artifact_path("lad_closure_diagnostics")
  write_logvar_lad_closure_csv(
    do.call(rbind, c(list(logvar_lad_closure_schema()), unname(closure_by_tau))),
    lad_csv
  )

  # register the median bounds-by-tau figure entry (attained-only by construction);
  # this entry and lad_cache must survive cleanup. warm_chain = FALSE holds the
  # grid-tau walk to the same single-pass, no-extra-start protocol the display taus
  # use, so the two row sets on one figure are searched alike (D3.1)
  registry_entry <- logvar_bounds_registry_entry(
    estimator = est_lad,
    results = final_res,
    b_seed = search_seed,
    warm_chain = FALSE,
    engine_opts = list(
      max_grid_points = LOGVAR_LAD_CONTROL$grid_cap,
      max_fit_evals = LOGVAR_LAD_CONTROL$fit_budget,
      starts_per_side = LOGVAR_SEARCH_CONTROL$primary_starts_per_side,
      cache = lad_cache
    )
  )
  logvar_bounds_tau_registry[[length(logvar_bounds_tau_registry) + 1L]] <-
    registry_entry

  logvar_lad_console_block(log_var_eq_lad, taus)

  # remove only scratch locals: keep log_var_eq_lad, logvar_bounds_tau_registry,
  # lad_cache (registry-referenced), and every sourced function
  rm(
    w1, w2, pcr, qtr, x_mat, ref_resid, e_scale_ref, est_lad,
    ref_fit, theta_reference, na_coef, b_point, pfit, theta_point,
    point_feasible, grid_base, search_seed, taus, qs_fn, cfg,
    map_one, mapped, final_res, details, wit_cov, sens_audit, tail_cls,
    closure_by_tau, min_eps, core, lad_out, lad_csv, registry_entry
  )
}
