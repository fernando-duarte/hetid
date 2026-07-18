# Fitted median-|residual| envelope for the median (LAD) estimator at the
# baseline mean-equation slack, the analog of run.R's PPML and
# Harvey volatility envelopes. The LAD is nonsmooth with a punctured domain and
# no Jacobian, so the engine's per-date derivative-free profile is not used here;
# instead the identified news set is grid-sampled, the LAD is fit at each feasible
# b_N, and the per-date envelope is the min/max of the fitted median-log-variance
# path R'theta^0.5 over those fits -- an attained inner hull, the same claim the
# shared caption already makes. exp(0.5 R'theta^0.5) is the fitted conditional
# median |residual| (not the SD: the LAD imposes no conditional-normality scale
# correction). Guarded on log_var_eq_lad so it renders only when the LAD map ran.
# Reuses logvar_fitted_vol_data / _render / _path. Run via run_pipeline.R after
# run.R.

paper_source_once(paper_path("log_variance", "figures", "fitted_volatility", "envelope.R"))
paper_source_once(paper_path("log_variance", "figures", "fitted_volatility", "plot.R"))

lad_result <- paper_logvar_result("lad", required = FALSE)
if (!is.null(lad_result)) {
  lad_vol_est <- lad_result$estimator
  lad_vol_tau <- set_id_mean_eq$tau_baseline
  lad_vol_key <- paper_tau_key(lad_vol_tau)
  lad_vol_b_tab <- mean_eq_bounds_tau[[lad_vol_key]]
  stopifnot(!is.null(lad_vol_b_tab))
  lad_vol_qs <- tau_quadratic_system(
    set_id_mean_eq$gamma, lad_vol_tau, set_id_mean_eq$moments
  )
  lad_vol_qtr <- log_var_eq$inputs$qtr
  lad_vol_x <- logvar_design_matrix(
    log_var_eq$inputs$pcr,
    PAPER_ANALYSIS_CONTRACT$model$return_pc_cols
  )
  stopifnot(identical(colnames(lad_vol_x), lad_vol_est$coef_labels))

  # feasible b_N grid over the joint identified set (the LAD map's grid builder);
  # the tau = 0 Lewbel point, when feasible, is prepended so the red point curve
  # sits inside the hull by construction
  lad_vol_grid <- logvar_coarsen_grid(
    logvar_feasible_grid(
      lad_vol_qs,
      lad_vol_b_tab$set_lower,
      lad_vol_b_tab$set_upper,
      LOGVAR_SEARCH_CONTROL$fitted_lad_grid_n
    ),
    LOGVAR_SEARCH_CONTROL$fitted_lad_grid_cap
  )
  stopifnot(nrow(lad_vol_grid) > 0L)
  lad_vol_bpoint <- set_id_mean_eq$theta_table$point
  lad_vol_point_ok <- !anyNA(lad_vol_bpoint) &&
    .feasibility_residual(
      lad_vol_qs, lad_vol_bpoint, rep(1, length(lad_vol_qs$A_i))
    ) <= 0
  if (lad_vol_point_ok) lad_vol_grid <- rbind(lad_vol_bpoint, lad_vol_grid)

  # fit the LAD at each feasible b_N; keep the coefficient vectors of the ok fits
  # (a br fit is undefined on a residual crossing, so those b_N drop out honestly)
  lad_vol_fits <- lapply(
    seq_len(nrow(lad_vol_grid)), function(i) lad_vol_est$fit_at_b(lad_vol_grid[i, ])
  )
  lad_vol_ok <- vapply(
    lad_vol_fits, logvar_fit_ok, logical(1)
  )
  stopifnot(any(lad_vol_ok))
  lad_vol_theta <- vapply(
    lad_vol_fits[lad_vol_ok],
    function(f) unname(f$coef[colnames(lad_vol_x)]), numeric(ncol(lad_vol_x))
  )
  lad_vol_eta <- lad_vol_x %*% lad_vol_theta
  stopifnot(all(is.finite(lad_vol_eta)))

  # the tau = 0 Lewbel-point path (NA when the point is infeasible or fails)
  lad_vol_point_eta <- rep(NA_real_, length(lad_vol_qtr))
  if (lad_vol_point_ok) {
    lad_vol_pfit <- lad_vol_est$fit_at_b(lad_vol_bpoint)
    if (logvar_fit_ok(lad_vol_pfit)) {
      lad_vol_point_eta <- drop(
        lad_vol_x %*% unname(lad_vol_pfit$coef[colnames(lad_vol_x)])
      )
    }
  }

  lad_vol_schema <- data.frame(
    lower = apply(lad_vol_eta, 1, min), upper = apply(lad_vol_eta, 1, max),
    lower_status = "bounded", upper_status = "bounded",
    lower_provenance = "grid_inner_hull", upper_provenance = "grid_inner_hull",
    stringsAsFactors = FALSE
  )
  lad_vol_envelope <- list(
    data = logvar_fitted_vol_data(lad_vol_qtr, lad_vol_schema, lad_vol_point_eta),
    metadata = list(
      estimator = "lad", sample_id = lad_result$sample_id,
      tau = lad_vol_tau, response_scale = "log", grid_only = TRUE,
      grid_n = LOGVAR_SEARCH_CONTROL$fitted_lad_grid_n,
      grid_cap = LOGVAR_SEARCH_CONTROL$fitted_lad_grid_cap,
      estimand = paste(
        "fitted conditional median absolute value of the consumption-growth",
        "structural residual"
      ),
      envelope = "grid-sampled inner hull of the fitted median-|residual| path"
    )
  )
  lad_vol_path <- logvar_fitted_vol_path("lad")
  logvar_fitted_vol_render(lad_vol_envelope, lad_vol_path)
  lad_vol_envelope$output_path <- lad_vol_path
  log_var_eq_fitted_volatility_lad <- lad_vol_envelope
  cat(sprintf(
    "fitted-volatility envelope (lad): %d dates, %d/%d feasible LAD fits; wrote %s\n",
    nrow(lad_vol_envelope$data), sum(lad_vol_ok), length(lad_vol_ok), lad_vol_path
  ))

  rm(
    lad_vol_est, lad_vol_tau, lad_vol_key, lad_vol_b_tab, lad_vol_qs,
    lad_vol_qtr, lad_vol_x, lad_vol_grid, lad_vol_bpoint, lad_vol_point_ok,
    lad_vol_fits, lad_vol_ok, lad_vol_theta, lad_vol_eta, lad_vol_point_eta,
    lad_vol_schema, lad_vol_envelope, lad_vol_path
  )
}
