paper_source_once(paper_path("support", "reporting", "inference.R"))
paper_source_once(paper_path("support", "statistics", "api.R"))
paper_source_once(paper_path("log_variance", "inference", "set_envelope.R"))
paper_source_once(paper_path("log_variance", "inference", "set_bootstrap_core.R"))
paper_source_once(paper_path("log_variance", "inference", "set_bootstrap_builders.R"))
paper_source_once(paper_path(
  "log_variance", "inference", "set_bootstrap_artifacts.R"
))
stopifnot(is.finite(boot_reps), boot_reps >= 2L)
inference_alpha <- PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha
ests <- paper_logvar_estimator_ids(
  capability = "set_bootstrap",
  primary = TRUE
)
estimator_results <- stats::setNames(
  lapply(ests, paper_logvar_result),
  ests
)
scale_val <-
  estimator_results[["ppml"]]$estimator$metadata$response_scale_value
logols_val <- stats::setNames(log_var_eq$table$ols, log_var_eq$table$coef)
builders <- logvar_set_boot_builders(scale_val, logols_val)
prep <- logvar_set_boot_prepare(set_id_mean_eq, lag_asset_return_pc)
display_taus <- set_id_mean_eq$tau_display
disp_key <- vapply(display_taus, paper_tau_key, character(1))
disp_idx <- seq_along(display_taus) + 1L # slots in spec$taus for the display taus
spec <- list(
  coefs = log_var_eq$table$coef, gamma = set_id_mean_eq$gamma,
  taus = c(0, display_taus), # tau = 0 first (point acceptance diagnostic)
  x_cols = set_id_mean_eq$x_cols, y1_col = set_id_mean_eq$y1_col,
  y2_cols = set_id_mean_eq$y2_cols, z_col = z_col,
  impose_null = impose_beta2r_null, pc_cols = prep$pc_cols,
  grid_cap = logvar_boot_grid_cap, fit_budget = logvar_boot_fit_budget,
  estimator_ids = ests, builders = builders
)
anchor <- logvar_set_boot_draw(prep$data, spec)
anchor_live <- function(est) {
  any(unlist(lapply(anchor[[est]], function(r) {
    c(r$lower_status, r$upper_status) != PAPER_ENDPOINT_STATUS[["failed"]]
  })))
}
stopifnot(
  all(vapply(ests, anchor_live, logical(1))),
  all(vapply(ests, function(est) {
    identical(
      estimator_results[[est]]$sets[[
        paper_tau_key(set_id_mean_eq$tau_baseline)
      ]]$coef,
      spec$coefs
    )
  }, logical(1)))
)
full <- lapply(ests, function(est) {
  lapply(anchor[[est]], function(rec) {
    data.frame(
      coef = spec$coefs, set_lower = rec$lower, set_upper = rec$upper,
      lower_status = rec$lower_status, upper_status = rec$upper_status,
      stringsAsFactors = FALSE
    )
  })
})
names(full) <- ests
n <- nrow(prep$data)
m <- if (is.null(logvar_boot_m)) n else logvar_boot_m
draw_logvar <- function(index, draw_id) {
  logvar_set_boot_draw(
    prep$data[index, , drop = FALSE],
    spec
  )
}
boot_run <- paper_run_mbb_draws(
  n_draws = boot_reps,
  sample_size = n,
  block_length = boot_block,
  truncation = m,
  cores = logvar_boot_cores,
  seed = boot_seed,
  draw = draw_logvar
)
boot_idx <- boot_run$indices
boot_t0 <- boot_run$started_at
raw <- boot_run$draws
n_failed <- sum(vapply(raw, is.character, logical(1)))
if (n_failed > paper_bootstrap_failure_limit(boot_reps)) {
  stop("vol set-endpoint bootstrap: ", n_failed, " of ", boot_reps, " draws failed")
}
collected <- logvar_set_boot_collect(raw, spec)
endpoint_stability <-
  PAPER_INFERENCE_SEARCH_CONTROL$logvar_endpoint$stability_share
envelope <- function(coll) {
  e <- lapply(ests, function(est) {
    ee <- lapply(seq_along(display_taus), function(d) {
      logvar_endpoint_envelope(
        coll[[est]][[disp_idx[d]]], full[[est]][[disp_idx[d]]],
        alpha = inference_alpha, stability = endpoint_stability
      )
    })
    names(ee) <- disp_key
    ee
  })
  names(e) <- ests
  e
}
prim_env <- envelope(collected)
c_sim <- lapply(ests, function(est) {
  stats::setNames(vapply(seq_along(display_taus), function(d) {
    logvar_simultaneous_critical(
      collected[[est]][[disp_idx[d]]], full[[est]][[disp_idx[d]]],
      alpha = inference_alpha, stability = endpoint_stability
    )
  }, numeric(1)), disp_key)
})
names(c_sim) <- ests
n_reported <- sum(vapply(ests, function(est) {
  sum(vapply(prim_env[[est]], function(df) sum(df$reason == "reported"), integer(1)))
}, integer(1)))
se_type <- stats::setNames(
  vapply(
    ests,
    function(est) PAPER_REPORTING_CONTROL[[est]]$se_type,
    character(1)
  ),
  ests
)
se_obj <- estimator_results
tau0 <- lapply(ests, function(est) {
  sd_boot <- apply(collected[[est]][[1]]$upper, 2, function(v) {
    ok <- is.finite(v)
    if (sum(ok) >= 2L) robust_scale(v[ok]) else NA_real_
  })
  se_df <- se_obj[[est]]$se$point
  se_an <- stats::setNames(se_df[[se_type[[est]]]], se_df$coef)[spec$coefs]
  message(sprintf(
    "  %s tau=0 bootstrap SD / analytic %s SE: %s", est, se_type[[est]],
    paste(
      paper_format_general(
        sd_boot / se_an,
        PAPER_REPORTING_CONTROL$precision$console_significant
      ),
      collapse = " "
    )
  ))
  data.frame(coef = spec$coefs, sd_boot = sd_boot, se_analytic = se_an, ratio = sd_boot / se_an)
})
names(tau0) <- ests
sens_env <- NULL
if (!is.null(logvar_boot_block_sens)) {
  sensitivity_reps <- paper_bootstrap_sensitivity_reps(boot_reps)
  sens_run <- paper_run_mbb_draws(
    n_draws = sensitivity_reps,
    sample_size = n,
    block_length = logvar_boot_block_sens,
    truncation = m,
    cores = logvar_boot_cores,
    seed = boot_seed,
    draw = draw_logvar
  )
  sens_idx <- sens_run$indices
  sens_env <- envelope(
    logvar_set_boot_collect(sens_run$draws, spec)
  )
}
log_var_eq_set_boot <- c(
  prim_env,
  list(
    b_reps = boot_reps, block = boot_block,
    seed = boot_seed, m = m,
    inference_contract =
      PAPER_ANALYSIS_CONTRACT$inference,
    coverage_target = "whole-set outer envelope",
    c_sim = c_sim, tau0 = tau0
  )
)
write_logvar_set_boot_artifacts(
  ests = ests,
  display_taus = display_taus,
  disp_key = disp_key,
  disp_idx = disp_idx,
  prim_env = prim_env,
  c_sim = c_sim,
  full = full,
  se_obj = se_obj,
  sens_env = sens_env,
  tau0 = tau0,
  spec = spec,
  collected = collected,
  boot_reps = boot_reps,
  boot_block = boot_block,
  boot_seed = boot_seed,
  m = m
)
cat(sprintf(
  "vol set-endpoint bootstrap: B = %d, block = %d, %d failed, %d reported cells, %.1f min\n",
  boot_reps, boot_block, n_failed, n_reported,
  as.numeric(difftime(Sys.time(), boot_t0, units = "mins"))
))
