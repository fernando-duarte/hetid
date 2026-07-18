# Moving-block bootstrap for the vol-equation set-identified endpoints: per block
# resample re-run the whole pipeline (mean eq, Lewbel set, PPML/Harvey maps at every
# tau), then form the centered max-root OUTER confidence envelope for whole-set
# containment; tau = 0 is carried first as a point-SE acceptance diagnostic.
# Deterministic given boot_seed. Writes draws RDS and diagnostics via run_pipeline.R.
paper_source_once(paper_path("support", "statistics", "api.R"))
paper_source_once(paper_path("log_variance", "inference", "set_envelope.R"))
paper_source_once(paper_path("log_variance", "inference", "set_bootstrap_core.R"))
paper_source_once(paper_path("log_variance", "inference", "set_bootstrap_builders.R"))
stopifnot(is.finite(boot_reps), boot_reps >= 2L)
inference_alpha <- PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha
# Builders reuse the frozen PPML response scale and Harvey log-OLS start from run_sets.R.
scale_val <- log_var_eq_ppml$estimator$metadata$response_scale_value
logols_val <- stats::setNames(log_var_eq$table$ols, log_var_eq$table$coef)
builders <- logvar_set_boot_builders(scale_val, logols_val)
build_ppml <- builders$ppml
build_harvey <- builders$harvey
prep <- logvar_set_boot_prepare(set_id_mean_eq, lag_asset_return_pc)
display_taus <- set_id_mean_eq$tau_display
ests <- c("ppml", "harvey")
disp_key <- vapply(display_taus, paper_tau_key, character(1))
disp_idx <- seq_along(display_taus) + 1L # slots in spec$taus for the display taus
spec <- list(
  coefs = log_var_eq$table$coef, gamma = set_id_mean_eq$gamma,
  taus = c(0, display_taus), # tau = 0 first (point acceptance diagnostic)
  x_cols = set_id_mean_eq$x_cols, y1_col = set_id_mean_eq$y1_col,
  y2_cols = set_id_mean_eq$y2_cols, z_col = z_col,
  impose_null = impose_beta2r_null, pc_cols = prep$pc_cols,
  grid_cap = logvar_boot_grid_cap, fit_budget = logvar_boot_fit_budget,
  build_ppml = build_ppml, build_harvey = build_harvey
)
# Bootstrap-consistent anchor = identity-resample re-estimate = envelope center (NOT
# the published sets). An all-"failed" anchor = spec$coefs/engine-schema mismatch.
anchor <- logvar_set_boot_draw(prep$data, spec)
anchor_live <- function(est) {
  any(unlist(lapply(anchor[[est]], function(r) {
    c(r$lower_status, r$upper_status) != "failed"
  })))
}
stopifnot(
  anchor_live("ppml"), anchor_live("harvey"),
  identical(
    log_var_eq_ppml$sets[[paper_tau_key(set_id_mean_eq$tau_baseline)]]$coef,
    spec$coefs
  ),
  identical(
    log_var_eq_harvey$sets[[paper_tau_key(set_id_mean_eq$tau_baseline)]]$coef,
    spec$coefs
  )
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
# Indices drawn up front; block starts range over the WHOLE series, truncated to m.
n <- nrow(prep$data)
m <- if (is.null(logvar_boot_m)) n else logvar_boot_m
run_draws <- function(idx_list) {
  fn <- function(idx) {
    tryCatch(
      logvar_set_boot_draw(prep$data[idx, , drop = FALSE], spec),
      error = function(e) conditionMessage(e)
    )
  }
  if (isTRUE(logvar_boot_cores > 1L)) {
    parallel::mclapply(idx_list, fn, mc.cores = logvar_boot_cores, mc.set.seed = TRUE)
  } else {
    lapply(idx_list, fn)
  }
}
set.seed(boot_seed)
boot_idx <- lapply(seq_len(boot_reps), function(b) mbb_index(n, boot_block)[seq_len(m)])
boot_t0 <- Sys.time()
raw <- run_draws(boot_idx)
# a large errored share means the resampling is broken (not unboundedness), so stop
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
# tau = 0 acceptance diagnostic: the set collapses to a point (upper == lower), so
# robust_scale of the upper draws is the boot point SD; expect it >= the analytic SE.
se_type <- list(ppml = logvar_ppml_se_type, harvey = logvar_harvey_se_type)
se_obj <- list(ppml = log_var_eq_ppml, harvey = log_var_eq_harvey)
tau0 <- lapply(ests, function(est) {
  sd_boot <- apply(collected[[est]][[1]]$upper, 2, function(v) {
    ok <- is.finite(v)
    if (sum(ok) >= 2L) robust_scale(v[ok]) else NA_real_
  })
  se_df <- se_obj[[est]]$se$point
  se_an <- stats::setNames(se_df[[se_type[[est]]]], se_df$coef)[spec$coefs]
  message(sprintf(
    "  %s tau=0 bootstrap SD / analytic %s SE: %s", est, se_type[[est]],
    paste(sprintf("%.3g", sd_boot / se_an), collapse = " ")
  ))
  data.frame(coef = spec$coefs, sd_boot = sd_boot, se_analytic = se_an, ratio = sd_boot / se_an)
})
names(tau0) <- ests
# ell = 8 block sensitivity (reports mandate it): own seed, reduced reps; CIs to diag.
sens_env <- NULL
if (!is.null(logvar_boot_block_sens)) {
  set.seed(boot_seed)
  sensitivity_reps <- paper_bootstrap_sensitivity_reps(boot_reps)
  sens_idx <- lapply(seq_len(sensitivity_reps), function(b) {
    mbb_index(n, logvar_boot_block_sens)[seq_len(m)]
  })
  sens_env <- envelope(logvar_set_boot_collect(run_draws(sens_idx), spec))
}
log_var_eq_set_boot <- list(
  ppml = prim_env$ppml, harvey = prim_env$harvey,
  b_reps = boot_reps, block = boot_block, seed = boot_seed, m = m,
  coverage_target = "whole-set outer envelope", c_sim = c_sim, tau0 = tau0
)
diag_rows <- list()
for (est in ests) {
  for (d in seq_along(display_taus)) {
    key <- disp_key[d]
    pub <- se_obj[[est]]$sets[[key]]
    stopifnot(identical(pub$coef, spec$coefs))
    sci <- if (is.null(sens_env)) {
      data.frame(ci_lower = NA_real_, ci_upper = NA_real_)
    } else {
      sens_env[[est]][[key]][c("ci_lower", "ci_upper")]
    }
    diag_rows[[length(diag_rows) + 1L]] <- data.frame(
      estimator = est, tau = display_taus[d], prim_env[[est]][[key]],
      c_sim = c_sim[[est]][[key]],
      anchor_lower = full[[est]][[disp_idx[d]]]$set_lower,
      anchor_upper = full[[est]][[disp_idx[d]]]$set_upper,
      published_lower = pub$set_lower, published_upper = pub$set_upper,
      published_status = pub$status,
      sens_ci_lower = sci$ci_lower, sens_ci_upper = sci$ci_upper,
      tau0_sd_boot = tau0[[est]]$sd_boot, tau0_se_analytic = tau0[[est]]$se_analytic,
      tau0_ratio = tau0[[est]]$ratio, row.names = NULL, stringsAsFactors = FALSE
    )
  }
}
paper_write_typed_csv(
  do.call(rbind, diag_rows),
  artifact_path("log_variance_inference_diagnostics"),
  "log_variance_inference_diagnostics"
)
paper_write_exact_rds(
  list(
    b_reps = boot_reps, block = boot_block, seed = boot_seed, m = m,
    taus = spec$taus, coefs = spec$coefs, collected = collected, full = full
  ),
  artifact_path("log_variance_bootstrap_draws"),
  "log_variance_bootstrap_draws"
)
cat(sprintf(
  "vol set-endpoint bootstrap: B = %d, block = %d, %d failed, %d reported cells, %.1f min\n",
  boot_reps, boot_block, n_failed, n_reported,
  as.numeric(difftime(Sys.time(), boot_t0, units = "mins"))
))
rm(list = intersect(ls(), c(
  "scale_val", "logols_val", "builders", "build_ppml", "build_harvey", "prep", "display_taus",
  "ests", "disp_key", "disp_idx", "spec", "anchor", "anchor_live", "full", "n", "m",
  "run_draws", "boot_idx", "boot_t0", "raw", "n_failed", "collected", "envelope",
  "prim_env", "c_sim", "n_reported", "se_type", "se_obj", "tau0", "sens_env",
  "sensitivity_reps", "sens_idx", "diag_rows", "inference_alpha", "est", "d",
  "key", "pub", "sci", "endpoint_stability"
)))
