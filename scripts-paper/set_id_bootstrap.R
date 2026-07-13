# Moving-block bootstrap for the set-identified mean equation: resample the
# aligned estimation frame of set_id_mean_eq.R in boot_block-quarter blocks,
# re-run the full estimation per draw (core machinery in
# scripts/utils/set_id_bootstrap_core.R), and summarize with robust endpoint
# scales, Stoye-calibrated nominal intervals, a robust tau = 0 point
# interval, and a tau* percentile range. Deterministic given boot_seed
# (run_all.R constants). Every display tau is evaluated in every draw; the
# per-draw tau* uses a coarse 0.05-step bracket (cap forced onto the grid)
# plus a short bisection. Writes the draw matrices and a per-cell diagnostics
# table to scripts-paper/output/.
# Run via run_all.R after set_id_mean_eq.R.

source("scripts/utils/identification_utils.R")
source("scripts/utils/profile_bounds_core.R")
source("scripts/utils/profile_bounds.R")
source("scripts/utils/tau_star_utils.R")
source("scripts/utils/stats_utils.R")
source("scripts/utils/set_id_inference.R")
source("scripts/utils/set_id_bootstrap_core.R")

stopifnot(is.finite(boot_reps), boot_reps >= 2L)
# moving blocks assume a gapless quarterly index; a dropped interior quarter
# would silently stitch non-adjacent periods into one block. as.numeric(), not
# as.integer(): tsibble's yearquarter has no as.integer method in the pinned
# tsibble version, so as.integer() errors on the vctrs default cast.
stopifnot(all(diff(as.numeric(set_id_mean_eq$data$qtr)) == 1L))

boot_spec <- list(
  coefs = c(set_id_mean_eq$beta1_table$coef, set_id_mean_eq$theta_table$coef),
  gamma = set_id_mean_eq$gamma,
  taus = set_id_mean_eq$tau_display,
  tau_grid = unique(
    c(seq(0, set_id_mean_eq$tau_cap, by = 0.05), set_id_mean_eq$tau_cap)
  ),
  y1_col = set_id_mean_eq$y1_col, x_cols = set_id_mean_eq$x_cols,
  y2_cols = set_id_mean_eq$y2_cols, z_col = z_col,
  impose_null = impose_beta2r_null
)

# indices drawn up front: the resampling stream cannot be perturbed if a
# solver ever consumes random numbers mid-draw, and a parallel lapply over
# boot_idx stays a drop-in change
set.seed(boot_seed)
boot_idx <- lapply(
  seq_len(boot_reps),
  function(b) mbb_index(set_id_mean_eq$sample$n, boot_block)
)
boot_t0 <- Sys.time()
boot_raw <- vector("list", boot_reps)
for (b in seq_len(boot_reps)) {
  boot_raw[[b]] <- tryCatch(
    set_id_boot_draw(set_id_mean_eq$data[boot_idx[[b]], ], boot_spec),
    error = function(e) conditionMessage(e)
  )
  if (b %% 25L == 0L) {
    cat(sprintf(
      "  endpoint bootstrap draw %d of %d (%.1f min elapsed)\n", b, boot_reps,
      as.numeric(difftime(Sys.time(), boot_t0, units = "mins"))
    ))
  }
}

collected <- set_id_boot_collect(boot_raw, boot_spec)
# a large failure share means the resampling itself is broken, not
# statistical unboundedness, so the pipeline stops rather than reporting
# scales built from the surviving minority
if (collected$n_failed > 0L) {
  cat("  failed draws by cause:\n")
  print(collected$failure_causes)
}
if (collected$n_failed > boot_reps %/% 4L) {
  stop(
    "endpoint bootstrap: ", collected$n_failed, " of ", boot_reps,
    " draws failed"
  )
}
names(collected$endpoint_draws) <- names(set_id_mean_eq$set_tables)

inference <- lapply(seq_along(boot_spec$taus), function(j) {
  st <- set_id_mean_eq$set_tables[[j]]
  endpoint_inference(
    collected$endpoint_draws[[j]]$lower, collected$endpoint_draws[[j]]$upper,
    rbind(st$beta1, st$theta),
    alpha = 0.10
  )
})
names(inference) <- names(set_id_mean_eq$set_tables)

# robust tau = 0 point inference: closed-form point plus/minus the one-sided
# normal quantile times the robust scale of the point draws, gated by the
# same half-the-draws rule as the set cells (a point interval built from a
# minority of full-rank resamples would hide the deficiency it rides on)
point_hat <- c(set_id_mean_eq$beta1_table$point, set_id_mean_eq$theta_table$point)
point_se <- apply(collected$point_draws, 2, robust_scale)
n_point_finite <- colSums(is.finite(collected$point_draws))
point_ok <- is.finite(point_hat) & is.finite(point_se) & point_se > 0 &
  n_point_finite >= boot_reps %/% 2L
point_ci <- data.frame(
  coef = boot_spec$coefs,
  lower = ifelse(point_ok, point_hat - stats::qnorm(0.95) * point_se, NA_real_),
  upper = ifelse(point_ok, point_hat + stats::qnorm(0.95) * point_se, NA_real_),
  row.names = NULL, stringsAsFactors = FALSE
)

set_id_boot <- c(
  list(
    b_reps = boot_reps, block = boot_block, seed = boot_seed,
    point_se = point_se, point_ci = point_ci,
    point_band = apply(collected$point_draws, 2, boot_band),
    tau_star_band = boot_band(collected$tau_star_draws),
    tau_star_share_bounded = mean(
      collected$tau_star_draws > set_id_mean_eq$tau_baseline,
      na.rm = TRUE
    ),
    inference = inference
  ),
  collected
)

diagnostics <- set_id_boot_diagnostics(
  collected, inference, set_id_mean_eq$set_tables, boot_spec$taus
)
utils::write.csv(
  diagnostics, file.path(out_dir, "set_id_inference_diagnostics.csv"),
  row.names = FALSE
)
saveRDS(
  set_id_boot[c(
    "b_reps", "block", "seed", "point_draws", "endpoint_draws",
    "tau_star_draws"
  )],
  file.path(out_dir, "set_id_boot_draws.rds")
)

cat(sprintf(
  "endpoint bootstrap: B = %d, block = %d, %.1f min; tau* range [%.3g, %.3g] (n = %d)\n",
  boot_reps, boot_block,
  as.numeric(difftime(Sys.time(), boot_t0, units = "mins")),
  set_id_boot$tau_star_band[["p05"]], set_id_boot$tau_star_band[["p95"]],
  set_id_boot$tau_star_band[["n"]]
))
cat(sprintf(
  "  %d failed, %d capped, %d point-deficient; bounded at baseline in %.0f%% of draws\n",
  set_id_boot$n_failed, set_id_boot$n_capped, set_id_boot$n_point_deficient,
  100 * set_id_boot$tau_star_share_bounded
))
print(set_id_boot$inference[[1]], digits = 3)

rm(
  boot_spec, boot_idx, boot_t0, boot_raw, collected, inference, point_hat,
  point_se, n_point_finite, point_ok, point_ci, diagnostics, b
)
