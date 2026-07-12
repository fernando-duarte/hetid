# Moving-block bootstrap for the set-identified mean equation: resample the
# aligned estimation frame of set_id_mean_eq.R in boot_block-quarter blocks,
# re-run the full estimation per draw (reduced form, moments, tau = 0 point,
# per-coefficient intervals at each display slack, tau*), and summarize with
# endpoint standard errors, Imbens-Manski confidence intervals, and a tau*
# percentile band. Deterministic given boot_seed (run_all.R constants). The
# per-draw tau* uses a coarse 0.05-step bracket plus a short bisection; the
# full-sample 0.005-step grid would be ~5x the solver work for band-irrelevant
# precision.
# Run via run_all.R after set_id_mean_eq.R.

source("scripts/utils/identification_utils.R")
source("scripts/utils/profile_bounds_core.R")
source("scripts/utils/profile_bounds.R")
source("scripts/utils/tau_star_utils.R")
source("scripts/utils/stats_utils.R")
source("scripts/utils/set_id_inference.R")

stopifnot(is.finite(boot_reps), boot_reps >= 2L)
# moving blocks assume a gapless quarterly index; a dropped interior quarter
# would silently stitch non-adjacent periods into one block. as.numeric(), not
# as.integer(): tsibble's yearquarter has no as.integer method in the pinned
# tsibble version, so as.integer() errors on the vctrs default cast.
stopifnot(all(diff(as.numeric(set_id_mean_eq$data$qtr)) == 1L))

boot_coefs <- c(set_id_mean_eq$beta1_table$coef, set_id_mean_eq$theta_table$coef)
boot_gamma <- set_id_mean_eq$gamma
boot_taus <- set_id_mean_eq$tau_display
# per-draw tau* bracket: coarse 0.05 steps with the cap forced onto the grid
# (seq alone stops at 0.95, silently censoring high-tau* draws)
boot_tau_grid <- unique(
  c(seq(0, set_id_mean_eq$tau_cap, by = 0.05), set_id_mean_eq$tau_cap)
)

# one draw: full re-estimation on the block-resampled frame, mirroring the
# set_id_mean_eq.R steps; uncertified interval sides come back NA
boot_draw <- function(idx) {
  dat <- set_id_mean_eq$data[idx, ]
  fit1 <- stats::lm(
    stats::reformulate(set_id_mean_eq$x_cols, response = set_id_mean_eq$y1_col),
    data = dat
  )
  b1r <- stats::coef(fit1)
  w1b <- stats::residuals(fit1)
  if (impose_beta2r_null) {
    w2b <- as.matrix(dat[set_id_mean_eq$y2_cols])
    b2r <- matrix(
      0, length(set_id_mean_eq$y2_cols), length(b1r),
      dimnames = list(set_id_mean_eq$y2_cols, names(b1r))
    )
  } else {
    fit2 <- stats::lm(
      as.matrix(dat[set_id_mean_eq$y2_cols]) ~ .,
      data = dat[set_id_mean_eq$x_cols]
    )
    w2b <- stats::residuals(fit2)
    b2r <- t(stats::coef(fit2))
  }
  zb <- dat[[z_col]] - mean(dat[[z_col]])
  mom <- hetid::compute_identification_moments(
    w1b, w2b, matrix(zb, ncol = 1, dimnames = list(NULL, z_col))
  )
  qs0 <- build_pipeline_quadratic_system(
    boot_gamma, rep(0, ncol(boot_gamma)), mom
  )
  point0 <- solve_point_identification(qs0$components)
  if (is.null(point0)) {
    stop("rank-deficient tau = 0 system in resampled draw")
  }
  point <- c(
    hetid::recover_structural_coefficients(b1r, b2r, point0$theta),
    point0$theta
  )
  coarse <- sweep_fixed_gamma(boot_gamma, mom, boot_tau_grid, "boot")
  ts <- tau_star_fixed(boot_gamma, mom, coarse, iters = 15L)
  bounds <- lapply(boot_taus, function(tau) {
    # at or beyond this draw's tau* the joint set is no longer certified
    # bounded; record missing sides instead of paying for the slow
    # certify-unbounded solves (width-0 design rows are IM-gated anyway)
    if (tau >= ts$tau_star) {
      return(list(
        lower = rep(NA_real_, length(boot_coefs)),
        upper = rep(NA_real_, length(boot_coefs))
      ))
    }
    it <- coef_interval_tables(boot_gamma, tau, mom, b1r, b2r)
    tab <- rbind(it$beta1, it$theta)
    ok <- tab$status == "bounded"
    list(
      lower = ifelse(ok, tab$set_lower, NA_real_),
      upper = ifelse(ok, tab$set_upper, NA_real_)
    )
  })
  list(point = point, bounds = bounds, tau_star = ts$tau_star, capped = ts$capped)
}

na_draw <- list(
  point = rep(NA_real_, length(boot_coefs)),
  bounds = rep(
    list(list(
      lower = rep(NA_real_, length(boot_coefs)),
      upper = rep(NA_real_, length(boot_coefs))
    )),
    length(boot_taus)
  ),
  tau_star = NA_real_, capped = FALSE
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
    boot_draw(boot_idx[[b]]),
    error = function(e) conditionMessage(e)
  )
  if (b %% 25L == 0L) {
    cat(sprintf(
      "  endpoint bootstrap draw %d of %d (%.1f min elapsed)\n", b, boot_reps,
      as.numeric(difftime(Sys.time(), boot_t0, units = "mins"))
    ))
  }
}

# failed draws keep their error messages so causes are visible, then become
# NA rows; a large failure share means the resampling itself is broken, not
# statistical unboundedness, so the pipeline stops rather than reporting
# bands built from the surviving minority
failed <- vapply(boot_raw, is.character, logical(1))
n_failed <- sum(failed)
failure_causes <- if (n_failed > 0L) table(unlist(boot_raw[failed])) else NULL
if (n_failed > 0L) {
  cat("  failed draws by cause:\n")
  print(failure_causes)
}
if (n_failed > boot_reps %/% 4L) {
  stop("endpoint bootstrap: ", n_failed, " of ", boot_reps, " draws failed")
}
boot_raw[failed] <- list(na_draw)

point_draws <- do.call(rbind, lapply(boot_raw, `[[`, "point"))
colnames(point_draws) <- boot_coefs
tau_star_draws <- vapply(boot_raw, `[[`, numeric(1), "tau_star")

# per-display-tau endpoint inference against the stored full-sample tables
inference <- lapply(seq_along(boot_taus), function(j) {
  lower <- do.call(rbind, lapply(boot_raw, function(d) d$bounds[[j]]$lower))
  upper <- do.call(rbind, lapply(boot_raw, function(d) d$bounds[[j]]$upper))
  st <- set_id_mean_eq$set_tables[[j]]
  endpoint_inference(lower, upper, rbind(st$beta1, st$theta), alpha = 0.10)
})
names(inference) <- names(set_id_mean_eq$set_tables)

set_id_boot <- list(
  b_reps = boot_reps, block = boot_block, seed = boot_seed,
  point_draws = point_draws,
  point_se = apply(point_draws, 2, function(x) stats::sd(x[is.finite(x)])),
  tau_star_draws = tau_star_draws,
  tau_star_band = boot_band(tau_star_draws),
  tau_star_share_below = mean(
    tau_star_draws < set_id_mean_eq$tau_baseline,
    na.rm = TRUE
  ),
  n_capped = sum(vapply(boot_raw, `[[`, logical(1), "capped")),
  n_failed = n_failed,
  failure_causes = failure_causes,
  inference = inference
)

cat(sprintf(
  "endpoint bootstrap: B = %d, block = %d, %.1f min; tau* band [%.3g, %.3g] (n = %d)\n",
  boot_reps, boot_block,
  as.numeric(difftime(Sys.time(), boot_t0, units = "mins")),
  set_id_boot$tau_star_band[["p05"]], set_id_boot$tau_star_band[["p95"]],
  set_id_boot$tau_star_band[["n"]]
))
cat(sprintf(
  "  %d failed draws, %d capped tau* draws; bounded at baseline in %.0f%% of draws\n",
  set_id_boot$n_failed, set_id_boot$n_capped,
  100 * (1 - set_id_boot$tau_star_share_below)
))
print(set_id_boot$inference[[1]], digits = 3)

rm(
  boot_coefs, boot_gamma, boot_taus, boot_tau_grid, boot_draw, na_draw,
  boot_idx, boot_t0, boot_raw, failed, n_failed, failure_causes,
  point_draws, tau_star_draws, inference, b
)
