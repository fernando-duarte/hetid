# tau* (critical slack) for the fixed VFCI weights, with a moving-block bootstrap
# band. This is honest sampling-uncertainty quantification for the identification
# frontier -- it does NOT select the weights on the data (the rejected sample
# split), it resamples quarters and recomputes tau*, the 2-year bound, and the
# conditioning of the fixed-VFCI system. Deterministic given seed 123.

source(here::here("scripts/utils/common_settings.R"))
set.seed(SEED)
out_dir <- file.path(OUTPUT_TEMP_DIR, "results_companion")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

inputs <- load_identification_inputs()
resid <- compute_identification_residuals(inputs$data)
w1 <- resid$w1
w2 <- resid$w2
pcs <- resid$pcs_aligned
n <- length(w1)
gamma_vfci <- get_baseline_gamma(
  "vfci",
  n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS, n_components = ncol(w2)
)
coarse_taus <- c(0.005, 0.01, 0.02, 0.03, 0.04, 0.05, 0.08, 0.12)

tau_star_of <- function(mom) {
  coarse <- sweep_fixed_gamma(gamma_vfci, mom, coarse_taus, "coarse")
  tau_star_fixed(gamma_vfci, mom, coarse, iters = 25L)$tau_star
}
two_year_width <- function(mom) {
  b <- solve_all_profile_bounds(
    build_pipeline_quadratic_system(gamma_vfci, rep(BASELINE_TAU, ncol(w2)), mom)$quadratic
  )
  b$width[1]
}

# --- point estimates (verify tau* unchanged by the status-branch fix) ---
mom0 <- compute_identification_moments(w1, w2, pcs)
ts0 <- tau_star_of(mom0)
cond0 <- solve_point_identification(
  build_pipeline_quadratic_system(gamma_vfci, rep(0, ncol(w2)), mom0)$components
)$cond
cat("=== point estimates (fixed VFCI) ===\n")
cat("tau* =", signif(ts0, 5), " (expect ~0.0304)\n")
cat("cond(Q) =", signif(cond0, 6), "\n")
cat(
  sprintf("2y bound width at tau=%s =", BASELINE_TAU),
  signif(two_year_width(mom0), 5), "\n\n"
)

# --- moving-block bootstrap ---
b_reps <- 200L
block <- 15L
mbb_index <- function(nn, bl) {
  nblocks <- ceiling(nn / bl)
  starts <- sample.int(nn - bl + 1L, nblocks, replace = TRUE)
  idx <- unlist(lapply(starts, function(s) s:(s + bl - 1L)))
  idx[seq_len(nn)]
}
ts_b <- rep(NA_real_, b_reps)
w2y_b <- rep(NA_real_, b_reps)
cond_b <- rep(NA_real_, b_reps)
for (b in seq_len(b_reps)) {
  idx <- mbb_index(n, block)
  mom <- tryCatch(
    compute_identification_moments(w1[idx], w2[idx, , drop = FALSE], pcs[idx, , drop = FALSE]),
    error = function(e) NULL
  )
  if (is.null(mom)) next
  ts_b[b] <- tryCatch(tau_star_of(mom), error = function(e) NA_real_)
  w2y_b[b] <- tryCatch(two_year_width(mom), error = function(e) NA_real_)
  cond_b[b] <- tryCatch(
    {
      pt <- solve_point_identification(
        build_pipeline_quadratic_system(gamma_vfci, rep(0, ncol(w2)), mom)$components
      )
      if (is.null(pt)) NA_real_ else pt$cond
    },
    error = function(e) NA_real_
  )
}
qs_report <- function(x, nm) {
  x <- x[is.finite(x)]
  cat(sprintf(
    paste0(
      "%-14s n=%d  median=%.4g  [p05,p95]=[%.4g, %.4g]  ",
      "share(tau*<", BASELINE_TAU, ")=%.2f\n"
    ),
    nm, length(x), stats::median(x),
    stats::quantile(x, 0.05), stats::quantile(x, 0.95),
    if (nm == "tau*") mean(x < BASELINE_TAU) else NA
  ))
}
cat("=== moving-block bootstrap (B =", b_reps, ", block =", block, ") ===\n")
qs_report(ts_b, "tau*")
qs_report(cond_b, "cond(Q)")
cat(
  "2y width: finite in", sum(is.finite(w2y_b)), "of", b_reps,
  "draws; median(finite) =", signif(stats::median(w2y_b[is.finite(w2y_b)]), 4), "\n"
)

saveRDS(
  list(
    tau_star_point = ts0, cond_point = cond0,
    tau_star_boot = ts_b, cond_boot = cond_b, w2y_boot = w2y_b,
    b_reps = b_reps, block = block
  ),
  file.path(out_dir, "taustar_bootstrap.rds")
)
cat("\nsaved:", file.path(out_dir, "taustar_bootstrap.rds"), "\n")
