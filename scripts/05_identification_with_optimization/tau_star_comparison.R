# Identification strength via tau* -- the slack at which the identified set
# goes bounded -> unbounded -- compared across gamma choices:
#   VFCI (rank-1 baseline), reduced-form (rank-N benchmark from the Y2-on-PC
#   slopes), and optimized (gamma re-optimized at each candidate tau).
# tau* is scale-free and needs no cherry-picked tau, so it is the honest
# quantitative summary of "what optimization buys" (it EXTENDS tau*).
# Single home of the tau* computation, including the VFCI deep dive: a fine
# tau grid plus the recorded bisection evaluations resolve the width blow-up
# as tau -> tau*, every evaluation carries the house bounded / unbounded /
# unreliable status, and the curvature (recession) metric is computed as a
# degeneracy DIAGNOSTIC -- not a second tau* locator (for the rank-1 VFCI
# gamma its normalized margin is negligible on both sides of tau*,
# certifying a knife edge it cannot resolve).
# NOTE: each A_i = Q_iQ_i' - d_i S_i^(2) has at most one positive eigenvalue
# (rank-1 PSD minus d_i>=0 times a Gram/PSD matrix); boundedness is the JOINT
# recession condition that the positive-curvature directions cover R^I, a
# (gamma, tau) property -- not implied by rank(gamma).
# Outputs (machine-readable, temp/identification_optimized): raw sweep CSVs
# and self-describing rds bundles. The paper artifacts are written by
# tau_star_report.R from those bundles.

source(here::here("scripts/utils/common_settings.R"))
# Cap-aware tau* formatting (.fmt_tau_star) shared with the report builders.
source(here::here(
  "scripts/05_identification_with_optimization/tau_star_report_utils.R"
))

COARSE_TAUS <- seq(0, 0.2, by = 0.005)
FINE_N <- 10L
BISECT_ITERS <- 40L
RECESSION_N_DIR <- 8000L
RECESSION_SEED <- 1L
OPT_TAU_LO <- 0.2
# Admissible slack is tau in [0,1): the correlation-bound interpretation fails
# at tau >= 1 (and hetid rejects it), so the optimizer search is capped
# strictly below 1. A cap-censored tau* is displayed as ">= cap", never exact.
OPT_TAU_CAP <- 0.99

temp_dir <- file.path(OUTPUT_TEMP_DIR, "identification_optimized")
dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

# Sweep + bisect one FIXED gamma: coarse grid (with the curvature diagnostic),
# fine grid inside the bounded region, and the bisection trace, all stacked
# into one sweep table alongside that gamma's tau*.
analyze_fixed_gamma <- function(label, gamma, moments) {
  coarse <- sweep_fixed_gamma(gamma, moments, COARSE_TAUS, "coarse")
  rec <- lapply(coarse$tau, function(t) {
    if (t == 0) {
      return(list(raw = NA_real_, normalized = NA_real_))
    }
    recession_metric(gamma, t, moments,
      n_dir = RECESSION_N_DIR, seed = RECESSION_SEED
    )
  })
  coarse$recession_raw <- vapply(rec, `[[`, numeric(1), "raw")
  coarse$recession_normalized <- vapply(rec, `[[`, numeric(1), "normalized")
  ts <- tau_star_fixed(gamma, moments, coarse, iters = BISECT_ITERS)
  fine <- sweep_fixed_gamma(gamma, moments, fine_tau_grid(coarse, FINE_N), "fine")
  extra <- rbind(fine, ts$trace)
  if (!is.null(extra)) {
    extra$recession_raw <- NA_real_
    extra$recession_normalized <- NA_real_
  }
  sweep <- rbind(coarse, extra)
  sweep$gamma <- label
  list(sweep = sweep, tau_star = ts$tau_star, capped = ts$capped)
}

run_tau_star_analysis <- function() {
  cli_h1("Identification strength: tau* across gamma choices")

  inp <- load_identification_inputs()
  resid <- compute_identification_residuals(inp$data)
  moments <- compute_identification_moments(resid$w1, resid$w2, resid$pcs_aligned)
  n_comp <- nrow(inp$lookup)
  gamma_base <- resolve_baseline_gamma(
    baseline_gamma_method(), moments
  )
  base_label <- if (identical(attr(gamma_base, "method"), "vfci")) {
    "VFCI (rank-1)"
  } else {
    paste0(attr(gamma_base, "method"), " (rank-", qr(gamma_base)$rank, ")")
  }
  fixed <- list()
  fixed[[base_label]] <- gamma_base
  n_inst <- nrow(moments$r_i_0)
  # Reduced-form benchmark from the Y2-on-PC slopes (beta2R), the
  # higher-rank fixed gamma that isolates "what optimization buys beyond rank".
  gamma_rf_mat <- build_reduced_form_gamma(resid$w2_coefficients)
  if (nrow(gamma_rf_mat) == n_inst) {
    rf_rank <- qr(gamma_rf_mat)$rank
    fixed[[paste0("reduced-form (rank-", rf_rank, ")")]] <- gamma_rf_mat
  } else {
    cli_alert_warning(paste0(
      "reduced-form gamma skipped: its J = ", nrow(gamma_rf_mat),
      " rows do not match the instrument count ", n_inst
    ))
  }

  cli_h2("tau* (fixed gammas; sweep + bisection)")
  fixed_res <- lapply(names(fixed), function(lbl) {
    r <- analyze_fixed_gamma(lbl, fixed[[lbl]], moments)
    ts_str <- .fmt_tau_star(r$tau_star, r$capped)
    cli_alert_info("tau*({lbl}) = {ts_str}")
    r
  })
  names(fixed_res) <- names(fixed)
  ts_vfci <- fixed_res[[base_label]]$tau_star

  cli_h2("tau* (optimizer; bracket + bisection)")
  opt <- tau_star_optimized(fixed[[base_label]], moments,
    whiten = list(z = resid$pcs_aligned),
    tau_lo = OPT_TAU_LO, cap = OPT_TAU_CAP
  )
  if (is.na(opt$tau_star)) {
    cli_alert_warning(
      "Optimizer not bounded even at tau={OPT_TAU_LO}; tau*(opt) < {OPT_TAU_LO}"
    )
  } else {
    opt_str <- .fmt_tau_star(opt$tau_star, opt$capped)
    cli_alert_success("tau*(optimized) = {opt_str}")
  }
  if (is.finite(opt$tau_star) && is.finite(ts_vfci) && ts_vfci > 0) {
    # A cap-censored tau*(opt) makes the extension factor a lower bound.
    ratio_str <- paste0(
      if (isTRUE(opt$capped)) ">= " else "~",
      round(opt$tau_star / ts_vfci, 1), "x"
    )
    cli_alert_success("Optimization extends tau* by {ratio_str} vs {base_label}")
  }

  sweep <- do.call(rbind, lapply(fixed_res, `[[`, "sweep"))
  sweep <- sweep[order(sweep$gamma, sweep$tau), c(
    "tau", "gamma", "total_width", "all_bounded", "all_valid", "status",
    "grid", "recession_raw", "recession_normalized"
  )]
  rownames(sweep) <- NULL
  vf_rec <- abs(sweep$recession_normalized[sweep$gamma == base_label])
  cli_alert_info(paste0(
    "Curvature diagnostic (VFCI, normalized): within ",
    formatC(max(vf_rec, na.rm = TRUE), format = "e", digits = 1),
    " of zero across the grid -- a degeneracy check, not a tau* locator"
  ))

  tau_stars <- data.frame(
    gamma = c(names(fixed_res), "optimized"),
    tau_star = c(vapply(fixed_res, `[[`, numeric(1), "tau_star"), opt$tau_star),
    capped = c(vapply(fixed_res, `[[`, logical(1), "capped"), opt$capped),
    stringsAsFactors = FALSE
  )
  res <- list(
    tau_stars = tau_stars, sweep = sweep,
    settings = list(
      coarse_taus = COARSE_TAUS, fine_n = FINE_N, bisect_iters = BISECT_ITERS,
      n_dir = RECESSION_N_DIR, recession_seed = RECESSION_SEED,
      n_starts = TAU_STAR_N_STARTS, opt_tau_lo = OPT_TAU_LO,
      opt_tau_cap = OPT_TAU_CAP
    )
  )

  write.csv(sweep, file.path(temp_dir, "tau_star_sweep.csv"),
    row.names = FALSE
  )
  saveRDS(res, file.path(temp_dir, "tau_star_comparison.rds"))

  cli_h2("Summary")
  print(tau_stars, digits = 4)
  cli_alert_success(
    "Saved tau* sweep + results to {.path {temp_dir}}"
  )
  res
}

run_tau_star_analysis()
